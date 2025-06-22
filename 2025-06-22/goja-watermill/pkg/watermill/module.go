package watermill

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/dop251/goja"
	"github.com/example/goja-watermill/pkg/pubsub"
	"github.com/rs/zerolog"
)

// Module represents the main Goja-Watermill integration module
type Module struct {
	vm     *goja.Runtime
	router *message.Router
	logger zerolog.Logger
	
	// Thread safety for JS execution
	jsExecutionMutex sync.Mutex
	jsExecutionChan  chan func()
	
	// PubSub instances
	pubsubInstances map[string]*PubSubInstance
	pubsubMutex     sync.RWMutex
	
	// CQRS facade
	cqrsFacade *CQRSFacade
	
	// Context for lifecycle management
	ctx    context.Context
	cancel context.CancelFunc
	
	// Router lifecycle
	routerStarted bool
	routerMutex   sync.Mutex
}

// PubSubInstance represents a single pub/sub instance
type PubSubInstance struct {
	id          string
	pubsubType  string
	subscriber  message.Subscriber
	publisher   message.Publisher
	config      map[string]interface{}
	handlers    map[string]*HandlerInfo
	handlersMutex sync.RWMutex
	middleware  []MiddlewareFunc
	module      *Module
}

// HandlerInfo stores information about a registered handler
type HandlerInfo struct {
	id       string
	topic    string
	jsFunc   goja.Callable
	handler  *message.Handler
	stopped  bool
}

// MiddlewareFunc represents a JavaScript middleware function
type MiddlewareFunc struct {
	jsFunc goja.Callable
	name   string
}

// JSMessage represents a message object exposed to JavaScript
type JSMessage struct {
	UUID     string                 `json:"uuid"`
	Payload  string                 `json:"payload"`
	Metadata map[string]string      `json:"metadata"`
	rawMsg   *message.Message
	module   *Module
}

// NewModule creates a new Goja-Watermill integration module
func NewModule(vm *goja.Runtime, logger zerolog.Logger) (*Module, error) {
	ctx, cancel := context.WithCancel(context.Background())
	
	// Create Watermill router with zerolog
	watermillLogger := watermill.NewStdLogger(false, false)
	router, err := message.NewRouter(message.RouterConfig{}, watermillLogger)
	if err != nil {
		cancel()
		return nil, fmt.Errorf("failed to create router: %w", err)
	}
	
	module := &Module{
		vm:              vm,
		router:          router,
		logger:          logger,
		ctx:             ctx,
		cancel:          cancel,
		pubsubInstances: make(map[string]*PubSubInstance),
		jsExecutionChan: make(chan func(), 100), // Buffered channel for JS execution
	}
	
	// Start JS execution goroutine
	go module.jsExecutionLoop()
	
	// Bind the module to JavaScript
	if err := module.bindToJS(); err != nil {
		cancel()
		return nil, fmt.Errorf("failed to bind to JS: %w", err)
	}
	
	logger.Info().Msg("Goja-Watermill module initialized successfully")
	
	return module, nil
}

// jsExecutionLoop handles all JavaScript execution in a single goroutine
func (m *Module) jsExecutionLoop() {
	for {
		select {
		case fn := <-m.jsExecutionChan:
			func() {
				defer func() {
					if r := recover(); r != nil {
						m.logger.Error().
							Interface("panic", r).
							Msg("Panic in JS execution")
					}
				}()
				fn()
			}()
		case <-m.ctx.Done():
			return
		}
	}
}

// executeInJS safely executes a function in the JavaScript runtime
func (m *Module) executeInJS(fn func()) {
	done := make(chan struct{})
	m.jsExecutionChan <- func() {
		fn()
		close(done)
	}
	<-done
}

// executeInJSWithResult safely executes a function in the JavaScript runtime and returns a result
func (m *Module) executeInJSWithResult(fn func() (interface{}, error)) (interface{}, error) {
	var result interface{}
	var err error
	done := make(chan struct{})
	
	m.jsExecutionChan <- func() {
		result, err = fn()
		close(done)
	}
	<-done
	
	return result, err
}

// bindToJS binds the watermill module to the JavaScript runtime
func (m *Module) bindToJS() error {
	watermillObj := m.vm.NewObject()
	
	// Bind createPubSub function
	watermillObj.Set("createPubSub", m.createPubSub)
	
	// Bind CQRS functions
	watermillObj.Set("createCQRS", m.createCQRS)
	
	// Bind start function
	watermillObj.Set("start", m.start)
	
	// Bind stop function
	watermillObj.Set("stop", m.stop)
	
	// Set the watermill object as a global
	m.vm.Set("watermill", watermillObj)
	
	return nil
}

// createPubSub creates a new PubSub instance
func (m *Module) createPubSub(call goja.FunctionCall) goja.Value {
	if len(call.Arguments) < 1 {
		panic(m.vm.NewTypeError("createPubSub requires at least 1 argument (type)"))
	}
	
	pubsubType := call.Arguments[0].String()
	
	var config map[string]interface{}
	if len(call.Arguments) > 1 && !goja.IsUndefined(call.Arguments[1]) {
		configVal := call.Arguments[1].Export()
		if configMap, ok := configVal.(map[string]interface{}); ok {
			config = configMap
		}
	}
	
	if config == nil {
		config = make(map[string]interface{})
	}
	
	instance, err := m.createPubSubInstance(pubsubType, config)
	if err != nil {
		panic(m.vm.NewGoError(err))
	}
	
	return m.pubSubInstanceToJS(instance)
}

// createPubSubInstance creates a new PubSub instance with the specified type and config
func (m *Module) createPubSubInstance(pubsubType string, config map[string]interface{}) (*PubSubInstance, error) {
	instanceID := fmt.Sprintf("%s_%d", pubsubType, time.Now().UnixNano())
	var subscriber message.Subscriber
	var publisher message.Publisher
	
	switch pubsubType {
	case "memory":
		// Create enhanced in-memory pub/sub
		memConfig := pubsub.DefaultMemoryPubSubConfig()
		
		// Override with user config
		if persistent, ok := config["persistent"].(bool); ok {
			memConfig.Persistent = persistent
		}
		if buffer, ok := config["output_channel_buffer"].(float64); ok {
			memConfig.OutputChannelBuffer = int64(buffer)
		}
		if blockPublish, ok := config["block_publish_until_subscriber_ack"].(bool); ok {
			memConfig.BlockPublishUntilSubscriberAck = blockPublish
		}
		if maxRetries, ok := config["max_retries"].(float64); ok {
			memConfig.MaxRetries = int(maxRetries)
		}
		if retryDelayMs, ok := config["retry_delay_ms"].(float64); ok {
			memConfig.RetryDelay = time.Duration(retryDelayMs) * time.Millisecond
		}
		if enableMetrics, ok := config["enable_metrics"].(bool); ok {
			memConfig.EnableMetrics = enableMetrics
		}
		
		memPubSub, err := pubsub.NewMemoryPubSub(memConfig, m.logger.With().Str("pubsub_id", instanceID).Logger())
		if err != nil {
			return nil, fmt.Errorf("failed to create memory pub/sub: %w", err)
		}
		
		subscriber = memPubSub
		publisher = memPubSub
		
	default:
		return nil, fmt.Errorf("unsupported pubsub type: %s", pubsubType)
	}
	
	instance := &PubSubInstance{
		id:         instanceID,
		pubsubType: pubsubType,
		subscriber: subscriber,
		publisher:  publisher,
		config:     config,
		handlers:   make(map[string]*HandlerInfo),
		module:     m,
	}
	
	m.pubsubMutex.Lock()
	m.pubsubInstances[instanceID] = instance
	m.pubsubMutex.Unlock()
	
	m.logger.Info().
		Str("instance_id", instanceID).
		Str("type", pubsubType).
		Msg("Created PubSub instance")
	
	return instance, nil
}

// pubSubInstanceToJS converts a PubSubInstance to a JavaScript object
func (m *Module) pubSubInstanceToJS(instance *PubSubInstance) goja.Value {
	obj := m.vm.NewObject()
	
	// Bind methods
	obj.Set("subscribe", instance.subscribe)
	obj.Set("publish", instance.publish)
	obj.Set("unsubscribe", instance.unsubscribe)
	obj.Set("useMiddleware", instance.useMiddleware)
	obj.Set("addHandler", instance.addHandler)
	obj.Set("getId", func() string { return instance.id })
	obj.Set("getType", func() string { return instance.pubsubType })
	
	return obj
}

// start starts the Watermill router
func (m *Module) start(call goja.FunctionCall) goja.Value {
	m.routerMutex.Lock()
	defer m.routerMutex.Unlock()
	
	if m.routerStarted {
		m.logger.Warn().Msg("Router already started")
		return goja.Undefined()
	}
	
	m.logger.Info().Msg("Starting Watermill router")
	
	go func() {
		if err := m.router.Run(m.ctx); err != nil {
			m.logger.Error().Err(err).Msg("Router run error")
		}
	}()
	
	// Wait for router to be fully running
	select {
	case <-m.router.Running():
		m.logger.Info().Msg("Watermill router is now running")
		m.routerStarted = true
	case <-m.ctx.Done():
		m.logger.Error().Msg("Context cancelled while waiting for router to start")
		return goja.Undefined()
	case <-time.After(5 * time.Second):
		m.logger.Error().Msg("Timeout waiting for router to start")
		return goja.Undefined()
	}
	
	return goja.Undefined()
}

// stop stops the module and all its components
func (m *Module) stop(call goja.FunctionCall) goja.Value {
	m.logger.Info().Msg("Stopping Goja-Watermill module")
	
	// Stop all handlers
	m.pubsubMutex.RLock()
	for _, instance := range m.pubsubInstances {
		instance.stopAllHandlers()
	}
	m.pubsubMutex.RUnlock()
	
	// Stop router
	if err := m.router.Close(); err != nil {
		m.logger.Error().Err(err).Msg("Error closing router")
	}
	
	// Cancel context
	m.cancel()
	
	m.logger.Info().Msg("Goja-Watermill module stopped")
	
	return goja.Undefined()
}

// Close gracefully shuts down the module
func (m *Module) Close() error {
	m.stop(goja.FunctionCall{})
	return nil
}

