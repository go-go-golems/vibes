package watermill

import (
	"fmt"
	"sync"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/dop251/goja"
	"github.com/rs/zerolog"
)

// RouterHandler represents a Watermill router handler with JavaScript integration
type RouterHandler struct {
	name           string
	subscribeTopic string
	publishTopic   string
	handlerFunc    goja.Callable
	middleware     []goja.Callable
	module         *Module
	handler        *message.Handler
	stopped        bool
	mutex          sync.RWMutex
	logger         zerolog.Logger
}

// HandlerConfig represents configuration for a router handler
type HandlerConfig struct {
	Name           string                 `json:"name"`
	SubscribeTopic string                 `json:"subscribeTopic"`
	PublishTopic   string                 `json:"publishTopic,omitempty"`
	Options        map[string]interface{} `json:"options,omitempty"`
}

// addHandler adds a new router handler with publishing capability
func (p *PubSubInstance) addHandler(call goja.FunctionCall) goja.Value {
	if len(call.Arguments) < 2 {
		panic(p.module.vm.NewGoError(fmt.Errorf("addHandler requires at least 2 arguments: (config, handlerFunc, [publisherFunc])")))
	}

	// Parse handler configuration
	configArg := call.Arguments[0].Export()
	configMap, ok := configArg.(map[string]interface{})
	if !ok {
		panic(p.module.vm.NewGoError(fmt.Errorf("first argument must be a configuration object")))
	}

	config := HandlerConfig{}
	if name, exists := configMap["name"]; exists {
		config.Name = fmt.Sprintf("%v", name)
	} else {
		panic(p.module.vm.NewGoError(fmt.Errorf("handler name is required")))
	}

	if topic, exists := configMap["subscribeTopic"]; exists {
		config.SubscribeTopic = fmt.Sprintf("%v", topic)
	} else {
		panic(p.module.vm.NewGoError(fmt.Errorf("subscribeTopic is required")))
	}

	if topic, exists := configMap["publishTopic"]; exists {
		config.PublishTopic = fmt.Sprintf("%v", topic)
	}

	// Get handler function
	handlerFunc, ok := goja.AssertFunction(call.Arguments[1])
	if !ok {
		panic(p.module.vm.NewGoError(fmt.Errorf("second argument must be a function")))
	}

	// Optional publisher function for custom publishing logic
	var publisherFunc goja.Callable
	if len(call.Arguments) > 2 && !goja.IsUndefined(call.Arguments[2]) {
		publisherFunc, ok = goja.AssertFunction(call.Arguments[2])
		if !ok {
			panic(p.module.vm.NewGoError(fmt.Errorf("third argument must be a function or undefined")))
		}
	}

	// Create router handler
	routerHandler := &RouterHandler{
		name:           config.Name,
		subscribeTopic: config.SubscribeTopic,
		publishTopic:   config.PublishTopic,
		handlerFunc:    handlerFunc,
		middleware:     []goja.Callable{},
		module:         p.module,
		logger:         p.module.logger.With().Str("handler_name", config.Name).Logger(),
	}

	// Create Watermill handler function
	var watermillHandler message.HandlerFunc
	if config.PublishTopic != "" && publisherFunc != nil {
		// Handler with custom publishing logic
		watermillHandler = func(msg *message.Message) ([]*message.Message, error) {
			return routerHandler.handleMessageWithPublisher(msg, publisherFunc)
		}
	} else if config.PublishTopic != "" {
		// Handler with automatic publishing
		watermillHandler = func(msg *message.Message) ([]*message.Message, error) {
			return routerHandler.handleMessageWithAutoPublish(msg)
		}
	} else {
		// No-publisher handler
		watermillHandler = func(msg *message.Message) ([]*message.Message, error) {
			return routerHandler.handleMessage(msg)
		}
	}

	// Add handler to router
	var handler *message.Handler
	if config.PublishTopic != "" {
		handler = p.module.router.AddHandler(
			config.Name,
			config.SubscribeTopic,
			p.subscriber,
			config.PublishTopic,
			p.publisher,
			watermillHandler,
		)
	} else {
		handler = p.module.router.AddNoPublisherHandler(
			config.Name,
			config.SubscribeTopic,
			p.subscriber,
			func(msg *message.Message) error {
				_, err := watermillHandler(msg)
				return err
			},
		)
	}

	routerHandler.handler = handler
	
	// Store handler with updated HandlerInfo structure
	p.handlers[config.Name] = &HandlerInfo{
		id:      config.Name,
		topic:   config.SubscribeTopic,
		jsFunc:  handlerFunc,
		handler: handler,
		stopped: false,
	}

	p.module.logger.Info().
		Str("handler_name", config.Name).
		Str("subscribe_topic", config.SubscribeTopic).
		Str("publish_topic", config.PublishTopic).
		Msg("Added router handler")

	// Return handler object for JavaScript
	handlerObj := p.module.vm.NewObject()
	handlerObj.Set("name", config.Name)
	handlerObj.Set("subscribeTopic", config.SubscribeTopic)
	handlerObj.Set("publishTopic", config.PublishTopic)
	handlerObj.Set("addMiddleware", routerHandler.addMiddleware)
	handlerObj.Set("stop", routerHandler.stop)
	handlerObj.Set("isRunning", routerHandler.isRunning)

	return handlerObj
}

// handleMessage processes a message without publishing
func (rh *RouterHandler) handleMessage(msg *message.Message) ([]*message.Message, error) {
	rh.logger.Debug().
		Str("message_uuid", msg.UUID).
		Str("topic", rh.subscribeTopic).
		Msg("Processing message in router handler")

	// Create JavaScript message object
	jsMsg := &JSMessage{
		UUID:     msg.UUID,
		Payload:  string(msg.Payload),
		Metadata: make(map[string]string),
		rawMsg:   msg,
		module:   rh.module,
	}

	// Copy metadata
	for key, value := range msg.Metadata {
		jsMsg.Metadata[key] = value
	}

	// Execute middleware chain and handler
	err := rh.executeJSHandler(jsMsg)
	if err != nil {
		rh.logger.Error().Err(err).
			Str("message_uuid", msg.UUID).
			Msg("Router handler failed")
		return nil, fmt.Errorf("JS handler error: %w", err)
	}

	rh.logger.Debug().
		Str("message_uuid", msg.UUID).
		Msg("Router handler completed successfully")

	return nil, nil
}

// handleMessageWithAutoPublish processes a message and automatically publishes returned messages
func (rh *RouterHandler) handleMessageWithAutoPublish(msg *message.Message) ([]*message.Message, error) {
	rh.logger.Debug().
		Str("message_uuid", msg.UUID).
		Str("topic", rh.subscribeTopic).
		Str("publish_topic", rh.publishTopic).
		Msg("Processing message in router handler with auto-publish")

	// Create JavaScript message object
	jsMsg := &JSMessage{
		UUID:     msg.UUID,
		Payload:  string(msg.Payload),
		Metadata: make(map[string]string),
		rawMsg:   msg,
		module:   rh.module,
	}

	// Copy metadata
	for key, value := range msg.Metadata {
		jsMsg.Metadata[key] = value
	}

	var resultMessages []*message.Message

	// Execute middleware chain and handler
	result, err := rh.executeJSHandlerWithResult(jsMsg)
	if err != nil {
		rh.logger.Error().Err(err).
			Str("message_uuid", msg.UUID).
			Msg("Router handler failed")
		return nil, fmt.Errorf("JS handler error: %w", err)
	}

	// Process returned messages
	if result != nil {
		resultMessages = rh.extractReturnedMessages(result)
	}

	rh.logger.Debug().
		Str("message_uuid", msg.UUID).
		Int("returned_messages", len(resultMessages)).
		Msg("Router handler completed successfully")

	return resultMessages, nil
}

// handleMessageWithPublisher processes a message with custom publishing logic
func (rh *RouterHandler) handleMessageWithPublisher(msg *message.Message, publisherFunc goja.Callable) ([]*message.Message, error) {
	rh.logger.Debug().
		Str("message_uuid", msg.UUID).
		Str("topic", rh.subscribeTopic).
		Msg("Processing message in router handler with custom publisher")

	// Create JavaScript message object
	jsMsg := &JSMessage{
		UUID:     msg.UUID,
		Payload:  string(msg.Payload),
		Metadata: make(map[string]string),
		rawMsg:   msg,
		module:   rh.module,
	}

	// Copy metadata
	for key, value := range msg.Metadata {
		jsMsg.Metadata[key] = value
	}

	var resultMessages []*message.Message

	// Execute handler and publisher
	rh.module.executeInJS(func() {
		// Apply middleware and call handler
		processedMsg := jsMsg
		for _, middleware := range rh.middleware {
			result, err := middleware(goja.Undefined(), processedMsg.ToJSObject(rh.module.vm))
			if err != nil {
				panic(rh.module.vm.NewGoError(fmt.Errorf("middleware error: %w", err)))
			}
			
			// Check if middleware returned a modified message
			if !goja.IsUndefined(result) {
				if msgObj, ok := result.(*goja.Object); ok {
					if newMsg := rh.extractJSMessage(msgObj); newMsg != nil {
						processedMsg = newMsg
					}
				}
			}
		}

		// Call handler function
		handlerResult, err := rh.handlerFunc(goja.Undefined(), processedMsg.ToJSObject(rh.module.vm))
		if err != nil {
			panic(rh.module.vm.NewGoError(err))
		}

		// Call custom publisher function
		publishResult, err := publisherFunc(goja.Undefined(), handlerResult)
		if err != nil {
			panic(rh.module.vm.NewGoError(fmt.Errorf("publisher function error: %w", err)))
		}

		// Process published messages
		if !goja.IsUndefined(publishResult) {
			resultMessages = rh.extractReturnedMessages(publishResult)
		}
	})

	rh.logger.Debug().
		Str("message_uuid", msg.UUID).
		Int("published_messages", len(resultMessages)).
		Msg("Router handler with custom publisher completed successfully")

	return resultMessages, nil
}

// executeJSHandler executes the JavaScript handler with middleware
func (rh *RouterHandler) executeJSHandler(jsMsg *JSMessage) error {
	var handlerErr error
	rh.module.executeInJS(func() {
		// Apply middleware
		processedMsg := jsMsg
		for _, middleware := range rh.middleware {
			result, err := middleware(goja.Undefined(), processedMsg.ToJSObject(rh.module.vm))
			if err != nil {
				handlerErr = fmt.Errorf("middleware error: %w", err)
				return
			}
			
			// Check if middleware returned a modified message
			if !goja.IsUndefined(result) {
				if msgObj, ok := result.(*goja.Object); ok {
					if newMsg := rh.extractJSMessage(msgObj); newMsg != nil {
						processedMsg = newMsg
					}
				}
			}
		}

		// Call handler function
		_, err := rh.handlerFunc(goja.Undefined(), processedMsg.ToJSObject(rh.module.vm))
		if err != nil {
			handlerErr = err
		}
	})
	
	return handlerErr
}

// executeJSHandlerWithResult executes the JavaScript handler and returns the result
func (rh *RouterHandler) executeJSHandlerWithResult(jsMsg *JSMessage) (goja.Value, error) {
	var result goja.Value
	var handlerErr error
	
	rh.module.executeInJS(func() {
		// Apply middleware
		processedMsg := jsMsg
		for _, middleware := range rh.middleware {
			middlewareResult, err := middleware(goja.Undefined(), processedMsg.ToJSObject(rh.module.vm))
			if err != nil {
				handlerErr = fmt.Errorf("middleware error: %w", err)
				return
			}
			
			// Check if middleware returned a modified message
			if !goja.IsUndefined(middlewareResult) {
				if msgObj, ok := middlewareResult.(*goja.Object); ok {
					if newMsg := rh.extractJSMessage(msgObj); newMsg != nil {
						processedMsg = newMsg
					}
				}
			}
		}

		// Call handler function
		handlerResult, err := rh.handlerFunc(goja.Undefined(), processedMsg.ToJSObject(rh.module.vm))
		if err != nil {
			handlerErr = err
			return
		}
		
		result = handlerResult
	})
	
	return result, handlerErr
}

// addMiddleware adds middleware to the handler
func (rh *RouterHandler) addMiddleware(call goja.FunctionCall) goja.Value {
	if len(call.Arguments) < 1 {
		panic(rh.module.vm.NewGoError(fmt.Errorf("addMiddleware requires a function argument")))
	}

	middlewareFunc, ok := goja.AssertFunction(call.Arguments[0])
	if !ok {
		panic(rh.module.vm.NewGoError(fmt.Errorf("argument must be a function")))
	}

	rh.mutex.Lock()
	rh.middleware = append(rh.middleware, middlewareFunc)
	rh.mutex.Unlock()

	rh.logger.Debug().
		Int("middleware_count", len(rh.middleware)).
		Msg("Added middleware to router handler")

	return goja.Undefined()
}

// stop stops the router handler
func (rh *RouterHandler) stop(call goja.FunctionCall) goja.Value {
	rh.mutex.Lock()
	defer rh.mutex.Unlock()

	if !rh.stopped && rh.handler != nil {
		rh.stopped = true
		rh.handler.Stop()
		rh.logger.Info().Msg("Router handler stopped")
	}

	return goja.Undefined()
}

// isRunning checks if the handler is running
func (rh *RouterHandler) isRunning(call goja.FunctionCall) goja.Value {
	rh.mutex.RLock()
	defer rh.mutex.RUnlock()
	
	return rh.module.vm.ToValue(!rh.stopped)
}

// extractJSMessage extracts a JSMessage from a JavaScript object
func (rh *RouterHandler) extractJSMessage(obj *goja.Object) *JSMessage {
	// Implementation to extract JSMessage from JavaScript object
	// This would parse the object properties and create a new JSMessage
	return nil // Simplified for now
}

// extractReturnedMessages extracts Watermill messages from JavaScript return value
func (rh *RouterHandler) extractReturnedMessages(value goja.Value) []*message.Message {
	var messages []*message.Message

	if goja.IsUndefined(value) || goja.IsNull(value) {
		return messages
	}

	// Handle array of messages
	if obj, ok := value.(*goja.Object); ok {
		if obj.ClassName() == "Array" {
			length := obj.Get("length").ToInteger()
			for i := int64(0); i < length; i++ {
				item := obj.Get(fmt.Sprintf("%d", i))
				if msg := rh.extractSingleMessage(item); msg != nil {
					messages = append(messages, msg)
				}
			}
		} else {
			// Handle single message object
			if msg := rh.extractSingleMessage(value); msg != nil {
				messages = append(messages, msg)
			}
		}
	}

	return messages
}

// extractSingleMessage extracts a single Watermill message from JavaScript value
func (rh *RouterHandler) extractSingleMessage(value goja.Value) *message.Message {
	if goja.IsUndefined(value) || goja.IsNull(value) {
		return nil
	}

	obj, ok := value.(*goja.Object)
	if !ok {
		// Handle string payload
		if str := value.String(); str != "" {
			return message.NewMessage(watermill.NewUUID(), []byte(str))
		}
		return nil
	}

	// Extract message properties
	var payload []byte
	metadata := make(message.Metadata)

	if payloadVal := obj.Get("payload"); !goja.IsUndefined(payloadVal) {
		payload = []byte(payloadVal.String())
	}

	if metadataVal := obj.Get("metadata"); !goja.IsUndefined(metadataVal) {
		if metaObj, ok := metadataVal.(*goja.Object); ok {
			for _, key := range metaObj.Keys() {
				value := metaObj.Get(key)
				metadata[key] = value.String()
			}
		}
	}

	if len(payload) == 0 {
		return nil
	}

	msg := message.NewMessage(watermill.NewUUID(), payload)
	msg.Metadata = metadata

	return msg
}

