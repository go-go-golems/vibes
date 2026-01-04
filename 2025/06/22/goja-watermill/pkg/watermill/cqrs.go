package watermill

import (
	"context"
	"fmt"
	"sync"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill/components/cqrs"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/dop251/goja"
	"github.com/rs/zerolog"
)

// CQRSConfig holds the configuration for CQRS components
type CQRSConfig struct {
	CommandsPublisher     message.Publisher
	CommandsSubscriber    message.Subscriber
	EventsPublisher       message.Publisher
	EventsSubscriber      message.Subscriber
	Marshaler             cqrs.CommandEventMarshaler
	Logger                zerolog.Logger
	GenerateCommandsTopic func(commandName string) string
	GenerateEventsTopic   func(eventName string) string
}

// CQRSFacade provides a JavaScript-friendly interface to Watermill CQRS
type CQRSFacade struct {
	module           *Module
	config           CQRSConfig
	commandBus       *cqrs.CommandBus
	eventBus         *cqrs.EventBus
	commandProcessor *cqrs.CommandProcessor
	eventProcessor   *cqrs.EventProcessor
	
	// Track registered handlers for proper cleanup
	commandHandlers map[string]*JSCommandHandler
	eventHandlers   map[string][]*JSEventHandler
	mutex           sync.RWMutex
}

// JSCommand represents a command in JavaScript
type JSCommand struct {
	Name     string                 `json:"name"`
	Payload  map[string]interface{} `json:"payload"`
	Metadata map[string]string      `json:"metadata"`
}

// JSEvent represents an event in JavaScript
type JSEvent struct {
	Name     string                 `json:"name"`
	Payload  map[string]interface{} `json:"payload"`
	Metadata map[string]string      `json:"metadata"`
}

// JSCommandHandler wraps a JavaScript command handler function
type JSCommandHandler struct {
	name     string
	jsFunc   goja.Callable
	module   *Module
	logger   zerolog.Logger
}

func (h *JSCommandHandler) HandlerName() string {
	return h.name
}

func (h *JSCommandHandler) NewCommand() interface{} {
	return &JSCommand{}
}

// Handle implements cqrs.CommandHandler interface
func (h *JSCommandHandler) Handle(ctx context.Context, cmd any) error {
	jsCmd, ok := cmd.(*JSCommand)
	if !ok {
		return fmt.Errorf("expected *JSCommand, got %T", cmd)
	}

	logger := h.logger.With().
		Str("handler", h.name).
		Str("command", jsCmd.Name).
		Str("caller", "JSCommandHandler.Handle").
		Logger()

	logger.Debug().
		Interface("command", jsCmd).
		Msg("Processing command in JS handler")

	var result goja.Value
	var err error

	// Execute in JavaScript context
	h.module.executeInJS(func() {
		// Convert command to JS object
		jsObj := h.module.vm.NewObject()
		jsObj.Set("name", jsCmd.Name)
		jsObj.Set("payload", h.module.vm.ToValue(jsCmd.Payload))
		jsObj.Set("metadata", h.module.vm.ToValue(jsCmd.Metadata))

		// Call the JS function
		result, err = h.jsFunc(goja.Undefined(), jsObj)
	})

	if err != nil {
		logger.Error().Err(err).Msg("JavaScript command handler failed")
		return fmt.Errorf("command handler failed: %w", err)
	}

	logger.Debug().
		Interface("result", result).
		Msg("Command handler completed successfully")

	return nil
}

// JSEventHandler wraps a JavaScript event handler function
type JSEventHandler struct {
	name     string
	jsFunc   goja.Callable
	module   *Module
	logger   zerolog.Logger
}

func (h *JSEventHandler) HandlerName() string {
	return h.name
}

func (h *JSEventHandler) NewEvent() interface{} {
	return &JSEvent{}
}

// Handle implements cqrs.EventHandler interface
func (h *JSEventHandler) Handle(ctx context.Context, event any) error {
	jsEvent, ok := event.(*JSEvent)
	if !ok {
		return fmt.Errorf("expected *JSEvent, got %T", event)
	}

	logger := h.logger.With().
		Str("handler", h.name).
		Str("event", jsEvent.Name).
		Str("caller", "JSEventHandler.Handle").
		Logger()

	logger.Debug().
		Interface("event", jsEvent).
		Msg("Processing event in JS handler")

	var result goja.Value
	var err error

	// Execute in JavaScript context
	h.module.executeInJS(func() {
		// Convert event to JS object
		jsObj := h.module.vm.NewObject()
		jsObj.Set("name", jsEvent.Name)
		jsObj.Set("payload", h.module.vm.ToValue(jsEvent.Payload))
		jsObj.Set("metadata", h.module.vm.ToValue(jsEvent.Metadata))

		// Call the JS function
		result, err = h.jsFunc(goja.Undefined(), jsObj)
	})

	if err != nil {
		logger.Error().Err(err).Msg("JavaScript event handler failed")
		return fmt.Errorf("event handler failed: %w", err)
	}

	logger.Debug().
		Interface("result", result).
		Msg("Event handler completed successfully")

	return nil
}

// NewCQRSFacade creates a new CQRS facade
func NewCQRSFacade(module *Module, config CQRSConfig) (*CQRSFacade, error) {
	logger := config.Logger.With().
		Str("component", "CQRSFacade").
		Str("caller", "NewCQRSFacade").
		Logger()

	if config.Marshaler == nil {
		config.Marshaler = &cqrs.JSONMarshaler{
			GenerateName: cqrs.StructName,
		}
		logger.Debug().Msg("Using default JSON marshaler with StructName")
	}
	
	if config.GenerateCommandsTopic == nil {
		config.GenerateCommandsTopic = func(commandName string) string {
			topic := fmt.Sprintf("commands.%s", commandName)
			logger.Debug().
				Str("commandName", commandName).
				Str("topic", topic).
				Str("caller", "GenerateCommandsTopic").
				Msg("Generated default command topic")
			return topic
		}
	}
	
	if config.GenerateEventsTopic == nil {
		config.GenerateEventsTopic = func(eventName string) string {
			topic := fmt.Sprintf("events.%s", eventName)
			logger.Debug().
				Str("eventName", eventName).
				Str("topic", topic).
				Str("caller", "GenerateEventsTopic").
				Msg("Generated default event topic")
			return topic
		}
	}
	
	facade := &CQRSFacade{
		module:          module,
		config:          config,
		commandHandlers: make(map[string]*JSCommandHandler),
		eventHandlers:   make(map[string][]*JSEventHandler),
	}
	
	logger.Info().Msg("Initializing CQRS components")
	
	err := facade.initializeCQRSComponents()
	if err != nil {
		logger.Error().Err(err).Msg("Failed to initialize CQRS components")
		return nil, fmt.Errorf("failed to initialize CQRS components: %w", err)
	}
	
	logger.Info().Msg("CQRS facade created successfully")
	return facade, nil
}

// initializeCQRSComponents sets up the Watermill CQRS components
func (f *CQRSFacade) initializeCQRSComponents() error {
	logger := f.config.Logger.With().
		Str("component", "CQRSFacade").
		Str("caller", "initializeCQRSComponents").
		Logger()

	// Command Bus
	logger.Debug().Msg("Creating command bus")
	commandBusConfig := cqrs.CommandBusConfig{
		GeneratePublishTopic: func(params cqrs.CommandBusGeneratePublishTopicParams) (string, error) {
			topic := f.config.GenerateCommandsTopic(params.CommandName)
			logger.Debug().
				Str("commandName", params.CommandName).
				Str("topic", topic).
				Str("caller", "CommandBus.GeneratePublishTopic").
				Msg("Generated command publish topic")
			return topic, nil
		},
		Marshaler: f.config.Marshaler,
		Logger:    watermill.NewStdLogger(false, false),
	}
	
	var err error
	f.commandBus, err = cqrs.NewCommandBusWithConfig(f.config.CommandsPublisher, commandBusConfig)
	if err != nil {
		return fmt.Errorf("failed to create command bus: %w", err)
	}
	logger.Debug().Msg("Command bus created")

	// Event Bus
	logger.Debug().Msg("Creating event bus")
	eventBusConfig := cqrs.EventBusConfig{
		GeneratePublishTopic: func(params cqrs.GenerateEventPublishTopicParams) (string, error) {
			topic := f.config.GenerateEventsTopic(params.EventName)
			logger.Debug().
				Str("eventName", params.EventName).
				Str("topic", topic).
				Str("caller", "EventBus.GeneratePublishTopic").
				Msg("Generated event publish topic")
			return topic, nil
		},
		Marshaler: f.config.Marshaler,
		Logger:    watermill.NewStdLogger(false, false),
	}
	
	f.eventBus, err = cqrs.NewEventBusWithConfig(f.config.EventsPublisher, eventBusConfig)
	if err != nil {
		return fmt.Errorf("failed to create event bus: %w", err)
	}
	logger.Debug().Msg("Event bus created")
	
	// Command Processor
	logger.Debug().Msg("Creating command processor")
	commandProcessorConfig := cqrs.CommandProcessorConfig{
		GenerateSubscribeTopic: func(params cqrs.CommandProcessorGenerateSubscribeTopicParams) (string, error) {
			topic := f.config.GenerateCommandsTopic(params.CommandName)
			logger.Debug().
				Str("commandName", params.CommandName).
				Str("handlerName", params.CommandHandler.HandlerName()).
				Str("topic", topic).
				Str("caller", "CommandProcessor.GenerateSubscribeTopic").
				Msg("Generated command subscribe topic")
			return topic, nil
		},
		SubscriberConstructor: func(params cqrs.CommandProcessorSubscriberConstructorParams) (message.Subscriber, error) {
			logger.Debug().
				Str("handlerName", params.HandlerName).
				Str("caller", "CommandProcessor.SubscriberConstructor").
				Msg("Creating command subscriber")
			return f.config.CommandsSubscriber, nil
		},
		Marshaler: f.config.Marshaler,
		Logger:    watermill.NewStdLogger(false, false),
	}
	
	f.commandProcessor, err = cqrs.NewCommandProcessorWithConfig(f.module.router, commandProcessorConfig)
	if err != nil {
		return fmt.Errorf("failed to create command processor: %w", err)
	}
	logger.Debug().Msg("Command processor created")

	// Event Processor
	logger.Debug().Msg("Creating event processor")
	eventProcessorConfig := cqrs.EventProcessorConfig{
		GenerateSubscribeTopic: func(params cqrs.EventProcessorGenerateSubscribeTopicParams) (string, error) {
			topic := f.config.GenerateEventsTopic(params.EventName)
			logger.Debug().
				Str("eventName", params.EventName).
				Str("handlerName", params.EventHandler.HandlerName()).
				Str("topic", topic).
				Str("caller", "EventProcessor.GenerateSubscribeTopic").
				Msg("Generated event subscribe topic")
			return topic, nil
		},
		SubscriberConstructor: func(params cqrs.EventProcessorSubscriberConstructorParams) (message.Subscriber, error) {
			logger.Debug().
				Str("handlerName", params.HandlerName).
				Str("caller", "EventProcessor.SubscriberConstructor").
				Msg("Creating event subscriber")
			return f.config.EventsSubscriber, nil
		},
		Marshaler: f.config.Marshaler,
		Logger:    watermill.NewStdLogger(false, false),
	}
	
	f.eventProcessor, err = cqrs.NewEventProcessorWithConfig(f.module.router, eventProcessorConfig)
	if err != nil {
		return fmt.Errorf("failed to create event processor: %w", err)
	}
	logger.Debug().Msg("Event processor created")

	logger.Info().Msg("All CQRS components initialized successfully")
	return nil
}

// sendCommand sends a command via the command bus
func (f *CQRSFacade) sendCommand(call goja.FunctionCall) goja.Value {
	logger := f.config.Logger.With().
		Str("component", "CQRSFacade").
		Str("caller", "sendCommand").
		Logger()

	if len(call.Arguments) < 1 {
		panic(f.module.vm.NewGoError(fmt.Errorf("sendCommand requires at least 1 argument: command")))
	}

	// Parse command from JavaScript
	commandObj := call.Arguments[0].ToObject(f.module.vm)
	
	name := commandObj.Get("name").String()
	if name == "" {
		panic(f.module.vm.NewGoError(fmt.Errorf("command name is required")))
	}

	payload := make(map[string]interface{})
	if payloadVal := commandObj.Get("payload"); !goja.IsUndefined(payloadVal) {
		payload = payloadVal.Export().(map[string]interface{})
	}

	metadata := make(map[string]string)
	if metadataVal := commandObj.Get("metadata"); !goja.IsUndefined(metadataVal) {
		metadataMap := metadataVal.Export().(map[string]interface{})
		for k, v := range metadataMap {
			metadata[k] = fmt.Sprintf("%v", v)
		}
	}

	command := &JSCommand{
		Name:     name,
		Payload:  payload,
		Metadata: metadata,
	}

	logger.Info().
		Str("commandName", name).
		Interface("payload", payload).
		Msg("Sending command")

	ctx := context.Background()
	err := f.commandBus.Send(ctx, command)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to send command")
		panic(f.module.vm.NewGoError(fmt.Errorf("failed to send command: %w", err)))
	}

	logger.Debug().
		Str("commandName", name).
		Msg("Command sent successfully")

	return goja.Undefined()
}

// publishEvent publishes an event via the event bus
func (f *CQRSFacade) publishEvent(call goja.FunctionCall) goja.Value {
	logger := f.config.Logger.With().
		Str("component", "CQRSFacade").
		Str("caller", "publishEvent").
		Logger()

	if len(call.Arguments) < 1 {
		panic(f.module.vm.NewGoError(fmt.Errorf("publishEvent requires at least 1 argument: event")))
	}

	// Parse event from JavaScript
	eventObj := call.Arguments[0].ToObject(f.module.vm)
	
	name := eventObj.Get("name").String()
	if name == "" {
		panic(f.module.vm.NewGoError(fmt.Errorf("event name is required")))
	}

	payload := make(map[string]interface{})
	if payloadVal := eventObj.Get("payload"); !goja.IsUndefined(payloadVal) {
		payload = payloadVal.Export().(map[string]interface{})
	}

	metadata := make(map[string]string)
	if metadataVal := eventObj.Get("metadata"); !goja.IsUndefined(metadataVal) {
		metadataMap := metadataVal.Export().(map[string]interface{})
		for k, v := range metadataMap {
			metadata[k] = fmt.Sprintf("%v", v)
		}
	}

	event := &JSEvent{
		Name:     name,
		Payload:  payload,
		Metadata: metadata,
	}

	logger.Info().
		Str("eventName", name).
		Interface("payload", payload).
		Msg("Publishing event")

	ctx := context.Background()
	err := f.eventBus.Publish(ctx, event)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to publish event")
		panic(f.module.vm.NewGoError(fmt.Errorf("failed to publish event: %w", err)))
	}

	logger.Debug().
		Str("eventName", name).
		Msg("Event published successfully")

	return goja.Undefined()
}

// addCommandHandler adds a command handler
func (f *CQRSFacade) addCommandHandler(call goja.FunctionCall) goja.Value {
	logger := f.config.Logger.With().
		Str("component", "CQRSFacade").
		Str("caller", "addCommandHandler").
		Logger()

	if len(call.Arguments) < 2 {
		panic(f.module.vm.NewGoError(fmt.Errorf("addCommandHandler requires 2 arguments: commandName, handlerFunction")))
	}

	commandName := call.Arguments[0].String()
	handlerFunc, ok := goja.AssertFunction(call.Arguments[1])
	if !ok {
		panic(f.module.vm.NewGoError(fmt.Errorf("second argument must be a function")))
	}

	logger.Info().
		Str("commandName", commandName).
		Msg("Adding command handler")

	f.mutex.Lock()
	defer f.mutex.Unlock()

	// Check if handler already exists
	if _, exists := f.commandHandlers[commandName]; exists {
		panic(f.module.vm.NewGoError(fmt.Errorf("command handler for '%s' already exists", commandName)))
	}

	// Create JS command handler
	handler := &JSCommandHandler{
		name:   commandName,
		jsFunc: handlerFunc,
		module: f.module,
		logger: logger,
	}

	// Add to command processor
	err := f.commandProcessor.AddHandlers(handler)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to add command handler")
		panic(f.module.vm.NewGoError(fmt.Errorf("failed to add command handler: %w", err)))
	}

	// Store handler reference
	f.commandHandlers[commandName] = handler

	logger.Info().
		Str("commandName", commandName).
		Msg("Command handler added successfully")

	return goja.Undefined()
}

// addEventHandler adds an event handler
func (f *CQRSFacade) addEventHandler(call goja.FunctionCall) goja.Value {
	logger := f.config.Logger.With().
		Str("component", "CQRSFacade").
		Str("caller", "addEventHandler").
		Logger()

	if len(call.Arguments) < 3 {
		panic(f.module.vm.NewGoError(fmt.Errorf("addEventHandler requires 3 arguments: eventName, handlerName, handlerFunction")))
	}

	eventName := call.Arguments[0].String()
	handlerName := call.Arguments[1].String()
	handlerFunc, ok := goja.AssertFunction(call.Arguments[2])
	if !ok {
		panic(f.module.vm.NewGoError(fmt.Errorf("third argument must be a function")))
	}

	logger.Info().
		Str("eventName", eventName).
		Str("handlerName", handlerName).
		Msg("Adding event handler")

	f.mutex.Lock()
	defer f.mutex.Unlock()

	// Create JS event handler
	handler := &JSEventHandler{
		name:   handlerName,
		jsFunc: handlerFunc,
		module: f.module,
		logger: logger,
	}

	// Add to event processor
	err := f.eventProcessor.AddHandlers(handler)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to add event handler")
		panic(f.module.vm.NewGoError(fmt.Errorf("failed to add event handler: %w", err)))
	}

	// Store handler reference
	if f.eventHandlers[eventName] == nil {
		f.eventHandlers[eventName] = make([]*JSEventHandler, 0)
	}
	f.eventHandlers[eventName] = append(f.eventHandlers[eventName], handler)

	logger.Info().
		Str("eventName", eventName).
		Str("handlerName", handlerName).
		Msg("Event handler added successfully")

	return goja.Undefined()
}

// ToJSObject converts the CQRS facade to a JavaScript object
func (f *CQRSFacade) ToJSObject(vm *goja.Runtime) goja.Value {
	obj := vm.NewObject()
	obj.Set("sendCommand", f.sendCommand)
	obj.Set("publishEvent", f.publishEvent)
	obj.Set("addCommandHandler", f.addCommandHandler)
	obj.Set("addEventHandler", f.addEventHandler)
	return obj
}

