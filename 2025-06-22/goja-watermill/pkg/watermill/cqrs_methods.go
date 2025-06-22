package watermill

import (
	"fmt"

	"github.com/ThreeDotsLabs/watermill/components/cqrs"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/dop251/goja"
	"github.com/example/goja-watermill/pkg/pubsub"
)

// createCQRS creates a new CQRS facade
func (m *Module) createCQRS(call goja.FunctionCall) goja.Value {
	logger := m.logger.With().
		Str("component", "Module").
		Str("caller", "createCQRS").
		Logger()

	if len(call.Arguments) < 1 {
		panic(m.vm.NewGoError(fmt.Errorf("createCQRS requires at least 1 argument: pubsubType")))
	}
	
	pubsubType := call.Arguments[0].String()
	
	// Parse configuration if provided
	var config map[string]interface{}
	if len(call.Arguments) > 1 && !goja.IsUndefined(call.Arguments[1]) {
		config = call.Arguments[1].Export().(map[string]interface{})
	}

	logger.Info().
		Str("pubsubType", pubsubType).
		Interface("config", config).
		Msg("Creating CQRS facade")
	
	// Create pub/sub instances for commands and events
	var commandsPublisher, eventsPublisher, commandsSubscriber, eventsSubscriber interface{}
	
	switch pubsubType {
	case "memory":
		memoryPubSub, err := pubsub.NewMemoryPubSub(pubsub.MemoryPubSubConfig{
			OutputChannelBuffer: 1000,
			Persistent:          true,
		}, m.logger)
		if err != nil {
			panic(m.vm.NewGoError(fmt.Errorf("failed to create memory pubsub: %w", err)))
		}
		
		commandsPublisher = memoryPubSub
		commandsSubscriber = memoryPubSub
		eventsPublisher = memoryPubSub
		eventsSubscriber = memoryPubSub
		
		logger.Debug().Msg("Using memory pub/sub for commands and events")
		
	default:
		panic(m.vm.NewGoError(fmt.Errorf("unsupported pubsub type: %s", pubsubType)))
	}
	
	// Create CQRS configuration
	cqrsConfig := CQRSConfig{
		CommandsPublisher:  commandsPublisher.(message.Publisher),
		CommandsSubscriber: commandsSubscriber.(message.Subscriber),
		EventsPublisher:    eventsPublisher.(message.Publisher),
		EventsSubscriber:   eventsSubscriber.(message.Subscriber),
		Marshaler:          &cqrs.JSONMarshaler{GenerateName: cqrs.StructName},
		Logger:             logger,
	}
	
	// Apply custom configuration
	if config != nil {
		if commandsTopic, ok := config["commandsTopic"].(string); ok {
			cqrsConfig.GenerateCommandsTopic = func(commandName string) string {
				topic := fmt.Sprintf("%s.%s", commandsTopic, commandName)
				logger.Debug().
					Str("commandName", commandName).
					Str("topic", topic).
					Str("caller", "GenerateCommandsTopic").
					Msg("Generated custom command topic")
				return topic
			}
		}
		if eventsTopic, ok := config["eventsTopic"].(string); ok {
			cqrsConfig.GenerateEventsTopic = func(eventName string) string {
				topic := fmt.Sprintf("%s.%s", eventsTopic, eventName)
				logger.Debug().
					Str("eventName", eventName).
					Str("topic", topic).
					Str("caller", "GenerateEventsTopic").
					Msg("Generated custom event topic")
				return topic
			}
		}
	}
	
	// Create CQRS facade
	facade, err := NewCQRSFacade(m, cqrsConfig)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to create CQRS facade")
		panic(m.vm.NewGoError(fmt.Errorf("failed to create CQRS facade: %w", err)))
	}
	
	// Store the facade
	m.cqrsFacade = facade

	logger.Info().Msg("CQRS facade created successfully")
	
	return facade.ToJSObject(m.vm)
}

