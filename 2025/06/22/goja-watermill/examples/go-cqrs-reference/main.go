package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill/components/cqrs"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/ThreeDotsLabs/watermill/pubsub/gochannel"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

// Commands
type CreateUser struct {
	Email string `json:"email"`
	Name  string `json:"name"`
}

type UpdateUser struct {
	UserID  string                 `json:"userId"`
	Changes map[string]interface{} `json:"changes"`
}

// Events
type UserCreated struct {
	UserID string `json:"userId"`
	Email  string `json:"email"`
	Name   string `json:"name"`
}

type UserUpdated struct {
	UserID  string                 `json:"userId"`
	Changes map[string]interface{} `json:"changes"`
}

type WelcomeEmailSent struct {
	UserID string `json:"userId"`
	Email  string `json:"email"`
	SentAt string `json:"sentAt"`
}

// Command Handlers
type CreateUserHandler struct {
	eventBus *cqrs.EventBus
	logger   zerolog.Logger
}

func (h CreateUserHandler) HandlerName() string {
	return "CreateUserHandler"
}

func (h CreateUserHandler) NewCommand() interface{} {
	return &CreateUser{}
}

func (h CreateUserHandler) Handle(ctx context.Context, cmd any) error {
	createUserCmd, ok := cmd.(*CreateUser)
	if !ok {
		return fmt.Errorf("expected *CreateUser, got %T", cmd)
	}

	logger := h.logger.With().
		Str("handler", "CreateUserHandler").
		Str("command", "CreateUser").
		Str("caller", "CreateUserHandler.Handle").
		Logger()

	logger.Info().
		Str("email", createUserCmd.Email).
		Str("name", createUserCmd.Name).
		Msg("Processing CreateUser command")

	// Validate command
	if createUserCmd.Email == "" || createUserCmd.Name == "" {
		err := fmt.Errorf("email and name are required")
		logger.Error().Err(err).Msg("Command validation failed")
		return err
	}

	// Generate user ID
	userID := fmt.Sprintf("user-%d", time.Now().UnixNano()%1000000)
	
	logger.Info().
		Str("userId", userID).
		Msg("Creating user with generated ID")

	// Publish UserCreated event
	event := &UserCreated{
		UserID: userID,
		Email:  createUserCmd.Email,
		Name:   createUserCmd.Name,
	}

	logger.Debug().
		Interface("event", event).
		Msg("Publishing UserCreated event")

	err := h.eventBus.Publish(ctx, event)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to publish UserCreated event")
		return fmt.Errorf("failed to publish UserCreated event: %w", err)
	}

	logger.Info().
		Str("userId", userID).
		Msg("User created successfully")

	return nil
}

type UpdateUserHandler struct {
	eventBus *cqrs.EventBus
	logger   zerolog.Logger
}

func (h UpdateUserHandler) HandlerName() string {
	return "UpdateUserHandler"
}

func (h UpdateUserHandler) NewCommand() interface{} {
	return &UpdateUser{}
}

func (h UpdateUserHandler) Handle(ctx context.Context, cmd any) error {
	updateUserCmd, ok := cmd.(*UpdateUser)
	if !ok {
		return fmt.Errorf("expected *UpdateUser, got %T", cmd)
	}

	logger := h.logger.With().
		Str("handler", "UpdateUserHandler").
		Str("command", "UpdateUser").
		Str("caller", "UpdateUserHandler.Handle").
		Logger()

	logger.Info().
		Str("userId", updateUserCmd.UserID).
		Interface("changes", updateUserCmd.Changes).
		Msg("Processing UpdateUser command")

	// Validate command
	if updateUserCmd.UserID == "" {
		err := fmt.Errorf("userID is required")
		logger.Error().Err(err).Msg("Command validation failed")
		return err
	}

	logger.Info().
		Str("userId", updateUserCmd.UserID).
		Msg("Updating user")

	// Publish UserUpdated event
	event := &UserUpdated{
		UserID:  updateUserCmd.UserID,
		Changes: updateUserCmd.Changes,
	}

	logger.Debug().
		Interface("event", event).
		Msg("Publishing UserUpdated event")

	err := h.eventBus.Publish(ctx, event)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to publish UserUpdated event")
		return fmt.Errorf("failed to publish UserUpdated event: %w", err)
	}

	logger.Info().
		Str("userId", updateUserCmd.UserID).
		Msg("User updated successfully")

	return nil
}

// Event Handlers
type WelcomeEmailHandler struct {
	eventBus *cqrs.EventBus
	logger   zerolog.Logger
}

func (h WelcomeEmailHandler) HandlerName() string {
	return "WelcomeEmailHandler"
}

func (h WelcomeEmailHandler) NewEvent() interface{} {
	return &UserCreated{}
}

func (h WelcomeEmailHandler) Handle(ctx context.Context, event any) error {
	userCreatedEvent, ok := event.(*UserCreated)
	if !ok {
		return fmt.Errorf("expected *UserCreated, got %T", event)
	}

	logger := h.logger.With().
		Str("handler", "WelcomeEmailHandler").
		Str("event", "UserCreated").
		Str("caller", "WelcomeEmailHandler.Handle").
		Logger()

	logger.Info().
		Str("userId", userCreatedEvent.UserID).
		Str("email", userCreatedEvent.Email).
		Msg("Processing UserCreated event for welcome email")

	logger.Info().
		Str("email", userCreatedEvent.Email).
		Msg("Sending welcome email")

	// Simulate email sending
	time.Sleep(10 * time.Millisecond)

	// Publish WelcomeEmailSent event
	emailEvent := &WelcomeEmailSent{
		UserID: userCreatedEvent.UserID,
		Email:  userCreatedEvent.Email,
		SentAt: time.Now().Format(time.RFC3339),
	}

	logger.Debug().
		Interface("event", emailEvent).
		Msg("Publishing WelcomeEmailSent event")

	err := h.eventBus.Publish(ctx, emailEvent)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to publish WelcomeEmailSent event")
		return fmt.Errorf("failed to publish WelcomeEmailSent event: %w", err)
	}

	logger.Info().
		Str("email", userCreatedEvent.Email).
		Msg("Welcome email sent successfully")

	return nil
}

type AnalyticsHandler struct {
	logger zerolog.Logger
}

func (h AnalyticsHandler) HandlerName() string {
	return "AnalyticsHandler"
}

func (h AnalyticsHandler) NewEvent() interface{} {
	return &UserCreated{}
}

func (h AnalyticsHandler) Handle(ctx context.Context, event any) error {
	userCreatedEvent, ok := event.(*UserCreated)
	if !ok {
		return fmt.Errorf("expected *UserCreated, got %T", event)
	}

	logger := h.logger.With().
		Str("handler", "AnalyticsHandler").
		Str("event", "UserCreated").
		Str("caller", "AnalyticsHandler.Handle").
		Logger()

	logger.Info().
		Str("userId", userCreatedEvent.UserID).
		Str("email", userCreatedEvent.Email).
		Msg("Recording user creation analytics")

	// Simulate analytics recording
	logger.Info().
		Str("userId", userCreatedEvent.UserID).
		Msg("User registration tracked for analytics")

	return nil
}

type EmailConfirmationHandler struct {
	logger zerolog.Logger
}

func (h EmailConfirmationHandler) HandlerName() string {
	return "EmailConfirmationHandler"
}

func (h EmailConfirmationHandler) NewEvent() interface{} {
	return &WelcomeEmailSent{}
}

func (h EmailConfirmationHandler) Handle(ctx context.Context, event any) error {
	welcomeEmailSentEvent, ok := event.(*WelcomeEmailSent)
	if !ok {
		return fmt.Errorf("expected *WelcomeEmailSent, got %T", event)
	}

	logger := h.logger.With().
		Str("handler", "EmailConfirmationHandler").
		Str("event", "WelcomeEmailSent").
		Str("caller", "EmailConfirmationHandler.Handle").
		Logger()

	logger.Info().
		Str("userId", welcomeEmailSentEvent.UserID).
		Str("email", welcomeEmailSentEvent.Email).
		Str("sentAt", welcomeEmailSentEvent.SentAt).
		Msg("Processing WelcomeEmailSent event")

	logger.Info().
		Str("email", welcomeEmailSentEvent.Email).
		Msg("Email delivery confirmed")

	return nil
}

func main() {
	// Setup zerolog with caller info
	zerolog.TimeFieldFormat = time.RFC3339
	log.Logger = log.Output(zerolog.ConsoleWriter{
		Out:        os.Stderr,
		TimeFormat: time.Kitchen,
	}).With().Caller().Logger()

	logger := log.With().
		Str("component", "main").
		Str("caller", "main").
		Logger()

	logger.Info().Msg("Starting Go CQRS reference implementation")

	// Create pub/sub
	pubSub := gochannel.NewGoChannel(
		gochannel.Config{
			OutputChannelBuffer: 1000,
			Persistent:          true,
		},
		watermill.NewStdLogger(false, false),
	)

	// Create router
	router, err := message.NewRouter(message.RouterConfig{}, watermill.NewStdLogger(false, false))
	if err != nil {
		logger.Fatal().Err(err).Msg("Failed to create router")
	}

	// Create CQRS marshaler
	cqrsMarshaler := cqrs.JSONMarshaler{
		GenerateName: cqrs.StructName,
	}

	logger.Info().Msg("Creating CQRS components")

	// Create command bus
	commandBus, err := cqrs.NewCommandBusWithConfig(
		pubSub,
		cqrs.CommandBusConfig{
			GeneratePublishTopic: func(params cqrs.CommandBusGeneratePublishTopicParams) (string, error) {
				topic := fmt.Sprintf("commands.%s", params.CommandName)
				logger.Debug().
					Str("commandName", params.CommandName).
					Str("topic", topic).
					Str("caller", "CommandBus.GeneratePublishTopic").
					Msg("Generated command topic")
				return topic, nil
			},
			Marshaler: cqrsMarshaler,
			Logger:    watermill.NewStdLogger(false, false),
		},
	)
	if err != nil {
		logger.Fatal().Err(err).Msg("Failed to create command bus")
	}

	// Create event bus
	eventBus, err := cqrs.NewEventBusWithConfig(
		pubSub,
		cqrs.EventBusConfig{
			GeneratePublishTopic: func(params cqrs.GenerateEventPublishTopicParams) (string, error) {
				topic := fmt.Sprintf("events.%s", params.EventName)
				logger.Debug().
					Str("eventName", params.EventName).
					Str("topic", topic).
					Str("caller", "EventBus.GeneratePublishTopic").
					Msg("Generated event topic")
				return topic, nil
			},
			Marshaler: cqrsMarshaler,
			Logger:    watermill.NewStdLogger(false, false),
		},
	)
	if err != nil {
		logger.Fatal().Err(err).Msg("Failed to create event bus")
	}

	// Create command processor
	commandProcessor, err := cqrs.NewCommandProcessorWithConfig(
		router,
		cqrs.CommandProcessorConfig{
			GenerateSubscribeTopic: func(params cqrs.CommandProcessorGenerateSubscribeTopicParams) (string, error) {
				topic := fmt.Sprintf("commands.%s", params.CommandName)
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
				return pubSub, nil
			},
			Marshaler: cqrsMarshaler,
			Logger:    watermill.NewStdLogger(false, false),
		},
	)
	if err != nil {
		logger.Fatal().Err(err).Msg("Failed to create command processor")
	}

	// Create event processor
	eventProcessor, err := cqrs.NewEventProcessorWithConfig(
		router,
		cqrs.EventProcessorConfig{
			GenerateSubscribeTopic: func(params cqrs.EventProcessorGenerateSubscribeTopicParams) (string, error) {
				topic := fmt.Sprintf("events.%s", params.EventName)
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
				return pubSub, nil
			},
			Marshaler: cqrsMarshaler,
			Logger:    watermill.NewStdLogger(false, false),
		},
	)
	if err != nil {
		logger.Fatal().Err(err).Msg("Failed to create event processor")
	}

	logger.Info().Msg("Adding command handlers")

	// Add command handlers
	err = commandProcessor.AddHandlers(
		CreateUserHandler{eventBus: eventBus, logger: logger},
		UpdateUserHandler{eventBus: eventBus, logger: logger},
	)
	if err != nil {
		logger.Fatal().Err(err).Msg("Failed to add command handlers")
	}

	logger.Info().Msg("Adding event handlers")

	// Add event handlers
	err = eventProcessor.AddHandlers(
		WelcomeEmailHandler{eventBus: eventBus, logger: logger},
		AnalyticsHandler{logger: logger},
		EmailConfirmationHandler{logger: logger},
	)
	if err != nil {
		logger.Fatal().Err(err).Msg("Failed to add event handlers")
	}

	logger.Info().Msg("Starting router")

	// Start router
	go func() {
		err := router.Run(context.Background())
		if err != nil {
			logger.Error().Err(err).Msg("Router stopped with error")
		}
	}()

	// Wait for router to be running
	<-router.Running()
	logger.Info().Msg("Router is running")

	// Send some commands
	ctx := context.Background()

	logger.Info().Msg("Sending CreateUser commands")

	err = commandBus.Send(ctx, &CreateUser{
		Email: "john.doe@example.com",
		Name:  "John Doe",
	})
	if err != nil {
		logger.Error().Err(err).Msg("Failed to send CreateUser command")
	}

	err = commandBus.Send(ctx, &CreateUser{
		Email: "jane.smith@example.com",
		Name:  "Jane Smith",
	})
	if err != nil {
		logger.Error().Err(err).Msg("Failed to send CreateUser command")
	}

	logger.Info().Msg("Sending UpdateUser command")

	err = commandBus.Send(ctx, &UpdateUser{
		UserID: "user-123",
		Changes: map[string]interface{}{
			"name":  "John Doe Jr.",
			"phone": "+1-555-0123",
		},
	})
	if err != nil {
		logger.Error().Err(err).Msg("Failed to send UpdateUser command")
	}

	logger.Info().Msg("Commands sent, waiting for processing...")

	// Wait for processing
	time.Sleep(2 * time.Second)

	logger.Info().Msg("Go CQRS reference implementation completed successfully")

	// Wait for interrupt
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	<-c

	logger.Info().Msg("Shutting down...")
	router.Close()
}

