package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill-kafka/v2/pkg/kafka"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/ThreeDotsLabs/watermill/message/router/middleware"
	"github.com/google/uuid"
	
	orderEvents "github.com/scrapybara/kafka-watermill-project/idl/go"
)

var (
	logger = watermill.NewStdLogger(false, false)
)

func main() {
	// Get brokers from environment or use default
	brokers := os.Getenv("KAFKA_BROKERS")
	if brokers == "" {
		brokers = "kafka:9092"
	}

	// Create Kafka publisher
	publisher, err := kafka.NewPublisher(
		kafka.PublisherConfig{
			Brokers:   []string{brokers},
			Marshaler: kafka.DefaultMarshaler{},
		},
		logger,
	)
	if err != nil {
		panic(err)
	}

	// Create Kafka subscriber
	subscriber, err := kafka.NewSubscriber(
		kafka.SubscriberConfig{
			Brokers:               []string{brokers},
			Unmarshaler:           kafka.DefaultMarshaler{},
			OverwriteSaramaConfig: kafka.DefaultSaramaSubscriberConfig(),
			ConsumerGroup:         "payment-service",
		},
		logger,
	)
	if err != nil {
		panic(err)
	}

	// Create router
	router, err := message.NewRouter(message.RouterConfig{}, logger)
	if err != nil {
		panic(err)
	}

	// Add middleware
	router.AddMiddleware(middleware.Recoverer)

	// Set up handler for order.created events
	router.AddHandler(
		"payment-process-handler",
		"order.created",
		subscriber,
		"payment.processed",
		publisher,
		func(msg *message.Message) ([]*message.Message, error) {
			// Parse the incoming order created event
			var orderCreated orderEvents.OrderCreated
			if err := orderEvents.FromJSON(msg.Payload, &orderCreated); err != nil {
				return nil, fmt.Errorf("error unmarshaling order.created event: %w", err)
			}

			log.Printf("Processing payment for order: %s", orderCreated.OrderID)

			// Simulate payment processing
			time.Sleep(1 * time.Second)

			// Create a payment processed event
			paymentProcessed := orderEvents.PaymentProcessed{
				OrderID:       orderCreated.OrderID,
				PaymentID:     uuid.New().String(),
				Status:        "success",
				TransactionID: uuid.New().String(),
				Amount:        orderCreated.TotalAmount,
				Timestamp:     time.Now(),
			}

			// Marshal the event to JSON
			payloadBytes, err := orderEvents.ToJSON(paymentProcessed)
			if err != nil {
				return nil, fmt.Errorf("error marshaling payment.processed event: %w", err)
			}

			// Create a new message with the payment processed event
			outMsg := message.NewMessage(
				uuid.New().String(),
				payloadBytes,
			)

			log.Printf("Payment processed for order: %s, status: %s", 
				paymentProcessed.OrderID, 
				paymentProcessed.Status,
			)

			return []*message.Message{outMsg}, nil
		},
	)

	// Start API server
	api := NewAPI()
	go api.StartServer()

	// Run the router
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	go func() {
		if err := router.Run(ctx); err != nil {
			panic(err)
		}
	}()

	// Wait for interrupt signal to gracefully shutdown
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	<-c
	
	log.Println("Shutting down")
	cancel()
	if err := publisher.Close(); err != nil {
		log.Printf("Error closing publisher: %v", err)
	}
	if err := subscriber.Close(); err != nil {
		log.Printf("Error closing subscriber: %v", err)
	}
}