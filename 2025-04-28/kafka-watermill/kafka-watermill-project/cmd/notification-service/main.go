package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"os/signal"
	"syscall"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill-kafka/v2/pkg/kafka"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/ThreeDotsLabs/watermill/message/router/middleware"
	
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
			ConsumerGroup:         "notification-service",
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

	// Handle order.created events for order confirmation notifications
	router.AddNoPublisherHandler(
		"order-confirmation-notification-handler",
		"order.created",
		subscriber,
		func(msg *message.Message) error {
			var orderCreated orderEvents.OrderCreated
			if err := orderEvents.FromJSON(msg.Payload, &orderCreated); err != nil {
				return fmt.Errorf("error unmarshaling order.created event: %w", err)
			}

			// In a real app, this would send an email, SMS, or push notification
			log.Printf("Sending order confirmation notification for order: %s", orderCreated.OrderID)
			
			return nil
		},
	)

	// Handle payment.processed events for payment notifications
	router.AddNoPublisherHandler(
		"payment-notification-handler",
		"payment.processed",
		subscriber,
		func(msg *message.Message) error {
			var paymentProcessed orderEvents.PaymentProcessed
			if err := orderEvents.FromJSON(msg.Payload, &paymentProcessed); err != nil {
				return fmt.Errorf("error unmarshaling payment.processed event: %w", err)
			}

			// In a real app, this would send an email, SMS, or push notification
			log.Printf("Sending payment %s notification for order: %s", 
				paymentProcessed.Status, 
				paymentProcessed.OrderID,
			)
			
			return nil
		},
	)

	// Handle inventory.checked events for inventory notifications
	router.AddNoPublisherHandler(
		"inventory-notification-handler",
		"inventory.checked",
		subscriber,
		func(msg *message.Message) error {
			var inventoryChecked orderEvents.InventoryChecked
			if err := orderEvents.FromJSON(msg.Payload, &inventoryChecked); err != nil {
				return fmt.Errorf("error unmarshaling inventory.checked event: %w", err)
			}

			// Only send notifications for out-of-stock items
			if !inventoryChecked.AllItemsAvailable {
				log.Printf("Sending out-of-stock notification for order: %s", inventoryChecked.OrderID)
			} else {
				log.Printf("Sending inventory confirmation for order: %s", inventoryChecked.OrderID)
			}
			
			return nil
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