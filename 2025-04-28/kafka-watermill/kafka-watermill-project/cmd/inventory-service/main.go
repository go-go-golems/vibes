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

// Mock inventory database
var inventory = map[string]int{
	"product1": 10,
	"product2": 5,
	"product3": 0,
}

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
			ConsumerGroup:         "inventory-service",
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

	// Set up handler for payment.processed events
	router.AddHandler(
		"inventory-check-handler",
		"payment.processed",
		subscriber,
		"inventory.checked",
		publisher,
		func(msg *message.Message) ([]*message.Message, error) {
			// Parse the incoming payment processed event
			var paymentProcessed orderEvents.PaymentProcessed
			if err := orderEvents.FromJSON(msg.Payload, &paymentProcessed); err != nil {
				return nil, fmt.Errorf("error unmarshaling payment.processed event: %w", err)
			}

			// Only proceed if payment was successful
			if paymentProcessed.Status != "success" {
				log.Printf("Payment for order %s failed, skipping inventory check", paymentProcessed.OrderID)
				return nil, nil
			}

			log.Printf("Checking inventory for order: %s", paymentProcessed.OrderID)

			// Simulate inventory check
			time.Sleep(500 * time.Millisecond)

			// For this example, we'll just use the order ID to get the order details
			// In a real app, we would need to fetch the order from a database
			
			// Hardcoded example items to check in inventory
			itemsToCheck := []orderEvents.OrderItem{
				{
					ProductID: "product1",
					Name:      "Sample Product 1",
					Quantity:  2,
					Price:     19.99,
				},
				{
					ProductID: "product3",
					Name:      "Out of Stock Product",
					Quantity:  1,
					Price:     29.99,
				},
			}

			// Check if all items are available
			var unavailableItems []orderEvents.UnavailableItem
			allAvailable := true

			for _, item := range itemsToCheck {
				available, ok := inventory[item.ProductID]
				if !ok || available < item.Quantity {
					allAvailable = false
					unavailableItems = append(unavailableItems, orderEvents.UnavailableItem{
						ProductID:         item.ProductID,
						Name:              item.Name,
						RequestedQuantity: item.Quantity,
						AvailableQuantity: available,
					})
				}
			}

			// Create inventory checked event
			inventoryChecked := orderEvents.InventoryChecked{
				OrderID:          paymentProcessed.OrderID,
				AllItemsAvailable: allAvailable,
				UnavailableItems:  unavailableItems,
				Timestamp:         time.Now(),
			}

			// Marshal the event to JSON
			payloadBytes, err := orderEvents.ToJSON(inventoryChecked)
			if err != nil {
				return nil, fmt.Errorf("error marshaling inventory.checked event: %w", err)
			}

			// Create a new message with the inventory checked event
			outMsg := message.NewMessage(
				uuid.New().String(),
				payloadBytes,
			)

			log.Printf("Inventory checked for order: %s, all items available: %v", 
				inventoryChecked.OrderID, 
				inventoryChecked.AllItemsAvailable,
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