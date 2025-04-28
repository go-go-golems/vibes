package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill-kafka/v2/pkg/kafka"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/google/uuid"
	"github.com/scrapybara/kafka-watermill-project/idl/go"
	"github.com/scrapybara/kafka-watermill-project/pkg/mediator"
	"github.com/scrapybara/kafka-watermill-project/pkg/orderprocessing"
	"github.com/scrapybara/kafka-watermill-project/pkg/saga"
)

var (
	brokers = []string{"kafka:9092"}
	logger  = watermill.NewStdLogger(false, false)
)

func main() {
	// Create Kafka publisher
	publisher, err := kafka.NewPublisher(
		kafka.PublisherConfig{
			Brokers:   brokers,
			Marshaler: kafka.DefaultMarshaler{},
		},
		logger,
	)
	if err != nil {
		panic(err)
	}
	defer publisher.Close()

	// Create Kafka subscriber
	subscriber, err := kafka.NewSubscriber(
		kafka.SubscriberConfig{
			Brokers:               brokers,
			Unmarshaler:           kafka.DefaultMarshaler{},
			OverwriteSaramaConfig: kafka.DefaultSaramaSubscriberConfig(),
			ConsumerGroup:         "patterns-demo",
		},
		logger,
	)
	if err != nil {
		panic(err)
	}
	defer subscriber.Close()

	// Set up mediator pattern
	med := mediator.NewMediator()

	// Set up order processor with mediator and pipeline patterns
	orderProcessor := orderprocessing.NewOrderProcessor(med, publisher)

	// Register command handlers
	med.RegisterCommandHandler("CreateOrder", orderprocessing.NewCreateOrderHandler(med, publisher))
	med.RegisterCommandHandler("ProcessPayment", orderprocessing.NewProcessPaymentHandler(med, publisher))

	// Register event handlers to print events
	med.RegisterEventHandler("OrderCreated", &LoggingEventHandler{name: "OrderCreated"})
	med.RegisterEventHandler("PaymentProcessed", &LoggingEventHandler{name: "PaymentProcessed"})
	med.RegisterEventHandler("InventoryChecked", &LoggingEventHandler{name: "InventoryChecked"})
	med.RegisterEventHandler("OrderFulfilled", &LoggingEventHandler{name: "OrderFulfilled"})

	// Setup context with cancellation
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Start a goroutine to generate orders
	go generateOrders(ctx, orderProcessor)

	// Start a goroutine to handle messages via saga pattern
	go handleMessagesWithSaga(ctx, subscriber, publisher)

	// Wait for interrupt signal
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	<-c

	log.Println("Shutting down...")
}

// LoggingEventHandler is a simple event handler that logs events
type LoggingEventHandler struct {
	name string
}

func (h *LoggingEventHandler) Handle(ctx context.Context, event mediator.Event) error {
	log.Printf("[EVENT] %s: %+v", h.name, event)
	return nil
}

// generateOrders creates new orders periodically
func generateOrders(ctx context.Context, processor *orderprocessing.OrderProcessor) {
	for {
		select {
		case <-ctx.Done():
			return
		case <-time.After(5 * time.Second):
			// Create a random order
			items := []order.OrderItem{
				{
					ProductID: fmt.Sprintf("product%d", (time.Now().Unix() % 3) + 1),
					Name:      fmt.Sprintf("Product %d", (time.Now().Unix() % 3) + 1),
					Quantity:  int((time.Now().Unix() % 5) + 1),
					Price:     19.99,
				},
				{
					ProductID: fmt.Sprintf("product%d", ((time.Now().Unix() + 1) % 3) + 1),
					Name:      fmt.Sprintf("Product %d", ((time.Now().Unix() + 1) % 3) + 1),
					Quantity:  int(((time.Now().Unix() + 1) % 3) + 1),
					Price:     29.99,
				},
			}

			totalAmount := 0.0
			for _, item := range items {
				totalAmount += float64(item.Quantity) * item.Price
			}

			cmd := orderprocessing.CreateOrderCommand{
				UserID:      fmt.Sprintf("user-%d", time.Now().Unix() % 10),
				Items:       items,
				TotalAmount: totalAmount,
			}

			orderData, err := processor.ProcessOrder(ctx, cmd)
			if err != nil {
				log.Printf("Error processing order: %v", err)
				continue
			}

			log.Printf("Order created: %s, Total: $%.2f, Items: %d", 
				orderData.OrderID, orderData.TotalAmount, len(orderData.Items))
		}
	}
}

// handleMessagesWithSaga processes messages using the saga pattern
func handleMessagesWithSaga(ctx context.Context, subscriber message.Subscriber, publisher message.Publisher) {
	// Define topics to process
	topics := []string{"order.created", "payment.processed", "inventory.checked"}

	// Create a WaitGroup to wait for all handlers
	var wg sync.WaitGroup

	// Process each topic
	for _, topic := range topics {
		topic := topic // Create a new variable to avoid closure issues
		
		wg.Add(1)
		go func() {
			defer wg.Done()

			// Subscribe to the topic
			messages, err := subscriber.Subscribe(ctx, topic)
			if err != nil {
				log.Printf("Error subscribing to %s: %v", topic, err)
				return
			}

			// Process messages
			for {
				select {
				case <-ctx.Done():
					return
				case msg, ok := <-messages:
					if !ok {
						log.Printf("Channel closed for topic %s", topic)
						return
					}

					// Process the message with saga pattern
					processSaga(ctx, msg, topic, publisher)
					
					// Mark the message as processed
					msg.Ack()
				}
			}
		}()
	}

	// Wait for all handlers to complete
	wg.Wait()
}

// processSaga applies saga pattern to messages
func processSaga(ctx context.Context, msg *message.Message, topic string, publisher message.Publisher) {
	switch topic {
	case "order.created":
		var orderCreated order.OrderCreated
		if err := order.FromJSON(msg.Payload, &orderCreated); err != nil {
			log.Printf("Error unmarshaling OrderCreated: %v", err)
			return
		}

		// Create saga data
		sagaData := &saga.OrderData{
			OrderID:     orderCreated.OrderID,
			UserID:      orderCreated.UserID,
			Items:       orderCreated.Items,
			TotalAmount: orderCreated.TotalAmount,
		}

		// Start order saga
		sagaInstance, err := saga.StartOrderSaga(ctx, publisher, orderCreated.UserID, orderCreated.Items, orderCreated.TotalAmount)
		if err != nil {
			log.Printf("Error starting order saga: %v", err)
			return
		}

		log.Printf("Saga started for order %s, status: %v", 
			sagaData.OrderID, sagaInstance.Error == nil)

	case "payment.processed":
		// Handle payment processed events
		var paymentProcessed order.PaymentProcessed
		if err := order.FromJSON(msg.Payload, &paymentProcessed); err != nil {
			log.Printf("Error unmarshaling PaymentProcessed: %v", err)
			return
		}

		log.Printf("Payment %s processed for order %s: %s", 
			paymentProcessed.PaymentID, paymentProcessed.OrderID, paymentProcessed.Status)

		// In a real system, you might retrieve the saga by ID and continue it
	
	case "inventory.checked":
		// Handle inventory checked events
		var inventoryChecked order.InventoryChecked
		if err := order.FromJSON(msg.Payload, &inventoryChecked); err != nil {
			log.Printf("Error unmarshaling InventoryChecked: %v", err)
			return
		}

		log.Printf("Inventory checked for order %s, all available: %v", 
			inventoryChecked.OrderID, inventoryChecked.AllItemsAvailable)

		// In a real system, you might retrieve the saga by ID and continue it
		
		// If inventory is not available, we need to compensate
		if !inventoryChecked.AllItemsAvailable {
			// Create a compensation event for demonstration
			orderCancelled := order.OrderCancelled{
				OrderID:      inventoryChecked.OrderID,
				Reason:       "Inventory not available",
				RefundStatus: "refunded",
				Timestamp:    time.Now(),
			}

			payload, err := order.ToJSON(orderCancelled)
			if err != nil {
				log.Printf("Error marshaling OrderCancelled: %v", err)
				return
			}

			msg := message.NewMessage(uuid.New().String(), payload)
			if err := publisher.Publish("order.cancelled", msg); err != nil {
				log.Printf("Error publishing OrderCancelled: %v", err)
				return
			}

			log.Printf("Compensation: Order %s cancelled due to inventory issues", 
				inventoryChecked.OrderID)
		}
	}
}