package orderprocessing

import (
	"context"
	"fmt"
	"log"
	"time"

	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/google/uuid"
	order "github.com/scrapybara/kafka-watermill-project/idl/go"
	"github.com/scrapybara/kafka-watermill-project/pkg/mediator"
	"github.com/scrapybara/kafka-watermill-project/pkg/pipeline"
)

// OrderProcessingData contains data for order processing
type OrderProcessingData struct {
	OrderID           string
	UserID            string
	Items             []order.OrderItem
	TotalAmount       float64
	PaymentID         string
	PaymentStatus     string
	InventoryOK       bool
	ShippingID        string
	TrackingNumber    string
	FulfillmentStatus string
	CreatedAt         time.Time
	UpdatedAt         time.Time
}

// OrderCommands
type CreateOrderCommand struct {
	UserID      string
	Items       []order.OrderItem
	TotalAmount float64
}

func (c CreateOrderCommand) CommandType() string {
	return "CreateOrder"
}

type ProcessPaymentCommand struct {
	OrderID     string
	TotalAmount float64
}

func (c ProcessPaymentCommand) CommandType() string {
	return "ProcessPayment"
}

type CheckInventoryCommand struct {
	OrderID string
	Items   []order.OrderItem
}

func (c CheckInventoryCommand) CommandType() string {
	return "CheckInventory"
}

type FulfillOrderCommand struct {
	OrderID string
}

func (c FulfillOrderCommand) CommandType() string {
	return "FulfillOrder"
}

// OrderEvents
type OrderCreatedEvent struct {
	OrderID     string
	UserID      string
	Items       []order.OrderItem
	TotalAmount float64
	Timestamp   time.Time
}

func (e OrderCreatedEvent) EventType() string {
	return "OrderCreated"
}

type PaymentProcessedEvent struct {
	OrderID       string
	PaymentID     string
	Status        string
	TransactionID string
	Amount        float64
	Timestamp     time.Time
}

func (e PaymentProcessedEvent) EventType() string {
	return "PaymentProcessed"
}

type InventoryCheckedEvent struct {
	OrderID           string
	AllItemsAvailable bool
	UnavailableItems  []order.UnavailableItem
	Timestamp         time.Time
}

func (e InventoryCheckedEvent) EventType() string {
	return "InventoryChecked"
}

type OrderFulfilledEvent struct {
	OrderID        string
	ShippingID     string
	TrackingNumber string
	Status         string
	Timestamp      time.Time
}

func (e OrderFulfilledEvent) EventType() string {
	return "OrderFulfilled"
}

// CommandHandlers
type CreateOrderHandler struct {
	mediator  mediator.Mediator
	publisher message.Publisher
}

func NewCreateOrderHandler(med mediator.Mediator, pub message.Publisher) *CreateOrderHandler {
	return &CreateOrderHandler{
		mediator:  med,
		publisher: pub,
	}
}

func (h *CreateOrderHandler) Handle(ctx context.Context, cmd mediator.Command) (interface{}, error) {
	createOrderCmd, ok := cmd.(CreateOrderCommand)
	if !ok {
		return nil, fmt.Errorf("invalid command type: %T", cmd)
	}

	// Create order
	orderID := uuid.New().String()
	timestamp := time.Now()

	orderData := &OrderProcessingData{
		OrderID:     orderID,
		UserID:      createOrderCmd.UserID,
		Items:       createOrderCmd.Items,
		TotalAmount: createOrderCmd.TotalAmount,
		CreatedAt:   timestamp,
		UpdatedAt:   timestamp,
	}

	// Publish OrderCreated event
	event := OrderCreatedEvent{
		OrderID:     orderID,
		UserID:      createOrderCmd.UserID,
		Items:       createOrderCmd.Items,
		TotalAmount: createOrderCmd.TotalAmount,
		Timestamp:   timestamp,
	}

	if err := h.mediator.Publish(ctx, event); err != nil {
		return nil, fmt.Errorf("error publishing OrderCreated event: %w", err)
	}

	// Convert to Kafka event
	kafkaEvent := order.OrderCreated{
		OrderID:     orderID,
		UserID:      createOrderCmd.UserID,
		Items:       createOrderCmd.Items,
		TotalAmount: createOrderCmd.TotalAmount,
		Timestamp:   timestamp,
	}

	payload, err := order.ToJSON(kafkaEvent)
	if err != nil {
		return nil, fmt.Errorf("error marshaling OrderCreated event: %w", err)
	}

	msg := message.NewMessage(uuid.New().String(), payload)
	if err := h.publisher.Publish("order.created", msg); err != nil {
		return nil, fmt.Errorf("error publishing to Kafka: %w", err)
	}

	return orderData, nil
}

type ProcessPaymentHandler struct {
	mediator  mediator.Mediator
	publisher message.Publisher
}

func NewProcessPaymentHandler(med mediator.Mediator, pub message.Publisher) *ProcessPaymentHandler {
	return &ProcessPaymentHandler{
		mediator:  med,
		publisher: pub,
	}
}

func (h *ProcessPaymentHandler) Handle(ctx context.Context, cmd mediator.Command) (interface{}, error) {
	processPaymentCmd, ok := cmd.(ProcessPaymentCommand)
	if !ok {
		return nil, fmt.Errorf("invalid command type: %T", cmd)
	}

	// Process payment (in a real system, this would call a payment service)
	paymentID := uuid.New().String()
	transactionID := uuid.New().String()
	status := "success" // Simulate success

	// For demo purposes, fail payments over $1000
	if processPaymentCmd.TotalAmount > 1000 {
		status = "failed"
	}

	timestamp := time.Now()

	// Publish PaymentProcessed event
	event := PaymentProcessedEvent{
		OrderID:       processPaymentCmd.OrderID,
		PaymentID:     paymentID,
		Status:        status,
		TransactionID: transactionID,
		Amount:        processPaymentCmd.TotalAmount,
		Timestamp:     timestamp,
	}

	if err := h.mediator.Publish(ctx, event); err != nil {
		return nil, fmt.Errorf("error publishing PaymentProcessed event: %w", err)
	}

	// Convert to Kafka event
	kafkaEvent := order.PaymentProcessed{
		OrderID:       processPaymentCmd.OrderID,
		PaymentID:     paymentID,
		Status:        status,
		TransactionID: transactionID,
		Amount:        processPaymentCmd.TotalAmount,
		Timestamp:     timestamp,
	}

	payload, err := order.ToJSON(kafkaEvent)
	if err != nil {
		return nil, fmt.Errorf("error marshaling PaymentProcessed event: %w", err)
	}

	msg := message.NewMessage(uuid.New().String(), payload)
	if err := h.publisher.Publish("payment.processed", msg); err != nil {
		return nil, fmt.Errorf("error publishing to Kafka: %w", err)
	}

	return &OrderProcessingData{
		OrderID:       processPaymentCmd.OrderID,
		PaymentID:     paymentID,
		PaymentStatus: status,
		UpdatedAt:     timestamp,
	}, nil
}

// Create an order processing pipeline that combines all steps
type OrderProcessor struct {
	mediator  mediator.Mediator
	publisher message.Publisher
	pipeline  *pipeline.Pipeline
}

func NewOrderProcessor(med mediator.Mediator, pub message.Publisher) *OrderProcessor {
	processor := &OrderProcessor{
		mediator:  med,
		publisher: pub,
	}

	// Create pipeline stages
	createOrderStage := func(ctx context.Context, data interface{}) (interface{}, error) {
		createOrderCmd, ok := data.(CreateOrderCommand)
		if !ok {
			return nil, fmt.Errorf("expected CreateOrderCommand, got %T", data)
		}

		result, err := med.Send(ctx, createOrderCmd)
		if err != nil {
			return nil, fmt.Errorf("error creating order: %w", err)
		}

		return result, nil
	}

	processPaymentStage := func(ctx context.Context, data interface{}) (interface{}, error) {
		orderData, ok := data.(*OrderProcessingData)
		if !ok {
			return nil, fmt.Errorf("expected OrderProcessingData, got %T", data)
		}

		processPaymentCmd := ProcessPaymentCommand{
			OrderID:     orderData.OrderID,
			TotalAmount: orderData.TotalAmount,
		}

		result, err := med.Send(ctx, processPaymentCmd)
		if err != nil {
			return nil, fmt.Errorf("error processing payment: %w", err)
		}

		paymentData := result.(*OrderProcessingData)

		// Update order data
		orderData.PaymentID = paymentData.PaymentID
		orderData.PaymentStatus = paymentData.PaymentStatus
		orderData.UpdatedAt = paymentData.UpdatedAt

		return orderData, nil
	}

	checkInventoryStage := func(ctx context.Context, data interface{}) (interface{}, error) {
		orderData, ok := data.(*OrderProcessingData)
		if !ok {
			return nil, fmt.Errorf("expected OrderProcessingData, got %T", data)
		}

		// Skip if payment failed
		if orderData.PaymentStatus != "success" {
			log.Printf("Skipping inventory check for order %s because payment failed", orderData.OrderID)
			return orderData, nil
		}

		// Check inventory
		_ = CheckInventoryCommand{
			OrderID: orderData.OrderID,
			Items:   orderData.Items,
		}

		// This would normally call the mediator, but for simplicity
		// we'll directly check inventory here
		allAvailable := true
		var unavailableItems []order.UnavailableItem

		// Simulate inventory check - mark some items as unavailable
		for _, item := range orderData.Items {
			if item.ProductID == "product3" {
				allAvailable = false
				unavailableItems = append(unavailableItems, order.UnavailableItem{
					ProductID:         item.ProductID,
					Name:              item.Name,
					RequestedQuantity: item.Quantity,
					AvailableQuantity: 0,
				})
			}
		}

		timestamp := time.Now()

		// Publish InventoryChecked event
		event := InventoryCheckedEvent{
			OrderID:           orderData.OrderID,
			AllItemsAvailable: allAvailable,
			UnavailableItems:  unavailableItems,
			Timestamp:         timestamp,
		}

		if err := med.Publish(ctx, event); err != nil {
			return nil, fmt.Errorf("error publishing InventoryChecked event: %w", err)
		}

		// Convert to Kafka event
		kafkaEvent := order.InventoryChecked{
			OrderID:           orderData.OrderID,
			AllItemsAvailable: allAvailable,
			UnavailableItems:  unavailableItems,
			Timestamp:         timestamp,
		}

		payload, err := order.ToJSON(kafkaEvent)
		if err != nil {
			return nil, fmt.Errorf("error marshaling InventoryChecked event: %w", err)
		}

		msg := message.NewMessage(uuid.New().String(), payload)
		if err := pub.Publish("inventory.checked", msg); err != nil {
			return nil, fmt.Errorf("error publishing to Kafka: %w", err)
		}

		// Update order data
		orderData.InventoryOK = allAvailable
		orderData.UpdatedAt = timestamp

		return orderData, nil
	}

	// Create pipeline using builder
	pipelineBuilder := pipeline.NewPipelineBuilder("OrderProcessing")
	pipelineBuilder.AddStage(createOrderStage)
	pipelineBuilder.AddStage(processPaymentStage)
	pipelineBuilder.AddConditionalStage(
		func(data interface{}) bool {
			orderData, ok := data.(*OrderProcessingData)
			return ok && orderData.PaymentStatus == "success"
		},
		checkInventoryStage,
	)

	processor.pipeline = pipelineBuilder.Build()

	return processor
}

// ProcessOrder processes a new order through the pipeline
func (p *OrderProcessor) ProcessOrder(ctx context.Context, cmd CreateOrderCommand) (*OrderProcessingData, error) {
	result := p.pipeline.Process(ctx, cmd)
	if result.Error != nil {
		return nil, result.Error
	}

	orderData, ok := result.Data.(*OrderProcessingData)
	if !ok {
		return nil, fmt.Errorf("expected OrderProcessingData, got %T", result.Data)
	}

	log.Printf("Order processed: %s, payment: %s, inventory: %v, duration: %v",
		orderData.OrderID, orderData.PaymentStatus, orderData.InventoryOK, result.Duration)

	return orderData, nil
}

// Process a Kafka message
func (p *OrderProcessor) ProcessKafkaMessage(msg *message.Message) error {
	// Extract event type from topic
	topic := msg.Metadata.Get("topic")
	var event interface{}

	switch topic {
	case "order.created":
		var orderCreated order.OrderCreated
		if err := order.FromJSON(msg.Payload, &orderCreated); err != nil {
			return fmt.Errorf("error unmarshaling OrderCreated: %w", err)
		}
		event = OrderCreatedEvent{
			OrderID:     orderCreated.OrderID,
			UserID:      orderCreated.UserID,
			Items:       orderCreated.Items,
			TotalAmount: orderCreated.TotalAmount,
			Timestamp:   orderCreated.Timestamp,
		}

	case "payment.processed":
		var paymentProcessed order.PaymentProcessed
		if err := order.FromJSON(msg.Payload, &paymentProcessed); err != nil {
			return fmt.Errorf("error unmarshaling PaymentProcessed: %w", err)
		}
		event = PaymentProcessedEvent{
			OrderID:       paymentProcessed.OrderID,
			PaymentID:     paymentProcessed.PaymentID,
			Status:        paymentProcessed.Status,
			TransactionID: paymentProcessed.TransactionID,
			Amount:        paymentProcessed.Amount,
			Timestamp:     paymentProcessed.Timestamp,
		}

	case "inventory.checked":
		var inventoryChecked order.InventoryChecked
		if err := order.FromJSON(msg.Payload, &inventoryChecked); err != nil {
			return fmt.Errorf("error unmarshaling InventoryChecked: %w", err)
		}
		event = InventoryCheckedEvent{
			OrderID:           inventoryChecked.OrderID,
			AllItemsAvailable: inventoryChecked.AllItemsAvailable,
			UnavailableItems:  inventoryChecked.UnavailableItems,
			Timestamp:         inventoryChecked.Timestamp,
		}

	default:
		return fmt.Errorf("unknown topic: %s", topic)
	}

	// Publish event to internal mediator
	ctx := context.Background()
	if err := p.mediator.Publish(ctx, event.(mediator.Event)); err != nil {
		return fmt.Errorf("error publishing to mediator: %w", err)
	}

	return nil
}
