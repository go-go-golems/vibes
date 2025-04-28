package saga

import (
	"context"
	"fmt"
	"log"
	"time"

	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/google/uuid"
	orderEvents "github.com/scrapybara/kafka-watermill-project/idl/go"
)

// OrderData holds the data for our order saga
type OrderData struct {
	OrderID     string
	UserID      string
	Items       []orderEvents.OrderItem
	TotalAmount float64
	PaymentID   string
	ShippingID  string
	InventoryOK bool
}

// NewOrderSagaDefinition creates a new saga definition for order processing
func NewOrderSagaDefinition(publisher message.Publisher) SagaDefinition {
	return NewSagaDefinition(
		"order_processing",
		// Step 1: Create the order
		SagaStep{
			Name: "create_order",
			Handler: func(ctx context.Context, data interface{}) (interface{}, error) {
				orderData, ok := data.(*OrderData)
				if !ok {
					return nil, fmt.Errorf("invalid data type for order creation")
				}

				log.Printf("Creating order for user: %s", orderData.UserID)

				// Simulate creating an order
				orderData.OrderID = uuid.New().String()

				// Publish order created event
				event := orderEvents.OrderCreated{
					OrderID:     orderData.OrderID,
					UserID:      orderData.UserID,
					Items:       orderData.Items,
					TotalAmount: orderData.TotalAmount,
					Timestamp:   time.Now(),
				}

				payload, err := orderEvents.ToJSON(event)
				if err != nil {
					return nil, fmt.Errorf("error marshaling order created event: %w", err)
				}

				msg := message.NewMessage(uuid.New().String(), payload)
				if err := publisher.Publish("order.created", msg); err != nil {
					return nil, fmt.Errorf("error publishing order created event: %w", err)
				}

				return orderData, nil
			},
			Compensation: func(ctx context.Context, data interface{}) error {
				orderData, ok := data.(*OrderData)
				if !ok {
					return fmt.Errorf("invalid data type for order compensation")
				}

				log.Printf("Cancelling order: %s", orderData.OrderID)

				// Publish order cancelled event
				event := orderEvents.OrderCancelled{
					OrderID:      orderData.OrderID,
					Reason:       "Saga compensation",
					RefundStatus: "not_needed",
					Timestamp:    time.Now(),
				}

				payload, err := orderEvents.ToJSON(event)
				if err != nil {
					return fmt.Errorf("error marshaling order cancelled event: %w", err)
				}

				msg := message.NewMessage(uuid.New().String(), payload)
				if err := publisher.Publish("order.cancelled", msg); err != nil {
					return fmt.Errorf("error publishing order cancelled event: %w", err)
				}

				return nil
			},
		},
		// Step 2: Process payment
		SagaStep{
			Name: "process_payment",
			Handler: func(ctx context.Context, data interface{}) (interface{}, error) {
				orderData, ok := data.(*OrderData)
				if !ok {
					return nil, fmt.Errorf("invalid data type for payment processing")
				}

				log.Printf("Processing payment for order: %s, amount: %v",
					orderData.OrderID, orderData.TotalAmount)

				// Simulate payment processing
				orderData.PaymentID = uuid.New().String()

				// Check if payment should fail (for demo purposes, fail if amount > 1000)
				var status string
				if orderData.TotalAmount > 1000 {
					status = "failed"
					return nil, fmt.Errorf("payment failed, amount too large")
				} else {
					status = "success"
				}

				// Publish payment processed event
				event := orderEvents.PaymentProcessed{
					OrderID:       orderData.OrderID,
					PaymentID:     orderData.PaymentID,
					Status:        status,
					TransactionID: uuid.New().String(),
					Amount:        orderData.TotalAmount,
					Timestamp:     time.Now(),
				}

				payload, err := orderEvents.ToJSON(event)
				if err != nil {
					return nil, fmt.Errorf("error marshaling payment processed event: %w", err)
				}

				msg := message.NewMessage(uuid.New().String(), payload)
				if err := publisher.Publish("payment.processed", msg); err != nil {
					return nil, fmt.Errorf("error publishing payment processed event: %w", err)
				}

				return orderData, nil
			},
			Compensation: func(ctx context.Context, data interface{}) error {
				orderData, ok := data.(*OrderData)
				if !ok {
					return fmt.Errorf("invalid data type for payment compensation")
				}

				if orderData.PaymentID == "" {
					// Payment wasn't processed, nothing to compensate
					return nil
				}

				log.Printf("Refunding payment: %s for order: %s",
					orderData.PaymentID, orderData.OrderID)

				// In a real system, this would call the payment provider's refund API

				// Publish payment refunded event
				event := orderEvents.PaymentProcessed{
					OrderID:       orderData.OrderID,
					PaymentID:     orderData.PaymentID,
					Status:        "refunded",
					TransactionID: uuid.New().String(),
					Amount:        orderData.TotalAmount,
					Timestamp:     time.Now(),
				}

				payload, err := orderEvents.ToJSON(event)
				if err != nil {
					return fmt.Errorf("error marshaling payment refunded event: %w", err)
				}

				msg := message.NewMessage(uuid.New().String(), payload)
				if err := publisher.Publish("payment.processed", msg); err != nil {
					return fmt.Errorf("error publishing payment refunded event: %w", err)
				}

				return nil
			},
		},
		// Step 3: Check inventory
		SagaStep{
			Name: "check_inventory",
			Handler: func(ctx context.Context, data interface{}) (interface{}, error) {
				orderData, ok := data.(*OrderData)
				if !ok {
					return nil, fmt.Errorf("invalid data type for inventory check")
				}

				log.Printf("Checking inventory for order: %s", orderData.OrderID)

				// Simulate inventory check
				// For demo purposes, mark inventory as unavailable for orders with > 5 items
				hasInventory := true
				var unavailableItems []orderEvents.UnavailableItem

				if len(orderData.Items) > 5 {
					hasInventory = false
					unavailableItems = append(unavailableItems, orderEvents.UnavailableItem{
						ProductID:         orderData.Items[0].ProductID,
						Name:              orderData.Items[0].Name,
						RequestedQuantity: orderData.Items[0].Quantity,
						AvailableQuantity: 0,
					})
				}

				orderData.InventoryOK = hasInventory

				if !hasInventory {
					return nil, fmt.Errorf("inventory not available")
				}

				// Publish inventory checked event
				event := orderEvents.InventoryChecked{
					OrderID:           orderData.OrderID,
					AllItemsAvailable: hasInventory,
					UnavailableItems:  unavailableItems,
					Timestamp:         time.Now(),
				}

				payload, err := orderEvents.ToJSON(event)
				if err != nil {
					return nil, fmt.Errorf("error marshaling inventory checked event: %w", err)
				}

				msg := message.NewMessage(uuid.New().String(), payload)
				if err := publisher.Publish("inventory.checked", msg); err != nil {
					return nil, fmt.Errorf("error publishing inventory checked event: %w", err)
				}

				return orderData, nil
			},
			Compensation: func(ctx context.Context, data interface{}) error {
				orderData, ok := data.(*OrderData)
				if !ok {
					return fmt.Errorf("invalid data type for inventory compensation")
				}

				if !orderData.InventoryOK {
					// Inventory wasn't reserved, nothing to compensate
					return nil
				}

				log.Printf("Releasing inventory for order: %s", orderData.OrderID)

				// In a real system, this would release the reserved inventory

				return nil
			},
		},
		// Step 4: Fulfill order
		SagaStep{
			Name: "fulfill_order",
			Handler: func(ctx context.Context, data interface{}) (interface{}, error) {
				orderData, ok := data.(*OrderData)
				if !ok {
					return nil, fmt.Errorf("invalid data type for order fulfillment")
				}

				log.Printf("Fulfilling order: %s", orderData.OrderID)

				// Simulate order fulfillment
				orderData.ShippingID = uuid.New().String()

				// Publish order fulfilled event
				event := orderEvents.OrderFulfilled{
					OrderID:        orderData.OrderID,
					ShippingID:     orderData.ShippingID,
					TrackingNumber: fmt.Sprintf("TRK-%s", uuid.New().String()[:8]),
					Status:         "shipped",
					Timestamp:      time.Now(),
				}

				payload, err := orderEvents.ToJSON(event)
				if err != nil {
					return nil, fmt.Errorf("error marshaling order fulfilled event: %w", err)
				}

				msg := message.NewMessage(uuid.New().String(), payload)
				if err := publisher.Publish("order.fulfilled", msg); err != nil {
					return nil, fmt.Errorf("error publishing order fulfilled event: %w", err)
				}

				return orderData, nil
			},
			Compensation: func(ctx context.Context, data interface{}) error {
				orderData, ok := data.(*OrderData)
				if !ok {
					return fmt.Errorf("invalid data type for fulfillment compensation")
				}

				if orderData.ShippingID == "" {
					// Order wasn't fulfilled, nothing to compensate
					return nil
				}

				log.Printf("Cancelling shipment for order: %s, shipping ID: %s",
					orderData.OrderID, orderData.ShippingID)

				// In a real system, this would cancel the shipment

				return nil
			},
		},
	)
}

// StartOrderSaga initializes and starts an order processing saga
func StartOrderSaga(ctx context.Context, publisher message.Publisher, userID string, items []orderEvents.OrderItem, totalAmount float64) (*SagaInstance, error) {
	// Create the saga definition
	sagaDef := NewOrderSagaDefinition(publisher)

	// Initialize order data
	orderData := &OrderData{
		UserID:      userID,
		Items:       items,
		TotalAmount: totalAmount,
	}

	// Create a new saga instance
	sagaInstance := NewSagaInstance(sagaDef, orderData, publisher)

	// Start the saga
	if err := sagaInstance.Start(ctx); err != nil {
		return nil, fmt.Errorf("error executing order saga: %w", err)
	}

	return sagaInstance, nil
}
