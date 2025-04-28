package eventsourcing

import (
	"fmt"
	"time"

	"github.com/google/uuid"
	orderEvents "github.com/scrapybara/kafka-watermill-project/idl/go"
)

// Order status constants
const (
	OrderStatusCreated    = "created"
	OrderStatusPaid       = "paid"
	OrderStatusFulfilled  = "fulfilled"
	OrderStatusCancelled  = "cancelled"
)

// OrderItem represents an item in an order
type OrderItem struct {
	ProductID string
	Name      string
	Quantity  int
	Price     float64
}

// Order represents an order aggregate
type Order struct {
	BaseAggregate
	UserID      string
	Items       []OrderItem
	TotalAmount float64
	Status      string
	PaymentID   string
	ShippingID  string
}

// NewOrder creates a new order aggregate
func NewOrder(id string) *Order {
	return &Order{
		BaseAggregate: BaseAggregate{
			ID: id,
		},
		Status: "",
	}
}

// ApplyEvent applies an event to the order
func (o *Order) ApplyEvent(event Event) error {
	switch e := event.(type) {
	case *OrderCreatedEvent:
		return o.applyOrderCreated(e)
	case *OrderPaidEvent:
		return o.applyOrderPaid(e)
	case *OrderFulfilledEvent:
		return o.applyOrderFulfilled(e)
	case *OrderCancelledEvent:
		return o.applyOrderCancelled(e)
	default:
		return fmt.Errorf("unknown event type: %T", event)
	}
}

// Event definitions

// OrderCreatedEvent represents an order creation event
type OrderCreatedEvent struct {
	BaseEvent
	UserID      string
	Items       []OrderItem
	TotalAmount float64
}

// OrderPaidEvent represents an order payment event
type OrderPaidEvent struct {
	BaseEvent
	PaymentID string
	Amount    float64
}

// OrderFulfilledEvent represents an order fulfillment event
type OrderFulfilledEvent struct {
	BaseEvent
	ShippingID     string
	TrackingNumber string
}

// OrderCancelledEvent represents an order cancellation event
type OrderCancelledEvent struct {
	BaseEvent
	Reason string
}

// Create a new order
func CreateOrder(userID string, items []OrderItem, totalAmount float64) *Order {
	orderID := uuid.New().String()
	order := NewOrder(orderID)
	
	event := &OrderCreatedEvent{
		BaseEvent: BaseEvent{
			AggregateID: orderID,
			EventType:   "OrderCreated",
			Timestamp:   time.Now(),
		},
		UserID:      userID,
		Items:       items,
		TotalAmount: totalAmount,
	}
	
	order.ApplyEvent(event)
	order.AddEvent(event)
	
	return order
}

// MarkAsPaid marks an order as paid
func (o *Order) MarkAsPaid(paymentID string, amount float64) error {
	if o.Status == OrderStatusCancelled {
		return fmt.Errorf("cannot pay a cancelled order")
	}
	if o.Status == OrderStatusPaid || o.Status == OrderStatusFulfilled {
		return fmt.Errorf("order already paid")
	}
	
	event := &OrderPaidEvent{
		BaseEvent: BaseEvent{
			AggregateID: o.ID,
			EventType:   "OrderPaid",
			Timestamp:   time.Now(),
		},
		PaymentID: paymentID,
		Amount:    amount,
	}
	
	if err := o.ApplyEvent(event); err != nil {
		return err
	}
	
	o.AddEvent(event)
	return nil
}

// Fulfill marks an order as fulfilled
func (o *Order) Fulfill(shippingID string, trackingNumber string) error {
	if o.Status != OrderStatusPaid {
		return fmt.Errorf("cannot fulfill an unpaid order")
	}
	if o.Status == OrderStatusFulfilled {
		return fmt.Errorf("order already fulfilled")
	}
	if o.Status == OrderStatusCancelled {
		return fmt.Errorf("cannot fulfill a cancelled order")
	}
	
	event := &OrderFulfilledEvent{
		BaseEvent: BaseEvent{
			AggregateID: o.ID,
			EventType:   "OrderFulfilled",
			Timestamp:   time.Now(),
		},
		ShippingID:     shippingID,
		TrackingNumber: trackingNumber,
	}
	
	if err := o.ApplyEvent(event); err != nil {
		return err
	}
	
	o.AddEvent(event)
	return nil
}

// Cancel marks an order as cancelled
func (o *Order) Cancel(reason string) error {
	if o.Status == OrderStatusCancelled {
		return fmt.Errorf("order already cancelled")
	}
	if o.Status == OrderStatusFulfilled {
		return fmt.Errorf("cannot cancel a fulfilled order")
	}
	
	event := &OrderCancelledEvent{
		BaseEvent: BaseEvent{
			AggregateID: o.ID,
			EventType:   "OrderCancelled",
			Timestamp:   time.Now(),
		},
		Reason: reason,
	}
	
	if err := o.ApplyEvent(event); err != nil {
		return err
	}
	
	o.AddEvent(event)
	return nil
}

// Event handling

func (o *Order) applyOrderCreated(event *OrderCreatedEvent) error {
	o.UserID = event.UserID
	o.Items = event.Items
	o.TotalAmount = event.TotalAmount
	o.Status = OrderStatusCreated
	return nil
}

func (o *Order) applyOrderPaid(event *OrderPaidEvent) error {
	o.PaymentID = event.PaymentID
	o.Status = OrderStatusPaid
	return nil
}

func (o *Order) applyOrderFulfilled(event *OrderFulfilledEvent) error {
	o.ShippingID = event.ShippingID
	o.Status = OrderStatusFulfilled
	return nil
}

func (o *Order) applyOrderCancelled(event *OrderCancelledEvent) error {
	o.Status = OrderStatusCancelled
	return nil
}

// Event to domain event conversion

// ToOrderCreatedEvent converts a domain OrderCreated event to an event store event
func ToOrderCreatedEvent(event orderEvents.OrderCreated) *OrderCreatedEvent {
	items := make([]OrderItem, len(event.Items))
	for i, item := range event.Items {
		items[i] = OrderItem{
			ProductID: item.ProductID,
			Name:      item.Name,
			Quantity:  item.Quantity,
			Price:     item.Price,
		}
	}
	
	return &OrderCreatedEvent{
		BaseEvent: BaseEvent{
			AggregateID: event.OrderID,
			EventType:   "OrderCreated",
			Timestamp:   event.Timestamp,
		},
		UserID:      event.UserID,
		Items:       items,
		TotalAmount: event.TotalAmount,
	}
}

// ToOrderPaidEvent converts a domain PaymentProcessed event to an event store event
func ToOrderPaidEvent(event orderEvents.PaymentProcessed) *OrderPaidEvent {
	return &OrderPaidEvent{
		BaseEvent: BaseEvent{
			AggregateID: event.OrderID,
			EventType:   "OrderPaid",
			Timestamp:   event.Timestamp,
		},
		PaymentID: event.PaymentID,
		Amount:    event.Amount,
	}
}

// ToOrderFulfilledEvent converts a domain OrderFulfilled event to an event store event
func ToOrderFulfilledEvent(event orderEvents.OrderFulfilled) *OrderFulfilledEvent {
	return &OrderFulfilledEvent{
		BaseEvent: BaseEvent{
			AggregateID: event.OrderID,
			EventType:   "OrderFulfilled",
			Timestamp:   event.Timestamp,
		},
		ShippingID:     event.ShippingID,
		TrackingNumber: event.TrackingNumber,
	}
}

// ToOrderCancelledEvent converts a domain OrderCancelled event to an event store event
func ToOrderCancelledEvent(event orderEvents.OrderCancelled) *OrderCancelledEvent {
	return &OrderCancelledEvent{
		BaseEvent: BaseEvent{
			AggregateID: event.OrderID,
			EventType:   "OrderCancelled",
			Timestamp:   event.Timestamp,
		},
		Reason: event.Reason,
	}
}