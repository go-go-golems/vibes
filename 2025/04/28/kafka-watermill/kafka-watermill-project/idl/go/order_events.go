package order

import (
	"encoding/json"
	"time"
)

// OrderItem represents an item in an order
type OrderItem struct {
	ProductID string  `json:"product_id"`
	Name      string  `json:"name"`
	Quantity  int     `json:"quantity"`
	Price     float64 `json:"price"`
}

// UnavailableItem represents an item that's not available in inventory
type UnavailableItem struct {
	ProductID         string `json:"product_id"`
	Name              string `json:"name"`
	RequestedQuantity int    `json:"requested_quantity"`
	AvailableQuantity int    `json:"available_quantity"`
}

// OrderCreated event represents an order created by a user
type OrderCreated struct {
	OrderID     string      `json:"order_id"`
	UserID      string      `json:"user_id"`
	Items       []OrderItem `json:"items"`
	TotalAmount float64     `json:"total_amount"`
	Timestamp   time.Time   `json:"timestamp"`
}

// PaymentProcessed event represents a payment being processed for an order
type PaymentProcessed struct {
	OrderID       string    `json:"order_id"`
	PaymentID     string    `json:"payment_id"`
	Status        string    `json:"status"` // "success", "failed", "pending"
	TransactionID string    `json:"transaction_id"`
	Amount        float64   `json:"amount"`
	Timestamp     time.Time `json:"timestamp"`
}

// InventoryChecked event represents inventory being checked for an order
type InventoryChecked struct {
	OrderID          string            `json:"order_id"`
	AllItemsAvailable bool              `json:"all_items_available"`
	UnavailableItems []UnavailableItem `json:"unavailable_items"`
	Timestamp        time.Time         `json:"timestamp"`
}

// OrderFulfilled event represents order fulfillment
type OrderFulfilled struct {
	OrderID        string    `json:"order_id"`
	ShippingID     string    `json:"shipping_id"`
	TrackingNumber string    `json:"tracking_number"`
	Status         string    `json:"status"`
	Timestamp      time.Time `json:"timestamp"`
}

// OrderCancelled event represents an order cancellation
type OrderCancelled struct {
	OrderID      string    `json:"order_id"`
	Reason       string    `json:"reason"`
	RefundStatus string    `json:"refund_status"`
	Timestamp    time.Time `json:"timestamp"`
}

// Notification event for various order process steps
type Notification struct {
	OrderID          string    `json:"order_id"`
	NotificationType string    `json:"notification_type"` // "order_confirmation", "payment_success", "payment_failed", "shipping_update", etc.
	Recipient        string    `json:"recipient"`
	Content          string    `json:"content"`
	Timestamp        time.Time `json:"timestamp"`
}

// Serialization helpers

// ToJSON converts an event to JSON byte array
func ToJSON(event interface{}) ([]byte, error) {
	return json.Marshal(event)
}

// FromJSON converts JSON byte array to an event
func FromJSON(data []byte, event interface{}) error {
	return json.Unmarshal(data, event)
}