1	package saga
2	
3	import (
4		"context"
5		"fmt"
6		"log"
7		"time"
8	
9		"github.com/ThreeDotsLabs/watermill/message"
10		"github.com/google/uuid"
11		orderEvents "github.com/scrapybara/kafka-watermill-project/idl/go"
12	)
13	
14	// OrderData holds the data for our order saga
15	type OrderData struct {
16		OrderID      string
17		UserID       string
18		Items        []orderEvents.OrderItem
19		TotalAmount  float64
20		PaymentID    string
21		ShippingID   string
22		InventoryOK  bool
23	}
24	
25	// NewOrderSagaDefinition creates a new saga definition for order processing
26	func NewOrderSagaDefinition(publisher message.Publisher) SagaDefinition {
27		return NewSagaDefinition(
28			"order_processing",
29			// Step 1: Create the order
30			SagaStep{
31				Name: "create_order",
32				Handler: func(ctx context.Context, data interface{}) (interface{}, error) {
33					orderData, ok := data.(*OrderData)
34					if !ok {
35						return nil, fmt.Errorf("invalid data type for order creation")
36					}
37					
38					log.Printf("Creating order for user: %s", orderData.UserID)
39					
40					// Simulate creating an order
41					orderData.OrderID = uuid.New().String()
42					
43					// Publish order created event
44					event := orderEvents.OrderCreated{
45						OrderID:     orderData.OrderID,
46						UserID:      orderData.UserID,
47						Items:       orderData.Items,
48						TotalAmount: orderData.TotalAmount,
49						Timestamp:   time.Now(),
50					}