# Chapter 12: Event Sourcing

In the previous chapter, we explored how the Saga pattern helps maintain data consistency across distributed services in an event-driven architecture. Now, we turn our attention to another powerful pattern that fundamentally changes how we think about state management: Event Sourcing.

Event Sourcing represents a paradigm shift in how applications store and manage data. Rather than storing the current state of entities, an event-sourced system stores the complete sequence of events that led to that state. This approach aligns perfectly with Kafka's log-centric worldview discussed in Chapter 1 and provides numerous benefits for event-driven systems.

In this chapter, we'll explore the principles of Event Sourcing, examine its implementation in our reference system, and discuss how it complements other patterns like CQRS (which we'll cover in the next chapter).

## Understanding Event Sourcing

### The Traditional Approach: State-Based Persistence

In traditional applications, we typically store the current state of entities in a database. For example, an order in an e-commerce system might be represented as a row in an `orders` table:

```
orders
------
id: "order-123"
customer_id: "cust-456"
status: "shipped"
total_amount: 99.99
created_at: "2023-06-15T10:30:00Z"
updated_at: "2023-06-16T14:45:00Z"
```

When the order status changes (e.g., from "processing" to "shipped"), we update the `status` field and the `updated_at` timestamp. This approach is straightforward but has limitations:

1. **Loss of History**: We lose information about how the order reached its current state. We know it's "shipped" now, but we don't know when it was "processing" or "payment_confirmed".
2. **Limited Auditability**: Without additional logging, it's difficult to track who made changes and why.
3. **Temporal Queries**: Answering questions like "What was the status of this order yesterday?" requires additional mechanisms like temporal tables or audit logs.
4. **Concurrency Challenges**: Handling concurrent updates often relies on optimistic or pessimistic locking.

### The Event Sourcing Approach: Event-Based Persistence

Event Sourcing takes a fundamentally different approach. Instead of storing the current state, we store a sequence of events that represent all changes to an entity over time. For the same order, we might have:

```
order_events
-----------
id: "event-1", aggregate_id: "order-123", type: "OrderCreated", data: {...}, timestamp: "2023-06-15T10:30:00Z"
id: "event-2", aggregate_id: "order-123", type: "PaymentConfirmed", data: {...}, timestamp: "2023-06-15T10:35:00Z"
id: "event-3", aggregate_id: "order-123", type: "OrderProcessing", data: {...}, timestamp: "2023-06-15T11:00:00Z"
id: "event-4", aggregate_id: "order-123", type: "OrderShipped", data: {...}, timestamp: "2023-06-16T14:45:00Z"
```

The current state of the order is derived by replaying all events from the beginning. This approach addresses the limitations of state-based persistence:

1. **Complete History**: We retain the full history of how the order evolved over time.
2. **Built-in Auditability**: Each event captures what happened, when it happened, and often who initiated it.
3. **Temporal Queries**: We can reconstruct the state of the order at any point in time by replaying events up to that point.
4. **Simplified Concurrency**: Since events are append-only, many concurrency issues are eliminated.

### Key Concepts in Event Sourcing

Before diving into implementation details, let's clarify some key concepts:

#### Events

Events are immutable records of something that happened in the past. They are typically named using past tense verbs (e.g., `OrderCreated`, `PaymentProcessed`) and contain all the data relevant to that particular change.

```go
// Example event structure
type OrderShipped struct {
    BaseEvent           // Common fields like AggregateID, Timestamp
    TrackingNumber string
    ShippingMethod string
    EstimatedDelivery time.Time
}
```

#### Aggregates

An aggregate is a cluster of domain objects that can be treated as a single unit. In Event Sourcing, aggregates are the entities whose state is managed through events. Each aggregate has a unique identifier, and all events related to that aggregate reference this identifier.

```go
// Example aggregate structure
type Order struct {
    BaseAggregate       // Common fields and methods
    ID string
    CustomerID string
    Items []OrderItem
    Status string
    TotalAmount float64
    // ... other fields
}
```

#### Event Store

The event store is the persistence mechanism for events. It provides methods to save new events and retrieve the event stream for a specific aggregate. In our Kafka-based system, Kafka itself can serve as an event store, with topics organized by aggregate type.

#### Snapshots

As the number of events for an aggregate grows, replaying all events to reconstruct the current state can become inefficient. Snapshots provide a performance optimization by periodically capturing the aggregate's state, allowing us to start replay from the snapshot rather than from the beginning.

## Implementing Event Sourcing in Go

Our reference implementation includes a Go package for Event Sourcing (`kafka_content/pkg/eventsourcing`). Let's examine its key components.

### Event and Aggregate Interfaces

The package defines interfaces for events and aggregates:

```go
// From kafka_content/pkg/eventsourcing/eventsourcing.go
// Event is the base interface for all events
type Event interface {
    GetAggregateID() string
    GetEventType() string
    GetTimestamp() time.Time
}

// Aggregate is the base interface for event-sourced aggregates
type Aggregate interface {
    GetID() string
    ApplyEvent(event Event) error
    GetUncommittedEvents() []Event
    ClearUncommittedEvents()
}
```

These interfaces establish the contract for events and aggregates in our system. Events must provide methods to access their aggregate ID, type, and timestamp. Aggregates must be able to apply events, track uncommitted events, and provide access to their ID.

### Base Implementations

The package provides base implementations of these interfaces that can be embedded in concrete types:

```go
// From kafka_content/pkg/eventsourcing/eventsourcing.go
// BaseEvent provides common functionality for all events
type BaseEvent struct {
    AggregateID string    `json:"aggregate_id"`
    EventType   string    `json:"event_type"`
    Timestamp   time.Time `json:"timestamp"`
}

// BaseAggregate provides common functionality for all aggregates
type BaseAggregate struct {
    ID                string
    uncommittedEvents []Event
    mutex             sync.Mutex
}
```

The `BaseEvent` struct implements the `Event` interface, providing common fields and methods. Similarly, `BaseAggregate` implements most of the `Aggregate` interface, managing uncommitted events and providing thread-safe access.

### Event Store Interface and Implementation

The package defines an interface for event stores and provides a simple in-memory implementation:

```go
// From kafka_content/pkg/eventsourcing/eventsourcing.go
// EventStore is the interface for storing and retrieving events
type EventStore interface {
    SaveEvents(aggregateID string, events []Event) error
    GetEvents(aggregateID string) ([]Event, error)
}

// InMemoryEventStore is a simple in-memory implementation of EventStore
type InMemoryEventStore struct {
    events map[string][]Event
    mutex  sync.RWMutex
}
```

In a production system, we would implement this interface using Kafka or another persistent store. The in-memory implementation is useful for testing and demonstration purposes.

### Repository Pattern

The package uses the Repository pattern to provide a higher-level API for loading and saving aggregates:

```go
// From kafka_content/pkg/eventsourcing/eventsourcing.go
// Repository is the interface for loading and saving aggregates
type Repository[T Aggregate] interface {
    Load(id string) (T, error)
    Save(aggregate T) error
}

// GenericRepository is a generic implementation of Repository
type GenericRepository[T Aggregate] struct {
    eventStore       EventStore
    aggregateFactory func(id string) T
    eventHandlerMap  map[string]func(T, Event) error
}
```

The `GenericRepository` uses Go's generics to provide a type-safe way to work with different aggregate types. It takes an event store, a factory function for creating new aggregates, and a map of event handlers for applying different event types to the aggregate.

### Loading and Saving Aggregates

The repository's `Load` method reconstructs an aggregate by replaying its events:

```go
// From kafka_content/pkg/eventsourcing/eventsourcing.go
// Load loads an aggregate from the event store
func (r *GenericRepository[T]) Load(id string) (T, error) {
    // Create a new aggregate instance
    aggregate := r.aggregateFactory(id)

    // Get all events for this aggregate
    events, err := r.eventStore.GetEvents(id)
    if err != nil {
        return aggregate, fmt.Errorf("error loading events: %w", err)
    }

    // Apply each event to the aggregate
    for _, event := range events {
        if err := aggregate.ApplyEvent(event); err != nil {
            return aggregate, fmt.Errorf("error applying event: %w", err)
        }
    }

    return aggregate, nil
}
```

The `Save` method persists uncommitted events to the event store:

```go
// From kafka_content/pkg/eventsourcing/eventsourcing.go
// Save saves uncommitted events to the event store
func (r *GenericRepository[T]) Save(aggregate T) error {
    // Get uncommitted events
    events := aggregate.GetUncommittedEvents()
    if len(events) == 0 {
        return nil
    }

    // Save events to the event store
    if err := r.eventStore.SaveEvents(aggregate.GetID(), events); err != nil {
        return fmt.Errorf("error saving events: %w", err)
    }

    // Clear uncommitted events
    aggregate.ClearUncommittedEvents()

    return nil
}
```

## Using Event Sourcing in Our Order Service

Now that we understand the core components of our Event Sourcing implementation, let's see how they're used in the Order service.

### Defining Order Events

First, we define the events that can occur in the order lifecycle:

```go
// Example event definitions (simplified)
type OrderCreated struct {
    eventsourcing.BaseEvent
    CustomerID   string
    Items        []OrderItem
    TotalAmount  float64
}

type PaymentProcessed struct {
    eventsourcing.BaseEvent
    PaymentID     string
    Amount        float64
    Status        string // "success" or "failed"
    TransactionID string
}

type OrderShipped struct {
    eventsourcing.BaseEvent
    TrackingNumber    string
    ShippingMethod    string
    EstimatedDelivery time.Time
}

// ... other event types
```

Each event type embeds `BaseEvent` to inherit common functionality and adds fields specific to that event type.

### Implementing the Order Aggregate

Next, we implement the Order aggregate, which knows how to apply different event types:

```go
// Example Order aggregate (simplified)
type Order struct {
    eventsourcing.BaseAggregate
    CustomerID   string
    Items        []OrderItem
    Status       string
    TotalAmount  float64
    PaymentID    string
    TrackingNumber string
    // ... other fields
}

// ApplyEvent applies an event to the order
func (o *Order) ApplyEvent(event eventsourcing.Event) error {
    switch e := event.(type) {
    case *OrderCreated:
        return o.applyOrderCreated(e)
    case *PaymentProcessed:
        return o.applyPaymentProcessed(e)
    case *OrderShipped:
        return o.applyOrderShipped(e)
    // ... other event types
    default:
        return fmt.Errorf("unknown event type: %s", event.GetEventType())
    }
}

// Event-specific apply methods
func (o *Order) applyOrderCreated(e *OrderCreated) error {
    o.ID = e.GetAggregateID()
    o.CustomerID = e.CustomerID
    o.Items = e.Items
    o.TotalAmount = e.TotalAmount
    o.Status = "created"
    return nil
}

func (o *Order) applyPaymentProcessed(e *PaymentProcessed) error {
    o.PaymentID = e.PaymentID
    if e.Status == "success" {
        o.Status = "payment_confirmed"
    } else {
        o.Status = "payment_failed"
    }
    return nil
}

func (o *Order) applyOrderShipped(e *OrderShipped) error {
    o.TrackingNumber = e.TrackingNumber
    o.Status = "shipped"
    return nil
}
```

The `ApplyEvent` method uses a type switch to delegate to event-specific apply methods. Each apply method updates the aggregate's state based on the event's data.

### Creating and Modifying Orders

To create a new order, we instantiate the aggregate, create an event, apply it to the aggregate, and save it:

```go
// Example: Creating a new order
func CreateOrder(repo eventsourcing.Repository[*Order], customerID string, items []OrderItem) (*Order, error) {
    // Generate a new order ID
    orderID := uuid.New().String()
    
    // Create a new order aggregate
    order := &Order{
        BaseAggregate: eventsourcing.BaseAggregate{ID: orderID},
    }
    
    // Create the OrderCreated event
    event := &OrderCreated{
        BaseEvent: eventsourcing.BaseEvent{
            AggregateID: orderID,
            EventType:   "OrderCreated",
            Timestamp:   time.Now(),
        },
        CustomerID:  customerID,
        Items:       items,
        TotalAmount: calculateTotal(items),
    }
    
    // Apply the event to the aggregate
    if err := order.ApplyEvent(event); err != nil {
        return nil, err
    }
    
    // Add the event to uncommitted events
    order.AddEvent(event)
    
    // Save the aggregate (which saves the uncommitted events)
    if err := repo.Save(order); err != nil {
        return nil, err
    }
    
    return order, nil
}
```

To modify an existing order, we load it, create and apply a new event, and save it:

```go
// Example: Processing payment for an order
func ProcessPayment(repo eventsourcing.Repository[*Order], orderID string, paymentID string, amount float64, status string) (*Order, error) {
    // Load the order
    order, err := repo.Load(orderID)
    if err != nil {
        return nil, err
    }
    
    // Create the PaymentProcessed event
    event := &PaymentProcessed{
        BaseEvent: eventsourcing.BaseEvent{
            AggregateID: orderID,
            EventType:   "PaymentProcessed",
            Timestamp:   time.Now(),
        },
        PaymentID: paymentID,
        Amount:    amount,
        Status:    status,
    }
    
    // Apply the event to the aggregate
    if err := order.ApplyEvent(event); err != nil {
        return nil, err
    }
    
    // Add the event to uncommitted events
    order.AddEvent(event)
    
    // Save the aggregate (which saves the uncommitted events)
    if err := repo.Save(order); err != nil {
        return nil, err
    }
    
    return order, nil
}
```

### Integrating with Kafka

In a Kafka-based system, we can implement the `EventStore` interface using Kafka as the underlying storage mechanism:

```go
// KafkaEventStore implements EventStore using Kafka
type KafkaEventStore struct {
    producer  message.Publisher
    consumer  message.Subscriber
    topicName string
}

// SaveEvents publishes events to Kafka
func (s *KafkaEventStore) SaveEvents(aggregateID string, events []eventsourcing.Event) error {
    for _, event := range events {
        // Serialize the event (e.g., using Protocol Buffers or JSON)
        payload, err := serializeEvent(event)
        if err != nil {
            return err
        }
        
        // Create a Watermill message
        msg := message.NewMessage(uuid.New().String(), payload)
        msg.Metadata.Set("aggregate_id", aggregateID)
        msg.Metadata.Set("event_type", event.GetEventType())
        
        // Publish to Kafka
        if err := s.producer.Publish(s.topicName, msg); err != nil {
            return err
        }
    }
    
   
(Content truncated due to size limit. Use line ranges to read in chunks)