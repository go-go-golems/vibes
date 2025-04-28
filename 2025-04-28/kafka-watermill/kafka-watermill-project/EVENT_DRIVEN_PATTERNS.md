# Event-Driven Patterns in Multi-Language Microservices

This document describes the event-driven patterns implemented across our multi-language microservices architecture using Kafka as the messaging backbone.

## 1. Saga Pattern

The Saga pattern is a way to manage distributed transactions across multiple microservices where each service performs its transaction and publishes an event. The subsequent steps are triggered by these events.

### 1.1 Go Implementation

In our Go microservices, we've implemented the Saga pattern with a robust framework that supports both orchestration and compensation:

```go
// Define saga steps
sagaDefinition := NewSagaDefinition(
    "order_processing",
    SagaStep{
        Name: "create_order",
        Handler: func(ctx context.Context, data interface{}) (interface{}, error) {
            // Handle order creation logic
        },
        Compensation: func(ctx context.Context, data interface{}) error {
            // Compensation logic for order creation
        },
    },
    // Additional steps...
)

// Start the saga
sagaInstance := NewSagaInstance(sagaDefinition, initialData, publisher)
if err := sagaInstance.Start(ctx); err != nil {
    // Handle error
}
```

Key components:
- `SagaStep`: Contains a forward operation and a compensation operation
- `SagaDefinition`: Defines the sequence of steps in a saga
- `SagaInstance`: Represents a running instance of a saga

### 1.2 Kotlin Implementation

Our Kotlin implementation provides a coroutine-based saga coordinator:

```kotlin
// Define saga steps
val shippingSteps = listOf(
    SagaStep<ShippingSagaData>(
        name = "reserve_inventory",
        handler = { data ->
            // Reserve inventory logic
            data
        },
        compensation = { data ->
            // Release inventory logic
            data
        }
    ),
    // Additional steps...
)

// Create the saga coordinator
val coordinator = SagaCoordinatorFactory(kafkaTemplate)
    .createCoordinator("shipping_saga", shippingSteps)

// Execute the saga
val result = coordinator.executeSaga(sagaData)
```

Key components:
- `SagaStep`: Contains a suspending handler and compensation function
- `SagaCoordinator`: Executes the steps in sequence and handles compensation on failure
- `SagaData`: Data that flows through the saga steps

### 1.3 Ruby Implementation

While Ruby doesn't implement a full saga pattern, it uses event sourcing for proper state tracking:

```ruby
# Creating events in the aggregate
aggregate.add_event('ORDER_CREATED', { order_data })
aggregate.add_event('PAYMENT_PROCESSED', { payment_data })

# Event handlers updating the read model
def on_order_created(event)
  # Update the read model
end

def on_payment_processed(event)
  # Update the read model
end
```

## 2. Event Sourcing

Event Sourcing stores all changes to application state as a sequence of events, allowing for rebuilding the state from these events.

### 2.1 Ruby Implementation

Our Ruby service implements a complete event sourcing solution:

```ruby
# Base event
class Event
  attr_reader :event_id, :aggregate_id, :event_type, :timestamp, :data
  # Event methods...
end

# Base aggregate
class Aggregate
  def apply_event(event)
    method_name = "apply_#{event.event_type.downcase}"
    send(method_name, event.data) if respond_to?(method_name, true)
  end
  
  def add_event(event_type, data = {})
    event = Event.new(@id, event_type, data)
    apply_event(event)
    @uncommitted_events << event
    event
  end
end

# Repository to load and save aggregates
class Repository
  def load(aggregate_id)
    aggregate = @aggregate_class.new(aggregate_id)
    events = @event_store.get_events_for_aggregate(aggregate_id)
    aggregate.apply_all_events(events)
    aggregate
  end

  def save(aggregate)
    @event_store.save_events(aggregate.get_uncommitted_events)
    aggregate.clear_uncommitted_events
  end
end
```

### 2.2 Go Implementation

Go implements event sourcing in the `eventsourcing` package:

```go
// Event interface
type Event interface {
    GetAggregateID() string
    GetEventType() string
    GetTimestamp() time.Time
}

// Aggregate interface
type Aggregate interface {
    GetID() string
    ApplyEvent(event Event) error
    GetUncommittedEvents() []Event
    ClearUncommittedEvents()
}

// Repository implementation
func (r *GenericRepository[T]) Load(id string) (T, error) {
    aggregate := r.aggregateFactory(id)
    events, err := r.eventStore.GetEvents(id)
    if err != nil {
        return aggregate, err
    }
    
    for _, event := range events {
        aggregate.ApplyEvent(event)
    }
    
    return aggregate, nil
}
```

## 3. Command Query Responsibility Segregation (CQRS)

CQRS separates the command (write) and query (read) operations, allowing for optimization of each independently.

### 3.1 Ruby Implementation

Our Ruby service implements CQRS with separate command and query buses:

```ruby
# Command handling
class CommandBus
  def register(command_class, handler)
    @handlers[command_class.name] = handler
  end
  
  def dispatch(command)
    handler = @handlers[command.class.name]
    handler.execute(command)
  end
end

# Query handling
class QueryBus
  def register(query_class, handler)
    @handlers[query_class.name] = handler
  end
  
  def dispatch(query)
    handler = @handlers[query.class.name]
    handler.execute(query)
  end
end
```

### 3.2 Go Implementation

In Go, we use the mediator pattern to implement CQRS:

```go
// Command interface
type Command interface {
    CommandType() string
}

// Query interface
type Query interface {
    QueryType() string
}

// Mediator handles commands and queries
type Mediator interface {
    Send(ctx context.Context, command Command) (interface{}, error)
    Query(ctx context.Context, query Query) (interface{}, error)
}
```

## 4. Mediator Pattern

The mediator pattern decouples components by having them communicate via a mediator object.

### 4.1 Go Implementation

Our Go services use a mediator for handling commands, queries, and events:

```go
// Register handlers
med.RegisterCommandHandler("CreateOrder", orderHandler)
med.RegisterQueryHandler("GetOrderStatus", statusHandler)
med.RegisterEventHandler("OrderCreated", notificationHandler)

// Send a command
result, err := med.Send(ctx, createOrderCmd)

// Query data
result, err := med.Query(ctx, getStatusQuery)

// Publish an event
err := med.Publish(ctx, orderCreatedEvent)
```

## 5. Pipeline Pattern

The pipeline pattern chains processors in a sequence, with each performing a specific operation on the data.

### 5.1 Go Implementation

```go
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

// Process data through the pipeline
result := pipeline.Process(ctx, inputData)
```

## 6. Choreography vs. Orchestration

Our architecture demonstrates both choreography and orchestration approaches:

### 6.1 Choreography (Event-Based)

Services react to events without a central coordinator:

- **Go Services**: Order service publishes `order.created`, Payment service reacts and publishes `payment.processed`, etc.
- **Kotlin Service**: Shipping service listens for `inventory.checked` and decides whether to fulfill or cancel
- **Ruby Service**: Analytics service processes events as they occur and updates metrics

### 6.2 Orchestration (Saga-Based)

A central coordinator manages the workflow:

- **Go Services**: Saga pattern in the order process coordinates the steps with compensation
- **Kotlin Services**: ShippingSagaManager orchestrates the shipping process

## 7. Compensation Transactions

Compensation transactions allow the system to recover from failures by undoing previous steps:

```go
// Go compensation example
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
    
    // Publish the event
    return nil
}
```

```kotlin
// Kotlin compensation example
compensation = { data ->
    logger.info("Releasing inventory for order: ${data.orderId}")
    
    // In a real system, this would call the inventory service to release the hold
    data.inventoryReserved = false
    data
}
```

## 8. Event-Driven Architecture Benefits

Our implementation demonstrates several benefits of event-driven architecture:

1. **Loose Coupling**: Services communicate through events, not direct calls
2. **Scalability**: Each service can scale independently
3. **Resilience**: Service failures don't bring down the entire system
4. **Polyglot Architecture**: Different languages (Go, Kotlin, Ruby) can coexist
5. **Asynchronous Processing**: Non-blocking operations improve throughput
6. **Observability**: Events provide an audit trail of system behavior

## 9. Implementation Challenges

Working with event-driven architectures presents several challenges:

1. **Eventual Consistency**: Services may see data at different points in time
2. **Event Schema Evolution**: Changing event schemas requires careful planning
3. **Debugging**: Tracing issues across distributed events is complex
4. **Testing**: End-to-end testing requires all services and Kafka
5. **Idempotency**: Services must handle duplicate events correctly

## Conclusion

Our multi-language microservices implementation successfully demonstrates several key event-driven patterns, combining the strengths of Go, Kotlin, and Ruby with Kafka as the messaging backbone. These patterns provide a robust foundation for building complex, distributed systems that are scalable, resilient, and maintainable.