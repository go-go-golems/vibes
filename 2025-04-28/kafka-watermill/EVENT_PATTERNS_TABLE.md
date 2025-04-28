# Event-Driven Patterns Implemented

| Pattern | Description | Implementation | Used In |
|---------|-------------|----------------|---------|
| **Saga Pattern** | A sequence of local transactions where each transaction publishes an event that triggers the next transaction. Includes compensating transactions for failures. | Implemented in Go using Watermill with explicit step definitions and compensation actions | Order flow from creation to fulfillment |
| **Event Sourcing** | Storing state changes as a sequence of events rather than just the current state | Implemented in Ruby Analytics Service to rebuild state from event history | Order analytics and reporting |
| **CQRS** | Separating read and write operations into different models | Implemented with Mediator pattern in Go services | Order queries and commands |
| **Publish-Subscribe** | Services publish messages without knowing who will consume them | Implemented across all services using Kafka as the message broker | All inter-service communication |
| **Mediator Pattern** | A central component that coordinates communication between multiple components | Implemented in Go services for command and event handling | Command processing in Go services |
| **Pipeline Pattern** | Processing data through a series of stages | Implemented for order processing flow | Order service processing flow |

## Key Message Flows

### Order Creation Flow
1. Order Service creates order → `order.created` topic
2. Payment Service processes payment → `payment.processed` topic
3. Inventory Service checks/reserves inventory → `inventory.checked` topic
4. Shipping Service prepares shipment → `shipping.initiated` topic
5. Notification Service sends confirmation → `notification.sent` topic

### Order Cancellation Flow
1. Order Service cancels order → `order.cancelled` topic
2. Payment Service issues refund → `payment.processed` (status: refunded) topic
3. Inventory Service releases reserved items → `inventory.released` topic
4. Notification Service sends cancellation notice → `notification.sent` topic