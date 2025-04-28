# Kafka and Watermill Multi-Language Microservices Project

This project demonstrates how to use Apache Kafka with the Watermill framework to implement event-driven patterns across multiple microservices written in different languages:

- Go (using Watermill)
- Kotlin (using Spring Kafka)
- Ruby (using ruby-kafka)

## Architecture

The project implements an e-commerce order processing flow with multiple microservices:

1. **Order Service (Go)**: Creates orders and publishes order events
2. **Payment Service (Go)**: Processes payments when orders are created
3. **Inventory Service (Go)**: Checks inventory when payments are successful
4. **Shipping Service (Kotlin)**: Fulfills orders when inventory is available
5. **Notification Service (Go)**: Sends notifications for various events
6. **Analytics Service (Ruby)**: Collects and analyzes event data

All services communicate asynchronously through Kafka topics, implementing various event-driven patterns.

## Common IDL (Interface Definition Language)

We use a common event definition across all services:

- Protocol Buffers `.proto` files for schema definition
- Generated or manually created code for each language
- Consistent event structure across all services

## Event Flow

The basic flow of events in this system:

1. `order.created` - When a new order is created
2. `payment.processed` - When payment is processed (success or failure)
3. `inventory.checked` - When inventory is checked for availability
4. `order.fulfilled` - When an order is fulfilled and shipped
5. `order.cancelled` - When an order is cancelled (due to inventory, payment issues, etc.)

## Setup

### Prerequisites

- Docker and Docker Compose
- Go 1.18 or higher
- Java 17 or higher with Kotlin (for Kotlin service)
- Ruby 3.0 or higher (for Ruby service)

### Running the Project

1. Start Kafka and related services:

```bash
docker-compose up -d
```

2. Build and run the Go services:

```bash
# Build all services
cd cmd/order-service && go build -o ../../bin/order-service
cd cmd/payment-service && go build -o ../../bin/payment-service
cd cmd/inventory-service && go build -o ../../bin/inventory-service
cd cmd/notification-service && go build -o ../../bin/notification-service

# Run services
./bin/order-service &
./bin/payment-service &
./bin/inventory-service &
./bin/notification-service &
```

3. Build and run the Kotlin service:

```bash
cd kotlin-service
./gradlew bootRun
```

4. Run the Ruby service:

```bash
cd ruby-service
bundle install
ruby analytics_service.rb
```

## Event-Driven Patterns Demonstrated

This project demonstrates several event-driven patterns:

1. **Event Notification**: Services publish events when something happens
2. **Event-Carried State Transfer**: Events contain all necessary state
3. **Event Sourcing**: Order state can be rebuilt from the sequence of events
4. **CQRS**: Separation of write (commands) and read (queries) operations
5. **Sagas**: Complex workflows coordinated across multiple services

## Monitoring

- Kafka UI is available at http://localhost:8080
- Analytics service provides a REST API at http://localhost:3000/analytics

## License

MIT