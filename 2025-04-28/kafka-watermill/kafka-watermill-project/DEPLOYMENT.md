# Deployment Guide

This document provides instructions for deploying and running the Kafka-Watermill multi-language microservices system.

## Prerequisites

- Docker and Docker Compose
- Go 1.18 or higher (for local development)
- Java 17 or higher (for Kotlin service development)
- Ruby 3.0 or higher (for Ruby service development)

## Deployment Steps

### 1. Clone the Repository

```bash
git clone <repository-url>
cd kafka-watermill-project
```

### 2. Automated Deployment

The easiest way to deploy the system is using the provided deployment script:

```bash
./deploy.sh
```

This script will:
- Check if Docker is running
- Install necessary Go dependencies
- Build and start all services using Docker Compose
- Run health checks to ensure all services are functioning correctly

### 3. Manual Deployment

If you prefer to deploy manually, follow these steps:

1. Build and start the services:

```bash
docker-compose build
docker-compose up -d
```

2. Verify the services are running:

```bash
docker-compose ps
```

3. Run the tests:

```bash
./test_system.sh
```

## Service Endpoints

After deployment, the following services will be available:

- **Order Service**: http://localhost:8001
  - `GET /health` - Health check endpoint
  - `POST /orders` - Create a new order
  - `GET /orders/{id}` - Get order details

- **Payment Service**: http://localhost:8002
  - `GET /health` - Health check endpoint

- **Inventory Service**: http://localhost:8003
  - `GET /health` - Health check endpoint

- **Notification Service**: http://localhost:8004
  - `GET /health` - Health check endpoint

- **Shipping Service**: http://localhost:8085
  - `GET /actuator/health` - Health check endpoint
  - `GET /api/shipments/{orderId}` - Get shipment status by order ID
  - `GET /api/shipments/tracking/{trackingNumber}` - Get shipment by tracking number

- **Analytics Service**: http://localhost:3000
  - `GET /health` - Health check endpoint
  - `GET /` - Analytics dashboard
  - `GET /api/analytics/summary` - Get summary analytics
  - `GET /metrics` - Prometheus metrics

- **Kafka UI**: http://localhost:8080
  - Web interface for monitoring Kafka topics and messages

## Testing the System

### Using the Test Client

A test client is provided to generate test orders:

```bash
./test_client.sh
```

This interactive client allows you to:
1. Create individual orders
2. Generate multiple orders at a specified interval

### Running System Tests

To verify that all components of the system are working correctly:

```bash
./test_system.sh
```

This script checks:
1. Service health endpoints
2. Kafka topic configuration
3. End-to-end order processing flow
4. Saga pattern events

### Monitoring Logs

To monitor logs from the services:

```bash
# Monitor logs from all services
./monitor_logs.sh all

# Monitor logs from a specific service
./monitor_logs.sh order-service
```

## Troubleshooting

### Common Issues

1. **Services failing to start**:
   - Check Docker Compose logs: `docker-compose logs [service-name]`
   - Ensure Kafka is running: `docker-compose ps kafka`
   - Verify network connectivity between containers

2. **Events not flowing through the system**:
   - Check Kafka topics: `docker exec kafka kafka-topics --list --bootstrap-server localhost:9092`
   - View Kafka UI for message flow: http://localhost:8080

3. **Docker build failures**:
   - Check Docker build logs: `docker-compose build --no-cache [service-name]`
   - Ensure all dependencies are available

### Resetting the System

To completely reset the system:

```bash
docker-compose down -v
./deploy.sh
```

This will remove all containers, volumes, and networks, then redeploy the system.

## Scaling

For horizontal scaling of services:

```bash
# Scale a specific service
docker-compose up -d --scale payment-service=3

# Note: Services with port mappings cannot be scaled without configuration changes
```

## Conclusion

Once deployed, the system demonstrates a complete event-driven microservices architecture using Kafka for messaging and implementing various event-driven patterns across multiple programming languages.