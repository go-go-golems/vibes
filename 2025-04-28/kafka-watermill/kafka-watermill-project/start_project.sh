#!/bin/bash

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
  echo "Docker is not running. Please start Docker and try again."
  exit 1
fi

# Start Docker Compose services
echo "Starting Kafka and related services..."
docker-compose up -d

# Wait for Kafka to be ready
echo "Waiting for Kafka to be ready..."
sleep 10

# Create required topics
echo "Creating Kafka topics..."
docker exec kafka kafka-topics --create --if-not-exists --topic order.created --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092
docker exec kafka kafka-topics --create --if-not-exists --topic payment.processed --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092
docker exec kafka kafka-topics --create --if-not-exists --topic inventory.checked --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092
docker exec kafka kafka-topics --create --if-not-exists --topic order.fulfilled --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092
docker exec kafka kafka-topics --create --if-not-exists --topic order.cancelled --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092
docker exec kafka kafka-topics --create --if-not-exists --topic saga.order_processing.events --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092

# Build Go services
echo "Building Go microservices..."
mkdir -p bin
cd cmd/order-service && go build -o ../../bin/order-service && cd ../..
cd cmd/payment-service && go build -o ../../bin/payment-service && cd ../..
cd cmd/inventory-service && go build -o ../../bin/inventory-service && cd ../..
cd cmd/notification-service && go build -o ../../bin/notification-service && cd ../..

# Build Kotlin service
echo "Building Kotlin service..."
cd kotlin-service && ./gradlew build && cd ..

# Install Ruby dependencies
echo "Installing Ruby dependencies..."
cd ruby-service && bundle install && cd ..

# Start Go services
echo "Starting Go microservices..."
./bin/order-service > logs/order-service.log 2>&1 &
./bin/payment-service > logs/payment-service.log 2>&1 &
./bin/inventory-service > logs/inventory-service.log 2>&1 &
./bin/notification-service > logs/notification-service.log 2>&1 &

# Start Kotlin service
echo "Starting Kotlin service..."
cd kotlin-service && ./gradlew bootRun > ../logs/kotlin-service.log 2>&1 & cd ..

# Start Ruby service
echo "Starting Ruby service..."
cd ruby-service && ruby analytics_service.rb > ../logs/ruby-service.log 2>&1 & cd ..

echo "All services started. Check the logs directory for output."
echo "Kafka UI is available at http://localhost:8080"