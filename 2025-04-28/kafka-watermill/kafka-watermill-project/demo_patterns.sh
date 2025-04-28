#!/bin/bash

echo "Starting Event-Driven Patterns Demo"
echo "=================================="
echo

# Ensure Kafka is running
echo "Checking if Kafka is running..."
docker-compose ps | grep kafka

if [ $? -ne 0 ]; then
    echo "Starting Kafka..."
    docker-compose up -d
    echo "Waiting for Kafka to start..."
    sleep 10
else
    echo "Kafka is already running."
fi

# Create necessary topics
echo
echo "Creating Kafka topics..."
docker exec kafka kafka-topics --create --if-not-exists --topic order.created --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092
docker exec kafka kafka-topics --create --if-not-exists --topic payment.processed --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092
docker exec kafka kafka-topics --create --if-not-exists --topic inventory.checked --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092
docker exec kafka kafka-topics --create --if-not-exists --topic order.fulfilled --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092
docker exec kafka kafka-topics --create --if-not-exists --topic order.cancelled --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092
docker exec kafka kafka-topics --create --if-not-exists --topic saga.events --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092
docker exec kafka kafka-topics --create --if-not-exists --topic saga.step.events --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092
docker exec kafka kafka-topics --create --if-not-exists --topic analytics.insights.inventory --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092
docker exec kafka kafka-topics --create --if-not-exists --topic analytics.reports.daily --partitions 3 --replication-factor 1 --bootstrap-server localhost:9092

# Build the Go patterns demo
echo
echo "Building Go patterns demo..."
cd cmd/patterns-demo
go build -o ../../bin/patterns-demo
cd ../..

# Start a consumer to monitor events
echo
echo "Starting Kafka consumer for monitoring events..."
docker exec -d kafka kafka-console-consumer --bootstrap-server localhost:9092 --from-beginning --whitelist 'order.*|payment.*|inventory.*|saga.*' > kafka_events.log &
KAFKA_CONSUMER_PID=$!

echo "Kafka events will be logged to kafka_events.log"

# Run the patterns demo in background
echo
echo "Starting patterns demo..."
mkdir -p logs
./bin/patterns-demo > logs/patterns-demo.log 2>&1 &
PATTERNS_DEMO_PID=$!

echo "Patterns demo is running with PID $PATTERNS_DEMO_PID (logs in logs/patterns-demo.log)"

# Start the Ruby analytics service
echo
echo "Starting Ruby analytics service..."
cd ruby-service
bundle install
ruby app.rb > ../logs/ruby-analytics.log 2>&1 &
RUBY_PID=$!
cd ..

echo "Ruby analytics service is running with PID $RUBY_PID (logs in logs/ruby-analytics.log)"

# Start the Kotlin shipping service
echo
echo "Starting Kotlin shipping service..."
cd kotlin-service
./gradlew bootRun > ../logs/kotlin-shipping.log 2>&1 &
KOTLIN_PID=$!
cd ..

echo "Kotlin shipping service is running with PID $KOTLIN_PID (logs in logs/kotlin-shipping.log)"

echo
echo "All services are running. Press CTRL+C to stop the demo."
echo "You can monitor the logs in the logs directory."

# Trap Ctrl+C
trap "echo 'Stopping all services...'; kill $PATTERNS_DEMO_PID $RUBY_PID $KOTLIN_PID $KAFKA_CONSUMER_PID; echo 'Demo stopped.'" INT

# Wait for Ctrl+C
while true; do
    sleep 1
done