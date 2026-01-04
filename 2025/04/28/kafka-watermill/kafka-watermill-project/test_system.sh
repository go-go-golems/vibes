#!/bin/bash

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Starting system tests for Kafka Watermill Multi-Language Microservices${NC}"
echo "===================================================================="
echo

# Function to check if a service is healthy
check_service() {
    service_name=$1
    port=$2
    endpoint=${3:-"/health"}
    
    echo -n "Checking service $service_name on port $port... "
    
    # Try to connect multiple times with a short delay
    for i in {1..5}; do
        response=$(curl -s -o /dev/null -w "%{http_code}" http://localhost:$port$endpoint || echo "failed")
        
        if [[ "$response" == "200" ]]; then
            echo -e "${GREEN}OK${NC}"
            return 0
        fi
        
        sleep 2
    done
    
    echo -e "${RED}FAILED${NC}"
    return 1
}

# Function to verify Kafka topics
check_kafka_topics() {
    echo -n "Checking Kafka topics... "
    
    # Get list of topics
    topics=$(docker exec kafka kafka-topics --list --bootstrap-server localhost:9092)
    
    required_topics=("order.created" "payment.processed" "inventory.checked" "order.fulfilled" "order.cancelled")
    missing=false
    
    for topic in "${required_topics[@]}"; do
        if [[ ! "$topics" == *"$topic"* ]]; then
            echo -e "${RED}Missing topic: $topic${NC}"
            missing=true
        fi
    done
    
    if [[ "$missing" == false ]]; then
        echo -e "${GREEN}All required topics found${NC}"
        return 0
    else
        return 1
    fi
}

# Function to send a test order and verify the event flow
test_order_flow() {
    echo "Testing order processing flow..."
    
    # Generate a unique order ID
    order_id=$(date +%s)
    
    # Create an order
    echo -n "Creating test order... "
    create_order_response=$(curl -s -X POST -H "Content-Type: application/json" \
        -d "{\"user_id\": \"test-user\", \"items\": [{\"product_id\": \"test-product-1\", \"name\": \"Test Product\", \"quantity\": 2, \"price\": 19.99}], \"total_amount\": 39.98}" \
        http://localhost:8001/orders)
    
    if [[ "$create_order_response" == *"order_id"* ]]; then
        echo -e "${GREEN}Order created${NC}"
        order_id=$(echo $create_order_response | sed 's/.*"order_id":"\([^"]*\)".*/\1/')
        echo "Order ID: $order_id"
    else
        echo -e "${RED}Failed to create order${NC}"
        return 1
    fi
    
    # Wait for events to propagate
    echo "Waiting for events to propagate through the system..."
    sleep 10
    
    # Check for payment processed event
    echo -n "Checking payment processed event... "
    payment_events=$(docker exec kafka kafka-console-consumer --bootstrap-server localhost:9092 --topic payment.processed --from-beginning --max-messages 100 --timeout-ms 5000)
    
    if [[ "$payment_events" == *"$order_id"* ]]; then
        echo -e "${GREEN}Payment event found${NC}"
    else
        echo -e "${RED}Payment event not found${NC}"
        return 1
    fi
    
    # Check for inventory checked event
    echo -n "Checking inventory checked event... "
    inventory_events=$(docker exec kafka kafka-console-consumer --bootstrap-server localhost:9092 --topic inventory.checked --from-beginning --max-messages 100 --timeout-ms 5000)
    
    if [[ "$inventory_events" == *"$order_id"* ]]; then
        echo -e "${GREEN}Inventory event found${NC}"
    else
        echo -e "${RED}Inventory event not found${NC}"
        return 1
    fi
    
    # Check if order was fulfilled or cancelled
    echo -n "Checking order fulfillment/cancellation... "
    fulfillment_events=$(docker exec kafka kafka-console-consumer --bootstrap-server localhost:9092 --topic order.fulfilled --from-beginning --max-messages 100 --timeout-ms 5000)
    cancellation_events=$(docker exec kafka kafka-console-consumer --bootstrap-server localhost:9092 --topic order.cancelled --from-beginning --max-messages 100 --timeout-ms 5000)
    
    if [[ "$fulfillment_events" == *"$order_id"* ]]; then
        echo -e "${GREEN}Order fulfilled${NC}"
    elif [[ "$cancellation_events" == *"$order_id"* ]]; then
        echo -e "${YELLOW}Order cancelled (expected if inventory unavailable)${NC}"
    else
        echo -e "${RED}No fulfillment or cancellation event found${NC}"
        return 1
    fi
    
    # Check analytics service
    echo -n "Checking analytics service for order data... "
    analytics_response=$(curl -s http://localhost:3000/api/analytics/summary)
    
    if [[ "$analytics_response" == *"orders_created"* ]]; then
        echo -e "${GREEN}Analytics data available${NC}"
    else
        echo -e "${RED}Analytics data not found${NC}"
        return 1
    fi
    
    echo -e "${GREEN}Order flow test completed successfully${NC}"
    return 0
}

# Function to check saga events
check_saga_events() {
    echo -n "Checking saga events... "
    
    saga_events=$(docker exec kafka kafka-console-consumer --bootstrap-server localhost:9092 --topic saga.events --from-beginning --max-messages 10 --timeout-ms 5000)
    
    if [[ "$saga_events" == *"sagaId"* ]]; then
        echo -e "${GREEN}Saga events found${NC}"
        return 0
    else
        echo -e "${RED}No saga events found${NC}"
        return 1
    fi
}

# Main test sequence
echo "Step 1: Verifying that all services are running"
check_service "order-service" 8001 || exit 1
check_service "payment-service" 8002 || exit 1
check_service "inventory-service" 8003 || exit 1
check_service "notification-service" 8004 || exit 1
check_service "shipping-service" 8085 "/actuator/health" || exit 1
check_service "analytics-service" 3000 "/health" || exit 1
check_service "kafka-ui" 8080 || exit 1

echo 
echo "Step 2: Verifying Kafka topics"
check_kafka_topics || exit 1

echo
echo "Step 3: Testing complete order flow"
test_order_flow || exit 1

echo
echo "Step 4: Checking saga pattern events"
check_saga_events || exit 1

echo
echo -e "${GREEN}All tests completed successfully!${NC}"
echo "System is working as expected with all microservices communicating correctly."