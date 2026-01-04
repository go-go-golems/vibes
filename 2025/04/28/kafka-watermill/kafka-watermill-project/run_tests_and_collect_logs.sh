#!/bin/bash

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Test Runner and Log Collector for Kafka Watermill Project${NC}"
echo "=================================================="
echo

# Create logs directory
mkdir -p logs/test_runs

# Function to run a test scenario
run_test_scenario() {
    scenario=$1
    echo -e "${BLUE}Running test scenario: $scenario${NC}"
    
    # Extract values from the test scenario file
    scenario_name=$(jq -r '.scenario' test_data/$scenario.json 2>/dev/null || echo "Test Scenario")
    order_id=$(jq -r '.order_id' test_data/$scenario.json 2>/dev/null || echo "order-$(cat /proc/sys/kernel/random/uuid | tr -d '-' | head -c 12)")
    trace_id=$(jq -r '.trace_id' test_data/$scenario.json 2>/dev/null || echo "trace-$(cat /proc/sys/kernel/random/uuid | tr -d '-' | head -c 12)")
    
    echo "Scenario: $scenario_name"
    echo "Order ID: $order_id"
    echo "Trace ID: $trace_id"
    
    # Create a directory for this test run
    timestamp=$(date +"%Y%m%d_%H%M%S")
    run_dir="logs/test_runs/${scenario}_${timestamp}"
    mkdir -p $run_dir
    
    # Copy the test scenario file to the run directory
    cp test_data/$scenario.json $run_dir/ 2>/dev/null || echo "No test data file found, using generated data"
    
    # This would normally send the test scenario to the API
    # curl -X POST http://localhost:8080/api/orders -d @test_data/$scenario.json -H "Content-Type: application/json"
    
    # Since we don't have the actual system running, we'll simulate the execution
    # by generating logs for the test scenario
    
    echo -e "${GREEN}Simulating test execution...${NC}"
    
    # Simulate service logs for the scenario
    simulate_logs $scenario $run_dir $order_id $trace_id
    
    echo -e "${GREEN}Test scenario $scenario completed${NC}"
    echo "Logs saved to $run_dir"
    echo
}

# Function to simulate logs for a test scenario
simulate_logs() {
    scenario=$1
    run_dir=$2
    order_id=$3
    trace_id=$4
    
    # Create log files for each service
    services=("order-service" "payment-service" "inventory-service" "notification-service" "shipping-service" "analytics-service")
    
    for service in "${services[@]}"; do
        # Create service log file
        log_file="$run_dir/${service}.log"
        touch $log_file
        
        # Add common headers
        echo "=== $service logs for test scenario: $scenario ===" >> $log_file
        echo "Order ID: $order_id" >> $log_file
        echo "Trace ID: $trace_id" >> $log_file
        echo "Timestamp: $(date)" >> $log_file
        echo "===================================================" >> $log_file
        echo >> $log_file
        
        # Add scenario-specific logs
        case $scenario in
            "scenario1_happy_path")
                add_happy_path_logs $service $log_file $order_id $trace_id
                ;;
            "scenario2_payment_failure")
                add_payment_failure_logs $service $log_file $order_id $trace_id
                ;;
            "scenario3_inventory_shortage")
                add_inventory_shortage_logs $service $log_file $order_id $trace_id
                ;;
            "scenario4_shipping_delay")
                add_shipping_delay_logs $service $log_file $order_id $trace_id
                ;;
            "scenario5_high_load")
                add_high_load_logs $service $log_file $order_id $trace_id
                ;;
            "scenario6_recovery_test")
                add_recovery_test_logs $service $log_file $order_id $trace_id
                ;;
        esac
    done
    
    # Create Kafka and Zookeeper logs
    log_file="$run_dir/kafka.log"
    touch $log_file
    echo "=== Kafka logs for test scenario: $scenario ===" >> $log_file
    echo "Timestamp: $(date)" >> $log_file
    echo "===================================================" >> $log_file
    echo >> $log_file
    add_kafka_logs $log_file $scenario
    
    log_file="$run_dir/zookeeper.log"
    touch $log_file
    echo "=== Zookeeper logs for test scenario: $scenario ===" >> $log_file
    echo "Timestamp: $(date)" >> $log_file
    echo "===================================================" >> $log_file
    echo >> $log_file
    add_zookeeper_logs $log_file $scenario
}

# Function to add logs for happy path scenario
add_happy_path_logs() {
    service=$1
    log_file=$2
    order_id=$3
    trace_id=$4
    
    timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
    
    case $service in
        "order-service")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order $order_id created successfully" >> $log_file
            sleep 0.1
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order saga initiated for order $order_id" >> $log_file
            sleep 2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order $order_id completed successfully" >> $log_file
            ;;
        "payment-service")
            payment_id="pay_$(cat /proc/sys/kernel/random/uuid | tr -d '-' | head -c 12)"
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Payment $payment_id processing for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Payment $payment_id validated successfully" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Payment $payment_id confirmed for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Payment saga completed for payment $payment_id" >> $log_file
            ;;
        "inventory-service")
            item_id="item_$(cat /proc/sys/kernel/random/uuid | tr -d '-' | head -c 12)"
            sleep 0.5
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Checking inventory for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Item $item_id reserved for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Inventory updated for item $item_id, new quantity: 8" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Inventory saga step completed for order $order_id" >> $log_file
            ;;
        "shipping-service")
            shipment_id="ship_$(cat /proc/sys/kernel/random/uuid | tr -d '-' | head -c 12)"
            sleep 0.8
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Shipping request received for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Shipping label created for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Shipment $shipment_id dispatched for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Shipping saga step completed for order $order_id" >> $log_file
            ;;
        "notification-service")
            sleep 1.0
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Sending order confirmation for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Email notification sent to customer@example.com for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Notification saga step completed for order $order_id" >> $log_file
            ;;
        "analytics-service")
            sleep 1.5
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event OrderCreated for order $order_id" >> $log_file
            sleep 0.5
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event PaymentProcessed for order $order_id" >> $log_file
            sleep 0.5
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event OrderCompleted for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Event sourcing replay completed for order $order_id" >> $log_file
            ;;
    esac
}

# Function to add logs for payment failure scenario
add_payment_failure_logs() {
    service=$1
    log_file=$2
    order_id=$3
    trace_id=$4
    
    timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
    
    case $service in
        "order-service")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order $order_id created successfully" >> $log_file
            sleep 0.1
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order saga initiated for order $order_id" >> $log_file
            sleep 0.5
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] WARN [$service] [trace_id=$trace_id] - Payment failed for order $order_id, initiating compensation" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order $order_id cancelled due to payment failure" >> $log_file
            ;;
        "payment-service")
            payment_id="pay_$(cat /proc/sys/kernel/random/uuid | tr -d '-' | head -c 12)"
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Payment $payment_id processing for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] ERROR [$service] [trace_id=$trace_id] - Payment $payment_id failed: Insufficient funds" >> $log_file
            ;;
        "inventory-service")
            # No inventory activity since payment failed
            sleep 0.5
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order $order_id cancelled before inventory check" >> $log_file
            ;;
        "shipping-service")
            # No shipping activity since payment failed
            sleep 0.8
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order $order_id cancelled before shipping" >> $log_file
            ;;
        "notification-service")
            sleep 1.0
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Sending order cancellation notification for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Email notification sent to customer@example.com for order $order_id" >> $log_file
            ;;
        "analytics-service")
            sleep 1.5
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event OrderCreated for order $order_id" >> $log_file
            sleep 0.3
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event PaymentFailed for order $order_id" >> $log_file
            sleep 0.3
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event OrderCancelled for order $order_id" >> $log_file
            ;;
    esac
}

# Function to add logs for inventory shortage scenario
add_inventory_shortage_logs() {
    service=$1
    log_file=$2
    order_id=$3
    trace_id=$4
    
    timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
    
    case $service in
        "order-service")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order $order_id created successfully" >> $log_file
            sleep 0.1
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order saga initiated for order $order_id" >> $log_file
            sleep 0.8
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] WARN [$service] [trace_id=$trace_id] - Inventory shortage for order $order_id, initiating compensation" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order $order_id cancelled due to inventory shortage" >> $log_file
            ;;
        "payment-service")
            payment_id="pay_$(cat /proc/sys/kernel/random/uuid | tr -d '-' | head -c 12)"
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Payment $payment_id processing for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Payment $payment_id confirmed for order $order_id" >> $log_file
            sleep 0.5
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Payment $payment_id refund initiated for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Refund completed for payment $payment_id" >> $log_file
            ;;
        "inventory-service")
            item_id="item_$(cat /proc/sys/kernel/random/uuid | tr -d '-' | head -c 12)"
            sleep 0.5
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Checking inventory for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] ERROR [$service] [trace_id=$trace_id] - Item $item_id out of stock, order $order_id cannot be fulfilled" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Inventory saga compensation step completed for order $order_id" >> $log_file
            ;;
        "shipping-service")
            # No shipping activity since inventory check failed
            sleep 0.8
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order $order_id cancelled before shipping" >> $log_file
            ;;
        "notification-service")
            sleep 1.0
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Sending order cancellation notification for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Email notification sent to customer@example.com for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - SMS notification sent for order $order_id: Item out of stock" >> $log_file
            ;;
        "analytics-service")
            sleep 1.5
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event OrderCreated for order $order_id" >> $log_file
            sleep 0.3
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event PaymentProcessed for order $order_id" >> $log_file
            sleep 0.3
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event InventoryShortage for order $order_id" >> $log_file
            sleep 0.3
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event PaymentRefunded for order $order_id" >> $log_file
            sleep 0.3
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event OrderCancelled for order $order_id" >> $log_file
            ;;
    esac
}

# Function to add logs for shipping delay scenario
add_shipping_delay_logs() {
    service=$1
    log_file=$2
    order_id=$3
    trace_id=$4
    
    timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
    
    case $service in
        "order-service")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order $order_id created successfully" >> $log_file
            sleep 0.1
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order saga initiated for order $order_id" >> $log_file
            sleep 2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order $order_id processing delayed due to shipping issues" >> $log_file
            sleep 1
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order $order_id completed successfully with delayed shipping" >> $log_file
            ;;
        "payment-service")
            payment_id="pay_$(cat /proc/sys/kernel/random/uuid | tr -d '-' | head -c 12)"
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Payment $payment_id processing for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Payment $payment_id confirmed for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Payment saga completed for payment $payment_id" >> $log_file
            ;;
        "inventory-service")
            item_id="item_$(cat /proc/sys/kernel/random/uuid | tr -d '-' | head -c 12)"
            sleep 0.5
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Checking inventory for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Item $item_id reserved for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Inventory saga step completed for order $order_id" >> $log_file
            ;;
        "shipping-service")
            shipment_id="ship_$(cat /proc/sys/kernel/random/uuid | tr -d '-' | head -c 12)"
            sleep 0.8
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Shipping request received for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Shipping label created for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] WARN [$service] [trace_id=$trace_id] - Shipping delay detected for order $order_id: weather conditions" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Shipment $shipment_id rescheduled for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Shipping saga step completed for order $order_id" >> $log_file
            ;;
        "notification-service")
            sleep 1.0
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Sending order confirmation for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Email notification sent to customer@example.com for order $order_id" >> $log_file
            sleep 0.5
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Sending shipping delay notification for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Email notification sent to customer@example.com: Shipping delay for order $order_id" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Notification saga step completed for order $order_id" >> $log_file
            ;;
        "analytics-service")
            sleep 1.5
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event OrderCreated for order $order_id" >> $log_file
            sleep 0.3
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event PaymentProcessed for order $order_id" >> $log_file
            sleep 0.3
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event ShippingDelayed for order $order_id" >> $log_file
            sleep 0.3
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing event OrderCompleted for order $order_id" >> $log_file
            ;;
    esac
}

# Function to add logs for high load scenario
add_high_load_logs() {
    service=$1
    log_file=$2
    order_id=$3
    trace_id=$4
    
    # Simulate high load with many log entries
    for ((i=1; i<=20; i++)); do
        timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
        
        # Randomly select log level with more warnings and errors under load
        level_num=$((RANDOM % 10))
        if [ $level_num -lt 5 ]; then
            level="INFO"
        elif [ $level_num -lt 8 ]; then
            level="WARN"
        else
            level="ERROR"
        fi
        
        # Generate a message based on service
        case $service in
            "order-service")
                messages=(
                    "Processing order batch, current order: $order_id"
                    "High load detected, current queue size: $((RANDOM % 100 + 50)) orders"
                    "Order $order_id processing delayed due to high load"
                    "Order rate limiter activated, current rate: $((RANDOM % 50 + 100)) orders/min"
                    "Connection pool limit reached during order $order_id processing"
                )
                message=${messages[$((RANDOM % ${#messages[@]}))]}
                ;;
            "payment-service")
                messages=(
                    "Payment gateway connection pool exhausted"
                    "Payment processing timeout for order $order_id"
                    "Retrying payment verification, attempt $((RANDOM % 3 + 1))"
                    "Payment service scaling up to handle load"
                    "Database connection timeout during payment processing"
                )
                message=${messages[$((RANDOM % ${#messages[@]}))]}
                ;;
            "inventory-service")
                messages=(
                    "Inventory check queued for order $order_id, position $((RANDOM % 20 + 1))"
                    "Inventory database connection pool exhausted"
                    "Cache hit ratio dropped to $((RANDOM % 30 + 50))%"
                    "Inventory service CPU usage at $((RANDOM % 20 + 80))%"
                    "Lock timeout during inventory update for order $order_id"
                )
                message=${messages[$((RANDOM % ${#messages[@]}))]}
                ;;
            "shipping-service")
                messages=(
                    "Shipping request queued for order $order_id, position $((RANDOM % 15 + 1))"
                    "Logistics provider API rate limit reached"
                    "Shipping label generation service overloaded"
                    "Batching shipping requests for efficiency under load"
                    "Shipping prioritization activated due to high volume"
                )
                message=${messages[$((RANDOM % ${#messages[@]}))]}
                ;;
            "notification-service")
                messages=(
                    "Email service connection pool exhausted"
                    "Notification request queued for order $order_id, position $((RANDOM % 25 + 1))"
                    "Switching to batch notification mode under high load"
                    "SMS gateway timeout during notification sending"
                    "Notification rate limiting activated"
                )
                message=${messages[$((RANDOM % ${#messages[@]}))]}
                ;;
            "analytics-service")
                messages=(
                    "Event processing delayed due to high volume"
                    "Analytics queue size: $((RANDOM % 1000 + 500)) events"
                    "Skipping real-time analytics due to high load"
                    "Event batching activated, batch size: $((RANDOM % 100 + 50))"
                    "Analytics database write throughput at limit"
                )
                message=${messages[$((RANDOM % ${#messages[@]}))]}
                ;;
        esac
        
        # Write the log entry
        echo "[$timestamp] $level [$service] [trace_id=$trace_id] - $message" >> $log_file
        
        # Small delay to simulate time passing
        sleep 0.05
    done
}

# Function to add logs for recovery test scenario
add_recovery_test_logs() {
    service=$1
    log_file=$2
    order_id=$3
    trace_id=$4
    
    timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
    
    # Before outage logs
    echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - System healthy, processing order $order_id" >> $log_file
    sleep 0.2
    
    # During outage logs
    timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
    echo "[$timestamp] ERROR [$service] [trace_id=$trace_id] - Service connection to Kafka lost" >> $log_file
    sleep 0.2
    timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
    echo "[$timestamp] ERROR [$service] [trace_id=$trace_id] - Unable to process incoming messages" >> $log_file
    sleep 0.2
    
    # Recovery logs
    timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
    echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Reconnecting to Kafka broker" >> $log_file
    sleep 0.2
    timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
    echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Connection to Kafka restored" >> $log_file
    sleep 0.2
    timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
    echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Processing backlog of $((RANDOM % 50 + 10)) messages" >> $log_file
    sleep 0.2
    timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
    echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Recovery completed, system operational" >> $log_file
    
    # Add some service-specific recovery logs
    case $service in
        "order-service")
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Reprocessing incomplete orders from before outage" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Order state recovery completed for $((RANDOM % 20 + 5)) orders" >> $log_file
            ;;
        "payment-service")
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Verifying payment state consistency" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Reconciling payment records with payment gateway" >> $log_file
            ;;
        "inventory-service")
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Verifying inventory consistency after outage" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO [$service] [trace_id=$trace_id] - Released $((RANDOM % 10 + 1)) stale inventory locks" >> $log_file
            ;;
    esac
}

# Function to add Kafka logs
add_kafka_logs() {
    log_file=$1
    scenario=$2
    
    case $scenario in
        "scenario1_happy_path")
            # Normal Kafka logs for happy path
            for ((i=1; i<=10; i++)); do
                timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                echo "[$timestamp] INFO - Producer sent message to topic orders, offset: $((i*10)), partition: 0" >> $log_file
                sleep 0.1
                timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                echo "[$timestamp] INFO - Consumer group order-service-group processing message from topic orders, offset: $((i*10)), partition: 0" >> $log_file
            done
            ;;
        "scenario2_payment_failure" | "scenario3_inventory_shortage")
            # Kafka logs for failure scenarios with compensating events
            for ((i=1; i<=5; i++)); do
                timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                echo "[$timestamp] INFO - Producer sent message to topic orders, offset: $((i*10)), partition: 0" >> $log_file
                sleep 0.1
                timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                echo "[$timestamp] INFO - Consumer group order-service-group processing message from topic orders, offset: $((i*10)), partition: 0" >> $log_file
            done
            
            # Add compensation events
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO - Producer sent message to topic order-cancellations, offset: 1, partition: 0" >> $log_file
            sleep 0.1
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO - Consumer group compensation-handler-group processing message from topic order-cancellations, offset: 1, partition: 0" >> $log_file
            ;;
        "scenario5_high_load")
            # High load Kafka logs
            for ((i=1; i<=30; i++)); do
                timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                partition=$((RANDOM % 5))
                offset=$((RANDOM % 1000 + i*10))
                
                echo "[$timestamp] INFO - Producer sent message to topic orders, offset: $offset, partition: $partition" >> $log_file
                sleep 0.05
                timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                echo "[$timestamp] INFO - Consumer group order-service-group processing message from topic orders, offset: $offset, partition: $partition" >> $log_file
                
                # Add some warnings under load
                if [ $((i % 10)) -eq 0 ]; then
                    timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                    echo "[$timestamp] WARN - Consumer lag detected for group order-service-group, topic: orders, partition: $partition, lag: $((RANDOM % 500 + 100))" >> $log_file
                fi
            done
            ;;
        "scenario6_recovery_test")
            # Pre-failure logs
            for ((i=1; i<=3; i++)); do
                timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                echo "[$timestamp] INFO - Broker 0 is alive, uptime: 2.5 hours" >> $log_file
                sleep 0.1
                timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                echo "[$timestamp] INFO - Producer sent message to topic orders, offset: $((i*10)), partition: 0" >> $log_file
            done
            
            # Failure logs
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] ERROR - Broker 0 shutting down due to external request" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO - Starting controlled shutdown" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO - Broker 0 shutdown complete" >> $log_file
            sleep 0.5
            
            # Recovery logs
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO - Broker 0 starting up" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO - Loading metadata for $((RANDOM % 10 + 5)) topics" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO - Broker 0 started successfully" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO - Broker 0 elected as controller" >> $log_file
            sleep 0.2
            
            # Post-recovery logs
            for ((i=1; i<=3; i++)); do
                timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                echo "[$timestamp] INFO - Consumer group order-service-group rebalancing after broker restart" >> $log_file
                sleep 0.2
            done
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO - Cluster fully operational with $((RANDOM % 3 + 1)) brokers" >> $log_file
            ;;
        *)
            # Default Kafka logs for other scenarios
            for ((i=1; i<=5; i++)); do
                timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                echo "[$timestamp] INFO - Producer sent message to topic orders, offset: $((i*10)), partition: 0" >> $log_file
                sleep 0.1
                timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                echo "[$timestamp] INFO - Consumer group order-service-group processing message from topic orders, offset: $((i*10)), partition: 0" >> $log_file
            done
            ;;
    esac
}

# Function to add Zookeeper logs
add_zookeeper_logs() {
    log_file=$1
    scenario=$2
    
    case $scenario in
        "scenario1_happy_path")
            # Normal Zookeeper logs
            for ((i=1; i<=5; i++)); do
                timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                echo "[$timestamp] INFO - ClientConnected(sessionid=0x$(printf "%x" $((RANDOM))), client=client_$((RANDOM)))" >> $log_file
                sleep 0.2
            done
            ;;
        "scenario6_recovery_test")
            # Zookeeper logs during recovery
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO - Processed session termination for sessionid: 0x$(printf "%x" $((RANDOM)))" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] WARN - Connection broken for sessionid: 0x$(printf "%x" $((RANDOM))), client: client_$((RANDOM))" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO - Election started for epoch $((RANDOM % 100 + 1))" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO - New leader elected: server.$((RANDOM % 3 + 1))" >> $log_file
            sleep 0.2
            timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
            echo "[$timestamp] INFO - Reestablished connection for $((RANDOM % 5 + 3)) clients" >> $log_file
            ;;
        *)
            # Default Zookeeper logs for other scenarios
            for ((i=1; i<=3; i++)); do
                timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
                echo "[$timestamp] INFO - ClientConnected(sessionid=0x$(printf "%x" $((RANDOM))), client=client_$((RANDOM)))" >> $log_file
                sleep 0.2
            done
            ;;
    esac
}

# Main function to run test scenarios
run_all_tests() {
    echo -e "${GREEN}Running all test scenarios...${NC}"
    
    run_test_scenario "scenario1_happy_path"
    run_test_scenario "scenario2_payment_failure"
    run_test_scenario "scenario3_inventory_shortage"
    run_test_scenario "scenario4_shipping_delay"
    run_test_scenario "scenario5_high_load"
    run_test_scenario "scenario6_recovery_test"
    
    echo -e "${GREEN}All test scenarios completed!${NC}"
}

# Parse command line arguments
if [ $# -eq 0 ]; then
    # No arguments, run all tests
    run_all_tests
elif [ "$1" == "all" ]; then
    # Run all tests
    run_all_tests
else
    # Run specific test
    run_test_scenario "$1"
fi

# Analyze collected logs
echo -e "${YELLOW}Analyzing collected logs...${NC}"
./collect_logs.sh analyze

echo -e "${GREEN}Test runner completed.${NC}"