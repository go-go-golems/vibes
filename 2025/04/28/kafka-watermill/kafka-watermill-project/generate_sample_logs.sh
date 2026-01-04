#!/bin/bash

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Sample Log Generator for Kafka Watermill Project${NC}"
echo "=================================================="
echo

# Create logs directory if it doesn't exist
mkdir -p logs

# Services to generate logs for
services=(
    "order-service"
    "payment-service"
    "inventory-service"
    "notification-service"
    "shipping-service" # Kotlin service
    "analytics-service" # Ruby service
    "kafka"
    "zookeeper"
)

# Log levels
log_levels=(
    "INFO"
    "DEBUG"
    "WARN"
    "ERROR"
    "FATAL"
)

# Define message templates for each service
declare -A order_service_msgs
order_service_msgs[0]="Order ORDER_ID created successfully"
order_service_msgs[1]="Order ORDER_ID validation failed: Invalid payment information"
order_service_msgs[2]="Order ORDER_ID processing started"
order_service_msgs[3]="Order ORDER_ID received with items: ITEMS"
order_service_msgs[4]="Order ORDER_ID payment successful, sending confirmation"
order_service_msgs[5]="Error processing order ORDER_ID: Payment validation failed"
order_service_msgs[6]="Order ORDER_ID cancelled by user"
order_service_msgs[7]="Order saga initiated for order ORDER_ID"
order_service_msgs[8]="Order ORDER_ID completed successfully"

declare -A payment_service_msgs
payment_service_msgs[0]="Payment PAYMENT_ID processing for order ORDER_ID"
payment_service_msgs[1]="Payment PAYMENT_ID validated successfully"
payment_service_msgs[2]="Payment PAYMENT_ID confirmed for order ORDER_ID"
payment_service_msgs[3]="Payment PAYMENT_ID failed: Insufficient funds"
payment_service_msgs[4]="Payment PAYMENT_ID refund initiated for order ORDER_ID"
payment_service_msgs[5]="Retry payment processing for order ORDER_ID"
payment_service_msgs[6]="External payment gateway timeout for payment PAYMENT_ID"
payment_service_msgs[7]="Payment saga completed for payment PAYMENT_ID"

declare -A inventory_service_msgs
inventory_service_msgs[0]="Checking inventory for order ORDER_ID"
inventory_service_msgs[1]="Item ITEM_ID reserved for order ORDER_ID"
inventory_service_msgs[2]="Item ITEM_ID out of stock, order ORDER_ID cannot be fulfilled"
inventory_service_msgs[3]="Inventory updated for item ITEM_ID, new quantity: QUANTITY"
inventory_service_msgs[4]="Inventory reservation released for order ORDER_ID"
inventory_service_msgs[5]="Error accessing inventory database: Connection timeout"
inventory_service_msgs[6]="Inventory saga step completed for order ORDER_ID"

declare -A notification_service_msgs
notification_service_msgs[0]="Sending order confirmation for order ORDER_ID"
notification_service_msgs[1]="Email notification sent to EMAIL for order ORDER_ID"
notification_service_msgs[2]="SMS notification failed: Invalid phone number PHONE"
notification_service_msgs[3]="Push notification sent to user USER_ID for order ORDER_ID"
notification_service_msgs[4]="Error connecting to notification provider"
notification_service_msgs[5]="Notification saga step completed for order ORDER_ID"

declare -A shipping_service_msgs
shipping_service_msgs[0]="Shipping request received for order ORDER_ID"
shipping_service_msgs[1]="Shipping label created for order ORDER_ID"
shipping_service_msgs[2]="Shipment SHIPMENT_ID dispatched for order ORDER_ID"
shipping_service_msgs[3]="Shipment SHIPMENT_ID status updated: STATUS"
shipping_service_msgs[4]="Error connecting to logistics provider"
shipping_service_msgs[5]="Shipping address validation failed for order ORDER_ID"
shipping_service_msgs[6]="Shipping saga step completed for order ORDER_ID"

declare -A analytics_service_msgs
analytics_service_msgs[0]="Processing event EVENT_TYPE for order ORDER_ID"
analytics_service_msgs[1]="User behavior analysis completed for user USER_ID"
analytics_service_msgs[2]="Anomaly detected in order pattern for user USER_ID"
analytics_service_msgs[3]="Daily sales report generated"
analytics_service_msgs[4]="Error connecting to analytics database"
analytics_service_msgs[5]="Processing batch of COUNT events"
analytics_service_msgs[6]="Event sourcing replay completed for EVENT_TYPE"

declare -A kafka_msgs
kafka_msgs[0]="Topic TOPIC created with PARTITIONS partitions"
kafka_msgs[1]="Producer PRODUCER_ID connected"
kafka_msgs[2]="Consumer CONSUMER_ID joined consumer group GROUP_ID"
kafka_msgs[3]="Message batch of size SIZE produced to topic TOPIC"
kafka_msgs[4]="Broker BROKER_ID elected as controller"
kafka_msgs[5]="Rebalancing consumer group GROUP_ID"
kafka_msgs[6]="Error handling request from consumer CONSUMER_ID"
kafka_msgs[7]="Topic TOPIC partition PARTITION leader changed to broker BROKER_ID"

declare -A zookeeper_msgs
zookeeper_msgs[0]="Established session 0xSESSION_ID with negotiated timeout TIMEOUT"
zookeeper_msgs[1]="Processing request: REQUEST_TYPE"
zookeeper_msgs[2]="New leader elected: LEADER_ID"
zookeeper_msgs[3]="Node NODE_PATH created"
zookeeper_msgs[4]="Node NODE_PATH deleted"
zookeeper_msgs[5]="Connection received from client CLIENT_ID"
zookeeper_msgs[6]="Error processing transaction TXN_ID"

# Function to generate a random UUID
generate_uuid() {
    cat /proc/sys/kernel/random/uuid | tr -d '-' | head -c 12
}

# Function to generate a timestamp within the last hour
generate_timestamp() {
    seconds_ago=$((RANDOM % 3600))
    date -d "@$(($(date +%s) - seconds_ago))" +"%Y-%m-%d %H:%M:%S.%3N"
}

# Function to generate a log entry
generate_log_entry() {
    local service=$1
    local level=${log_levels[$((RANDOM % ${#log_levels[@]}))]}
    
    # Weight the log levels to make errors less common
    if [ "$level" == "ERROR" ]; then
        if [ $((RANDOM % 10)) -lt 8 ]; then
            level="INFO"
        fi
    fi
    if [ "$level" == "FATAL" ]; then
        if [ $((RANDOM % 20)) -lt 19 ]; then
            level="WARN"
        fi
    fi
    
    local timestamp=$(generate_timestamp)
    
    # Generate random IDs for substitution
    local order_id=$(generate_uuid)
    local payment_id=$(generate_uuid)
    local item_id=$(generate_uuid)
    local shipment_id=$(generate_uuid)
    local user_id=$(generate_uuid)
    local message_id=$(generate_uuid)
    
    # Other random values
    local items="Item1, Item2, Item3"
    local quantity=$((RANDOM % 100))
    local email="user_${user_id}@example.com"
    local phone="+1${RANDOM}${RANDOM}${RANDOM}${RANDOM}"
    local status=("processing" "shipped" "delivered" "returned" "cancelled")
    local event_type=("OrderCreated" "PaymentProcessed" "ItemShipped" "OrderCompleted")
    local count=$((RANDOM % 1000))
    local topic=("orders" "payments" "inventory" "notifications" "shipments" "analytics")
    local partitions=$((RANDOM % 10 + 1))
    local producer_id="producer_${RANDOM}"
    local consumer_id="consumer_${RANDOM}"
    local group_id="group_${RANDOM}"
    local size=$((RANDOM % 1000))
    local broker_id=$((RANDOM % 5))
    local partition=$((RANDOM % 5))
    local session_id=$(printf "%x" $RANDOM)
    local timeout=$((RANDOM % 60000))
    local request_type=("create" "read" "update" "delete")
    local leader_id=$((RANDOM % 5))
    local node_path="/brokers/ids/$broker_id"
    local client_id="client_${RANDOM}"
    local txn_id=$((RANDOM % 10000))
    
    # Select a random message template for the service
    local msg_num=0
    local message=""
    
    case $service in
        order-service)
            msg_num=$((RANDOM % ${#order_service_msgs[@]}))
            message="${order_service_msgs[$msg_num]}"
            ;;
        payment-service)
            msg_num=$((RANDOM % ${#payment_service_msgs[@]}))
            message="${payment_service_msgs[$msg_num]}"
            ;;
        inventory-service)
            msg_num=$((RANDOM % ${#inventory_service_msgs[@]}))
            message="${inventory_service_msgs[$msg_num]}"
            ;;
        notification-service)
            msg_num=$((RANDOM % ${#notification_service_msgs[@]}))
            message="${notification_service_msgs[$msg_num]}"
            ;;
        shipping-service)
            msg_num=$((RANDOM % ${#shipping_service_msgs[@]}))
            message="${shipping_service_msgs[$msg_num]}"
            ;;
        analytics-service)
            msg_num=$((RANDOM % ${#analytics_service_msgs[@]}))
            message="${analytics_service_msgs[$msg_num]}"
            ;;
        kafka)
            msg_num=$((RANDOM % ${#kafka_msgs[@]}))
            message="${kafka_msgs[$msg_num]}"
            ;;
        zookeeper)
            msg_num=$((RANDOM % ${#zookeeper_msgs[@]}))
            message="${zookeeper_msgs[$msg_num]}"
            ;;
    esac
    
    # Replace placeholders with values
    message=${message//ORDER_ID/$order_id}
    message=${message//PAYMENT_ID/$payment_id}
    message=${message//ITEM_ID/$item_id}
    message=${message//ITEMS/$items}
    message=${message//QUANTITY/$quantity}
    message=${message//EMAIL/$email}
    message=${message//PHONE/$phone}
    message=${message//USER_ID/$user_id}
    message=${message//SHIPMENT_ID/$shipment_id}
    message=${message//STATUS/${status[$((RANDOM % ${#status[@]}))]}}
    message=${message//EVENT_TYPE/${event_type[$((RANDOM % ${#event_type[@]}))]}}
    message=${message//COUNT/$count}
    message=${message//TOPIC/${topic[$((RANDOM % ${#topic[@]}))]}}
    message=${message//PARTITIONS/$partitions}
    message=${message//PRODUCER_ID/$producer_id}
    message=${message//CONSUMER_ID/$consumer_id}
    message=${message//GROUP_ID/$group_id}
    message=${message//SIZE/$size}
    message=${message//BROKER_ID/$broker_id}
    message=${message//PARTITION/$partition}
    message=${message//SESSION_ID/$session_id}
    message=${message//TIMEOUT/$timeout}
    message=${message//REQUEST_TYPE/${request_type[$((RANDOM % ${#request_type[@]}))]}}
    message=${message//LEADER_ID/$leader_id}
    message=${message//NODE_PATH/$node_path}
    message=${message//CLIENT_ID/$client_id}
    message=${message//TXN_ID/$txn_id}
    
    # Format the log entry
    if [ "$service" == "kafka" ] || [ "$service" == "zookeeper" ]; then
        echo "[$timestamp] $level - $message"
    else
        echo "[$timestamp] $level [$service] [trace_id=$message_id] - $message"
    fi
}

# Function to generate logs for a service
generate_service_logs() {
    local service=$1
    local count=$2
    
    echo -e "${GREEN}Generating $count logs for $service...${NC}"
    
    # Create service log directory
    mkdir -p logs/$service
    
    # Generate log entries
    for ((i=1; i<=$count; i++)); do
        generate_log_entry $service >> logs/$service/${service}_sample.log
    done
    
    echo "Generated logs saved to logs/$service/${service}_sample.log"
}

# Generate logs for each service
count=$1
if [ -z "$count" ]; then
    count=100
fi

for service in "${services[@]}"; do
    generate_service_logs $service $count
done

# Create a special case with some correlated events for order flow
order_id=$(generate_uuid)
message_id=$(generate_uuid)
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
payment_id=$(generate_uuid)
shipment_id=$(generate_uuid)

# Log the complete flow of an order through all services
echo -e "${YELLOW}Generating correlated order flow logs...${NC}"

mkdir -p logs/flow

# Order service
echo "[$timestamp] INFO [order-service] [trace_id=$message_id] - Order $order_id created successfully" >> logs/flow/order_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [order-service] [trace_id=$message_id] - Order saga initiated for order $order_id" >> logs/flow/order_flow.log

# Payment service
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [payment-service] [trace_id=$message_id] - Payment $payment_id processing for order $order_id" >> logs/flow/order_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [payment-service] [trace_id=$message_id] - Payment $payment_id confirmed for order $order_id" >> logs/flow/order_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [payment-service] [trace_id=$message_id] - Payment saga completed for payment $payment_id" >> logs/flow/order_flow.log

# Inventory service
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [inventory-service] [trace_id=$message_id] - Checking inventory for order $order_id" >> logs/flow/order_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [inventory-service] [trace_id=$message_id] - Item item-1234 reserved for order $order_id" >> logs/flow/order_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [inventory-service] [trace_id=$message_id] - Inventory saga step completed for order $order_id" >> logs/flow/order_flow.log

# Shipping service
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [shipping-service] [trace_id=$message_id] - Shipping request received for order $order_id" >> logs/flow/order_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [shipping-service] [trace_id=$message_id] - Shipping label created for order $order_id" >> logs/flow/order_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [shipping-service] [trace_id=$message_id] - Shipment $shipment_id dispatched for order $order_id" >> logs/flow/order_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [shipping-service] [trace_id=$message_id] - Shipping saga step completed for order $order_id" >> logs/flow/order_flow.log

# Notification service
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [notification-service] [trace_id=$message_id] - Sending order confirmation for order $order_id" >> logs/flow/order_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [notification-service] [trace_id=$message_id] - Email notification sent to customer@example.com for order $order_id" >> logs/flow/order_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [notification-service] [trace_id=$message_id] - Notification saga step completed for order $order_id" >> logs/flow/order_flow.log

# Order service completion
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [order-service] [trace_id=$message_id] - Order $order_id completed successfully" >> logs/flow/order_flow.log

# Analytics service
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [analytics-service] [trace_id=$message_id] - Processing event OrderCompleted for order $order_id" >> logs/flow/order_flow.log

echo "Order flow logs saved to logs/flow/order_flow.log"

# Create a separate error flow
error_order_id=$(generate_uuid)
error_message_id=$(generate_uuid)
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")

echo -e "${YELLOW}Generating error flow logs...${NC}"

# Order service
echo "[$timestamp] INFO [order-service] [trace_id=$error_message_id] - Order $error_order_id created successfully" >> logs/flow/error_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [order-service] [trace_id=$error_message_id] - Order saga initiated for order $error_order_id" >> logs/flow/error_flow.log

# Payment service
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [payment-service] [trace_id=$error_message_id] - Payment payment-9876 processing for order $error_order_id" >> logs/flow/error_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] ERROR [payment-service] [trace_id=$error_message_id] - Payment payment-9876 failed: Insufficient funds" >> logs/flow/error_flow.log

# Order service compensation
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] WARN [order-service] [trace_id=$error_message_id] - Payment failed for order $error_order_id, initiating compensation" >> logs/flow/error_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [order-service] [trace_id=$error_message_id] - Order $error_order_id cancelled due to payment failure" >> logs/flow/error_flow.log

# Notification service
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [notification-service] [trace_id=$error_message_id] - Sending order cancellation notification for order $error_order_id" >> logs/flow/error_flow.log
sleep 0.1
timestamp=$(date +"%Y-%m-%d %H:%M:%S.%3N")
echo "[$timestamp] INFO [notification-service] [trace_id=$error_message_id] - Email notification sent to customer@example.com for order $error_order_id" >> logs/flow/error_flow.log

echo "Error flow logs saved to logs/flow/error_flow.log"

echo -e "${GREEN}Sample log generation complete.${NC}"