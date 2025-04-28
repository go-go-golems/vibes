#!/bin/bash

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Test Client for Kafka-Watermill Microservices${NC}"
echo "============================================"

# Function to create an order
create_order() {
    user_id="user-$RANDOM"
    product1="product$((RANDOM % 3 + 1))"
    product2="product$((RANDOM % 3 + 1))"
    quantity1=$((RANDOM % 3 + 1))
    quantity2=$((RANDOM % 2 + 1))
    price1="19.99"
    price2="29.99"
    
    total_amount=$(echo "$quantity1 * $price1 + $quantity2 * $price2" | bc)
    
    echo -e "${YELLOW}Creating order for user $user_id${NC}"
    echo "Product 1: $product1 (Quantity: $quantity1, Price: $price1)"
    echo "Product 2: $product2 (Quantity: $quantity2, Price: $price2)"
    echo "Total Amount: $total_amount"
    
    curl -s -X POST -H "Content-Type: application/json" \
        -d "{
            \"user_id\": \"$user_id\",
            \"items\": [
                {
                    \"product_id\": \"$product1\",
                    \"name\": \"Product ${product1:7:1}\",
                    \"quantity\": $quantity1,
                    \"price\": $price1
                },
                {
                    \"product_id\": \"$product2\",
                    \"name\": \"Product ${product2:7:1}\",
                    \"quantity\": $quantity2,
                    \"price\": $price2
                }
            ],
            \"total_amount\": $total_amount
        }" \
        http://localhost:8001/orders
}

# Function to generate multiple orders
generate_orders() {
    count=$1
    interval=$2
    
    echo -e "${YELLOW}Generating $count orders with $interval second interval${NC}"
    echo
    
    for ((i=1; i<=$count; i++)); do
        echo -e "${YELLOW}Order $i of $count${NC}"
        response=$(create_order)
        
        if [[ $response == *"order_id"* ]]; then
            order_id=$(echo $response | sed 's/.*"order_id":"\([^"]*\)".*/\1/')
            echo -e "${GREEN}Order created with ID: $order_id${NC}"
        else
            echo -e "${RED}Failed to create order${NC}"
            echo "$response"
        fi
        
        echo
        
        if [ $i -lt $count ]; then
            echo "Waiting $interval seconds before next order..."
            sleep $interval
        fi
    done
}

# Main menu
while true; do
    echo
    echo "1. Create a single order"
    echo "2. Generate multiple orders"
    echo "3. Exit"
    echo
    read -p "Select an option: " option
    
    case $option in
        1)
            echo
            response=$(create_order)
            
            if [[ $response == *"order_id"* ]]; then
                order_id=$(echo $response | sed 's/.*"order_id":"\([^"]*\)".*/\1/')
                echo -e "${GREEN}Order created with ID: $order_id${NC}"
            else
                echo -e "${RED}Failed to create order${NC}"
                echo "$response"
            fi
            ;;
        2)
            echo
            read -p "How many orders? " count
            read -p "Interval between orders (seconds)? " interval
            
            if [[ ! $count =~ ^[0-9]+$ ]] || [[ ! $interval =~ ^[0-9]+$ ]]; then
                echo -e "${RED}Invalid input. Count and interval must be numbers.${NC}"
                continue
            fi
            
            generate_orders $count $interval
            ;;
        3)
            echo "Exiting..."
            exit 0
            ;;
        *)
            echo -e "${RED}Invalid option${NC}"
            ;;
    esac
done