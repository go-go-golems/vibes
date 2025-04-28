#!/bin/bash

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Test Scenarios Generator for Kafka Watermill Project${NC}"
echo "=================================================="
echo

# Function to generate a random UUID
generate_uuid() {
    cat /proc/sys/kernel/random/uuid | tr -d '-' | head -c 12
}

# Test Scenarios
echo -e "${GREEN}Generating test scenarios...${NC}"

# Directory for test data
mkdir -p test_data

# 1. Happy Path - Complete Order Flow
echo -e "${BLUE}Scenario 1: Happy Path - Complete Order Flow${NC}"
order_id=$(generate_uuid)
trace_id=$(generate_uuid)

cat > test_data/scenario1_happy_path.json << EOF
{
  "scenario": "Happy Path - Complete Order Flow",
  "order_id": "${order_id}",
  "trace_id": "${trace_id}",
  "customer": {
    "id": "cust_$(generate_uuid)",
    "name": "John Smith",
    "email": "john.smith@example.com"
  },
  "items": [
    {
      "id": "item_$(generate_uuid)",
      "name": "Product A",
      "quantity": 2,
      "price": 29.99
    },
    {
      "id": "item_$(generate_uuid)",
      "name": "Product B",
      "quantity": 1,
      "price": 49.99
    }
  ],
  "payment": {
    "id": "pay_$(generate_uuid)",
    "amount": 109.97,
    "method": "credit_card"
  },
  "shipping": {
    "id": "ship_$(generate_uuid)",
    "address": "123 Main St, City, Country",
    "method": "standard"
  }
}
EOF

echo "Created test data for Scenario 1"

# 2. Payment Failure Flow
echo -e "${BLUE}Scenario 2: Payment Failure Flow${NC}"
order_id=$(generate_uuid)
trace_id=$(generate_uuid)

cat > test_data/scenario2_payment_failure.json << EOF
{
  "scenario": "Payment Failure Flow",
  "order_id": "${order_id}",
  "trace_id": "${trace_id}",
  "customer": {
    "id": "cust_$(generate_uuid)",
    "name": "Jane Doe",
    "email": "jane.doe@example.com"
  },
  "items": [
    {
      "id": "item_$(generate_uuid)",
      "name": "Product C",
      "quantity": 1,
      "price": 299.99
    }
  ],
  "payment": {
    "id": "pay_$(generate_uuid)",
    "amount": 299.99,
    "method": "credit_card",
    "error": "insufficient_funds"
  },
  "error_details": {
    "stage": "payment",
    "message": "Payment failed due to insufficient funds",
    "requires_compensation": true
  }
}
EOF

echo "Created test data for Scenario 2"

# 3. Inventory Shortage Flow
echo -e "${BLUE}Scenario 3: Inventory Shortage Flow${NC}"
order_id=$(generate_uuid)
trace_id=$(generate_uuid)

cat > test_data/scenario3_inventory_shortage.json << EOF
{
  "scenario": "Inventory Shortage Flow",
  "order_id": "${order_id}",
  "trace_id": "${trace_id}",
  "customer": {
    "id": "cust_$(generate_uuid)",
    "name": "Alice Johnson",
    "email": "alice.johnson@example.com"
  },
  "items": [
    {
      "id": "item_$(generate_uuid)",
      "name": "Limited Edition Product",
      "quantity": 5,
      "price": 19.99
    }
  ],
  "payment": {
    "id": "pay_$(generate_uuid)",
    "amount": 99.95,
    "method": "paypal"
  },
  "error_details": {
    "stage": "inventory",
    "message": "Not enough items in stock (requested: 5, available: 2)",
    "requires_compensation": true
  }
}
EOF

echo "Created test data for Scenario 3"

# 4. Shipping Delay Flow
echo -e "${BLUE}Scenario 4: Shipping Delay Flow${NC}"
order_id=$(generate_uuid)
trace_id=$(generate_uuid)

cat > test_data/scenario4_shipping_delay.json << EOF
{
  "scenario": "Shipping Delay Flow",
  "order_id": "${order_id}",
  "trace_id": "${trace_id}",
  "customer": {
    "id": "cust_$(generate_uuid)",
    "name": "Bob Williams",
    "email": "bob.williams@example.com"
  },
  "items": [
    {
      "id": "item_$(generate_uuid)",
      "name": "Product D",
      "quantity": 1,
      "price": 59.99
    },
    {
      "id": "item_$(generate_uuid)",
      "name": "Product E",
      "quantity": 2,
      "price": 12.50
    }
  ],
  "payment": {
    "id": "pay_$(generate_uuid)",
    "amount": 84.99,
    "method": "credit_card"
  },
  "shipping": {
    "id": "ship_$(generate_uuid)",
    "address": "456 Oak St, Another City, Country",
    "method": "express",
    "delay": true,
    "delay_reason": "weather_conditions"
  }
}
EOF

echo "Created test data for Scenario 4"

# 5. High Load Simulation
echo -e "${BLUE}Scenario 5: High Load Simulation${NC}"

cat > test_data/scenario5_high_load.json << EOF
{
  "scenario": "High Load Simulation",
  "orders": [
EOF

for ((i=1; i<=20; i++)); do
  order_id=$(generate_uuid)
  trace_id=$(generate_uuid)
  cust_id="cust_$(generate_uuid)"
  item_id="item_$(generate_uuid)"
  pay_id="pay_$(generate_uuid)"
  quantity=$((RANDOM % 5 + 1))
  price=$((RANDOM % 100 + 10))
  
  if [ $i -lt 20 ]; then
    comma=","
  else
    comma=""
  fi
  
  cat >> test_data/scenario5_high_load.json << EOF
    {
      "order_id": "${order_id}",
      "trace_id": "${trace_id}",
      "customer": {
        "id": "${cust_id}",
        "name": "Customer ${i}",
        "email": "customer${i}@example.com"
      },
      "items": [
        {
          "id": "${item_id}",
          "name": "Product ${i}",
          "quantity": ${quantity},
          "price": ${price}.99
        }
      ],
      "payment": {
        "id": "${pay_id}",
        "method": "credit_card"
      }
    }${comma}
EOF
done

cat >> test_data/scenario5_high_load.json << EOF
  ]
}
EOF

echo "Created test data for Scenario 5"

# 6. System Recovery Test
echo -e "${BLUE}Scenario 6: System Recovery Test${NC}"
cat > test_data/scenario6_recovery_test.json << EOF
{
  "scenario": "System Recovery Test",
  "actions": [
    {
      "service": "kafka",
      "action": "restart",
      "delay": 5
    },
    {
      "service": "order-service",
      "action": "kill",
      "delay": 10
    },
    {
      "service": "payment-service",
      "action": "kill",
      "delay": 15
    },
    {
      "service": "order-service",
      "action": "start",
      "delay": 20
    },
    {
      "service": "payment-service",
      "action": "start",
      "delay": 25
    }
  ],
  "test_orders": [
    {
      "order_id": "$(generate_uuid)",
      "trace_id": "$(generate_uuid)",
      "timing": "before_restart"
    },
    {
      "order_id": "$(generate_uuid)",
      "trace_id": "$(generate_uuid)",
      "timing": "during_outage"
    },
    {
      "order_id": "$(generate_uuid)",
      "trace_id": "$(generate_uuid)",
      "timing": "after_recovery"
    }
  ]
}
EOF

echo "Created test data for Scenario 6"

echo -e "${GREEN}Test scenario data generation complete.${NC}"
echo "Test data is available in the test_data directory."