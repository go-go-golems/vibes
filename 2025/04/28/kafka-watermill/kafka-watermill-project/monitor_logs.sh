#!/bin/bash

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Starting log monitoring for all services${NC}"
echo "============================================"
echo

# Array of services to monitor
services=(
    "order-service"
    "payment-service"
    "inventory-service"
    "notification-service"
    "shipping-service"
    "analytics-service"
    "kafka"
)

# Function to display usage
usage() {
    echo -e "${YELLOW}Usage:${NC}"
    echo "  $0 [service-name]"
    echo
    echo "Available services:"
    for service in "${services[@]}"; do
        echo "  - $service"
    done
    echo "  - all (monitors all services)"
    echo
}

# Check if a service is specified
if [ $# -eq 0 ]; then
    usage
    exit 1
fi

# Get the service to monitor
service=$1

# Check if the service is valid
valid_service=false
if [ "$service" == "all" ]; then
    valid_service=true
else
    for s in "${services[@]}"; do
        if [ "$s" == "$service" ]; then
            valid_service=true
            break
        fi
    done
fi

if [ "$valid_service" != "true" ]; then
    echo -e "${RED}Invalid service: $service${NC}"
    usage
    exit 1
fi

# Monitor logs
if [ "$service" == "all" ]; then
    echo -e "${GREEN}Monitoring logs for all services...${NC}"
    docker-compose logs -f
else
    echo -e "${GREEN}Monitoring logs for $service...${NC}"
    docker-compose logs -f "$service"
fi