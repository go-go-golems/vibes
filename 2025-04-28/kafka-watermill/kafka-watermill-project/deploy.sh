#!/bin/bash

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Starting deployment of Kafka Watermill Multi-Language Microservices${NC}"
echo "=================================================================="
echo

# Check if Docker is running
echo -n "Checking Docker... "
if ! docker info > /dev/null 2>&1; then
    echo -e "${RED}Docker is not running. Please start Docker and try again.${NC}"
    exit 1
else
    echo -e "${GREEN}OK${NC}"
fi

# Add required Go dependencies
echo "Installing Go dependencies..."
go get -u github.com/gorilla/mux

# Build and start the system
echo "Building and starting the system..."

# Create necessary directories
mkdir -p logs

# Build and start with Docker Compose
docker-compose down --remove-orphans
docker-compose build
docker-compose up -d

# Wait for services to be ready
echo "Waiting for services to start..."
sleep 15

# Display status
echo -e "\n${GREEN}Deployment completed!${NC}"
echo
echo "Service endpoints:"
echo "- Order Service: http://localhost:8001"
echo "- Payment Service: http://localhost:8002"
echo "- Inventory Service: http://localhost:8003"
echo "- Notification Service: http://localhost:8004"
echo "- Shipping Service: http://localhost:8085"
echo "- Analytics Service: http://localhost:3000"
echo "- Kafka UI: http://localhost:8080"
echo
echo "Commands:"
echo "- View logs: docker-compose logs -f [service-name]"
echo "- Run tests: ./test_system.sh"
echo "- Stop system: docker-compose down"
echo

# Run health checks
echo "Running health checks..."
./test_system.sh