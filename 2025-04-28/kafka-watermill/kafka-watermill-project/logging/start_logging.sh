#!/bin/bash

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Starting ELK Stack for Centralized Logging${NC}"
echo "============================================="
echo

# Check if Docker is running
echo -n "Checking Docker... "
if ! docker info > /dev/null 2>&1; then
    echo -e "${RED}Docker is not running. Please start Docker and try again.${NC}"
    exit 1
else
    echo -e "${GREEN}OK${NC}"
fi

# Function to wait for service to be ready
wait_for_service() {
    local service=$1
    local url=$2
    local max_attempts=$3
    local wait_seconds=$4
    
    echo -n "Waiting for $service to be ready "
    
    for ((i=1; i<=$max_attempts; i++)); do
        echo -n "."
        if curl -s -f "$url" > /dev/null 2>&1; then
            echo -e " ${GREEN}OK${NC}"
            return 0
        fi
        sleep $wait_seconds
    done
    
    echo -e " ${RED}Failed${NC}"
    echo "Could not connect to $service at $url after $max_attempts attempts"
    return 1
}

# Start the ELK stack
echo "Starting ELK Stack..."
docker-compose up -d

# Wait for Elasticsearch to be ready
wait_for_service "Elasticsearch" "http://localhost:9200" 30 5 || exit 1

# Wait for Kibana to be ready
wait_for_service "Kibana" "http://localhost:5601" 30 5 || exit 1

# Create Kibana index patterns
echo "Creating Kibana index patterns..."
curl -X POST "http://localhost:5601/api/saved_objects/index-pattern/logs-*" \
    -H "kbn-xsrf: true" \
    -H "Content-Type: application/json" \
    -d '{"attributes":{"title":"logs-*","timeFieldName":"@timestamp"}}' \
    > /dev/null 2>&1

curl -X POST "http://localhost:5601/api/saved_objects/index-pattern/error-logs-*" \
    -H "kbn-xsrf: true" \
    -H "Content-Type: application/json" \
    -d '{"attributes":{"title":"error-logs-*","timeFieldName":"@timestamp"}}' \
    > /dev/null 2>&1

echo -e "${GREEN}ELK Stack is ready!${NC}"
echo
echo "Elasticsearch: http://localhost:9200"
echo "Kibana: http://localhost:5601"
echo "Logstash: TCP/UDP port 5000, Beats port 5044"
echo
echo "To view logs in Kibana:"
echo "1. Go to http://localhost:5601"
echo "2. Navigate to 'Analytics' -> 'Discover'"
echo "3. Select the 'logs-*' index pattern"
echo
echo "To stop the ELK stack, run: docker-compose down"