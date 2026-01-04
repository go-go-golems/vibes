#!/bin/bash

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Log Collection Script for Kafka Watermill Project${NC}"
echo "=================================================="
echo

# Create logs directory if it doesn't exist
mkdir -p logs

# Get current timestamp for log file names
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")

# Function to collect logs from a specific service
collect_service_logs() {
    local service=$1
    echo -e "${GREEN}Collecting logs for $service...${NC}"
    
    # Create service log directory
    mkdir -p logs/$service
    
    # Copy the service logs to the logs directory
    docker logs $service > logs/$service/${service}_${TIMESTAMP}.log 2>&1
    
    echo "Logs saved to logs/$service/${service}_${TIMESTAMP}.log"
}

# Function to collect Kafka logs
collect_kafka_logs() {
    echo -e "${GREEN}Collecting Kafka logs...${NC}"
    
    # Create Kafka log directory
    mkdir -p logs/kafka
    
    # Collect Kafka broker logs
    docker logs kafka > logs/kafka/kafka_broker_${TIMESTAMP}.log 2>&1
    
    # Collect ZooKeeper logs
    docker logs zookeeper > logs/kafka/zookeeper_${TIMESTAMP}.log 2>&1
    
    echo "Kafka logs saved to logs/kafka/"
}

# Function to collect logs from all containers
collect_all_logs() {
    echo -e "${GREEN}Collecting logs from all containers...${NC}"
    
    # Get list of running containers
    containers=$(docker ps --format "{{.Names}}")
    
    # Collect logs from each container
    for container in $containers; do
        collect_service_logs $container
    done
}

# Function to extract log statistics
analyze_logs() {
    echo -e "${YELLOW}Analyzing logs...${NC}"
    echo "=================================================="
    
    # Create analysis directory
    mkdir -p logs/analysis
    
    # Count log entries by service
    echo -e "${BLUE}Log entries by service:${NC}" > logs/analysis/log_stats_${TIMESTAMP}.txt
    echo "=================================================" >> logs/analysis/log_stats_${TIMESTAMP}.txt
    
    for service_dir in logs/*/; do
        service=$(basename $service_dir)
        
        # Skip analysis directory
        if [ "$service" == "analysis" ]; then
            continue
        fi
        
        # Count total log entries for this service
        log_count=$(cat logs/$service/*.log 2>/dev/null | wc -l)
        echo "$service: $log_count entries" >> logs/analysis/log_stats_${TIMESTAMP}.txt
        
        # Count error logs
        error_count=$(grep -i "error" logs/$service/*.log 2>/dev/null | wc -l)
        echo "  - Errors: $error_count" >> logs/analysis/log_stats_${TIMESTAMP}.txt
        
        # Count warning logs
        warning_count=$(grep -i "warn" logs/$service/*.log 2>/dev/null | wc -l)
        echo "  - Warnings: $warning_count" >> logs/analysis/log_stats_${TIMESTAMP}.txt
        
        # Extract unique error messages
        echo -e "\n${BLUE}Unique error messages for $service:${NC}" >> logs/analysis/log_stats_${TIMESTAMP}.txt
        echo "=================================================" >> logs/analysis/log_stats_${TIMESTAMP}.txt
        grep -i "error" logs/$service/*.log 2>/dev/null | sort | uniq -c | sort -nr >> logs/analysis/log_stats_${TIMESTAMP}.txt
        
        echo >> logs/analysis/log_stats_${TIMESTAMP}.txt
    done
    
    # Analyze message flow across services
    echo -e "\n${BLUE}Message flow analysis:${NC}" >> logs/analysis/log_stats_${TIMESTAMP}.txt
    echo "=================================================" >> logs/analysis/log_stats_${TIMESTAMP}.txt
    
    # Extract message IDs and track them across services
    for service_dir in logs/*/; do
        service=$(basename $service_dir)
        
        # Skip analysis directory
        if [ "$service" == "analysis" ]; then
            continue
        fi
        
        # Find message IDs
        grep -o "message_id[\"']\s*:\s*[\"'][a-zA-Z0-9-]\+[\"']" logs/$service/*.log 2>/dev/null >> logs/analysis/message_ids.txt
    done
    
    # Sort and count unique message IDs
    if [ -f logs/analysis/message_ids.txt ]; then
        sort logs/analysis/message_ids.txt | uniq -c | sort -nr > logs/analysis/message_flow_${TIMESTAMP}.txt
        echo "Top 10 most frequent messages:" >> logs/analysis/log_stats_${TIMESTAMP}.txt
        head -10 logs/analysis/message_flow_${TIMESTAMP}.txt >> logs/analysis/log_stats_${TIMESTAMP}.txt
    fi
    
    echo -e "${GREEN}Log analysis complete.${NC}"
    echo "Results saved to logs/analysis/log_stats_${TIMESTAMP}.txt"
}

# Main execution
if [ "$1" == "analyze" ]; then
    # Only analyze existing logs
    analyze_logs
elif [ "$1" == "kafka" ]; then
    # Collect Kafka logs only
    collect_kafka_logs
    analyze_logs
elif [ -z "$1" ]; then
    # Collect logs from all services
    collect_all_logs
    analyze_logs
else
    # Collect logs from a specific service
    collect_service_logs $1
    analyze_logs
fi

echo -e "${GREEN}Log collection and analysis complete.${NC}"