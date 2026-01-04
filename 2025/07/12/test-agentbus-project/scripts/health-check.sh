#!/bin/bash

# Health Check Script for Pelican Farm Management System
set -euo pipefail

# Configuration
BASE_URL="${BASE_URL:-http://localhost:8080}"
TIMEOUT="${TIMEOUT:-10}"
LOG_FILE="./logs/health-check.log"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Create logs directory
mkdir -p logs

# Logging function
log() {
    echo -e "${1}" | tee -a $LOG_FILE
}

# HTTP health check
check_http() {
    local endpoint="$1"
    local description="$2"
    
    log "${BLUE}🔍 Checking $description...${NC}"
    
    if curl -f -s --max-time $TIMEOUT "$BASE_URL$endpoint" >/dev/null; then
        log "${GREEN}✅ $description: OK${NC}"
        return 0
    else
        log "${RED}❌ $description: FAILED${NC}"
        return 1
    fi
}

# API response check
check_api_response() {
    local endpoint="$1"
    local description="$2"
    
    log "${BLUE}🔍 Checking $description response...${NC}"
    
    response=$(curl -f -s --max-time $TIMEOUT "$BASE_URL$endpoint" || echo "ERROR")
    
    if [[ "$response" != "ERROR" ]] && echo "$response" | jq . >/dev/null 2>&1; then
        log "${GREEN}✅ $description: Valid JSON response${NC}"
        return 0
    else
        log "${RED}❌ $description: Invalid or no response${NC}"
        return 1
    fi
}

# Database connectivity check
check_database() {
    log "${BLUE}🔍 Checking database connectivity via API...${NC}"
    
    response=$(curl -f -s --max-time $TIMEOUT "$BASE_URL/api/v1/pelicans/stats" || echo "ERROR")
    
    if [[ "$response" != "ERROR" ]]; then
        # Try to parse the response as JSON
        if echo "$response" | jq .total >/dev/null 2>&1; then
            total=$(echo "$response" | jq -r .total)
            log "${GREEN}✅ Database: Connected (Total pelicans: $total)${NC}"
            return 0
        fi
    fi
    
    log "${RED}❌ Database: Connection failed${NC}"
    return 1
}

# Container health check (if running in Docker)
check_container_health() {
    if command -v docker >/dev/null 2>&1; then
        log "${BLUE}🔍 Checking container health...${NC}"
        
        container_status=$(docker ps --filter "name=pelican-farm" --format "{{.Status}}" | head -1 || echo "")
        
        if [[ -n "$container_status" ]]; then
            if echo "$container_status" | grep -q "healthy\|Up"; then
                log "${GREEN}✅ Container: $container_status${NC}"
                return 0
            else
                log "${RED}❌ Container: $container_status${NC}"
                return 1
            fi
        else
            log "${YELLOW}⚠️  Container: Not running in Docker or container not found${NC}"
            return 0  # Not an error if not running in Docker
        fi
    else
        log "${YELLOW}⚠️  Docker not available for container health check${NC}"
        return 0
    fi
}

# Comprehensive health check
run_health_check() {
    log "${BLUE}🏥 Starting comprehensive health check...${NC}"
    log "${BLUE}Base URL: $BASE_URL${NC}"
    log "${BLUE}Timeout: ${TIMEOUT}s${NC}"
    log "${BLUE}Timestamp: $(date)${NC}\n"
    
    failed_checks=0
    
    # Basic connectivity
    check_http "/" "Web Interface" || ((failed_checks++))
    
    # API endpoints
    check_http "/api/v1/pelicans/stats" "API Health" || ((failed_checks++))
    check_api_response "/api/v1/pelicans" "Pelicans API" || ((failed_checks++))
    check_api_response "/api/v1/farms" "Farms API" || ((failed_checks++))
    
    # Database connectivity
    check_database || ((failed_checks++))
    
    # Container health (if applicable)
    check_container_health || ((failed_checks++))
    
    # Summary
    log "\n${BLUE}📊 Health Check Summary:${NC}"
    if [[ $failed_checks -eq 0 ]]; then
        log "${GREEN}🎉 All health checks passed!${NC}"
        exit 0
    else
        log "${RED}❌ $failed_checks health check(s) failed${NC}"
        exit 1
    fi
}

# Quick health check (just basic connectivity)
quick_check() {
    if curl -f -s --max-time 5 "$BASE_URL/api/v1/pelicans/stats" >/dev/null; then
        echo "healthy"
        exit 0
    else
        echo "unhealthy"
        exit 1
    fi
}

# Performance metrics
performance_check() {
    log "${BLUE}📈 Running performance check...${NC}"
    
    # Measure response time
    response_time=$(curl -o /dev/null -s -w "%{time_total}" --max-time $TIMEOUT "$BASE_URL/api/v1/pelicans/stats")
    
    if (( $(echo "$response_time < 1.0" | bc -l) )); then
        log "${GREEN}✅ Response time: ${response_time}s (Good)${NC}"
    elif (( $(echo "$response_time < 3.0" | bc -l) )); then
        log "${YELLOW}⚠️  Response time: ${response_time}s (Acceptable)${NC}"
    else
        log "${RED}❌ Response time: ${response_time}s (Slow)${NC}"
    fi
    
    # Check memory usage if in Docker
    if command -v docker >/dev/null 2>&1; then
        memory_usage=$(docker stats --no-stream --format "{{.MemUsage}}" pelican-farm-app 2>/dev/null || echo "N/A")
        log "${BLUE}📊 Memory usage: $memory_usage${NC}"
    fi
}

# Main script logic
case "${1:-full}" in
    "full")
        run_health_check
        ;;
    "quick")
        quick_check
        ;;
    "performance" | "perf")
        performance_check
        ;;
    "api")
        check_api_response "/api/v1/pelicans/stats" "API Health"
        ;;
    *)
        echo "Usage: $0 {full|quick|performance|api}"
        echo "  full        - Comprehensive health check (default)"
        echo "  quick       - Quick connectivity check"
        echo "  performance - Performance metrics check"
        echo "  api         - API-only health check"
        exit 1
        ;;
esac
