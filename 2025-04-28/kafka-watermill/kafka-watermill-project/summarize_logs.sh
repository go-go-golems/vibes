#!/bin/bash

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Log Summary for Kafka Watermill Project${NC}"
echo "=================================================="
echo

# Create log summary directory
mkdir -p log_summary

# Count logs by service
echo -e "${BLUE}Counting logs by service...${NC}"
echo "# Logs by Service" > log_summary/logs_by_service.md
echo "" >> log_summary/logs_by_service.md
echo "| Service | Count |" >> log_summary/logs_by_service.md
echo "|---------|-------|" >> log_summary/logs_by_service.md

for service in logs/*; do
    if [ -d "$service" ]; then
        service_name=$(basename "$service")
        if [ "$service_name" != "analysis" ] && [ "$service_name" != "test_runs" ]; then
            count=$(find "$service" -name "*.log" -exec cat {} \; | wc -l)
            echo "| $service_name | $count |" >> log_summary/logs_by_service.md
        fi
    fi
done

# Count logs by level
echo -e "${BLUE}Counting logs by level...${NC}"
echo "# Logs by Level" > log_summary/logs_by_level.md
echo "" >> log_summary/logs_by_level.md
echo "| Level | Count |" >> log_summary/logs_by_level.md
echo "|-------|-------|" >> log_summary/logs_by_level.md

for level in "INFO" "DEBUG" "WARN" "ERROR" "FATAL"; do
    count=$(find logs -name "*.log" -exec grep -i "\[$level\]" {} \; | wc -l)
    echo "| $level | $count |" >> log_summary/logs_by_level.md
done

# Summarize test scenarios
echo -e "${BLUE}Summarizing test scenarios...${NC}"
echo "# Test Scenarios" > log_summary/test_scenarios.md
echo "" >> log_summary/test_scenarios.md
echo "| Scenario | Status | Logs |" >> log_summary/test_scenarios.md
echo "|----------|--------|------|" >> log_summary/test_scenarios.md

for scenario in logs/test_runs/*; do
    if [ -d "$scenario" ]; then
        scenario_name=$(basename "$scenario")
        log_count=$(find "$scenario" -name "*.log" | wc -l)
        error_count=$(find "$scenario" -name "*.log" -exec grep -i "\[ERROR\]" {} \; | wc -l)
        
        if [ $error_count -eq 0 ]; then
            status="Success"
        else
            status="Failure"
        fi
        
        echo "| $scenario_name | $status | $log_count |" >> log_summary/test_scenarios.md
    fi
done

# Identify common errors
echo -e "${BLUE}Identifying common errors...${NC}"
echo "# Common Errors" > log_summary/common_errors.md
echo "" >> log_summary/common_errors.md
echo "| Service | Error | Count |" >> log_summary/common_errors.md
echo "|---------|-------|-------|" >> log_summary/common_errors.md

for service in logs/*; do
    if [ -d "$service" ]; then
        service_name=$(basename "$service")
        if [ "$service_name" != "analysis" ] && [ "$service_name" != "test_runs" ]; then
            # Extract error messages and count occurrences
            find "$service" -name "*.log" -exec grep -i "\[ERROR\]" {} \; | 
            sed -e 's/.*\[ERROR\].*- \(.*\)/\1/' | 
            sort | uniq -c | sort -nr | head -5 | 
            while read -r count error; do
                # Truncate long error messages
                if [ ${#error} -gt 50 ]; then
                    error="${error:0:50}..."
                fi
                echo "| $service_name | $error | $count |" >> log_summary/common_errors.md
            done
        fi
    fi
done

# Create a combined summary
echo -e "${BLUE}Creating combined summary...${NC}"
cat log_summary/logs_by_service.md > log_summary/LOG_SUMMARY.md
echo "" >> log_summary/LOG_SUMMARY.md
cat log_summary/logs_by_level.md | tail -n +2 >> log_summary/LOG_SUMMARY.md
echo "" >> log_summary/LOG_SUMMARY.md
cat log_summary/test_scenarios.md | tail -n +2 >> log_summary/LOG_SUMMARY.md
echo "" >> log_summary/LOG_SUMMARY.md
cat log_summary/common_errors.md | tail -n +2 >> log_summary/LOG_SUMMARY.md

echo -e "${GREEN}Log summary created!${NC}"
echo "Complete summary is available in log_summary/LOG_SUMMARY.md"

# Copy the summary to the reports directory
cp log_summary/LOG_SUMMARY.md reports/

# Add the summary to the zip file
cd reports && zip -u ../kafka-watermill-project-report.zip LOG_SUMMARY.md && cd ..