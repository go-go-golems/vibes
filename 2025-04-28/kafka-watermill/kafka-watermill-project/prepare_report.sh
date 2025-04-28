#!/bin/bash

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Preparing Final Report for Kafka Watermill Project${NC}"
echo "=================================================="
echo

# Create reports directory
mkdir -p reports

# Copy all documentation to reports directory
echo -e "${BLUE}Copying documentation files...${NC}"
cp FINAL_REPORT.md reports/
cp LOGGING_SUMMARY.md reports/
cp LOGGING_REPORT.md reports/
cp EVENT_DRIVEN_PATTERNS.md reports/
cp DEPLOYMENT.md reports/
cp README.md reports/

# Copy charts and images
echo -e "${BLUE}Copying charts and images...${NC}"
mkdir -p reports/images
cp *.png reports/images/

# Create a single combined report
echo -e "${BLUE}Creating combined report...${NC}"
cat << EOF > reports/COMPLETE_REPORT.md
# Kafka and Watermill Multi-Language Microservices Project

## Table of Contents

1. [Introduction](#introduction)
2. [System Architecture](#system-architecture)
3. [Implementation Details](#implementation-details)
4. [Event-Driven Patterns](#event-driven-patterns)
5. [Deployment](#deployment)
6. [Logging and Monitoring](#logging-and-monitoring)
7. [Testing and Results](#testing-and-results)
8. [Conclusion](#conclusion)

EOF

# Append the main report
echo "## Introduction" >> reports/COMPLETE_REPORT.md
grep -A 100 "## 1. Introduction" FINAL_REPORT.md | sed '1d' | grep -B 100 -m 1 "## 2. Technology Stack" | sed '$d' >> reports/COMPLETE_REPORT.md

echo -e "\n## System Architecture" >> reports/COMPLETE_REPORT.md
grep -A 500 "## 2. Technology Stack" FINAL_REPORT.md | sed '1d' | grep -B 500 -m 1 "## 3. Microservices Implementation" | sed '$d' >> reports/COMPLETE_REPORT.md

echo -e "\n## Implementation Details" >> reports/COMPLETE_REPORT.md
grep -A 1000 "## 3. Microservices Implementation" FINAL_REPORT.md | sed '1d' | grep -B 1000 -m 1 "## 4. Event-Driven Patterns Implementation" | sed '$d' >> reports/COMPLETE_REPORT.md

echo -e "\n## Event-Driven Patterns" >> reports/COMPLETE_REPORT.md
grep -A 500 "## 4. Event-Driven Patterns Implementation" FINAL_REPORT.md | sed '1d' | grep -B 500 -m 1 "## 5. Logging and Monitoring" | sed '$d' >> reports/COMPLETE_REPORT.md

echo -e "\n## Deployment" >> reports/COMPLETE_REPORT.md
grep -A 500 "## 6. Deployment and Testing" FINAL_REPORT.md | sed '1d' | grep -B 500 -m 1 "## 7. Conclusion" | sed '$d' >> reports/COMPLETE_REPORT.md

echo -e "\n## Logging and Monitoring" >> reports/COMPLETE_REPORT.md
grep -A 1000 "# Logging and Monitoring Summary" LOGGING_SUMMARY.md | sed '1d' >> reports/COMPLETE_REPORT.md

echo -e "\n## Testing and Results" >> reports/COMPLETE_REPORT.md
grep -A 300 "### 6.2 Testing Strategy" FINAL_REPORT.md | sed '1d' | grep -B 300 -m 1 "## 7. Conclusion" | sed '$d' >> reports/COMPLETE_REPORT.md

echo -e "\n## Conclusion" >> reports/COMPLETE_REPORT.md
grep -A 1000 "## 7. Conclusion" FINAL_REPORT.md | sed '1d' >> reports/COMPLETE_REPORT.md

# Fix image paths in the report
sed -i 's|!\[|![/images/|g' reports/COMPLETE_REPORT.md
sed -i 's|(images/|(\/images/|g' reports/COMPLETE_REPORT.md

# Create a zip file with all reports and images
echo -e "${BLUE}Creating zip archive...${NC}"
cd reports && zip -r ../kafka-watermill-project-report.zip * && cd ..

echo -e "${GREEN}Report preparation complete!${NC}"
echo "Complete report is available in reports/COMPLETE_REPORT.md"
echo "All reports and materials are archived in kafka-watermill-project-report.zip"