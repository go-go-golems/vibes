#!/bin/bash

# Drone Workflow Platform Stop Script
# This script stops all services and cleans up

set -e

SESSION_NAME="drone-workflow-platform"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}🛑 Stopping Drone Workflow Platform${NC}"
echo -e "${BLUE}===================================${NC}"

# Kill tmux session if it exists
if tmux has-session -t $SESSION_NAME 2>/dev/null; then
    echo -e "${YELLOW}🔄 Stopping tmux session: $SESSION_NAME${NC}"
    tmux kill-session -t $SESSION_NAME
    echo -e "${GREEN}✅ Tmux session stopped${NC}"
else
    echo -e "${YELLOW}⚠️  No tmux session found: $SESSION_NAME${NC}"
fi

# Kill any remaining processes
echo -e "${YELLOW}🔄 Cleaning up remaining processes...${NC}"

# Kill services by port
for port in 50051 50052 50053 50054; do
    PID=$(lsof -ti:$port 2>/dev/null || true)
    if [ ! -z "$PID" ]; then
        echo -e "${YELLOW}Killing process on port $port (PID: $PID)${NC}"
        kill -9 $PID 2>/dev/null || true
    fi
done

# Kill services by name
for service in workflow-service stage-service quality-service doc-service; do
    PID=$(pgrep -f $service 2>/dev/null || true)
    if [ ! -z "$PID" ]; then
        echo -e "${YELLOW}Killing $service (PID: $PID)${NC}"
        kill -9 $PID 2>/dev/null || true
    fi
done

echo -e "${GREEN}✅ All services stopped${NC}"

# Optional: Stop Redis (uncomment if you want to stop Redis too)
# echo -e "${YELLOW}🔄 Stopping Redis...${NC}"
# sudo systemctl stop redis-server 2>/dev/null || true
# echo -e "${GREEN}✅ Redis stopped${NC}"

echo -e "${GREEN}🎉 Platform stopped successfully!${NC}"

