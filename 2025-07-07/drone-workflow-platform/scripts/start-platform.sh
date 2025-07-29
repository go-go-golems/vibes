#!/bin/bash

# Drone Workflow Platform Startup Script
# This script starts all microservices using tmux for easy management

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
SESSION_NAME="drone-workflow-platform"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}🚀 Starting Drone Workflow Platform${NC}"
echo -e "${BLUE}====================================${NC}"

# Check if tmux is installed
if ! command -v tmux &> /dev/null; then
    echo -e "${RED}❌ tmux is not installed. Please install tmux first.${NC}"
    exit 1
fi

# Check if Redis is running
if ! command -v redis-server &> /dev/null; then
    echo -e "${YELLOW}⚠️  Redis not found. Installing Redis...${NC}"
    sudo apt update && sudo apt install -y redis-server
fi

# Kill existing session if it exists
if tmux has-session -t $SESSION_NAME 2>/dev/null; then
    echo -e "${YELLOW}⚠️  Killing existing session: $SESSION_NAME${NC}"
    tmux kill-session -t $SESSION_NAME
fi

# Start Redis if not running
if ! pgrep redis-server > /dev/null; then
    echo -e "${YELLOW}🔄 Starting Redis server...${NC}"
    sudo systemctl start redis-server || redis-server --daemonize yes
    sleep 2
fi

# Verify Redis is running
if ! redis-cli ping > /dev/null 2>&1; then
    echo -e "${RED}❌ Failed to start Redis. Please check Redis installation.${NC}"
    exit 1
fi

echo -e "${GREEN}✅ Redis is running${NC}"

# Build all services
echo -e "${YELLOW}🔨 Building services...${NC}"
cd "$PROJECT_ROOT"

# Build workflow service
echo -e "${BLUE}Building workflow service...${NC}"
go build -o bin/workflow-service ./cmd/workflow-service/

# Build stage service
echo -e "${BLUE}Building stage service...${NC}"
go build -o bin/stage-service ./cmd/stage-service/

# Build quality service
echo -e "${BLUE}Building quality service...${NC}"
go build -o bin/quality-service ./cmd/quality-service/

# Build documentation service
echo -e "${BLUE}Building documentation service...${NC}"
go build -o bin/doc-service ./cmd/doc-service/

# Build CLI tools
echo -e "${BLUE}Building CLI tools...${NC}"
go build -o bin/workflow-cli ./cmd/workflow-cli/
go build -o bin/stage-cli ./cmd/stage-cli/
go build -o bin/monitor-cli ./cmd/monitor-cli/

echo -e "${GREEN}✅ All services built successfully${NC}"

# Create tmux session
echo -e "${YELLOW}🔄 Creating tmux session: $SESSION_NAME${NC}"
tmux new-session -d -s $SESSION_NAME -n "main"

# Window 0: Redis and logs
tmux rename-window -t $SESSION_NAME:0 "redis-logs"
tmux send-keys -t $SESSION_NAME:0 "cd $PROJECT_ROOT" C-m
tmux send-keys -t $SESSION_NAME:0 "echo 'Redis Logs and System Status'" C-m
tmux send-keys -t $SESSION_NAME:0 "echo '========================'" C-m
tmux send-keys -t $SESSION_NAME:0 "redis-cli monitor" C-m

# Window 1: Workflow Service
tmux new-window -t $SESSION_NAME -n "workflow-svc"
tmux send-keys -t $SESSION_NAME:1 "cd $PROJECT_ROOT" C-m
tmux send-keys -t $SESSION_NAME:1 "echo 'Starting Workflow Service on port 50051...'" C-m
tmux send-keys -t $SESSION_NAME:1 "sleep 2" C-m
tmux send-keys -t $SESSION_NAME:1 "./bin/workflow-service" C-m

# Window 2: Stage Service
tmux new-window -t $SESSION_NAME -n "stage-svc"
tmux send-keys -t $SESSION_NAME:2 "cd $PROJECT_ROOT" C-m
tmux send-keys -t $SESSION_NAME:2 "echo 'Starting Stage Service on port 50052...'" C-m
tmux send-keys -t $SESSION_NAME:2 "sleep 3" C-m
tmux send-keys -t $SESSION_NAME:2 "./bin/stage-service" C-m

# Window 3: Quality Service
tmux new-window -t $SESSION_NAME -n "quality-svc"
tmux send-keys -t $SESSION_NAME:3 "cd $PROJECT_ROOT" C-m
tmux send-keys -t $SESSION_NAME:3 "echo 'Starting Quality Service on port 50053...'" C-m
tmux send-keys -t $SESSION_NAME:3 "sleep 4" C-m
tmux send-keys -t $SESSION_NAME:3 "./bin/quality-service" C-m

# Window 4: Documentation Service
tmux new-window -t $SESSION_NAME -n "doc-svc"
tmux send-keys -t $SESSION_NAME:4 "cd $PROJECT_ROOT" C-m
tmux send-keys -t $SESSION_NAME:4 "echo 'Starting Documentation Service on port 50054...'" C-m
tmux send-keys -t $SESSION_NAME:4 "sleep 5" C-m
tmux send-keys -t $SESSION_NAME:4 "./bin/doc-service" C-m

# Window 5: CLI Testing
tmux new-window -t $SESSION_NAME -n "cli-test"
tmux send-keys -t $SESSION_NAME:5 "cd $PROJECT_ROOT" C-m
tmux send-keys -t $SESSION_NAME:5 "echo 'CLI Testing Environment'" C-m
tmux send-keys -t $SESSION_NAME:5 "echo '====================='" C-m
tmux send-keys -t $SESSION_NAME:5 "echo 'Available commands:'" C-m
tmux send-keys -t $SESSION_NAME:5 "echo '  ./bin/workflow-cli --help'" C-m
tmux send-keys -t $SESSION_NAME:5 "echo '  ./bin/stage-cli --help'" C-m
tmux send-keys -t $SESSION_NAME:5 "echo '  ./bin/monitor-cli --help'" C-m
tmux send-keys -t $SESSION_NAME:5 "echo ''" C-m
tmux send-keys -t $SESSION_NAME:5 "echo 'Quick start:'" C-m
tmux send-keys -t $SESSION_NAME:5 "echo '  ./bin/workflow-cli sample -o sample.json'" C-m
tmux send-keys -t $SESSION_NAME:5 "echo '  ./bin/workflow-cli create -f sample.json'" C-m
tmux send-keys -t $SESSION_NAME:5 "echo '  ./bin/monitor-cli dashboard'" C-m

# Window 6: Monitor Dashboard
tmux new-window -t $SESSION_NAME -n "monitor"
tmux send-keys -t $SESSION_NAME:6 "cd $PROJECT_ROOT" C-m
tmux send-keys -t $SESSION_NAME:6 "echo 'Waiting for services to start...'" C-m
tmux send-keys -t $SESSION_NAME:6 "sleep 10" C-m
tmux send-keys -t $SESSION_NAME:6 "./bin/monitor-cli dashboard" C-m

# Window 7: Demo Script
tmux new-window -t $SESSION_NAME -n "demo"
tmux send-keys -t $SESSION_NAME:7 "cd $PROJECT_ROOT" C-m
tmux send-keys -t $SESSION_NAME:7 "echo 'Demo Script Environment'" C-m
tmux send-keys -t $SESSION_NAME:7 "echo '======================'" C-m
tmux send-keys -t $SESSION_NAME:7 "echo 'Run: ./scripts/demo.sh to start the demo'" C-m

# Go back to main window
tmux select-window -t $SESSION_NAME:5

echo -e "${GREEN}✅ Platform started successfully!${NC}"
echo -e "${BLUE}📋 Tmux session created: $SESSION_NAME${NC}"
echo ""
echo -e "${YELLOW}Available windows:${NC}"
echo -e "  0: redis-logs    - Redis monitoring"
echo -e "  1: workflow-svc  - Workflow Service (port 50051)"
echo -e "  2: stage-svc     - Stage Service (port 50052)"
echo -e "  3: quality-svc   - Quality Service (port 50053)"
echo -e "  4: doc-svc       - Documentation Service (port 50054)"
echo -e "  5: cli-test      - CLI Testing Environment"
echo -e "  6: monitor       - System Monitor Dashboard"
echo -e "  7: demo          - Demo Script Environment"
echo ""
echo -e "${GREEN}🎯 To attach to the session:${NC}"
echo -e "  tmux attach-session -t $SESSION_NAME"
echo ""
echo -e "${GREEN}🎯 To switch between windows:${NC}"
echo -e "  Ctrl+b then 0-7 (window number)"
echo ""
echo -e "${GREEN}🎯 To run the demo:${NC}"
echo -e "  tmux send-keys -t $SESSION_NAME:7 './scripts/demo.sh' C-m"
echo ""
echo -e "${YELLOW}⚠️  To stop all services:${NC}"
echo -e "  ./scripts/stop-platform.sh"
echo ""
echo -e "${BLUE}🚀 Platform is ready! Services will be available in ~10 seconds.${NC}"

