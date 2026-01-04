#!/bin/bash

# Drone Workflow Platform Demo Script
# This script demonstrates the platform functionality

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Demo configuration
DEMO_DELAY=3
WORKFLOW_CLI="$PROJECT_ROOT/bin/workflow-cli"
STAGE_CLI="$PROJECT_ROOT/bin/stage-cli"
MONITOR_CLI="$PROJECT_ROOT/bin/monitor-cli"

echo -e "${BLUE}🎬 Drone Workflow Platform Demo${NC}"
echo -e "${BLUE}===============================${NC}"
echo ""

# Function to wait and show progress
wait_with_message() {
    local message="$1"
    local delay="${2:-$DEMO_DELAY}"
    echo -e "${YELLOW}⏳ $message${NC}"
    sleep $delay
}

# Function to run command with demo formatting
demo_command() {
    local description="$1"
    local command="$2"
    local show_output="${3:-true}"
    
    echo -e "${PURPLE}📋 $description${NC}"
    echo -e "${CYAN}💻 Command: $command${NC}"
    echo ""
    
    if [ "$show_output" = "true" ]; then
        eval $command
        echo ""
    else
        eval $command > /dev/null 2>&1
    fi
    
    wait_with_message "Press Enter to continue..." 0
    read -p ""
}

# Check if services are running
check_services() {
    echo -e "${YELLOW}🔍 Checking if services are running...${NC}"
    
    for port in 50051 50052 50053 50054; do
        if ! nc -z localhost $port 2>/dev/null; then
            echo -e "${RED}❌ Service on port $port is not running${NC}"
            echo -e "${YELLOW}Please run './scripts/start-platform.sh' first${NC}"
            exit 1
        fi
    done
    
    echo -e "${GREEN}✅ All services are running${NC}"
    echo ""
}

# Main demo flow
main_demo() {
    echo -e "${GREEN}🎯 This demo will showcase:${NC}"
    echo -e "  1. Creating a sample workflow"
    echo -e "  2. Executing the workflow"
    echo -e "  3. Monitoring execution progress"
    echo -e "  4. Viewing quality reports"
    echo -e "  5. System health monitoring"
    echo ""
    
    wait_with_message "Starting demo in 5 seconds..." 5
    
    # Step 1: Create sample workflow
    echo -e "${BLUE}📝 Step 1: Creating Sample Workflow${NC}"
    echo -e "${BLUE}===================================${NC}"
    
    demo_command \
        "Generate a sample workflow JSON file" \
        "$WORKFLOW_CLI sample -o /tmp/demo-workflow.json"
    
    demo_command \
        "View the generated workflow structure" \
        "head -20 /tmp/demo-workflow.json"
    
    demo_command \
        "Create the workflow in the system" \
        "$WORKFLOW_CLI create -f /tmp/demo-workflow.json"
    
    # Step 2: List workflows
    echo -e "${BLUE}📋 Step 2: Listing Workflows${NC}"
    echo -e "${BLUE}============================${NC}"
    
    demo_command \
        "List all workflows in the system" \
        "$WORKFLOW_CLI list"
    
    # Get the workflow ID (simplified - in real demo you'd parse the output)
    WORKFLOW_ID=$(echo "workflow_QuadCopter_Basic_Assembly_$(date +%s)")
    
    # Step 3: Execute workflow
    echo -e "${BLUE}🚀 Step 3: Executing Workflow${NC}"
    echo -e "${BLUE}=============================${NC}"
    
    # For demo purposes, we'll simulate execution with a known workflow ID
    echo -e "${YELLOW}Note: Using simulated workflow ID for demo purposes${NC}"
    WORKFLOW_ID="workflow_QuadCopter_Basic_Assembly_1234567890"
    
    demo_command \
        "Execute the workflow with batch ID 'demo-batch-001'" \
        "$WORKFLOW_CLI execute $WORKFLOW_ID --batch-id demo-batch-001"
    
    # Step 4: Monitor execution
    echo -e "${BLUE}📊 Step 4: Monitoring Execution${NC}"
    echo -e "${BLUE}==============================${NC}"
    
    demo_command \
        "Check system health" \
        "$MONITOR_CLI health"
    
    demo_command \
        "View system metrics" \
        "$MONITOR_CLI metrics"
    
    # Step 5: Simulate stage execution
    echo -e "${BLUE}🔧 Step 5: Stage Execution Demo${NC}"
    echo -e "${BLUE}==============================${NC}"
    
    echo -e "${YELLOW}Simulating stage execution...${NC}"
    
    # Create a sample stage status file
    cat > /tmp/stage-status.json << EOF
{
  "stage_execution_id": "stage_exec_demo_123",
  "stage_id": "frame_prep",
  "status": "completed",
  "started_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "completed_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "worker_id": "worker_001",
  "quality_results": [
    {
      "check_id": "qc_visual_001",
      "type": "visual",
      "passed": true,
      "criteria": "No cracks, clean surfaces",
      "result_details": "Visual inspection passed",
      "inspector_id": "inspector_001",
      "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    }
  ]
}
EOF
    
    demo_command \
        "View sample stage status structure" \
        "cat /tmp/stage-status.json | jq ."
    
    # Step 6: System monitoring
    echo -e "${BLUE}📈 Step 6: System Monitoring${NC}"
    echo -e "${BLUE}============================${NC}"
    
    demo_command \
        "View recent system events" \
        "$MONITOR_CLI events --count 5"
    
    echo -e "${YELLOW}Starting real-time dashboard (will run for 10 seconds)...${NC}"
    timeout 10 $MONITOR_CLI dashboard || true
    echo ""
    
    # Step 7: Quality reporting
    echo -e "${BLUE}📊 Step 7: Quality Reporting${NC}"
    echo -e "${BLUE}============================${NC}"
    
    echo -e "${YELLOW}Quality reports would be generated for actual executions${NC}"
    echo -e "${YELLOW}Example quality report structure:${NC}"
    
    cat << EOF
{
  "execution_id": "exec_demo_123",
  "workflow_id": "$WORKFLOW_ID",
  "overall_score": 0.95,
  "passed": true,
  "results": [
    {
      "check_id": "qc_visual_001",
      "type": "visual",
      "passed": true,
      "criteria": "No cracks, clean surfaces"
    },
    {
      "check_id": "qc_mechanical_001", 
      "type": "mechanical",
      "passed": true,
      "criteria": "Proper torque specifications"
    }
  ],
  "generated_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
    
    echo ""
    wait_with_message "Press Enter to continue..." 0
    read -p ""
    
    # Step 8: CLI Help and Features
    echo -e "${BLUE}🛠️  Step 8: CLI Features Overview${NC}"
    echo -e "${BLUE}=================================${NC}"
    
    demo_command \
        "Workflow CLI help and available commands" \
        "$WORKFLOW_CLI --help"
    
    demo_command \
        "Stage CLI help and available commands" \
        "$STAGE_CLI --help"
    
    demo_command \
        "Monitor CLI help and available commands" \
        "$MONITOR_CLI --help"
    
    # Demo conclusion
    echo -e "${GREEN}🎉 Demo Complete!${NC}"
    echo -e "${GREEN}=================${NC}"
    echo ""
    echo -e "${BLUE}📋 What we demonstrated:${NC}"
    echo -e "  ✅ Workflow creation and management"
    echo -e "  ✅ Workflow execution"
    echo -e "  ✅ Stage monitoring"
    echo -e "  ✅ Quality control integration"
    echo -e "  ✅ Real-time system monitoring"
    echo -e "  ✅ Event tracking and metrics"
    echo -e "  ✅ CLI interfaces for all operations"
    echo ""
    echo -e "${BLUE}🔧 Key Features:${NC}"
    echo -e "  • Microservice architecture with gRPC"
    echo -e "  • Event-driven communication with Watermill + Redis"
    echo -e "  • Protobuf for type-safe messaging"
    echo -e "  • Comprehensive CLI tools"
    echo -e "  • Real-time monitoring and dashboards"
    echo -e "  • Quality control and reporting"
    echo -e "  • Document management with access control"
    echo ""
    echo -e "${GREEN}🚀 The platform is ready for production use!${NC}"
    echo ""
    echo -e "${YELLOW}💡 Next steps:${NC}"
    echo -e "  • Explore the CLI tools with --help"
    echo -e "  • Create your own workflows"
    echo -e "  • Monitor the system with the dashboard"
    echo -e "  • Check the documentation in the README"
    echo ""
}

# Cleanup function
cleanup() {
    echo -e "${YELLOW}🧹 Cleaning up demo files...${NC}"
    rm -f /tmp/demo-workflow.json /tmp/stage-status.json
    echo -e "${GREEN}✅ Cleanup complete${NC}"
}

# Main execution
cd "$PROJECT_ROOT"

# Check if binaries exist
for binary in "$WORKFLOW_CLI" "$STAGE_CLI" "$MONITOR_CLI"; do
    if [ ! -f "$binary" ]; then
        echo -e "${RED}❌ Binary not found: $binary${NC}"
        echo -e "${YELLOW}Please run './scripts/start-platform.sh' first to build the binaries${NC}"
        exit 1
    fi
done

# Check services
check_services

# Run main demo
main_demo

# Cleanup
cleanup

echo -e "${BLUE}🎬 Demo finished! Thank you for watching!${NC}"

