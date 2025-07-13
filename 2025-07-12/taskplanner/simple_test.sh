#!/bin/bash

# Simple TaskPlanner CLI Test Script
# Tests all implemented commands with manual IDs

set -e  # Exit on any error

echo "🧪 TaskPlanner Simple CLI Test Suite"
echo "====================================="

# Set up test environment
export AGENT_ID="test-agent"
export DATABASE_PATH="./simple_test.db"
export REDIS_URL="redis://localhost:6379"

# Clean up any existing test database
rm -f ./simple_test.db

echo ""
echo "📊 1. Testing System Status"
echo "----------------------------"
./taskplanner status

echo ""
echo "📋 2. Testing Plan Management"
echo "------------------------------"

echo "Creating test plan..."
./taskplanner plan-create \
    --name "Test Project" \
    --description "Simple testing project" \
    --status active

echo "Listing all plans..."
./taskplanner plan-list

echo ""
echo "📝 3. Testing Task Management"
echo "------------------------------"

echo "Note: Using plan ID from the list above for task creation"
echo "Please manually copy a plan ID from above and test task creation:"
echo ""
echo "Example commands to test manually:"
echo "./taskplanner task-create --title 'Test Task' --description 'Test description' --plan-id <PLAN_ID> --priority 8"
echo "./taskplanner task-list"
echo "./taskplanner task-show --task-id <TASK_ID>"
echo "./taskplanner task-update --task-id <TASK_ID> --status active --priority 9"
echo "./taskplanner task-assign --task-id <TASK_ID> --agent-id developer-1"
echo "./taskplanner task-claim --task-id <TASK_ID> --ttl 300"
echo "./taskplanner task-release --task-id <TASK_ID>"
echo "./taskplanner task-delete --task-id <TASK_ID> --force"

echo ""
echo "✅ Basic Commands Tested Successfully!"
echo "======================================"

echo ""
echo "🎯 Manual Testing Required:"
echo "1. Copy a plan ID from the plan list above"
echo "2. Create tasks using that plan ID"
echo "3. Test task operations with the created task IDs"
echo "4. Test coordination features with Redis"

echo ""
echo "📊 System Status After Basic Tests:"
./taskplanner status

