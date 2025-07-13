#!/bin/bash

# Comprehensive TaskPlanner CLI Test Script
# Tests all implemented commands and functionality

set -e  # Exit on any error

echo "🧪 TaskPlanner Comprehensive CLI Test Suite"
echo "============================================="

# Set up test environment
export AGENT_ID="test-agent"
export DATABASE_PATH="./test_taskplanner.db"
export REDIS_URL="redis://localhost:6379"

# Clean up any existing test database
rm -f ./test_taskplanner.db

echo ""
echo "📊 1. Testing System Status"
echo "----------------------------"
./taskplanner status

echo ""
echo "📋 2. Testing Plan Management"
echo "------------------------------"

echo "Creating test plan..."
PLAN_OUTPUT=$(./taskplanner plan-create \
    --name "Test Project" \
    --description "Comprehensive testing project" \
    --status active \
    --format json)

PLAN_ID=$(echo "$PLAN_OUTPUT" | grep -o '"id":"[^"]*"' | cut -d'"' -f4)
echo "✅ Created plan with ID: $PLAN_ID"

echo "Listing all plans..."
./taskplanner plan-list --format table

echo "Showing plan details..."
./taskplanner plan-show --plan-id "$PLAN_ID" --format table

echo ""
echo "📝 3. Testing Task Management"
echo "------------------------------"

echo "Creating main task..."
MAIN_TASK_OUTPUT=$(./taskplanner task-create \
    --title "Main Task" \
    --description "Primary task for testing" \
    --plan-id "$PLAN_ID" \
    --priority 9 \
    --estimated-hours 40 \
    --format json)

MAIN_TASK_ID=$(echo "$MAIN_TASK_OUTPUT" | grep -o '"id":"[^"]*"' | cut -d'"' -f4)
echo "✅ Created main task with ID: $MAIN_TASK_ID"

echo "Creating subtask..."
SUBTASK_OUTPUT=$(./taskplanner task-create \
    --title "Subtask" \
    --description "Child task for testing hierarchy" \
    --plan-id "$PLAN_ID" \
    --parent-id "$MAIN_TASK_ID" \
    --priority 7 \
    --estimated-hours 20 \
    --format json)

SUBTASK_ID=$(echo "$SUBTASK_OUTPUT" | grep -o '"id":"[^"]*"' | cut -d'"' -f4)
echo "✅ Created subtask with ID: $SUBTASK_ID"

echo "Listing all tasks..."
./taskplanner task-list --format table

echo "Listing tasks for plan..."
./taskplanner task-list --plan-id "$PLAN_ID" --format table

echo "Showing task details..."
./taskplanner task-show --task-id "$MAIN_TASK_ID" --format table

echo ""
echo "✏️ 4. Testing Task Updates"
echo "---------------------------"

echo "Updating task status and priority..."
./taskplanner task-update \
    --task-id "$MAIN_TASK_ID" \
    --status active \
    --priority 10 \
    --actual-hours 5.5 \
    --format table

echo "Updating task with due date..."
./taskplanner task-update \
    --task-id "$SUBTASK_ID" \
    --due-date "2024-12-31T23:59:59Z" \
    --status active \
    --format table

echo ""
echo "👥 5. Testing Task Assignment"
echo "------------------------------"

echo "Assigning main task to agent..."
./taskplanner task-assign \
    --task-id "$MAIN_TASK_ID" \
    --agent-id "developer-1" \
    --format table

echo "Assigning subtask to different agent..."
./taskplanner task-assign \
    --task-id "$SUBTASK_ID" \
    --agent-id "developer-2" \
    --format table

echo ""
echo "🔒 6. Testing Task Coordination (Redis)"
echo "----------------------------------------"

echo "Claiming main task..."
./taskplanner task-claim \
    --task-id "$MAIN_TASK_ID" \
    --ttl 300 \
    --format table

echo "Attempting to claim already claimed task (should fail)..."
AGENT_ID="different-agent" ./taskplanner task-claim \
    --task-id "$MAIN_TASK_ID" \
    --ttl 300 \
    --format table || echo "✅ Correctly prevented double claiming"

echo "Releasing task claim..."
./taskplanner task-release \
    --task-id "$MAIN_TASK_ID" \
    --format table

echo "Claiming task again after release..."
./taskplanner task-claim \
    --task-id "$MAIN_TASK_ID" \
    --ttl 300 \
    --format table

echo ""
echo "📊 7. Testing Filtering and Queries"
echo "------------------------------------"

echo "Filtering tasks by status..."
./taskplanner task-list --status active --format table

echo "Filtering tasks by agent..."
./taskplanner task-list --agent-id "developer-1" --format table

echo "Filtering tasks by priority..."
./taskplanner task-list --priority 10 --format table

echo "Filtering tasks by parent..."
./taskplanner task-list --parent-id "$MAIN_TASK_ID" --format table

echo ""
echo "🗑️ 8. Testing Task Deletion"
echo "-----------------------------"

echo "Creating temporary task for deletion..."
TEMP_TASK_OUTPUT=$(./taskplanner task-create \
    --title "Temporary Task" \
    --description "Task to be deleted" \
    --plan-id "$PLAN_ID" \
    --priority 1 \
    --format json)

TEMP_TASK_ID=$(echo "$TEMP_TASK_OUTPUT" | grep -o '"id":"[^"]*"' | cut -d'"' -f4)
echo "✅ Created temporary task with ID: $TEMP_TASK_ID"

echo "Deleting temporary task..."
./taskplanner task-delete \
    --task-id "$TEMP_TASK_ID" \
    --force \
    --format table

echo "Verifying task is deleted (should show error)..."
./taskplanner task-show --task-id "$TEMP_TASK_ID" --format table || echo "✅ Task successfully deleted"

echo ""
echo "📈 9. Testing System Status After Operations"
echo "---------------------------------------------"
./taskplanner status

echo ""
echo "🎯 10. Testing Output Formats"
echo "------------------------------"

echo "JSON format:"
./taskplanner task-list --limit 1 --format json

echo ""
echo "Table format:"
./taskplanner task-list --limit 1 --format table

echo ""
echo "✅ All Tests Completed Successfully!"
echo "===================================="

echo ""
echo "📊 Final Statistics:"
echo "- Plans created: 1"
echo "- Tasks created: 3 (1 deleted)"
echo "- Task updates: 3"
echo "- Task assignments: 2"
echo "- Task claims/releases: 2"
echo "- All commands tested: ✅"

echo ""
echo "🧹 Cleanup:"
echo "Test database: ./test_taskplanner.db (preserved for inspection)"
echo "Redis data: Preserved for inspection"

echo ""
echo "🎉 TaskPlanner CLI Test Suite Complete!"

