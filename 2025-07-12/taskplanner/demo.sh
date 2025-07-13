#!/bin/bash

# TaskPlanner Demo Script
# This script demonstrates the key features of the TaskPlanner application

set -e

echo "🚀 TaskPlanner Demo - Dynamic Hierarchical Task Planning"
echo "======================================================="
echo

# Set agent ID for demo
export AGENT_ID="demo-agent"

echo "📋 1. Creating a new project plan..."
PLAN_OUTPUT=$(./taskplanner plan-create \
    --name "E-commerce Platform" \
    --description "Build a complete e-commerce platform with modern features" \
    --status active \
    --format table)

# Extract plan ID from table output (more reliable than JSON parsing)
PLAN_ID=$(echo "$PLAN_OUTPUT" | grep -E '^[|].*[|].*[|].*[|].*[|].*[|].*[|]' | tail -1 | cut -d'|' -f2 | tr -d ' ')
echo "✅ Created plan: $PLAN_ID"
echo

echo "📝 2. Creating hierarchical tasks..."

# Create main tasks
echo "Creating main development phases..."

FRONTEND_TASK=$(./taskplanner task-create \
    --title "Frontend Development" \
    --description "Build the user interface and user experience" \
    --plan-id "$PLAN_ID" \
    --priority 9 \
    --estimated-hours 120 \
    --format table)
FRONTEND_ID=$(echo "$FRONTEND_TASK" | grep -E '^[|].*[|].*[|].*[|].*[|].*[|].*[|]' | tail -1 | cut -d'|' -f2 | tr -d ' ')

BACKEND_TASK=$(./taskplanner task-create \
    --title "Backend Development" \
    --description "Build APIs, database, and server infrastructure" \
    --plan-id "$PLAN_ID" \
    --priority 10 \
    --estimated-hours 160 \
    --format table)
BACKEND_ID=$(echo "$BACKEND_TASK" | grep -E '^[|].*[|].*[|].*[|].*[|].*[|].*[|]' | tail -1 | cut -d'|' -f2 | tr -d ' ')

TESTING_TASK=$(./taskplanner task-create \
    --title "Testing & QA" \
    --description "Comprehensive testing and quality assurance" \
    --plan-id "$PLAN_ID" \
    --priority 8 \
    --estimated-hours 80 \
    --format table)
TESTING_ID=$(echo "$TESTING_TASK" | grep -E '^[|].*[|].*[|].*[|].*[|].*[|].*[|]' | tail -1 | cut -d'|' -f2 | tr -d ' ')

echo "✅ Created main tasks: Frontend, Backend, Testing"
echo

# Create subtasks
echo "Creating detailed subtasks..."

./taskplanner task-create \
    --title "Design System" \
    --description "Create reusable UI components and design tokens" \
    --plan-id "$PLAN_ID" \
    --parent-id "$FRONTEND_ID" \
    --priority 9 \
    --estimated-hours 40 > /dev/null

./taskplanner task-create \
    --title "Product Catalog" \
    --description "Build product listing and detail pages" \
    --plan-id "$PLAN_ID" \
    --parent-id "$FRONTEND_ID" \
    --priority 8 \
    --estimated-hours 50 > /dev/null

./taskplanner task-create \
    --title "Shopping Cart" \
    --description "Implement shopping cart and checkout flow" \
    --plan-id "$PLAN_ID" \
    --parent-id "$FRONTEND_ID" \
    --priority 10 \
    --estimated-hours 30 > /dev/null

./taskplanner task-create \
    --title "Database Schema" \
    --description "Design and implement database structure" \
    --plan-id "$PLAN_ID" \
    --parent-id "$BACKEND_ID" \
    --priority 10 \
    --estimated-hours 30 > /dev/null

./taskplanner task-create \
    --title "API Development" \
    --description "Build REST APIs for all functionality" \
    --plan-id "$PLAN_ID" \
    --parent-id "$BACKEND_ID" \
    --priority 9 \
    --estimated-hours 80 > /dev/null

./taskplanner task-create \
    --title "Payment Integration" \
    --description "Integrate payment processing systems" \
    --plan-id "$PLAN_ID" \
    --parent-id "$BACKEND_ID" \
    --priority 8 \
    --estimated-hours 50 > /dev/null

echo "✅ Created detailed subtasks for each main task"
echo

echo "📊 3. Viewing project overview..."
echo "Plans:"
./taskplanner plan-list --format table
echo

echo "Tasks (showing first 10):"
./taskplanner task-list --plan-id "$PLAN_ID" --format table | head -15
echo

echo "🎯 4. Demonstrating task coordination..."
echo "Note: Redis coordination requires a running Redis server"
echo "The application gracefully handles Redis unavailability"
echo

echo "📈 5. Key Features Demonstrated:"
echo "✅ Hierarchical task organization (parent-child relationships)"
echo "✅ Priority-based task management"
echo "✅ Effort estimation and tracking"
echo "✅ Multi-format output (JSON, table)"
echo "✅ Agent-based task assignment"
echo "✅ Persistent SQLite storage"
echo "✅ Redis coordination (when available)"
echo "✅ CLI-driven interface with glazed framework"
echo

echo "🔧 6. Available Commands:"
./taskplanner --help | grep -A 20 "Available Commands:"
echo

echo "🎉 Demo completed! The TaskPlanner application is ready for use."
echo "💡 Try running commands manually to explore more features:"
echo "   ./taskplanner plan-show --plan-id $PLAN_ID"
echo "   ./taskplanner task-list --plan-id $PLAN_ID --status planned"
echo "   ./taskplanner --help"

