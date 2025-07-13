#!/bin/bash

# TaskPlanner Simple Demo Script
# This script demonstrates the key features of the TaskPlanner application

set -e

echo "🚀 TaskPlanner Demo - Dynamic Hierarchical Task Planning"
echo "======================================================="
echo

# Set agent ID for demo
export AGENT_ID="demo-agent"

echo "📋 1. Creating a new project plan..."
./taskplanner plan-create \
    --name "Mobile App Development" \
    --description "Build a cross-platform mobile application" \
    --status active
echo

echo "📝 2. Listing all plans..."
./taskplanner plan-list
echo

echo "📝 3. Creating some tasks (you'll need to copy plan ID from above)..."
echo "Example commands to run manually:"
echo "  ./taskplanner task-create --title 'UI Design' --description 'Design app interface' --plan-id <PLAN_ID> --priority 8"
echo "  ./taskplanner task-create --title 'Backend API' --description 'Build REST API' --plan-id <PLAN_ID> --priority 9"
echo "  ./taskplanner task-create --title 'Testing' --description 'QA and testing' --plan-id <PLAN_ID> --priority 7"
echo

echo "📊 4. Available commands:"
./taskplanner --help | grep -A 15 "Available Commands:"
echo

echo "🎯 5. Key Features:"
echo "✅ Hierarchical task organization (parent-child relationships)"
echo "✅ Priority-based task management (1-10 scale)"
echo "✅ Effort estimation and tracking (estimated hours)"
echo "✅ Multi-format output (JSON, table, text)"
echo "✅ Agent-based task assignment and coordination"
echo "✅ Persistent SQLite storage"
echo "✅ Redis coordination for real-time updates (when Redis is available)"
echo "✅ CLI-driven interface using glazed framework"
echo

echo "🔧 6. Architecture:"
echo "• SQLite database for persistent storage"
echo "• Redis for real-time coordination and task claiming"
echo "• Hierarchical task relationships with parent-child structure"
echo "• Task dependencies and status tracking"
echo "• Agent registration and task assignment"
echo "• Comprehensive audit trail with task history"
echo

echo "🎉 Demo completed! Try the commands manually to explore more features."

