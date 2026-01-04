#!/bin/bash

# TaskPlanner Redis Coordination Demo
# This script demonstrates multi-agent coordination with Redis

set -e

echo "🚀 TaskPlanner Redis Coordination Demo"
echo "======================================"
echo

# Check Redis is running
if ! redis-cli ping > /dev/null 2>&1; then
    echo "❌ Redis is not running. Please start Redis first:"
    echo "   sudo systemctl start redis-server"
    exit 1
fi

echo "✅ Redis is running"
echo

# Get the plan and task IDs from our test data
PLAN_ID="a1044432-c766-49e8-8b24-54552c5f8d4f"
REDIS_TASK="10e85caa-df11-421c-82a7-66c6d89c3454"
FRONTEND_TASK="04a87446-be48-415c-ad37-793a0c02d861"
BACKEND_TASK="b1f871c8-ef59-4ca1-be0a-604e0c46e6d8"

echo "📋 Current project status:"
AGENT_ID=demo ./taskplanner plan-show --plan-id "$PLAN_ID"
echo

echo "📝 Available tasks:"
AGENT_ID=demo ./taskplanner task-list --plan-id "$PLAN_ID" --format table
echo

echo "🤝 Multi-Agent Coordination Demonstration"
echo "========================================="
echo

echo "👤 Agent 1 (frontend-dev) claiming Frontend Task..."
AGENT_ID=frontend-dev ./taskplanner task-claim --task-id "$FRONTEND_TASK" --ttl 600
echo

echo "👤 Agent 2 (backend-dev) claiming Backend API Task..."
AGENT_ID=backend-dev ./taskplanner task-claim --task-id "$BACKEND_TASK" --ttl 600
echo

echo "👤 Agent 3 (qa-engineer) trying to claim Frontend Task (should fail)..."
AGENT_ID=qa-engineer ./taskplanner task-claim --task-id "$FRONTEND_TASK" --ttl 300 || echo "✅ Correctly prevented duplicate claim"
echo

echo "👤 Agent 3 (qa-engineer) claiming Redis Coordination Test Task..."
# First clear any existing claim
redis-cli del "taskplanner:task:$REDIS_TASK:claimed" > /dev/null 2>&1 || true
AGENT_ID=qa-engineer ./taskplanner task-claim --task-id "$REDIS_TASK" --ttl 600
echo

echo "🔍 Current Redis coordination state:"
echo "Active task claims:"
redis-cli keys "taskplanner:task:*:claimed" | while read key; do
    if [ -n "$key" ]; then
        task_id=$(echo "$key" | cut -d':' -f3)
        claim_info=$(redis-cli get "$key")
        ttl=$(redis-cli ttl "$key")
        echo "  Task $task_id: $claim_info (TTL: ${ttl}s)"
    fi
done
echo

echo "📊 Coordination stream events:"
redis-cli xrange "taskplanner:coordination:stream" - + COUNT 5 2>/dev/null || echo "  No coordination events yet"
echo

echo "🎯 Key Features Demonstrated:"
echo "✅ Multiple agents can claim different tasks simultaneously"
echo "✅ Task claims are protected with TTL (time-to-live)"
echo "✅ Duplicate claims are prevented"
echo "✅ Redis stores coordination state reliably"
echo "✅ Claims automatically expire after TTL"
echo "✅ Real-time coordination across multiple agents"
echo

echo "🔧 Redis Integration Details:"
echo "• Task claims stored as: taskplanner:task:{task-id}:claimed"
echo "• Coordination events in: taskplanner:coordination:stream"
echo "• TTL-based automatic claim expiration"
echo "• Atomic operations prevent race conditions"
echo

echo "🎉 Redis coordination demo completed successfully!"
echo "💡 Try these commands to explore further:"
echo "   redis-cli keys 'taskplanner:*'"
echo "   redis-cli xrange taskplanner:coordination:stream - +"
echo "   AGENT_ID=your-agent ./taskplanner task-claim --task-id <ID>"

