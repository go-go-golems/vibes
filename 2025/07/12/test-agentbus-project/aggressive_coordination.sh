#!/bin/bash

export AGENT_ID=orchestrator-boss
export PROJECT_PREFIX=pelican-farm

echo "🚨 STARTING 5-MINUTE AGGRESSIVE COORDINATION CYCLE 🚨"

# Track start time
start_time=$(date +%s)
end_time=$((start_time + 300)) # 5 minutes

cycle=1

while [ $(date +%s) -lt $end_time ]; do
    current_time=$(date +%s)
    remaining=$((end_time - current_time))
    minutes=$((remaining / 60))
    seconds=$((remaining % 60))
    
    echo "⏰ CYCLE $cycle - Time remaining: ${minutes}m ${seconds}s"
    
    # Aggressive status demands
    agentbus speak --topic coordination --msg "⚡ CYCLE $cycle - STATUS REPORT DEMANDED! All agents show IMMEDIATE progress or get new assignments! Time remaining: ${minutes}m ${seconds}s"
    
    # Monitor recent activity
    echo "📡 Monitoring agent activity..."
    agentbus overhear --topic all --limit 5
    
    # Issue new tasks based on time remaining
    if [ $remaining -gt 240 ]; then
        agentbus speak --topic work-assignment --msg "🎯 NEW TASKS AVAILABLE: 1) Walrus-wing API optimization 2) Frontend reactive updates 3) Performance monitoring 4) Security hardening - CLAIM YOUR TASK!"
    elif [ $remaining -gt 120 ]; then
        agentbus speak --topic work-assignment --msg "⚠️ FINAL SPRINT! Focus on: testing, documentation, deployment optimization. NO NEW FEATURES - POLISH WHAT EXISTS!"
    else
        agentbus speak --topic coordination --msg "🔥 FINAL MINUTE! Commit all work, update documentation, prepare deployment status! FINAL PUSH!"
    fi
    
    # Wait 60 seconds before next cycle
    sleep 60
    cycle=$((cycle + 1))
done

agentbus speak --topic coordination --msg "🏁 5-MINUTE COORDINATION COMPLETE! All agents report final status and deliverables NOW! Summary time!"

echo "✅ AGGRESSIVE COORDINATION CYCLE COMPLETE!"
