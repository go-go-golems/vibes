#!/bin/bash

# Comprehensive demo script for turn-inspector CLI
# This script demonstrates all features with diverse scenarios

set -e

CLI="./turn-inspector"

echo "========================================"
echo "    Turn Inspector CLI Demonstration"
echo "========================================"
echo

# Check if CLI exists
if [ ! -f "$CLI" ]; then
    echo "Error: turn-inspector binary not found. Please run 'go build' first."
    exit 1
fi

echo "Step 1: Show initial database statistics"
echo "----------------------------------------"
$CLI stats
echo

echo "Step 2: Create diverse test scenarios"
echo "-------------------------------------"
./test-scenarios.sh
echo

echo "Step 3: Show updated database statistics"
echo "----------------------------------------"
$CLI stats
echo

echo "Step 4: List all turns"
echo "----------------------"
$CLI list turns
echo

echo "Step 5: Show detailed statistics"
echo "--------------------------------"
$CLI stats --detailed
echo

echo "Step 6: Query turns by metadata"
echo "-------------------------------"
echo "Finding turns with session metadata:"
$CLI query turns --metadata-key session
echo

echo "Finding turns with user tier 'premium':"
$CLI query turns --metadata-key tier --metadata-value premium
echo

echo "Step 7: Query turns by content"
echo "------------------------------"
echo "Finding turns containing 'weather':"
$CLI query turns --text weather
echo

echo "Finding turns with tool_call blocks:"
$CLI query turns --block-kind tool_call
echo

echo "Step 8: Show detailed turn information"
echo "--------------------------------------"
echo "Showing turn 2 (weather conversation):"
$CLI show turn --id 2
echo

echo "Step 9: Show blocks for a specific turn"
echo "---------------------------------------"
echo "Showing blocks for turn 3 (travel conversation):"
$CLI show blocks --turn-id 3
echo

echo "Step 10: Show turn with JSON output"
echo "-----------------------------------"
echo "Showing turn 1 in JSON format:"
$CLI show turn --id 1 --json
echo

echo "Step 11: Demonstrate query capabilities"
echo "---------------------------------------"
echo "All turns with error-related metadata:"
$CLI query turns --metadata-key error_handled
echo

echo "All turns with system blocks:"
$CLI query turns --block-kind system
echo

echo "========================================"
echo "    Demonstration Complete!"
echo "========================================"
echo
echo "Summary of created data:"
echo "- 6 diverse conversation turns"
echo "- Multiple block types: user, llm_text, tool_call, tool_use, system"
echo "- Rich metadata at turn and block levels"
echo "- Various conversation scenarios: simple chat, tool usage, error handling"
echo
echo "Available commands demonstrated:"
echo "- create turn: Create new conversation turns"
echo "- list turns: List all turns with summary"
echo "- show turn: Show detailed turn information"
echo "- show blocks: Show blocks for a specific turn"
echo "- query turns: Search turns by metadata and content"
echo "- stats: Show database statistics"
echo "- delete: Remove turns (not demonstrated to preserve data)"
echo

