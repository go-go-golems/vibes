#!/bin/bash

# i3 Window Manager Go Program Demonstration Script

export DISPLAY=:99
export PATH=$PATH:/usr/local/go/bin

echo "=== i3 Window Manager Go Program Demo ==="
echo

echo "1. Checking i3 status..."
ps aux | grep -E "(i3|xterm)" | grep -v grep
echo

echo "2. Current i3 workspaces:"
i3-msg -t get_workspaces | jq '.[] | {name: .name, focused: .focused, visible: .visible}'
echo

echo "3. Current i3 tree (windows):"
i3-msg -t get_tree | jq '.nodes[].nodes[] | select(.type=="workspace") | {name: .name, windows: [.nodes[] | select(.window != null) | {id: .window, name: .name, class: .window_properties.class}]}'
echo

echo "4. Testing i3-msg commands..."
echo "Switching to workspace 2..."
i3-msg "workspace 2"
sleep 1

echo "Switching back to workspace 1..."
i3-msg "workspace 1"
sleep 1

echo "5. Running the Go program (press 'q' to quit)..."
echo "Features to test:"
echo "- Use arrow keys or j/k to navigate"
echo "- Press Tab to switch between workspaces and windows view"
echo "- Press Enter to focus selected workspace/window"
echo "- Press 1-9,0 for quick workspace switching"
echo "- Press 'r' to refresh"
echo "- Press 'q' to quit"
echo

./i3-window-manager

