#!/bin/bash

# Function to capture tmux pane and save as text
capture_pane() {
    local session=$1
    local window=$2
    local pane=$3
    local filename=$4
    
    echo "Capturing $session:$window.$pane to $filename"
    tmux capture-pane -t "$session:$window.$pane" -p > "$filename"
}

# Create screenshots directory
mkdir -p screenshots

# Capture all panes from the demo session
echo "Taking screenshots of tmux dashboard..."

# System tab (window 0)
capture_pane "dashboard-demo" "0" "0" "screenshots/system_overview.txt"
capture_pane "dashboard-demo" "0" "1" "screenshots/system_processes.txt"
capture_pane "dashboard-demo" "0" "2" "screenshots/system_network.txt"
capture_pane "dashboard-demo" "0" "3" "screenshots/system_disk.txt"

# Monitoring tab (window 1)
capture_pane "dashboard-demo" "1" "0" "screenshots/monitoring_cpu_mem.txt"
capture_pane "dashboard-demo" "1" "1" "screenshots/monitoring_load.txt"

# Logs tab (window 2)
capture_pane "dashboard-demo" "2" "0" "screenshots/logs_app.txt"
capture_pane "dashboard-demo" "2" "1" "screenshots/logs_system.txt"

# Development tab (window 3)
capture_pane "dashboard-demo" "3" "0" "screenshots/dev_status.txt"
capture_pane "dashboard-demo" "3" "1" "screenshots/dev_build.txt"

echo "Screenshots saved to screenshots/ directory"
ls -la screenshots/
