#!/bin/bash

# Test script for tmux integration in Goja REPL
echo "Testing tmux integration for Goja REPL"

# Build the REPL
cd ~/goja-repl
go build -o repl cmd/repl/main.go

# Create a test script that will run inside the REPL
cat > test_commands.txt << EOF
2 + 2
function hello() { return "Hello from tmux!"; }
hello()
/tmux start
/quit
EOF

# Run the REPL with the test commands
echo "Starting REPL with test commands..."
cat test_commands.txt | ./repl

# Check if tmux sessions exist
echo "Checking for tmux sessions..."
tmux ls

echo "Test completed."
