#!/bin/bash

# Test script for file watcher using sleep, cat, and sed
# This script performs various file operations to test the git integration

WATCH_DIR="../watched-dir"
TEST_FILE1="$WATCH_DIR/test1.txt"
TEST_FILE2="$WATCH_DIR/test2.txt"
TEST_FILE3="$WATCH_DIR/config.conf"

echo "=== File Watcher Test Script ==="
echo "Testing directory: $WATCH_DIR"
echo "Starting file operations in 3 seconds..."
sleep 3

echo
echo "1. Creating initial files..."

# Create first test file
cat > "$TEST_FILE1" << EOF
This is the initial content of test1.txt
Line 2: Hello World
Line 3: Testing file watcher
EOF
echo "Created test1.txt"
sleep 2

# Create second test file using echo and cat
echo "Initial content for test2.txt" > "$TEST_FILE2"
cat >> "$TEST_FILE2" << EOF
Additional line 1
Additional line 2
EOF
echo "Created test2.txt"
sleep 2

# Create a config file
cat > "$TEST_FILE3" << EOF
# Configuration file
server_port=8080
debug_mode=true
log_level=info
EOF
echo "Created config.conf"
sleep 3

echo
echo "2. Modifying files using sed..."

# Modify test1.txt using sed
sed -i 's/Hello World/Hello Universe/g' "$TEST_FILE1"
echo "Modified test1.txt: Changed 'Hello World' to 'Hello Universe'"
sleep 2

# Add a new line to test1.txt using sed
sed -i '$a Line 4: Added by sed command' "$TEST_FILE1"
echo "Modified test1.txt: Added new line"
sleep 2

# Modify config file
sed -i 's/debug_mode=true/debug_mode=false/g' "$TEST_FILE3"
echo "Modified config.conf: Changed debug_mode to false"
sleep 2

# Change port number
sed -i 's/server_port=8080/server_port=9090/g' "$TEST_FILE3"
echo "Modified config.conf: Changed port to 9090"
sleep 3

echo
echo "3. Appending content using cat..."

# Append to test2.txt using cat
cat >> "$TEST_FILE2" << EOF
Line added by cat append
Another line for testing
Final line of test2.txt
EOF
echo "Appended content to test2.txt"
sleep 2

echo
echo "4. Creating and modifying additional files..."

# Create a temporary file and modify it multiple times
TEMP_FILE="$WATCH_DIR/temp.log"
echo "Initial log entry" > "$TEMP_FILE"
sleep 1

echo "Second log entry" >> "$TEMP_FILE"
sleep 1

sed -i '1s/Initial/First/' "$TEMP_FILE"
echo "Modified temp.log using sed"
sleep 2

echo
echo "5. Demonstrating file deletion..."

# Create a file to be deleted
DELETE_FILE="$WATCH_DIR/to_delete.txt"
echo "This file will be deleted" > "$DELETE_FILE"
echo "Created to_delete.txt"
sleep 2

# Delete the file
rm "$DELETE_FILE"
echo "Deleted to_delete.txt"
sleep 2

echo
echo "6. Final modifications..."

# Make final changes to demonstrate continuous monitoring
sed -i '$a Final modification timestamp: '$(date) "$TEST_FILE1"
echo "Added timestamp to test1.txt"
sleep 1

# Update log level in config
sed -i 's/log_level=info/log_level=debug/g' "$TEST_FILE3"
echo "Updated log level in config.conf"
sleep 2

echo
echo "=== Test completed ==="
echo "All file operations have been performed."
echo "Check the git log to see the commits created by the file watcher."
echo
echo "To view the git log, run:"
echo "cd $WATCH_DIR && git log --oneline"
echo "For detailed changes, run:"
echo "cd $WATCH_DIR && git log -p"

