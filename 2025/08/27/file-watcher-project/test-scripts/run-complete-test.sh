#!/bin/bash

# Complete test runner for file watcher system
# This script starts the file watcher, runs tests, and displays results

PROJECT_DIR="/home/ubuntu/file-watcher-project"
SERVER_DIR="$PROJECT_DIR/server"
WATCH_DIR="$PROJECT_DIR/watched-dir"
TEST_SCRIPT="$PROJECT_DIR/test-scripts/test-file-operations.sh"

echo "=== File Watcher Complete Test Runner ==="
echo "Project directory: $PROJECT_DIR"
echo "Watch directory: $WATCH_DIR"
echo

# Check if server binary exists
if [ ! -f "$SERVER_DIR/file-watcher" ]; then
    echo "Building file watcher server..."
    cd "$SERVER_DIR"
    go build -o file-watcher main.go
    if [ $? -ne 0 ]; then
        echo "Error: Failed to build server"
        exit 1
    fi
    echo "Server built successfully"
fi

# Clean up any existing files in watch directory (except .git)
echo "Cleaning up watch directory..."
cd "$WATCH_DIR"
find . -type f ! -path './.git/*' ! -name '.git*' -delete 2>/dev/null || true

# Create initial commit if repository is empty
if [ -z "$(git log --oneline 2>/dev/null)" ]; then
    echo "Creating initial commit..."
    echo "# File Watcher Test Repository" > README.md
    git add README.md
    git commit -m "Initial commit: File watcher test repository"
fi

echo
echo "Starting file watcher server in background..."

# Start the file watcher in background using tmux
tmux new-session -d -s file-watcher -c "$SERVER_DIR" "./file-watcher $WATCH_DIR"

# Wait a moment for server to start
sleep 2

# Check if server is running
if ! tmux list-sessions | grep -q "file-watcher"; then
    echo "Error: Failed to start file watcher server"
    exit 1
fi

echo "File watcher server started successfully"
echo "Server output can be viewed with: tmux attach -t file-watcher"
echo

# Run the test script
echo "Running file operation tests..."
cd "$PROJECT_DIR/test-scripts"
./test-file-operations.sh

echo
echo "Waiting for final commits to be processed..."
sleep 3

echo
echo "=== Git Log Results ==="
cd "$WATCH_DIR"
echo "Commit history (one line per commit):"
git log --oneline

echo
echo "=== Detailed Git Log with Changes ==="
echo "Showing detailed log with patches:"
git log -p --reverse

echo
echo "=== Test Summary ==="
COMMIT_COUNT=$(git rev-list --count HEAD)
echo "Total commits created: $COMMIT_COUNT"
echo "Files currently in repository:"
ls -la | grep -v "^d" | grep -v "^total"

echo
echo "=== Stopping File Watcher ==="
tmux kill-session -t file-watcher 2>/dev/null || true
echo "File watcher server stopped"

echo
echo "Test completed successfully!"
echo "To restart the file watcher manually, run:"
echo "cd $SERVER_DIR && ./file-watcher $WATCH_DIR"

