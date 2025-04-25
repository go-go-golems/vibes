#!/bin/bash

# Test script for Go File Analyzer
# Tests the application with various Go codebases

set -e

# Function to display usage information
usage() {
    echo "Usage: $0 [options]"
    echo "Options:"
    echo "  -a, --analyzer    Path to the recursive analyzer executable (default: ./recursive_analyzer/recursive_analyzer)"
    echo "  -f, --formatter   Path to the JSON formatter executable (default: ./json_formatter/json_formatter)"
    echo "  -w, --webserver   Path to the webserver executable (default: ./webserver/webserver)"
    echo "  -h, --help        Display this help message"
    exit 1
}

# Default paths
ANALYZER="./recursive_analyzer/recursive_analyzer"
FORMATTER="./json_formatter/json_formatter"
WEBSERVER="./webserver/webserver"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -a|--analyzer)
            ANALYZER="$2"
            shift 2
            ;;
        -f|--formatter)
            FORMATTER="$2"
            shift 2
            ;;
        -w|--webserver)
            WEBSERVER="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done

# Check if executables exist
if [ ! -f "$ANALYZER" ]; then
    echo "Error: Analyzer executable not found at $ANALYZER"
    exit 1
fi

if [ ! -f "$FORMATTER" ]; then
    echo "Error: Formatter executable not found at $FORMATTER"
    exit 1
fi

if [ ! -f "$WEBSERVER" ]; then
    echo "Error: Webserver executable not found at $WEBSERVER"
    exit 1
fi

# Create test directory
TEST_DIR=$(mktemp -d)
echo "Created test directory: $TEST_DIR"

# Function to cleanup on exit
cleanup() {
    echo "Cleaning up..."
    if [ -n "$SERVER_PID" ]; then
        echo "Stopping server (PID: $SERVER_PID)..."
        kill $SERVER_PID || true
    fi
    echo "Removing test directory: $TEST_DIR"
    rm -rf "$TEST_DIR"
}

# Register cleanup function
trap cleanup EXIT

# Function to test a codebase
test_codebase() {
    local name=$1
    local repo=$2
    local dir=$3
    local duration=$4
    
    echo "=== Testing $name ==="
    echo "Repository: $repo"
    echo "Directory: $dir"
    echo "Timeout: ${duration}s"
    
    # Clone repository
    echo "Cloning repository..."
    if ! git clone --depth 1 "$repo" "$TEST_DIR/$dir" &> "$TEST_DIR/${dir}_clone.log"; then
        echo "Error: Failed to clone repository"
        return 1
    fi
    
    # Run analyzer
    echo "Running analyzer..."
    if ! timeout ${duration}s "$ANALYZER" -path="$TEST_DIR/$dir" -output="$TEST_DIR/${dir}_analysis.json" -verbose &> "$TEST_DIR/${dir}_analyzer.log"; then
        echo "Error: Analyzer failed"
        return 1
    fi
    
    # Run formatter
    echo "Running formatter..."
    if ! "$FORMATTER" "$TEST_DIR/${dir}_analysis.json" "$TEST_DIR/${dir}_enhanced.json" &> "$TEST_DIR/${dir}_formatter.log"; then
        echo "Error: Formatter failed"
        return 1
    fi
    
    # Check output files
    if [ ! -f "$TEST_DIR/${dir}_analysis.json" ]; then
        echo "Error: Analysis output file not found"
        return 1
    fi
    
    if [ ! -f "$TEST_DIR/${dir}_enhanced.json" ]; then
        echo "Error: Enhanced output file not found"
        return 1
    fi
    
    # Display stats
    echo "Stats:"
    grep "Files analyzed:" "$TEST_DIR/${dir}_analyzer.log" || echo "Files analyzed: Unknown"
    grep "Functions found:" "$TEST_DIR/${dir}_analyzer.log" || echo "Functions found: Unknown"
    grep "Methods found:" "$TEST_DIR/${dir}_analyzer.log" || echo "Methods found: Unknown"
    grep "Analysis time:" "$TEST_DIR/${dir}_analyzer.log" || echo "Analysis time: Unknown"
    
    echo "Test passed!"
    return 0
}

# Start the web server
echo "Starting web server..."
"$WEBSERVER" -port=8001 -analyzer="$ANALYZER" -formatter="$FORMATTER" &> "$TEST_DIR/webserver.log" &
SERVER_PID=$!
echo "Web server started with PID: $SERVER_PID"

# Wait for server to start
echo "Waiting for server to start..."
sleep 2

# Check if server is running
if ! ps -p $SERVER_PID > /dev/null; then
    echo "Error: Web server failed to start"
    exit 1
fi

# Test small codebase - sample calculator project
mkdir -p "$TEST_DIR/sample/calculator"
mkdir -p "$TEST_DIR/sample/main"

# Create calculator package
cat > "$TEST_DIR/sample/calculator/calculator.go" << 'EOF'
package calculator

// Add adds two numbers and returns the result
func Add(a, b int) int {
    return a + b
}

// Subtract subtracts b from a and returns the result
func Subtract(a, b int) int {
    return a - b
}

// Multiply multiplies two numbers and returns the result
func Multiply(a, b int) int {
    return a * b
}

// Divide divides a by b and returns the result
// If b is 0, it returns 0 to avoid division by zero
func Divide(a, b int) int {
    if b == 0 {
        return 0
    }
    return a / b
}

// Calculator represents a calculator with memory
type Calculator struct {
    memory int
}

// NewCalculator creates a new Calculator with memory set to 0
func NewCalculator() *Calculator {
    return &Calculator{memory: 0}
}

// Add adds the given number to the memory
func (c *Calculator) Add(num int) {
    c.memory += num
}

// Subtract subtracts the given number from the memory
func (c *Calculator) Subtract(num int) {
    c.memory -= num
}

// GetMemory returns the current memory value
func (c *Calculator) GetMemory() int {
    return c.memory
}

// Clear sets the memory to 0
func (c *Calculator) Clear() {
    c.memory = 0
}
EOF

# Create main package
cat > "$TEST_DIR/sample/main/main.go" << 'EOF'
package main

import (
    "fmt"
)

func main() {
    // Use the calculator package functions
    fmt.Println("5 + 3 =", Add(5, 3))
    fmt.Println("5 - 3 =", Subtract(5, 3))
    fmt.Println("5 * 3 =", Multiply(5, 3))
    fmt.Println("6 / 3 =", Divide(6, 3))
}

// Add adds two numbers and returns the result
func Add(a, b int) int {
    return a + b
}

// Subtract subtracts b from a and returns the result
func Subtract(a, b int) int {
    return a - b
}

// Multiply multiplies two numbers and returns the result
func Multiply(a, b int) int {
    return a * b
}

// Divide divides a by b and returns the result
func Divide(a, b int) int {
    if b == 0 {
        return 0
    }
    return a / b
}
EOF

# Create go.mod file
cat > "$TEST_DIR/sample/go.mod" << 'EOF'
module sample

go 1.18
EOF

echo "=== Testing sample project ==="
echo "Running analyzer..."
"$ANALYZER" -path="$TEST_DIR/sample" -output="$TEST_DIR/sample_analysis.json" -verbose &> "$TEST_DIR/sample_analyzer.log"

echo "Running formatter..."
"$FORMATTER" "$TEST_DIR/sample_analysis.json" "$TEST_DIR/sample_enhanced.json" &> "$TEST_DIR/sample_formatter.log"

echo "Stats:"
grep "Files analyzed:" "$TEST_DIR/sample_analyzer.log" || echo "Files analyzed: Unknown"
grep "Functions found:" "$TEST_DIR/sample_analyzer.log" || echo "Functions found: Unknown"
grep "Methods found:" "$TEST_DIR/sample_analyzer.log" || echo "Methods found: Unknown"
grep "Analysis time:" "$TEST_DIR/sample_analyzer.log" || echo "Analysis time: Unknown"

# Test a variety of Go codebases
test_codebase "Cobra CLI Framework" "https://github.com/spf13/cobra.git" "cobra" 60
test_codebase "Gorilla Mux Router" "https://github.com/gorilla/mux.git" "mux" 30
test_codebase "Viper Configuration" "https://github.com/spf13/viper.git" "viper" 60

# Test web server API
echo "=== Testing web server API ==="
echo "Testing /api/analyze endpoint..."
curl -X POST -H "Content-Type: application/json" -d "{\"path\":\"$TEST_DIR/sample\"}" http://localhost:8001/api/analyze -o "$TEST_DIR/web_analysis.json" &> "$TEST_DIR/web_analyze.log"

if [ ! -f "$TEST_DIR/web_analysis.json" ]; then
    echo "Error: Web API analysis output file not found"
    exit 1
fi

echo "Testing /api/file endpoint..."
curl -G --data-urlencode "path=$TEST_DIR/sample/calculator/calculator.go" http://localhost:8001/api/file -o "$TEST_DIR/calculator.go" &> "$TEST_DIR/web_file.log"

if [ ! -f "$TEST_DIR/calculator.go" ]; then
    echo "Error: Web API file output not found"
    exit 1
fi

# Compare files
if ! diff "$TEST_DIR/sample/calculator/calculator.go" "$TEST_DIR/calculator.go" > /dev/null; then
    echo "Error: Web API file content does not match original file"
    exit 1
fi

echo "Web API tests passed!"

# Display summary
echo ""
echo "=== Test Summary ==="
echo "All tests passed successfully!"
echo ""
echo "The Go File Analyzer has been tested with the following codebases:"
echo "- Sample calculator project"
echo "- Cobra CLI Framework"
echo "- Gorilla Mux Router"
echo "- Viper Configuration"
echo ""
echo "The web server API has been tested for functionality."
echo ""
echo "The test environment will be cleaned up automatically on exit."