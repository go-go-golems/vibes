#!/bin/bash

# Performance test script for Go File Analyzer
# Tests the application performance with various Go codebases

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

# Function to run a performance test
run_performance_test() {
    local name=$1
    local repo=$2
    local dir=$3
    local runs=$4
    
    echo "=== Performance Test: $name ==="
    echo "Repository: $repo"
    echo "Directory: $dir"
    echo "Runs: $runs"
    
    # Clone repository
    echo "Cloning repository..."
    if ! git clone --depth 1 "$repo" "$TEST_DIR/$dir" &> "$TEST_DIR/${dir}_clone.log"; then
        echo "Error: Failed to clone repository"
        return 1
    fi
    
    # Create results file
    local results_file="$TEST_DIR/${dir}_performance.csv"
    echo "Run,Analyzer Time (s),Formatter Time (s),File Count,Function Count,Method Count" > "$results_file"
    
    # Run performance tests
    for i in $(seq 1 $runs); do
        echo "Run $i of $runs..."
        
        # Run analyzer with time measurement
        echo "Running analyzer..."
        local analyzer_start=$(date +%s.%N)
        "$ANALYZER" -path="$TEST_DIR/$dir" -output="$TEST_DIR/${dir}_analysis_$i.json" -verbose &> "$TEST_DIR/${dir}_analyzer_$i.log"
        local analyzer_end=$(date +%s.%N)
        local analyzer_time=$(echo "$analyzer_end - $analyzer_start" | bc)
        
        # Run formatter with time measurement
        echo "Running formatter..."
        local formatter_start=$(date +%s.%N)
        "$FORMATTER" "$TEST_DIR/${dir}_analysis_$i.json" "$TEST_DIR/${dir}_enhanced_$i.json" &> "$TEST_DIR/${dir}_formatter_$i.log"
        local formatter_end=$(date +%s.%N)
        local formatter_time=$(echo "$formatter_end - $formatter_start" | bc)
        
        # Extract stats
        local file_count=$(grep "Files analyzed:" "$TEST_DIR/${dir}_analyzer_$i.log" | awk '{print $3}')
        local function_count=$(grep "Functions found:" "$TEST_DIR/${dir}_analyzer_$i.log" | awk '{print $3}')
        local method_count=$(grep "Methods found:" "$TEST_DIR/${dir}_analyzer_$i.log" | awk '{print $3}')
        
        # Log results
        echo "$i,$analyzer_time,$formatter_time,$file_count,$function_count,$method_count" >> "$results_file"
    done
    
    # Calculate average times
    local avg_analyzer_time=$(awk -F, 'NR>1 {sum+=$2} END {print sum/(NR-1)}' "$results_file")
    local avg_formatter_time=$(awk -F, 'NR>1 {sum+=$3} END {print sum/(NR-1)}' "$results_file")
    
    # Display summary
    echo ""
    echo "Performance Summary:"
    echo "Average Analyzer Time: $avg_analyzer_time seconds"
    echo "Average Formatter Time: $avg_formatter_time seconds"
    echo "File Count: $file_count"
    echo "Function Count: $function_count"
    echo "Method Count: $method_count"
    echo ""
    
    # Copy results to a permanent location
    cp "$results_file" "${dir}_performance.csv"
    echo "Results copied to: ${dir}_performance.csv"
    
    return 0
}

# Function to run web server performance tests
run_web_performance_test() {
    local name=$1
    local project_dir=$2
    local runs=$3
    
    echo "=== Web Server Performance Test: $name ==="
    echo "Project Directory: $project_dir"
    echo "Runs: $runs"
    
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
        return 1
    fi
    
    # Create results file
    local results_file="$TEST_DIR/${name}_web_performance.csv"
    echo "Run,API Time (s),File API Time (s)" > "$results_file"
    
    # Run performance tests
    for i in $(seq 1 $runs); do
        echo "Run $i of $runs..."
        
        # Test /api/analyze endpoint
        echo "Testing /api/analyze endpoint..."
        local api_start=$(date +%s.%N)
        curl -s -X POST -H "Content-Type: application/json" -d "{\"path\":\"$project_dir\"}" http://localhost:8001/api/analyze -o "$TEST_DIR/${name}_web_analysis_$i.json" &> "$TEST_DIR/${name}_web_analyze_$i.log"
        local api_end=$(date +%s.%N)
        local api_time=$(echo "$api_end - $api_start" | bc)
        
        # Test /api/file endpoint
        echo "Testing /api/file endpoint..."
        local file_path=$(find "$project_dir" -name "*.go" | head -1)
        local file_api_start=$(date +%s.%N)
        curl -s -G --data-urlencode "path=$file_path" http://localhost:8001/api/file -o "$TEST_DIR/${name}_file_$i.go" &> "$TEST_DIR/${name}_web_file_$i.log"
        local file_api_end=$(date +%s.%N)
        local file_api_time=$(echo "$file_api_end - $file_api_start" | bc)
        
        # Log results
        echo "$i,$api_time,$file_api_time" >> "$results_file"
    done
    
    # Calculate average times
    local avg_api_time=$(awk -F, 'NR>1 {sum+=$2} END {print sum/(NR-1)}' "$results_file")
    local avg_file_api_time=$(awk -F, 'NR>1 {sum+=$3} END {print sum/(NR-1)}' "$results_file")
    
    # Display summary
    echo ""
    echo "Web Performance Summary:"
    echo "Average API Time: $avg_api_time seconds"
    echo "Average File API Time: $avg_file_api_time seconds"
    echo ""
    
    # Copy results to a permanent location
    cp "$results_file" "${name}_web_performance.csv"
    echo "Results copied to: ${name}_web_performance.csv"
    
    # Stop the web server
    echo "Stopping web server..."
    kill $SERVER_PID || true
    SERVER_PID=""
    
    return 0
}

# Create a synthetic test project with many files and functions
echo "Creating synthetic test project..."
mkdir -p "$TEST_DIR/synthetic/pkg"

# Create go.mod file
cat > "$TEST_DIR/synthetic/go.mod" << 'EOF'
module synthetic

go 1.18
EOF

# Create main package
mkdir -p "$TEST_DIR/synthetic/cmd/app"
cat > "$TEST_DIR/synthetic/cmd/app/main.go" << 'EOF'
package main

import (
    "fmt"
    "synthetic/pkg/module1"
    "synthetic/pkg/module2"
)

func main() {
    fmt.Println("Synthetic test project")
    module1.Function1()
    module2.Function1()
}
EOF

# Create 50 Go files with 10 functions each
for i in {1..50}; do
    mkdir -p "$TEST_DIR/synthetic/pkg/module$i"
    
    cat > "$TEST_DIR/synthetic/pkg/module$i/module$i.go" << EOF
package module$i

import "fmt"

// Function1 is a function in module$i
func Function1() {
    fmt.Println("Function1 in module$i")
}

// Function2 is a function in module$i
func Function2() {
    fmt.Println("Function2 in module$i")
}

// Function3 is a function in module$i
func Function3() {
    fmt.Println("Function3 in module$i")
}

// Function4 is a function in module$i
func Function4() {
    fmt.Println("Function4 in module$i")
}

// Function5 is a function in module$i
func Function5() {
    fmt.Println("Function5 in module$i")
}

// Type is a struct in module$i
type Type struct {
    Field1 string
    Field2 int
}

// Method1 is a method of Type
func (t *Type) Method1() string {
    return t.Field1
}

// Method2 is a method of Type
func (t *Type) Method2() int {
    return t.Field2
}

// Method3 is a method of Type
func (t *Type) Method3() {
    fmt.Println("Method3 in module$i")
}

// Method4 is a method of Type
func (t *Type) Method4() {
    fmt.Println("Method4 in module$i")
}

// Method5 is a method of Type
func (t *Type) Method5() {
    fmt.Println("Method5 in module$i")
}
EOF
done

# Run performance tests
echo "=== Performance Tests ==="

# Test with the synthetic project
run_performance_test "Synthetic Project" "file://$TEST_DIR/synthetic" "synthetic" 3

# Test with real Go projects
run_performance_test "Gorilla WebSocket" "https://github.com/gorilla/websocket.git" "websocket" 3

# Test web server performance
run_web_performance_test "Synthetic" "$TEST_DIR/synthetic" 3

# Display summary
echo ""
echo "=== Performance Test Summary ==="
echo "All performance tests have been completed!"
echo ""
echo "Results files:"
echo "- synthetic_performance.csv"
echo "- websocket_performance.csv"
echo "- Synthetic_web_performance.csv"
echo ""
echo "The test environment will be cleaned up automatically on exit."
echo ""
echo "Performance Recommendations:"
echo "1. Consider optimizing the Tree-sitter parsing for large files"
echo "2. Add caching for frequently accessed files in the web server"
echo "3. Implement parallel processing for analyzing multiple files"
echo "4. Optimize the JSON formatter for large analysis results"
echo "5. Consider a streaming API for large projects in the web interface"