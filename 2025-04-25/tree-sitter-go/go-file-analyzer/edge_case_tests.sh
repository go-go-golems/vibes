#!/bin/bash

# Edge case tests for Go File Analyzer
# Tests how the application handles various edge cases and error conditions

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

# Create a result file
RESULTS_FILE="$TEST_DIR/edge_case_results.txt"
touch "$RESULTS_FILE"

# Helper function to run a test and log the result
run_test() {
    local test_name="$1"
    local test_cmd="$2"
    local expected_result="$3"
    
    echo "Running test: $test_name"
    echo "Command: $test_cmd"
    echo "Expected result: $expected_result"
    
    echo "=== Test: $test_name ===" >> "$RESULTS_FILE"
    echo "Command: $test_cmd" >> "$RESULTS_FILE"
    echo "Expected result: $expected_result" >> "$RESULTS_FILE"
    
    # Run the test command
    eval "$test_cmd" &> "$TEST_DIR/${test_name}.log"
    local exit_code=$?
    
    # Log the result
    echo "Exit code: $exit_code" >> "$RESULTS_FILE"
    echo "Output:" >> "$RESULTS_FILE"
    cat "$TEST_DIR/${test_name}.log" >> "$RESULTS_FILE"
    echo "" >> "$RESULTS_FILE"
    
    # Check the result
    if [ "$expected_result" = "success" ] && [ $exit_code -eq 0 ]; then
        echo "Test passed!"
        echo "Result: PASS" >> "$RESULTS_FILE"
    elif [ "$expected_result" = "failure" ] && [ $exit_code -ne 0 ]; then
        echo "Test passed (expected failure)!"
        echo "Result: PASS" >> "$RESULTS_FILE"
    else
        echo "Test failed!"
        echo "Result: FAIL" >> "$RESULTS_FILE"
    fi
    
    echo "" >> "$RESULTS_FILE"
}

echo "=== Edge Case Tests ==="

# Test 1: Analyze a non-existent directory
run_test "nonexistent_dir" "$ANALYZER -path=/path/that/does/not/exist -output=$TEST_DIR/nonexistent.json" "failure"

# Test 2: Analyze an empty directory
mkdir -p "$TEST_DIR/empty"
run_test "empty_dir" "$ANALYZER -path=$TEST_DIR/empty -output=$TEST_DIR/empty.json" "success"

# Test 3: Analyze a directory with no Go files
mkdir -p "$TEST_DIR/no_go_files"
touch "$TEST_DIR/no_go_files/file.txt"
touch "$TEST_DIR/no_go_files/file.md"
run_test "no_go_files" "$ANALYZER -path=$TEST_DIR/no_go_files -output=$TEST_DIR/no_go_files.json" "success"

# Test 4: Analyze a directory with an invalid Go file
mkdir -p "$TEST_DIR/invalid_go"
cat > "$TEST_DIR/invalid_go/invalid.go" << 'EOF'
package main

func main() {
    fmt.Println("Hello, World!"
}
EOF
run_test "invalid_go" "$ANALYZER -path=$TEST_DIR/invalid_go -output=$TEST_DIR/invalid_go.json" "success"

# Test 5: Analyze a directory with a very large Go file
mkdir -p "$TEST_DIR/large_go"
# Create a large Go file with 10,000 functions
{
    echo "package main"
    echo ""
    echo "func main() {"
    for i in {1..100}; do
        echo "    function$i()"
    done
    echo "}"
    echo ""
    for i in {1..10000}; do
        echo "func function$i() {"
        echo "    // This is function $i"
        echo "}"
        echo ""
    done
} > "$TEST_DIR/large_go/large.go"
run_test "large_go" "$ANALYZER -path=$TEST_DIR/large_go -output=$TEST_DIR/large_go.json -verbose" "success"

# Test 6: Process an invalid JSON file with the formatter
echo "Invalid JSON" > "$TEST_DIR/invalid.json"
run_test "invalid_json" "$FORMATTER $TEST_DIR/invalid.json $TEST_DIR/invalid_enhanced.json" "failure"

# Test 7: Test the web server with a non-existent path
run_test "web_nonexistent" "curl -X POST -H \"Content-Type: application/json\" -d '{\"path\":\"/path/that/does/not/exist\"}' http://localhost:8001/api/analyze" "failure"

# Test 8: Test the web server with an empty request
run_test "web_empty_request" "curl -X POST -H \"Content-Type: application/json\" -d '{}' http://localhost:8001/api/analyze" "failure"

# Test 9: Test the web server with an invalid file path
run_test "web_invalid_file" "curl -G --data-urlencode \"path=/file/that/does/not/exist\" http://localhost:8001/api/file" "failure"

# Test 10: Test the web server with a valid directory but no Go files
mkdir -p "$TEST_DIR/web_no_go"
touch "$TEST_DIR/web_no_go/file.txt"
run_test "web_no_go" "curl -X POST -H \"Content-Type: application/json\" -d '{\"path\":\"$TEST_DIR/web_no_go\"}' http://localhost:8001/api/analyze" "success"

# Test 11: Test with a file that has syntax errors but is still parseable
mkdir -p "$TEST_DIR/parseable_error"
cat > "$TEST_DIR/parseable_error/error.go" << 'EOF'
package main

import "fmt"

func main() {
    // This will have a syntax error but should still be parseable
    fmt.Println("Hello, World!")
    var x int = "string" // Type mismatch
}
EOF
run_test "parseable_error" "$ANALYZER -path=$TEST_DIR/parseable_error -output=$TEST_DIR/parseable_error.json" "success"

# Test 12: Test with a Go file that has unusual unicode characters
mkdir -p "$TEST_DIR/unicode"
cat > "$TEST_DIR/unicode/unicode.go" << 'EOF'
package main

import "fmt"

// Unicode function name with emoji
func main() {
    fmt.Println("Hello, World!")
    functionWithUnicode()
}

// Function with unicode characters
func functionWithUnicode() {
    // Some unicode characters
    str := "こんにちは世界" // Hello World in Japanese
    fmt.Println(str)
    
    // More unicode
    str2 := "你好，世界" // Hello World in Chinese
    fmt.Println(str2)
}

// Function with comment containing unicode
func anotherFunction() {
    // 안녕하세요 세계 (Hello World in Korean)
    fmt.Println("Hello")
}
EOF
run_test "unicode" "$ANALYZER -path=$TEST_DIR/unicode -output=$TEST_DIR/unicode.json" "success"

# Test 13: Test a directory with a very deep nesting
mkdir -p "$TEST_DIR/deep/nesting/levels/go/project/src/package/subpackage/module/component/util"
cat > "$TEST_DIR/deep/nesting/levels/go/project/src/package/subpackage/module/component/util/deep.go" << 'EOF'
package util

// DeepFunction is in a deeply nested package
func DeepFunction() string {
    return "I'm deep"
}
EOF
run_test "deep_nesting" "$ANALYZER -path=$TEST_DIR/deep -output=$TEST_DIR/deep.json" "success"

# Test 14: Test with a Go file that has a very long function
mkdir -p "$TEST_DIR/long_function"
{
    echo "package main"
    echo ""
    echo "func veryLongFunction() {"
    for i in {1..1000}; do
        echo "    // Line $i of the function"
        echo "    var x$i int = $i"
    done
    echo "}"
} > "$TEST_DIR/long_function/long.go"
run_test "long_function" "$ANALYZER -path=$TEST_DIR/long_function -output=$TEST_DIR/long_function.json" "success"

# Test 15: Test with a Go file that has multiple packages (invalid Go, but should handle gracefully)
mkdir -p "$TEST_DIR/multiple_packages"
cat > "$TEST_DIR/multiple_packages/multiple.go" << 'EOF'
package one

func FunctionOne() {
    // This is in package one
}

package two

func FunctionTwo() {
    // This is in package two
}
EOF
run_test "multiple_packages" "$ANALYZER -path=$TEST_DIR/multiple_packages -output=$TEST_DIR/multiple_packages.json" "success"

# Display summary
echo ""
echo "=== Test Summary ==="
echo "All edge case tests have been completed!"
echo "Results are available in: $RESULTS_FILE"
echo ""
echo "The test environment will be cleaned up automatically on exit."

# Copy results to a permanent location
cp "$RESULTS_FILE" "edge_case_results.txt"
echo "Results copied to: edge_case_results.txt"