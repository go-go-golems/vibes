#!/bin/bash

# Test script to validate TUI input handling
echo "Testing corrected TUI application..."

# Create a test input sequence
echo "Creating test input sequence..."
echo -e "+\n+\n+\np\np\n-\nr\nq" > test_input.txt

# Run the TUI app with the test input
echo "Running TUI with test input..."
timeout 10s ./tui-app < test_input.txt > test_output.txt 2>&1

echo "Test completed. Output:"
cat test_output.txt

echo ""
echo "Checking if commands were processed..."
if grep -q "Value: 1" test_output.txt; then
    echo "✅ Counter increment working"
else
    echo "❌ Counter increment not working"
fi

if grep -q "Progress:" test_output.txt; then
    echo "✅ Progress bar present"
else
    echo "❌ Progress bar not found"
fi

if grep -q "Goodbye" test_output.txt; then
    echo "✅ Quit command working"
else
    echo "❌ Quit command not working"
fi

# Clean up
rm -f test_input.txt test_output.txt

