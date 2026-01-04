#!/bin/bash

# Build the TTMP tool
echo "Building TTMP tool..."
go build -o ttmp ./cmd/ttmp

# Test validation
echo -e "\n\033[1mTesting validation...\033[0m"
./ttmp validate example-document.md
echo -e "\n\033[1mTesting validation of invalid document...\033[0m"
./ttmp validate invalid-document.md

# Test parsing errors
echo -e "\n\033[1mTesting parsing errors...\033[0m"
./ttmp validate parsing-error-document.md

# Test query with invalid parameters
echo -e "\n\033[1mTesting query with invalid operator...\033[0m"
./ttmp query . id invalid-operator value

# Test non-existent file
echo -e "\n\033[1mTesting non-existent file...\033[0m"
./ttmp validate non-existent-file.md

# Test list with invalid format
echo -e "\n\033[1mTesting list with invalid format...\033[0m"
./ttmp list . --format=invalid-format

echo -e "\n\033[1mAll tests completed.\033[0m"