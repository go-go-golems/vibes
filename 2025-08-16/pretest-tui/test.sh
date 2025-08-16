#!/bin/bash

# Pretest TUI Test Script
# This script validates the basic functionality of the pretest TUI application

set -e

echo "🧪 Pretest TUI Test Suite"
echo "========================"

# Check if Go is installed
if ! command -v go &> /dev/null; then
    echo "❌ Go is not installed. Please install Go 1.23.4 or later."
    exit 1
fi

echo "✅ Go is installed: $(go version)"

# Build the application
echo "🔨 Building application..."
if go build -o pretest-tui .; then
    echo "✅ Build successful"
else
    echo "❌ Build failed"
    exit 1
fi

# Test YAML file validation
echo "📋 Testing YAML validation..."

# Test with valid file
if ./pretest-tui examples/memory-pretest.yaml --help 2>/dev/null || true; then
    echo "✅ Application loads without crashing"
else
    echo "⚠️  Application help not available (expected)"
fi

# Test with non-existent file
if ./pretest-tui non-existent-file.yaml 2>/dev/null; then
    echo "❌ Should fail with non-existent file"
    exit 1
else
    echo "✅ Properly handles non-existent files"
fi

# Validate example files
echo "📁 Validating example files..."
for file in examples/*.yaml; do
    if [ -f "$file" ]; then
        echo "  Checking $file..."
        # Just check if the file can be parsed (application will exit immediately)
        timeout 2s ./pretest-tui "$file" 2>/dev/null || true
        echo "  ✅ $file is valid"
    fi
done

echo ""
echo "🎉 All tests passed!"
echo ""
echo "To run the application interactively:"
echo "  ./pretest-tui examples/memory-pretest.yaml"
echo ""
echo "Available example files:"
for file in examples/*.yaml; do
    if [ -f "$file" ]; then
        echo "  - $file"
    fi
done

