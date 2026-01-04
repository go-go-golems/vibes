#!/bin/bash

# Test streaming CLI functionality
echo "🚀 Testing YouTube Analyzer Streaming CLI"
echo "========================================="

# Check if API key is provided
if [ -z "$GEMINI_API_KEY" ]; then
    echo "❌ Please set GEMINI_API_KEY environment variable"
    echo "   export GEMINI_API_KEY=\"your-api-key-here\""
    exit 1
fi

echo "✅ API key found"
echo ""

# Test 1: Simple prompt with streaming
echo "🧪 Test 1: Simple story generation with streaming"
echo "---"
go run . stream --prompt "Write a short story about a robot learning to paint. Include emotions and discovery."

echo ""
echo "========================================="
echo "✅ Streaming test completed!"
echo ""
echo "💡 Try other prompts:"
echo "   go run . stream --prompt 'Explain quantum computing in simple terms'"
echo "   go run . stream --prompt 'Write a poem about the ocean'"
echo "   go run . stream  # Interactive mode"
