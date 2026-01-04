#!/bin/bash

echo "=== Pi-Go CLI Test Suite ==="
echo

# Test basic help
echo "1. Testing basic help..."
./pi --help | head -5
echo

# Test pod management
echo "2. Testing pod management..."
echo "Current pods:"
./pi pod list
echo

echo "Adding a second pod..."
./pi pod add gpu-server "ssh -i ~/.ssh/key user@gpu.example.com" --host gpu.example.com --user gpuuser --storage-path /data/models
echo

echo "Updated pod list:"
./pi pod list
echo

# Test model management
echo "3. Testing model management..."
echo "Current models:"
./pi model list
echo

echo "Starting another model..."
./pi model start Qwen/Qwen2.5-7B-Instruct --name qwen-test --memory 0.6 --pod gpu-server
echo

echo "Updated model list:"
./pi model list
echo

# Test structured output
echo "4. Testing structured output..."
echo "Pods as JSON:"
./pi pod list --output json | jq '.[] | {name, host, status, is_active}'
echo

echo "Models as YAML:"
./pi model list --output yaml | head -10
echo

# Test configuration
echo "5. Testing configuration..."
echo "Config file location: ~/.config/pi-go/pi-go.yaml"
echo "Config file size: $(wc -l < ~/.config/pi-go/pi-go.yaml) lines"
echo

echo "=== Test Suite Complete ==="
