#!/bin/bash

# Pelican Demo Development Script
set -e

echo "🦆 Pelican Genome Sequencer - Development Setup"
echo "================================================"

# Check if Redis is available and user wants to use it
if command -v redis-server &> /dev/null; then
    echo "Redis detected. Do you want to use Redis Streams? (y/N)"
    read -r use_redis
    if [[ $use_redis =~ ^[Yy]$ ]]; then
        echo "Starting Redis server..."
        redis-server --daemonize yes --port 6379
        export REDIS=1
        echo "✅ Redis started on port 6379"
    else
        echo "Using in-memory pub/sub"
    fi
else
    echo "Redis not found, using in-memory pub/sub"
fi

# Set Go path
export PATH=/usr/local/go/bin:$PATH

# Build the application
echo "Building application..."
go mod tidy
go build -o bin/api ./cmd/api
go build -o bin/worker ./cmd/worker

echo "✅ Build completed"

# Start the API server
echo "Starting API server on http://localhost:8080"
echo "Press Ctrl+C to stop"
echo ""

./bin/api

