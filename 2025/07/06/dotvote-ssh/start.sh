#!/bin/bash

# Dot Vote SSH Application Quick Start Script

echo "🗳️  Dot Vote SSH Application"
echo "=============================="
echo

# Check if Go is installed
if ! command -v go &> /dev/null; then
    echo "❌ Go is not installed. Please install Go 1.23+ from https://golang.org/"
    exit 1
fi

echo "✅ Go found: $(go version)"

# Build the application
echo "🔨 Building application..."
go build -o dotvote ./cmd/dotvote
if [ $? -ne 0 ]; then
    echo "❌ Build failed"
    exit 1
fi
echo "✅ Build successful"

# Check if SSH keys exist
if [ ! -f "facilitator_key" ] || [ ! -f "participant_key" ]; then
    echo "🔑 Generating SSH keys for testing..."
    ssh-keygen -t ed25519 -f facilitator_key -N "" -q
    ssh-keygen -t ed25519 -f participant_key -N "" -q
    echo "✅ SSH keys generated"
fi

# Generate roles.json if it doesn't exist
if [ ! -f "roles.json" ]; then
    echo "⚙️  Generating roles configuration..."
    go run create_roles.go
    echo "✅ Roles configuration created"
fi

echo
echo "🚀 Starting Dot Vote SSH server..."
echo "   Server will run on localhost:2323"
echo
echo "📋 To connect:"
echo "   Facilitator: ssh -i facilitator_key ubuntu@localhost -p 2323"
echo "   Participant: ssh -i participant_key ubuntu@localhost -p 2323"
echo
echo "Press Ctrl+C to stop the server"
echo

# Start the server
./dotvote

