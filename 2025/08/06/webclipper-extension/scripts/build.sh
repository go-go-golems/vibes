#!/bin/bash

# Web Clipper Extension Build Script

set -e

echo "Building Web Clipper Extension..."

# Build Go backend
echo "Building Go backend..."
cd backend
go build -o clipper-backend main.go
echo "✓ Go backend built successfully"

# Make backend executable
chmod +x clipper-backend

# Create clips directory
mkdir -p clips

cd ..

echo "Build complete!"
echo ""
echo "Next steps:"
echo "1. Install native messaging hosts:"
echo "   ./scripts/install-native-messaging.sh"
echo "2. Load extension in browser:"
echo "   - Chrome: chrome://extensions/ -> Load unpacked -> extension/chrome"
echo "   - Firefox: about:debugging -> Load Temporary Add-on -> extension/firefox/manifest.json"

