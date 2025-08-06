#!/bin/bash

# Native Messaging Installation Script

set -e

echo "Installing native messaging hosts..."

# Get absolute path to backend binary
BACKEND_PATH="$(cd "$(dirname "$0")/../backend" && pwd)/clipper-backend"

# Update paths in manifests
sed "s|/path/to/clipper-backend|$BACKEND_PATH|g" native-messaging/chrome-host.json > /tmp/chrome-host.json
sed "s|/path/to/clipper-backend|$BACKEND_PATH|g" native-messaging/firefox-host.json > /tmp/firefox-host.json

# Install Chrome native messaging host
if command -v google-chrome &> /dev/null || command -v chromium &> /dev/null; then
    echo "Installing Chrome native messaging host..."
    mkdir -p ~/.config/google-chrome/NativeMessagingHosts
    cp /tmp/chrome-host.json ~/.config/google-chrome/NativeMessagingHosts/com.clipper.host.json
    echo "✓ Chrome native messaging host installed"
else
    echo "⚠ Chrome not found, skipping Chrome installation"
fi

# Install Firefox native messaging host
if command -v firefox &> /dev/null; then
    echo "Installing Firefox native messaging host..."
    mkdir -p ~/.mozilla/native-messaging-hosts
    cp /tmp/firefox-host.json ~/.mozilla/native-messaging-hosts/com.clipper.host.json
    echo "✓ Firefox native messaging host installed"
else
    echo "⚠ Firefox not found, skipping Firefox installation"
fi

# Clean up
rm /tmp/chrome-host.json /tmp/firefox-host.json

echo ""
echo "Native messaging installation complete!"
echo "You can now load the extension in your browser."

