#!/bin/bash

# Create DB directory if it doesn't exist
mkdir -p db

# Install dependencies if needed
if [ ! -d "vendor/bundle" ]; then
  echo "Installing dependencies..."
  bundle install --path vendor/bundle
fi

# Start the analytics service
echo "Starting Analytics Service..."
bundle exec ruby app.rb