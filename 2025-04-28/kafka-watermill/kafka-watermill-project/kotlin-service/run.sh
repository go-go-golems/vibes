#!/bin/bash

echo "Building Kotlin shipping service..."
./gradlew clean build generateProto

if [ $? -eq 0 ]; then
    echo "Build successful. Starting service..."
    ./gradlew bootRun
else
    echo "Build failed. Please check the errors above."
    exit 1
fi