#!/bin/bash

# Pixel Art TUI Renderer - CLI Demo Script
# This script demonstrates various features and capabilities

echo "=== Pixel Art TUI Renderer - CLI Demo ==="
echo "This demo shows different sampling methods and configurations"
echo ""

# Check if the CLI tool exists
if [ ! -f "../pixel-tui-cli" ]; then
    echo "Error: pixel-tui-cli not found. Please build it first:"
    echo "cd ../source && go build -o ../pixel-tui-cli main_cli.go"
    exit 1
fi

CLI_TOOL="../pixel-tui-cli"
IMAGES_DIR="../original-images"

echo "Available test images:"
ls -1 "$IMAGES_DIR"
echo ""

# Demo 1: Basic usage with different sizes
echo "=== DEMO 1: Size Comparison ==="
echo "Processing the same image at different sizes..."

if [ -f "$IMAGES_DIR/pasted_file_4jP02c_image.png" ]; then
    echo "Small (16x12):"
    $CLI_TOOL -i "$IMAGES_DIR/pasted_file_4jP02c_image.png" -w 16 -h 12 -s nearest -v | grep "Generated palette"
    
    echo "Medium (32x24):"
    $CLI_TOOL -i "$IMAGES_DIR/pasted_file_4jP02c_image.png" -w 32 -h 24 -s nearest -v | grep "Generated palette"
    
    echo "Large (48x36):"
    $CLI_TOOL -i "$IMAGES_DIR/pasted_file_4jP02c_image.png" -w 48 -h 36 -s nearest -v | grep "Generated palette"
fi

echo ""

# Demo 2: Color sampling comparison
echo "=== DEMO 2: Color Sampling Comparison ==="
echo "Same image with different sampling methods..."

if [ -f "$IMAGES_DIR/pasted_file_zdUAFD_image.png" ]; then
    echo "Nearest sampling (exact colors):"
    $CLI_TOOL -i "$IMAGES_DIR/pasted_file_zdUAFD_image.png" -w 32 -h 32 -s nearest -v | grep "Generated palette"
    
    echo "Quantized sampling (reduced colors):"
    $CLI_TOOL -i "$IMAGES_DIR/pasted_file_zdUAFD_image.png" -w 32 -h 32 -s quantized -v | grep "Generated palette"
    
    echo "Interpolated sampling (smooth gradients):"
    $CLI_TOOL -i "$IMAGES_DIR/pasted_file_zdUAFD_image.png" -w 32 -h 32 -s interpolated -v | grep "Generated palette"
fi

echo ""

# Demo 3: Animation processing
echo "=== DEMO 3: Animation Processing ==="
if [ -f "$IMAGES_DIR/blockbob.gif" ]; then
    echo "Processing animated GIF..."
    $CLI_TOOL -i "$IMAGES_DIR/blockbob.gif" -w 32 -h 24 -s quantized -v | head -20
else
    echo "No animated GIF found for demo"
fi

echo ""

# Demo 4: Performance comparison
echo "=== DEMO 4: Performance Comparison ==="
echo "Measuring processing time for different configurations..."

if [ -f "$IMAGES_DIR/pasted_file_n1aHDK_image.png" ]; then
    echo "Fast processing (small size, quantized):"
    time $CLI_TOOL -i "$IMAGES_DIR/pasted_file_n1aHDK_image.png" -w 16 -h 16 -s quantized -v >/dev/null 2>&1
    
    echo "Detailed processing (large size, nearest):"
    time $CLI_TOOL -i "$IMAGES_DIR/pasted_file_n1aHDK_image.png" -w 64 -h 64 -s nearest -v >/dev/null 2>&1
fi

echo ""
echo "=== Demo Complete ==="
echo "To run the interactive TUI, use:"
echo "$CLI_TOOL -i <image_file> [options]"
echo ""
echo "For help and all options:"
echo "$CLI_TOOL --help"

