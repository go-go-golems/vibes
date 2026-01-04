#!/bin/bash

# Batch Processing Script for Pixel Art TUI Renderer
# Processes multiple images with optimal settings

CLI_TOOL="../pixel-tui-cli"
INPUT_DIR="../original-images"
OUTPUT_DIR="./batch_output"

# Create output directory
mkdir -p "$OUTPUT_DIR"

echo "=== Batch Processing Script ==="
echo "Processing all images in: $INPUT_DIR"
echo "Output directory: $OUTPUT_DIR"
echo ""

# Check if CLI tool exists
if [ ! -f "$CLI_TOOL" ]; then
    echo "Error: $CLI_TOOL not found"
    echo "Please build it first: cd ../source && go build -o ../pixel-tui-cli main_cli.go"
    exit 1
fi

# Function to determine optimal settings based on file
get_optimal_settings() {
    local file="$1"
    local filename=$(basename "$file")
    
    # Check file size to determine settings
    if command -v identify >/dev/null 2>&1; then
        width=$(identify -format "%w" "$file" 2>/dev/null)
        height=$(identify -format "%h" "$file" 2>/dev/null)
        
        if [ -n "$width" ] && [ -n "$height" ]; then
            if [ "$width" -le 32 ] && [ "$height" -le 32 ]; then
                # Small image - likely pixel art
                echo "32 32 nearest"
            elif [ "$width" -le 128 ] && [ "$height" -le 128 ]; then
                # Medium image
                echo "48 36 quantized"
            else
                # Large image
                echo "64 48 quantized"
            fi
        else
            # Default settings
            echo "32 32 nearest"
        fi
    else
        # Default settings if identify not available
        echo "32 32 nearest"
    fi
}

# Process each image file
processed_count=0
for file in "$INPUT_DIR"/*.{png,jpg,jpeg,gif}; do
    if [ -f "$file" ]; then
        filename=$(basename "$file")
        name_without_ext="${filename%.*}"
        
        echo "Processing: $filename"
        
        # Get optimal settings
        settings=$(get_optimal_settings "$file")
        width=$(echo $settings | cut -d' ' -f1)
        height=$(echo $settings | cut -d' ' -f2)
        sampling=$(echo $settings | cut -d' ' -f3)
        
        echo "  Settings: ${width}x${height}, sampling: $sampling"
        
        # Process the image (with timeout to prevent hanging)
        timeout 30s "$CLI_TOOL" -i "$file" -w "$width" -h "$height" -s "$sampling" -v --scale 15 > "$OUTPUT_DIR/${name_without_ext}_analysis.txt" 2>&1
        
        if [ $? -eq 0 ]; then
            echo "  ✓ Processed successfully"
            processed_count=$((processed_count + 1))
        else
            echo "  ✗ Processing failed or timed out"
        fi
        
        echo ""
    fi
done

echo "=== Batch Processing Complete ==="
echo "Processed $processed_count files"
echo "Analysis files saved to: $OUTPUT_DIR"
echo ""
echo "To view any image interactively:"
echo "$CLI_TOOL -i <image_file> [options]"

