#!/bin/bash

# Quality Comparison Script
# Compares different sampling methods and generates analysis

CLI_TOOL="../pixel-tui-cli"
TEST_IMAGE="$1"

if [ -z "$TEST_IMAGE" ]; then
    echo "Usage: $0 <image_file>"
    echo ""
    echo "This script compares different color sampling methods"
    echo "and generates detailed analysis for the specified image."
    echo ""
    echo "Example:"
    echo "  $0 ../original-images/pasted_file_4jP02c_image.png"
    exit 1
fi

if [ ! -f "$TEST_IMAGE" ]; then
    echo "Error: Image file '$TEST_IMAGE' not found"
    exit 1
fi

if [ ! -f "$CLI_TOOL" ]; then
    echo "Error: CLI tool not found at $CLI_TOOL"
    echo "Please build it first: cd ../source && go build -o ../pixel-tui-cli main_cli.go"
    exit 1
fi

filename=$(basename "$TEST_IMAGE")
name_without_ext="${filename%.*}"

echo "=== Quality Comparison Analysis ==="
echo "Image: $filename"
echo "Date: $(date)"
echo ""

# Get original image info if identify is available
if command -v identify >/dev/null 2>&1; then
    echo "Original Image Information:"
    identify "$TEST_IMAGE"
    echo ""
fi

# Test different sampling methods
echo "=== SAMPLING METHOD COMPARISON ==="
echo ""

# Nearest sampling
echo "1. NEAREST SAMPLING (Exact Colors)"
echo "   Best for: Pixel art, logos, images with distinct colors"
echo "   Processing..."
start_time=$(date +%s.%N)
"$CLI_TOOL" -i "$TEST_IMAGE" -w 32 -h 32 -s nearest -v > "${name_without_ext}_nearest_analysis.txt" 2>&1
end_time=$(date +%s.%N)
nearest_time=$(echo "$end_time - $start_time" | bc -l)

# Extract color count
nearest_colors=$(grep "Generated palette" "${name_without_ext}_nearest_analysis.txt" | grep -o '[0-9]\+ colors' | grep -o '[0-9]\+')
echo "   Colors: $nearest_colors"
echo "   Time: ${nearest_time}s"
echo ""

# Quantized sampling
echo "2. QUANTIZED SAMPLING (Reduced Colors)"
echo "   Best for: Photos, complex images, terminal compatibility"
echo "   Processing..."
start_time=$(date +%s.%N)
"$CLI_TOOL" -i "$TEST_IMAGE" -w 32 -h 32 -s quantized -v > "${name_without_ext}_quantized_analysis.txt" 2>&1
end_time=$(date +%s.%N)
quantized_time=$(echo "$end_time - $start_time" | bc -l)

# Extract color count
quantized_colors=$(grep "Generated palette" "${name_without_ext}_quantized_analysis.txt" | grep -o '[0-9]\+ colors' | grep -o '[0-9]\+')
echo "   Colors: $quantized_colors"
echo "   Time: ${quantized_time}s"
echo ""

# Interpolated sampling
echo "3. INTERPOLATED SAMPLING (Smooth Gradients)"
echo "   Best for: Artistic images, smooth transitions"
echo "   Processing..."
start_time=$(date +%s.%N)
"$CLI_TOOL" -i "$TEST_IMAGE" -w 32 -h 32 -s interpolated -v > "${name_without_ext}_interpolated_analysis.txt" 2>&1
end_time=$(date +%s.%N)
interpolated_time=$(echo "$end_time - $start_time" | bc -l)

# Extract color count
interpolated_colors=$(grep "Generated palette" "${name_without_ext}_interpolated_analysis.txt" | grep -o '[0-9]\+ colors' | grep -o '[0-9]\+')
echo "   Colors: $interpolated_colors"
echo "   Time: ${interpolated_time}s"
echo ""

# Size comparison
echo "=== SIZE COMPARISON (Quantized Sampling) ==="
echo ""

sizes=("16 12" "32 24" "48 36" "64 48")
for size in "${sizes[@]}"; do
    w=$(echo $size | cut -d' ' -f1)
    h=$(echo $size | cut -d' ' -f2)
    
    echo "Size: ${w}x${h}"
    start_time=$(date +%s.%N)
    "$CLI_TOOL" -i "$TEST_IMAGE" -w "$w" -h "$h" -s quantized -v > "${name_without_ext}_${w}x${h}_analysis.txt" 2>&1
    end_time=$(date +%s.%N)
    size_time=$(echo "$end_time - $start_time" | bc -l)
    
    size_colors=$(grep "Generated palette" "${name_without_ext}_${w}x${h}_analysis.txt" | grep -o '[0-9]\+ colors' | grep -o '[0-9]\+')
    echo "   Colors: $size_colors"
    echo "   Time: ${size_time}s"
    echo ""
done

# Summary
echo "=== SUMMARY ==="
echo ""
echo "Sampling Method Comparison:"
echo "  Nearest:      $nearest_colors colors, ${nearest_time}s"
echo "  Quantized:    $quantized_colors colors, ${quantized_time}s"
echo "  Interpolated: $interpolated_colors colors, ${interpolated_time}s"
echo ""

# Recommendations
echo "=== RECOMMENDATIONS ==="
echo ""

if [ "$nearest_colors" -lt 50 ]; then
    echo "✓ NEAREST sampling recommended - Low color count, good for pixel art"
elif [ "$quantized_colors" -lt 100 ]; then
    echo "✓ QUANTIZED sampling recommended - Balanced color count and quality"
else
    echo "✓ QUANTIZED sampling recommended - High color count, needs reduction"
fi

echo ""
echo "Generated analysis files:"
ls -1 ${name_without_ext}_*_analysis.txt
echo ""
echo "To view the image interactively:"
echo "$CLI_TOOL -i \"$TEST_IMAGE\" -w 32 -h 32 -s quantized"

