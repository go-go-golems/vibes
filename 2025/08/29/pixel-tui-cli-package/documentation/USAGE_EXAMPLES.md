# Pixel Art TUI Renderer - Usage Examples & Demonstrations

## Quick Start Examples

### Basic Usage
```bash
# View an image with default settings (32x32 pixels)
./pixel-tui-cli -i image.png

# View with custom dimensions
./pixel-tui-cli -i image.png -w 64 -h 48

# View with verbose output to see processing details
./pixel-tui-cli -i image.png -w 32 -h 32 -v
```

### Color Sampling Demonstrations

#### Example 1: Pixel Art Sprite
```bash
# Best settings for pixel art - preserves exact colors
./pixel-tui-cli -i sprite.png -w 32 -h 32 -s nearest

# What you'll see:
# - Crisp pixel boundaries
# - Exact original colors
# - Clean, blocky appearance
# - Larger color palette (50-200 colors typical)
```

#### Example 2: Photograph or Complex Image
```bash
# Best settings for photos - reduces color noise
./pixel-tui-cli -i photo.jpg -w 48 -h 36 -s quantized

# What you'll see:
# - Smoother color transitions
# - Reduced color palette (10-50 colors typical)
# - Better terminal compatibility
# - Less color noise
```

#### Example 3: Artistic Image with Gradients
```bash
# Best settings for artwork - preserves smooth transitions
./pixel-tui-cli -i artwork.png -w 40 -h 40 -s interpolated

# What you'll see:
# - Smooth color gradients
# - Preserved color nuances
# - Large color palette (100-500+ colors)
# - High visual fidelity
```

## Real-World Use Cases

### Use Case 1: Game Asset Preview
```bash
# Preview game sprites at actual pixel dimensions
./pixel-tui-cli -i character_sprite.png -w 16 -h 16 -s nearest

# Preview larger game assets
./pixel-tui-cli -i background_tile.png -w 32 -h 32 -s nearest

# Preview with export for documentation
./pixel-tui-cli -i item_icon.png -w 24 -h 24 -s nearest --scale 15
# Press 's' to save as export_item_icon.png (360x360 pixels)
```

### Use Case 2: Animation Preview
```bash
# Preview character animation
./pixel-tui-cli -i character_walk.gif -w 32 -h 32 -s quantized -v

# Controls during animation:
# - Press 'a' to enter animation mode
# - Press SPACE to pause/resume
# - Press '+' to speed up, '-' to slow down
# - Press 's' to save current frame
# - Press 'a' again to exit animation mode
```

### Use Case 3: Image Size Testing
```bash
# Test different output sizes to find optimal display
./pixel-tui-cli -i logo.png -w 20 -h 20 -s nearest    # Small
./pixel-tui-cli -i logo.png -w 32 -h 32 -s nearest    # Medium
./pixel-tui-cli -i logo.png -w 48 -h 48 -s nearest    # Large
./pixel-tui-cli -i logo.png -w 64 -h 64 -s nearest    # Extra Large
```

## Comparative Analysis Examples

### Color Sampling Comparison
```bash
# Process the same image with all three sampling methods
echo "=== NEAREST SAMPLING ==="
./pixel-tui-cli -i test_image.png -w 32 -h 32 -s nearest -v

echo "=== QUANTIZED SAMPLING ==="
./pixel-tui-cli -i test_image.png -w 32 -h 32 -s quantized -v

echo "=== INTERPOLATED SAMPLING ==="
./pixel-tui-cli -i test_image.png -w 32 -h 32 -s interpolated -v

# Compare the results:
# - Color count differences
# - Visual quality
# - Processing time
# - Terminal compatibility
```

### Size Impact Analysis
```bash
# Show how output size affects quality and performance
echo "=== 16x16 OUTPUT ==="
time ./pixel-tui-cli -i complex_image.png -w 16 -h 16 -s quantized -v

echo "=== 32x32 OUTPUT ==="
time ./pixel-tui-cli -i complex_image.png -w 32 -h 32 -s quantized -v

echo "=== 64x64 OUTPUT ==="
time ./pixel-tui-cli -i complex_image.png -w 64 -h 64 -s quantized -v

# Observe:
# - Processing time increases
# - Color count may increase
# - Detail preservation improves
# - Memory usage grows
```

## Export Functionality Examples

### Standard Export Workflow
```bash
# Load image and export with default scaling (10x)
./pixel-tui-cli -i pixel_art.png -w 32 -h 32 -s nearest

# In the TUI:
# 1. Press 's' to save
# 2. File saved as "export_pixel_art.png" (320x320 pixels)
# 3. Press 'q' to quit
```

### High-Resolution Export
```bash
# Create high-resolution exports for printing or detailed viewing
./pixel-tui-cli -i sprite.png -w 24 -h 24 -s nearest --scale 20

# In the TUI:
# 1. Press 's' to save
# 2. File saved as "export_sprite.png" (480x480 pixels)
# 3. Suitable for printing or detailed analysis
```

### Animation Frame Export
```bash
# Export specific frames from animations
./pixel-tui-cli -i animation.gif -w 32 -h 32 -s quantized

# In the TUI:
# 1. Press 'a' to enter animation mode
# 2. Wait for desired frame or use +/- to control speed
# 3. Press 's' to save current frame
# 4. File saved as "export_animation_frame_X.png"
```

## Batch Processing Examples

### Process Multiple Images
```bash
#!/bin/bash
# Batch process all PNG files in a directory

echo "Processing all PNG files..."
for file in *.png; do
    if [ -f "$file" ]; then
        echo "Processing: $file"
        # Use timeout to auto-quit after viewing
        timeout 5s ./pixel-tui-cli -i "$file" -w 32 -h 32 -s nearest -v
        echo "Completed: $file"
        echo "---"
    fi
done
```

### Quality Assessment Script
```bash
#!/bin/bash
# Compare different settings for the same image

IMAGE="$1"
if [ -z "$IMAGE" ]; then
    echo "Usage: $0 <image_file>"
    exit 1
fi

echo "=== QUALITY COMPARISON FOR: $IMAGE ==="

echo "1. Pixel Art Settings (Nearest, 32x32):"
./pixel-tui-cli -i "$IMAGE" -w 32 -h 32 -s nearest -v | head -20

echo "2. Photo Settings (Quantized, 48x36):"
./pixel-tui-cli -i "$IMAGE" -w 48 -h 36 -s quantized -v | head -20

echo "3. Artistic Settings (Interpolated, 40x40):"
./pixel-tui-cli -i "$IMAGE" -w 40 -h 40 -s interpolated -v | head -20
```

## Performance Testing Examples

### Memory Usage Test
```bash
# Test memory usage with different image sizes
echo "=== MEMORY USAGE COMPARISON ==="

echo "Small image (16x16):"
/usr/bin/time -v ./pixel-tui-cli -i test.png -w 16 -h 16 -s nearest 2>&1 | grep "Maximum resident"

echo "Medium image (32x32):"
/usr/bin/time -v ./pixel-tui-cli -i test.png -w 32 -h 32 -s nearest 2>&1 | grep "Maximum resident"

echo "Large image (64x64):"
/usr/bin/time -v ./pixel-tui-cli -i test.png -w 64 -h 64 -s nearest 2>&1 | grep "Maximum resident"
```

### Processing Speed Test
```bash
# Benchmark processing speed
echo "=== PROCESSING SPEED COMPARISON ==="

echo "Nearest sampling:"
time ./pixel-tui-cli -i test.png -w 32 -h 32 -s nearest -v | grep "Generated palette"

echo "Quantized sampling:"
time ./pixel-tui-cli -i test.png -w 32 -h 32 -s quantized -v | grep "Generated palette"

echo "Interpolated sampling:"
time ./pixel-tui-cli -i test.png -w 32 -h 32 -s interpolated -v | grep "Generated palette"
```

## Integration Examples

### File Manager Integration
```bash
#!/bin/bash
# Add to your file manager as a custom action

# For image files, open with pixel TUI
case "$1" in
    *.png|*.jpg|*.jpeg)
        # Determine optimal size based on file
        if [[ $(identify -format "%w" "$1") -lt 64 ]]; then
            # Small image - use nearest sampling
            ./pixel-tui-cli -i "$1" -w 32 -h 32 -s nearest
        else
            # Larger image - use quantized sampling
            ./pixel-tui-cli -i "$1" -w 48 -h 36 -s quantized
        fi
        ;;
    *.gif)
        # Animated GIF
        ./pixel-tui-cli -i "$1" -w 32 -h 32 -s quantized
        ;;
    *)
        echo "Unsupported file type: $1"
        ;;
esac
```

### Development Workflow Integration
```bash
#!/bin/bash
# Asset validation script for game development

ASSET_DIR="assets/sprites"
OUTPUT_DIR="previews"

mkdir -p "$OUTPUT_DIR"

echo "=== SPRITE ASSET VALIDATION ==="

for sprite in "$ASSET_DIR"/*.png; do
    if [ -f "$sprite" ]; then
        filename=$(basename "$sprite" .png)
        echo "Validating: $filename"
        
        # Check if sprite is appropriate size
        width=$(identify -format "%w" "$sprite")
        height=$(identify -format "%h" "$sprite")
        
        if [ "$width" -le 32 ] && [ "$height" -le 32 ]; then
            # Small sprite - use 1:1 scaling
            ./pixel-tui-cli -i "$sprite" -w "$width" -h "$height" -s nearest --scale 10
        else
            # Large sprite - scale down
            ./pixel-tui-cli -i "$sprite" -w 32 -h 32 -s nearest --scale 10
        fi
        
        echo "Preview saved for: $filename"
        echo "---"
    fi
done
```

## Troubleshooting Examples

### Color Palette Issues
```bash
# If you see "too many colors" or poor terminal display:

# Problem: Image has too many colors for terminal
./pixel-tui-cli -i complex_photo.jpg -w 32 -h 32 -s nearest
# Solution: Use quantized sampling
./pixel-tui-cli -i complex_photo.jpg -w 32 -h 32 -s quantized

# Problem: Colors look washed out
./pixel-tui-cli -i pixel_art.png -w 32 -h 32 -s quantized
# Solution: Use nearest sampling for pixel art
./pixel-tui-cli -i pixel_art.png -w 32 -h 32 -s nearest
```

### Size and Performance Issues
```bash
# Problem: Image appears too small
./pixel-tui-cli -i image.png -w 16 -h 16 -s nearest
# Solution: Increase output dimensions
./pixel-tui-cli -i image.png -w 48 -h 36 -s nearest

# Problem: Processing takes too long
./pixel-tui-cli -i huge_image.png -w 128 -h 128 -s interpolated
# Solution: Reduce size and use quantized sampling
./pixel-tui-cli -i huge_image.png -w 32 -h 32 -s quantized

# Problem: Animation is too fast/slow
# Solution: Use speed controls in animation mode
# Press 'a' to enter animation mode, then '+' or '-' to adjust speed
```

### File Format Issues
```bash
# Problem: Unsupported format
./pixel-tui-cli -i image.bmp
# Solution: Convert first
convert image.bmp image.png
./pixel-tui-cli -i image.png -w 32 -h 32 -s nearest

# Problem: Corrupted GIF
./pixel-tui-cli -i broken.gif
# Solution: Try to repair or convert
ffmpeg -i broken.gif -pix_fmt rgb24 fixed.gif
./pixel-tui-cli -i fixed.gif -w 32 -h 32 -s quantized
```

## Advanced Usage Patterns

### Custom Aspect Ratio Handling
```bash
# Maintain aspect ratio for different image types

# For wide images (16:9)
./pixel-tui-cli -i wide_image.png -w 64 -h 36 -s quantized

# For tall images (9:16)
./pixel-tui-cli -i tall_image.png -w 18 -h 32 -s quantized

# For square images (1:1)
./pixel-tui-cli -i square_image.png -w 32 -h 32 -s nearest
```

### Quality vs Performance Optimization
```bash
# Maximum quality (slow, large palette)
./pixel-tui-cli -i image.png -w 64 -h 64 -s interpolated --scale 20

# Balanced quality (medium speed, moderate palette)
./pixel-tui-cli -i image.png -w 32 -h 32 -s quantized --scale 10

# Maximum performance (fast, small palette)
./pixel-tui-cli -i image.png -w 16 -h 16 -s quantized --scale 5
```

### Export Workflow Optimization
```bash
# Batch export with consistent settings
for img in *.png; do
    echo "Exporting: $img"
    # Use echo to auto-press 's' then 'q'
    echo -e "s\nq" | ./pixel-tui-cli -i "$img" -w 32 -h 32 -s nearest --scale 15
done

# Results in export_*.png files with 480x480 resolution
```

## Expected Results Guide

### Visual Quality Expectations

#### Pixel Art Images
- **Input**: 16x16 to 64x64 pixel sprites
- **Recommended**: `-w 32 -h 32 -s nearest`
- **Expected Result**: Crisp, blocky appearance with exact color preservation
- **Color Count**: 5-50 colors typically

#### Photographs
- **Input**: Any resolution photos
- **Recommended**: `-w 48 -h 36 -s quantized`
- **Expected Result**: Recognizable subjects with reduced color noise
- **Color Count**: 10-50 colors typically

#### Digital Art
- **Input**: Illustrations, paintings, artwork
- **Recommended**: `-w 40 -h 40 -s interpolated`
- **Expected Result**: Smooth gradients and color transitions
- **Color Count**: 50-200+ colors typically

#### Animated GIFs
- **Input**: Any GIF animation
- **Recommended**: `-w 32 -h 32 -s quantized`
- **Expected Result**: Smooth animation with reduced color palette
- **Performance**: 5-15 FPS depending on complexity

This comprehensive guide provides practical examples for every aspect of the CLI tool, from basic usage to advanced integration scenarios. Users can follow these examples to achieve optimal results for their specific use cases.

