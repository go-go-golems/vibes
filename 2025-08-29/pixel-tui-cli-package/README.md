# Pixel Art TUI Renderer - CLI Version

A flexible command-line terminal user interface for rendering pixel art images and animations with configurable size and color sampling options.

## 🚀 Quick Start

```bash
# View an image with default settings
./pixel-tui-cli -i image.png

# Custom size and color sampling
./pixel-tui-cli -i image.png -w 48 -h 36 -s quantized

# Animated GIF with verbose output
./pixel-tui-cli -i animation.gif -w 32 -h 32 -s quantized -v
```

## 📦 Package Contents

```
pixel-tui-cli-package/
├── pixel-tui-cli              # Compiled executable (ready to use)
├── README.md                  # This file
├── source/                    # Source code
│   ├── main_cli.go           # Main CLI application
│   ├── go.mod                # Go module file
│   └── go.sum                # Go dependencies
├── documentation/             # Comprehensive guides
│   ├── CLI_IMPLEMENTATION_GUIDE.md  # Technical implementation details
│   └── USAGE_EXAMPLES.md            # Practical usage examples
├── examples/                  # Example scripts
│   ├── demo_script.sh        # Interactive demonstration
│   ├── batch_process.sh      # Batch processing example
│   └── quality_comparison.sh # Quality analysis tool
└── original-images/          # Test images
    ├── pasted_file_*.png     # Various pixel art samples
    └── blockbob.gif          # Animated GIF example
```

## 🎯 Key Features

### **Universal Image Support**
- PNG, JPG, JPEG, and GIF files
- Automatic format detection
- Animated GIF playback with controls

### **Configurable Output**
- Custom pixel dimensions (width × height)
- Maintains aspect ratio or forces specific dimensions
- Optimized for terminal display constraints

### **Color Sampling Control**
- **Nearest**: Exact colors from original (best for pixel art)
- **Quantized**: Reduced color palette (best for photos)
- **Interpolated**: Smooth gradients (best for artwork)

### **Export Functionality**
- Save terminal renderings as PNG files
- Configurable scaling (5x to 20x)
- High-quality pixel-perfect exports

## 🎨 Color Sampling Methods

| Method | Best For | Color Count | Quality | Performance |
|--------|----------|-------------|---------|-------------|
| **Nearest** | Pixel art, logos | 50-500+ | Highest fidelity | Fastest |
| **Quantized** | Photos, complex images | 10-50 | Balanced | Fast |
| **Interpolated** | Artwork, gradients | 100-1000+ | Smooth transitions | Moderate |

## 🛠 Command Line Options

```bash
Usage: ./pixel-tui-cli [options]

Required:
  -i, --input <file>     Input image file (PNG, JPG, or GIF)

Optional:
  -w, --width <pixels>   Output width in pixels (default: 32)
  -h, --height <pixels>  Output height in pixels (default: 32)
  -s, --sampling <method> Color sampling: nearest, quantized, interpolated (default: nearest)
  --scale <factor>       PNG export scale factor (default: 10)
  -v, --verbose          Enable verbose output
  --help                 Show help information
```

## 🎮 Interactive Controls

### Static Images
- **`s`**: Save current view as PNG
- **`q`**: Quit application

### Animated GIFs
- **`a`**: Toggle animation mode
- **`SPACE`**: Pause/resume animation
- **`+/-`**: Adjust animation speed
- **`s`**: Save current frame as PNG
- **`q`**: Quit application

## 📋 Usage Examples

### Basic Image Viewing
```bash
# Default settings (32x32, nearest sampling)
./pixel-tui-cli -i sprite.png

# Custom dimensions
./pixel-tui-cli -i image.png -w 64 -h 48

# With verbose output
./pixel-tui-cli -i image.png -w 32 -h 32 -v
```

### Color Sampling Comparison
```bash
# Pixel art (exact colors)
./pixel-tui-cli -i pixel_art.png -w 32 -h 32 -s nearest

# Photo (reduced colors)
./pixel-tui-cli -i photo.jpg -w 48 -h 36 -s quantized

# Artwork (smooth gradients)
./pixel-tui-cli -i artwork.png -w 40 -h 40 -s interpolated
```

### Animation Processing
```bash
# Animated GIF with optimal settings
./pixel-tui-cli -i animation.gif -w 32 -h 32 -s quantized

# Large animation with verbose output
./pixel-tui-cli -i large_anim.gif -w 48 -h 36 -s quantized -v
```

## 🔧 Building from Source

If you need to rebuild the application:

```bash
cd source/
go build -o ../pixel-tui-cli main_cli.go
```

### Requirements
- Go 1.19+ 
- Terminal with ANSI color support
- Dependencies automatically downloaded via Go modules

## 📊 Example Scripts

### Run Interactive Demo
```bash
cd examples/
./demo_script.sh
```

### Batch Process Images
```bash
cd examples/
./batch_process.sh
```

### Quality Analysis
```bash
cd examples/
./quality_comparison.sh ../original-images/image.png
```

## 🎯 Recommended Settings

### For Different Image Types

#### Pixel Art & Sprites
```bash
./pixel-tui-cli -i sprite.png -w 32 -h 32 -s nearest --scale 15
```
- Preserves exact pixel boundaries
- Maintains original color palette
- Best for 8-bit and 16-bit style graphics

#### Photographs
```bash
./pixel-tui-cli -i photo.jpg -w 48 -h 36 -s quantized --scale 10
```
- Reduces color noise
- Better terminal compatibility
- Recognizable subjects at small sizes

#### Digital Artwork
```bash
./pixel-tui-cli -i artwork.png -w 40 -h 40 -s interpolated --scale 12
```
- Preserves smooth gradients
- Maintains artistic quality
- Good for illustrations and paintings

#### Animated GIFs
```bash
./pixel-tui-cli -i animation.gif -w 32 -h 32 -s quantized
```
- Balanced performance and quality
- Smooth animation playback
- Reduced color palette for consistency

## 📈 Performance Guidelines

### Output Size Impact
- **16×16**: Very fast, minimal detail
- **32×32**: Fast, good balance (recommended)
- **48×36**: Moderate, more detail
- **64×48**: Slower, maximum detail

### Color Sampling Performance
- **Nearest**: Fastest processing
- **Quantized**: Fast with color reduction
- **Interpolated**: Moderate, preserves gradients

### Memory Usage
- Typical: 1-5MB depending on image complexity
- Large animations: Up to 10MB
- Export operations: Additional 2-3MB temporarily

## 🔍 Troubleshooting

### Common Issues

#### "File does not exist"
```bash
# Use absolute path
./pixel-tui-cli -i /full/path/to/image.png

# Check current directory
ls -la *.png
```

#### "Too many colors" in terminal
```bash
# Use quantized sampling
./pixel-tui-cli -i image.png -s quantized
```

#### Image appears too small/large
```bash
# Adjust dimensions
./pixel-tui-cli -i image.png -w 48 -h 36  # Larger
./pixel-tui-cli -i image.png -w 16 -h 12  # Smaller
```

#### Slow processing
```bash
# Reduce size and use quantized sampling
./pixel-tui-cli -i large_image.png -w 32 -h 32 -s quantized
```

### Getting Help

1. **Built-in help**: `./pixel-tui-cli --help`
2. **Documentation**: See `documentation/` folder
3. **Examples**: Run scripts in `examples/` folder
4. **Verbose mode**: Add `-v` flag for detailed output

## 🎨 Export Workflow

### Standard Export
```bash
# Load image
./pixel-tui-cli -i image.png -w 32 -h 32 -s nearest

# In TUI: Press 's' to save as PNG
# Result: export_image.png (320×320 pixels with 10x scaling)
```

### High-Resolution Export
```bash
# Use larger scale factor
./pixel-tui-cli -i image.png -w 32 -h 32 -s nearest --scale 20

# In TUI: Press 's' to save
# Result: export_image.png (640×640 pixels)
```

### Animation Frame Export
```bash
# Load GIF
./pixel-tui-cli -i animation.gif -w 32 -h 32 -s quantized

# In TUI: 
# 1. Press 'a' for animation mode
# 2. Wait for desired frame
# 3. Press 's' to save frame
# Result: export_animation_frame_X.png
```

## 🌟 Advanced Features

### Aspect Ratio Handling
The tool automatically handles different aspect ratios:
- Wide images (16:9): Use `-w 64 -h 36`
- Tall images (9:16): Use `-w 18 -h 32`
- Square images (1:1): Use `-w 32 -h 32`

### Terminal Compatibility
- Works with any ANSI-capable terminal
- Optimized for common terminal sizes
- Automatic color fallback for limited terminals

### Cross-Platform Support
- Linux (tested)
- macOS (compatible)
- Windows (with proper terminal)

## 📝 License & Credits

This tool demonstrates advanced terminal UI techniques using:
- **Go**: Programming language
- **Bubbletea**: Terminal UI framework
- **Lipgloss**: Terminal styling library

Built for pixel art enthusiasts, game developers, and terminal UI enthusiasts.

---

**Ready to start?** Try: `./pixel-tui-cli -i original-images/pasted_file_4jP02c_image.png -w 32 -h 24 -s nearest`

