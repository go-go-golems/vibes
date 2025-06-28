# Lipgloss v2 Complete Course Package

This package contains everything you need to master terminal UI development with Lipgloss v2.

## 📦 Package Contents

### `/lipgloss-course/` - Core Course Materials
- **`examples/`** - 10 progressive Go examples (01-10)
  - Basic styling, layers, canvas, positioning
  - Complex layering, Z-index, nested structures
  - Dynamic positioning, color showcase, borders
- **`vhs-recordings/`** - VHS tape files and generated GIFs
  - All 10 examples recorded as animated GIFs
  - VHS tape files for regenerating recordings
- **`go.mod`** - Go module with Lipgloss v2 dependencies

### `/lipgloss-course-website/` - Interactive Website
- **React-based course website** with all examples
- **Source code viewing** with syntax highlighting
- **Interactive demonstrations** with live GIFs
- **Quality assurance dashboard** showing validation results

## 🚀 Quick Start

### Running the Examples
```bash
cd lipgloss-course/examples
go run 01-basic-styling.go
go run 02-simple-layers.go
# ... and so on
```

### Running the Website Locally
```bash
cd lipgloss-course-website
npm install
npm run dev
```

### Regenerating GIFs (requires VHS)
```bash
cd lipgloss-course/vhs-recordings
vhs basic_styling.tape
# ... for each tape file
```

## 📚 Learning Path

1. **Beginner** (Examples 1-3, 9-10)
   - Basic styling and colors
   - Simple layering concepts
   - Canvas fundamentals
   - Border and color galleries

2. **Intermediate** (Examples 4, 6)
   - Advanced positioning
   - Z-index management
   - Layer stacking

3. **Advanced** (Examples 5, 7-8)
   - Complex desktop environments
   - Modal dialogs and nested structures
   - Dynamic positioning and animation

## 🔧 Requirements

- **Go 1.23.0+** (required for Lipgloss v2)
- **Node.js 18+** (for website development)
- **VHS** (optional, for regenerating GIFs)

## ✅ Validation Status

- **10/10 examples** compile successfully
- **10/10 GIFs** generated and validated
- **100% success rate** in automated testing
- **Text screenshot validation** enabled

## 🌐 Live Demo

Visit the deployed website: https://pkhwvyrx.manus.space

## 📖 Key Concepts Covered

- **Layer Management**: X/Y positioning, Z-index stacking
- **Canvas System**: Background management, bounds calculation
- **Advanced Styling**: Colors, borders, padding, typography
- **Dynamic UIs**: Animation simulation, interactive movement
- **Desktop Patterns**: Window management, modal dialogs
- **Best Practices**: Professional patterns and optimization

## 🛠️ Development Tools

- **VHS**: Terminal recording and GIF generation
- **Text Screenshots**: Programmatic validation support
- **Go Modules**: Modern dependency management
- **React**: Interactive documentation website

## 📝 Notes

- All examples use the latest Lipgloss v2 beta API
- VHS recordings capture actual terminal output
- Website includes source code for easy copying
- Examples progress from basic to advanced concepts

Happy coding with Lipgloss v2! 🎨✨
