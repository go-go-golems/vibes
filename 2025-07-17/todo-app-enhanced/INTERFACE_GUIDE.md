# 🌐 Bubbletea Todo App - Interface Access Guide

## 🚀 Quick Start

### 1. Build the Applications
```bash
cd bubbletea-todo-app
make build-enhanced
```

### 2. Start the Enhanced Web Server
```bash
# Default port (8080)
./web-app-enhanced

# Custom port
./web-app-enhanced --port 3000
./web-app-enhanced -p 9090
PORT=5000 ./web-app-enhanced
```

## 🖥️ Available Interfaces

### 🎯 **Full-Screen Terminal** (Recommended)
- **URL**: `http://localhost:8080/` (root path)
- **Description**: Immersive full-screen terminal experience
- **Features**: 
  - Complete viewport utilization
  - No distracting UI elements
  - Professional terminal aesthetics
  - Perfect for focus and productivity

### 🎨 **Enhanced Terminal with UI**
- **URL**: `http://localhost:8080/enhanced`
- **Description**: Terminal with additional UI elements and information
- **Features**:
  - Terminal window with header
  - Connection status indicator
  - Feature descriptions and keyboard shortcuts
  - Educational and demonstration-friendly

### 📊 **Health Check**
- **URL**: `http://localhost:8080/health`
- **Description**: JSON endpoint for monitoring
- **Response**: `{"status":"ok","service":"bubbletea-todo-enhanced"}`

## 🔧 File Structure Explanation

### JavaScript Files
- **`terminal-fullscreen.js`**: Powers both full-screen and enhanced interfaces
- **`terminal.js`**: Original basic terminal (for legacy web-app)

### HTML Files
- **`index-fullscreen.html`**: Full-screen interface (served at `/`)
- **`index-enhanced.html`**: Enhanced interface with UI (served at `/enhanced`)
- **`index.html`**: Original interface (for legacy web-app)

### CSS Files
- **`style-fullscreen.css`**: Full-screen terminal styling
- **`style-enhanced.css`**: Enhanced interface styling
- **`style.css`**: Original interface styling

## 🎮 Interface Comparison

| Feature | Full-Screen (`/`) | Enhanced (`/enhanced`) |
|---------|-------------------|------------------------|
| **Screen Usage** | 100% viewport | Windowed with UI |
| **Distractions** | None | Header, info panels |
| **Best For** | Production use | Demos, learning |
| **Terminal Feel** | Authentic | Styled window |
| **Mobile** | Optimized | Good |

## 🚀 Recommended Usage

### For Development & Production
```bash
# Start server
./web-app-enhanced --port 8080

# Access full-screen interface
open http://localhost:8080
```

### For Demonstrations & Learning
```bash
# Start server
./web-app-enhanced --port 8080

# Access enhanced interface with UI
open http://localhost:8080/enhanced
```

### For Different Ports
```bash
# Development on port 3000
./web-app-enhanced --port 3000
open http://localhost:3000

# Production on port 80 (requires sudo)
sudo ./web-app-enhanced --port 80
open http://localhost
```

## ⌨️ Keyboard Controls (Both Interfaces)

### Navigation
- **↑** or **k**: Move selection up
- **↓** or **j**: Move selection down

### Actions
- **Space** or **Enter**: Toggle todo completion
- **a**: Enter add mode
- **d** or **x**: Delete selected todo
- **q**: Quit (TUI only, not web)

### Add Mode
- **Type text**: Enter todo description
- **Enter** or **Shift+Enter**: Save todo
- **Esc**: Cancel and return to list

### Advanced Keys (Tested & Working)
- **Ctrl+C**: Gracefully handled
- **Alt+[key]**: Processed correctly
- **Shift+Enter**: Alternative save in add mode
- **Complex combinations**: All supported

## 🔍 Troubleshooting

### Interface Not Loading
1. **Check server is running**: Look for startup messages
2. **Verify port**: Ensure no conflicts with other services
3. **Check browser console**: Look for JavaScript errors
4. **Try different interface**: Test both `/` and `/enhanced`

### JavaScript Errors
- **File not found**: Ensure all files are in `web/static/` directory
- **xterm.js not loading**: Check internet connection for CDN
- **WebSocket errors**: Verify server is running and accessible

### Port Issues
```bash
# Check if port is in use
netstat -tulpn | grep :8080

# Use different port
./web-app-enhanced --port 9090
```

## 📱 Mobile Access

Both interfaces work on mobile devices:

### Full-Screen Interface
- Optimized for touch
- Virtual keyboard support
- Gesture-friendly

### Enhanced Interface
- Touch-friendly controls
- Responsive design
- Good for mobile demos

## 🎯 Best Practices

### For Focus & Productivity
- Use **full-screen interface** (`/`)
- Hide browser UI (F11 in most browsers)
- Use keyboard shortcuts exclusively

### For Sharing & Demos
- Use **enhanced interface** (`/enhanced`)
- Show keyboard shortcuts panel
- Explain features using UI elements

### For Development
- Use custom ports to avoid conflicts
- Monitor health endpoint for status
- Test both interfaces during development

## 🔧 Advanced Configuration

### Environment Variables
```bash
export PORT=3000
export NODE_ENV=production
./web-app-enhanced
```

### Makefile Shortcuts
```bash
# Development with custom port
make dev PORT=3000

# Enhanced interface specifically
make web-enhanced PORT=9090
```

### Production Deployment
```bash
# Build optimized version
make build-enhanced

# Run with production settings
PORT=80 ./web-app-enhanced
```

---

**🎉 Enjoy your enhanced Bubbletea todo experience with full ANSI colors and professional terminal emulation!**

