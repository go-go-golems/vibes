# Cyberpunk Eboy TUI

A terminal-based cyberpunk interface with maximum visual chaos, animated ASCII creatures, and retro aesthetics. Built with Go using Charm Bracelet's Bubbletea and Lipgloss.

## Features

🔥 **Maximum Visual Chaos**
- Animated window controls and glitch effects
- Dynamic background patterns with cyberpunk symbols
- Matrix rain effects across the entire interface
- Floating animated creatures (skulls, ghosts, lightning)

🎮 **Animated ASCII Creatures**
- Pixel cat with mouth movement and bounce effects
- Pixel alien with eye blinking and pulse effects  
- Animated heart with heartbeat rhythm
- 3-frame fire animation with color gradients

⚡ **Cyberpunk Aesthetics**
- Hot pink, electric cyan, and neon lime color palette
- Gradient simulation using ASCII characters
- Glitch mode with character corruption effects
- Retro terminal styling with ANSI colors

🎵 **Interactive Elements**
- Real-time music visualizer with animated bars
- System metrics with animated progress bars
- Terminal output with various animation effects
- Responsive layout that adapts to terminal size

## Quick Start

### Prerequisites
- Go 1.24.6 or later
- Terminal with 256-color support
- Minimum terminal size: 80×25 (recommended: 120×30)

### Installation
```bash
git clone <repository>
cd cyberpunk-tui
go mod tidy
go build -o cyberpunk-tui .
```

### Running
```bash
# Normal mode (stable, no automatic glitch effects)
TERM=xterm-256color ./cyberpunk-tui

# Chaos mode (maximum visual chaos with automatic glitch effects)
TERM=xterm-256color ./cyberpunk-tui --glitch
```

### Controls
- `q` or `Ctrl+C` - Quit
- `r` - Refresh interface
- `g` - Trigger manual glitch mode (only available with --glitch flag)

### Command-line Options
- `--glitch` - Enable automatic glitch mode (warning: maximum chaos!)
- `--help` - Show help information

## Testing Components

Individual components can be tested using the testing application:

```bash
cd testing
go run test_app.go <command>
```

Available commands:
- `topbar` - Test animated top bar
- `left-sidebar` - Test left sidebar with mascots
- `right-sidebar` - Test right sidebar with metrics
- `editor` - Test code editor with visualizer
- `terminal` - Test terminal with animated output
- `sprites` - Test matrix rain and floating creatures
- `full-layout` - Test complete interface

### Animation Testing
```bash
# Show 10 animation frames of a component
go run test_app.go animate topbar 10

# Save 15 frames to directory
go run test_app.go save-frames sprites 15 output/
```

## Architecture

```
cyberpunk-tui/
├── main.go                 # Main application
├── animations/             # Animation timing system
├── styles/                 # Color palette and theming
├── models/                 # Base interfaces
├── components/             # UI components
│   ├── topbar/            # Animated top bar
│   ├── sidebar/           # Left/right sidebars
│   ├── editor/            # Code editor panel
│   ├── terminal/          # Terminal panel
│   └── sprites/           # Matrix rain system
└── testing/               # Component testing tools
```

## Component Features

### Top Bar
- Animated window controls (orange circle, cyan square, pink diamond)
- Glitch effects on title with random character corruption
- Real-time clock display

### Left Sidebar - "CHAOS TOOLKIT"
- 8 animated tool buttons with pulsing effects
- Pixel cat mascot with mouth animation and bounce
- Pixel alien with eye blinking and opacity pulse
- Pink→Purple→Cyan gradient background

### Right Sidebar - "NEURAL STATUS"  
- System metrics with animated progress bars
- 8 module status lines with cycling prefixes
- Animated pixel heart with heartbeat effect
- 3-frame fire animation with color gradient
- Cyan→Purple→Pink gradient background

### Code Editor
- Dynamic background pattern with cyberpunk symbols
- Music visualizer with 20 animated bars
- Large ASCII art "NEURAL CHAOS" display
- Animated cursor moving through code

### Terminal Panel
- 5 output lines with different animation types
- Pulse, bounce, ping, and cursor effects
- Cyberpunk-themed status messages

### Sprite System
- Matrix rain falling across entire screen
- Floating creatures with unique behaviors
- Skull, ghost, and lightning sprites
- Proper z-order layering

## Color Palette

- **Hot Pink** (#EC4899) - Primary accent
- **Electric Cyan** (#22D3EE) - Secondary accent  
- **Neon Lime** (#84CC16) - Active/success
- **Deep Purple** (#7E22CE) - Background accent
- **Orange Flame** (#FB923C) - Energy/warning
- **Matrix Green** (#22C55E) - Matrix rain
- **Pure Black** (#000000) - Background
- **Ghost White** (#F8FAFC) - Text

## Performance

- 100ms animation tick rate for smooth effects
- Efficient ANSI color usage
- Bounded sprite system prevents memory bloat
- Responsive resize handling
- Minimal CPU usage with optimized rendering

## Documentation

- `IMPLEMENTATION_REPORT.md` - Comprehensive technical documentation
- `animation_frames/` - Saved animation frames for verification
- `screenshot*.txt` - Terminal captures showing the interface
- `testing/` - Component testing and debugging tools

## Troubleshooting

**Colors not displaying correctly:**
```bash
export TERM=xterm-256color
./cyberpunk-tui
```

**Interface appears broken:**
- Ensure terminal is at least 80×25 characters
- Try resizing terminal window
- Press `r` to refresh the interface

**Components not animating:**
- Check that terminal supports ANSI escape sequences
- Verify Go version is 1.24.6 or later

## License

MIT License - see LICENSE file for details.

---

*Experience maximum cyberpunk chaos in your terminal! 🔥⚡🎮*

