# i3 Window Manager - Go TUI Application

A terminal-based user interface for managing i3 window manager workspaces and windows, built with Go and Charmbracelet Bubbletea.

## Features

- 🖥️ **Workspace Management**: View and switch between i3 workspaces
- 🪟 **Window Management**: List and focus individual windows
- ⌨️ **Keyboard Navigation**: Intuitive vim-like navigation (j/k, arrow keys)
- 🎨 **Beautiful Interface**: Clean, colorful terminal UI with Lipgloss styling
- 🔄 **Real-time Updates**: Refresh workspace and window information
- 🖱️ **Cursor Movement**: Automatically move cursor to focused windows
- 🚀 **Fast Switching**: Quick workspace switching with number keys

## Prerequisites

- Linux system with i3 window manager
- Go 1.23+ 
- X11 display server
- xdotool (optional, for cursor movement)

## Installation

1. **Clone or download this project**

2. **Install dependencies**:
   ```bash
   go mod download
   ```

3. **Build the application**:
   ```bash
   go build -o i3-window-manager
   ```

## Usage

1. **Make sure i3 is running**

2. **Run the application**:
   ```bash
   ./i3-window-manager
   ```

3. **Navigate the interface**:
   - Use ↑/↓ or j/k to navigate
   - Press Tab to switch between workspaces and windows view
   - Press Enter to focus selected workspace/window
   - Press 1-9,0 for quick workspace switching
   - Press 'r' to refresh
   - Press 'q' to quit

## Interface

### Workspaces View
```
 i3 Window Manager 

Workspaces:

 ▶ 1 (2 windows) 
   2 (1 windows) 
   3 (0 windows) 

↑/↓: Navigate • Enter: Focus Workspace • Tab: View Windows • 1-9,0: Quick Switch • R: Refresh • Q: Quit
```

### Windows View
```
 i3 Window Manager - 1 

Windows in 1:

 ▶ Terminal [80x24 at 0,0] 
   Firefox [1920x1080 at 0,0] 

↑/↓: Navigate • Enter: Focus Window & Move Cursor • Tab: Back to Workspaces • R: Refresh • Q: Quit
```

## Key Bindings

| Key | Action |
|-----|--------|
| ↑/↓, j/k | Navigate up/down |
| Enter/Space | Focus selected workspace/window |
| Tab | Switch between workspaces and windows view |
| 1-9,0 | Quick switch to workspace (0 = workspace 10) |
| r | Refresh data |
| q, Ctrl+C | Quit program |

## Technical Details

- **Language**: Go 1.23+
- **UI Framework**: [Charmbracelet Bubbletea](https://github.com/charmbracelet/bubbletea)
- **Styling**: [Charmbracelet Lipgloss](https://github.com/charmbracelet/lipgloss)
- **i3 Integration**: [Official i3 Go library](https://github.com/i3/go-i3)
- **IPC Protocol**: Communicates with i3 via Unix socket

## Project Structure

```
i3-window-manager/
├── main.go              # Main application code
├── go.mod              # Go module definition
├── go.sum              # Go module checksums
├── demo.sh             # Demonstration script
├── README.md           # This file
└── i3-window-manager   # Compiled binary
```

## Dependencies

```go
go.i3wm.org/i3/v4                    // Official i3 IPC library
github.com/charmbracelet/bubbletea   // TUI framework
github.com/charmbracelet/lipgloss    // Styling library
```

## Development

To modify or extend the application:

1. **Edit main.go** for core functionality
2. **Run with**: `go run main.go`
3. **Build with**: `go build -o i3-window-manager`
4. **Test with**: `./demo.sh`

## Troubleshooting

**"i3 is not running or not accessible"**
- Ensure i3 window manager is running
- Check that the i3 IPC socket is accessible
- Verify DISPLAY environment variable is set correctly

**"xdotool: command not found"**
- Install xdotool: `sudo apt install xdotool`
- Or continue without cursor movement functionality

**Interface appears garbled**
- Ensure terminal supports colors and Unicode
- Try resizing terminal window
- Press 'r' to refresh the interface

## License

This project is provided as-is for educational and practical use.

## Contributing

Feel free to submit issues, feature requests, or pull requests to improve the application.

---

Built with ❤️ using Go and Charmbracelet Bubbletea

