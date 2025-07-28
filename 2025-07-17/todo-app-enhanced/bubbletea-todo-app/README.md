# Bubbletea Todo App

A modern todo list application built with Go and Charmbracelet Bubbletea that works both as a terminal user interface (TUI) and as a web application with real-time terminal rendering over websockets.

## Features

- **Dual Interface**: Works as both a standalone TUI application and a web application
- **Real-time Synchronization**: Web interface renders the actual terminal output via websockets
- **Full Todo Management**: Add, toggle, delete, and navigate through todo items
- **Keyboard Navigation**: Vim-style navigation (hjkl) and arrow keys
- **Beautiful UI**: Styled with Lipgloss for an attractive terminal interface
- **Responsive Web Design**: Mobile-friendly web interface with terminal aesthetics

## Architecture

The application consists of several key components:

### Core Components

1. **Todo Model** (`internal/todo/todo.go`): Core data structures and business logic
2. **TUI Interface** (`internal/todo/tui.go`): Bubbletea model implementation
3. **WebSocket Handler** (`internal/websocket/handler.go`): Real-time web communication
4. **TUI Application** (`cmd/tui/main.go`): Standalone terminal application
5. **Web Server** (`cmd/web/main.go`): HTTP server with websocket support
6. **Frontend** (`web/static/`): HTML, CSS, and JavaScript for web interface

### Key Features

- **Terminal Emulation**: The web interface renders the actual terminal output
- **Event Translation**: JavaScript keyboard events are converted to Bubbletea messages
- **State Synchronization**: Both interfaces share the same underlying todo state
- **Cross-platform**: Works on any system with Go support

## Installation

### Prerequisites

- Go 1.23.4 or later
- Modern web browser (for web interface)

### Building from Source

1. Clone or download the project
2. Navigate to the project directory
3. Install dependencies:
   ```bash
   go mod download
   ```
4. Build the applications:
   ```bash
   # Build TUI application
   go build -o tui-app ./cmd/tui
   
   # Build web application
   go build -o web-app ./cmd/web
   ```

## Usage

### Terminal User Interface (TUI)

Run the standalone TUI application:

```bash
./tui-app
```

#### TUI Controls

- **↑/k**: Move selection up
- **↓/j**: Move selection down
- **Space/Enter**: Toggle todo completion
- **a**: Add new todo item
- **d/x**: Delete selected todo item
- **q**: Quit application
- **Esc**: Cancel (when in add mode)

### Web Application

1. Start the web server:
   ```bash
   ./web-app
   ```

2. Open your browser and navigate to:
   ```
   http://localhost:8080
   ```

The web interface provides the same functionality as the TUI, with keyboard controls working through the browser.

#### Web Controls

The web interface supports all the same keyboard shortcuts as the TUI:

- **Arrow Keys/hjkl**: Navigation
- **Space/Enter**: Toggle todos
- **a**: Add new todo
- **d/x**: Delete todo
- **Esc**: Cancel operations

## Project Structure

```
bubbletea-todo-app/
├── cmd/
│   ├── tui/
│   │   └── main.go          # TUI application entry point
│   └── web/
│       └── main.go          # Web server entry point
├── internal/
│   ├── todo/
│   │   ├── todo.go          # Core todo data structures
│   │   └── tui.go           # Bubbletea TUI implementation
│   └── websocket/
│       └── handler.go       # WebSocket communication handler
├── web/
│   └── static/
│       ├── index.html       # Web interface HTML
│       ├── style.css        # Styling and terminal aesthetics
│       └── terminal.js      # WebSocket and event handling
├── go.mod                   # Go module definition
├── go.sum                   # Go module checksums
└── README.md               # This documentation
```

## Technical Details

### WebSocket Communication

The web interface communicates with the Go backend through WebSocket messages:

- **Keypress Events**: Browser keyboard events are sent as JSON messages
- **Render Updates**: Terminal output is sent back as rendered text
- **Real-time Sync**: Changes are immediately reflected in the web interface

### Message Format

```json
{
  "type": "keypress",
  "data": {
    "key": "ArrowDown",
    "ctrlKey": false,
    "altKey": false,
    "shiftKey": false,
    "metaKey": false
  }
}
```

### Styling

The web interface uses CSS to create an authentic terminal appearance:

- **Terminal Window**: Styled to look like a macOS terminal
- **Monospace Font**: Uses system monospace fonts for consistency
- **Color Scheme**: Dark theme with appropriate contrast
- **Responsive Design**: Works on desktop and mobile devices

## Development

### Dependencies

- **Bubbletea**: Terminal user interface framework
- **Lipgloss**: Styling for terminal applications
- **Gorilla WebSocket**: WebSocket implementation for Go

### Testing

The application has been thoroughly tested:

1. **TUI Testing**: Verified using tmux with screenshot capture
2. **Web Testing**: Browser-based testing of all functionality
3. **Integration Testing**: Confirmed websocket communication works correctly

### Adding Features

To extend the application:

1. **New Todo Operations**: Add methods to the `TodoList` struct
2. **UI Enhancements**: Modify the `View()` method in `tui.go`
3. **Web Features**: Update the JavaScript event handling
4. **Persistence**: Add file or database storage to the todo model

## Screenshots

### TUI Interface
The terminal interface provides a clean, keyboard-driven experience with vim-style navigation.

### Web Interface
The web application renders the terminal in a browser with full interactivity and responsive design.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly (both TUI and web interfaces)
5. Submit a pull request

## License

This project is open source and available under the MIT License.

## Acknowledgments

- **Charmbracelet Team**: For the excellent Bubbletea and Lipgloss libraries
- **Gorilla Toolkit**: For the robust WebSocket implementation
- **Go Team**: For the powerful and efficient Go programming language

