# Development Guide

This guide provides detailed information for developers who want to understand, modify, or extend the Bubbletea Todo App.

## Architecture Overview

The application follows a clean architecture pattern with clear separation of concerns:

### Core Layers

1. **Domain Layer** (`internal/todo/todo.go`)
   - Pure business logic
   - No external dependencies
   - Contains `TodoItem` and `TodoList` structs

2. **Application Layer** (`internal/todo/tui.go`)
   - Bubbletea model implementation
   - UI state management
   - Event handling logic

3. **Infrastructure Layer** (`internal/websocket/`)
   - WebSocket communication
   - Protocol translation
   - Network handling

4. **Presentation Layer** (`web/static/`)
   - HTML/CSS/JavaScript frontend
   - Browser event handling
   - Terminal rendering

### Data Flow

```
Browser Events → WebSocket → Go Handler → Bubbletea Model → Render → WebSocket → Browser
```

## Key Components Deep Dive

### Todo Model (`internal/todo/todo.go`)

The core data structures and business logic:

```go
type TodoItem struct {
    ID          int       `json:"id"`
    Text        string    `json:"text"`
    Completed   bool      `json:"completed"`
    CreatedAt   time.Time `json:"created_at"`
    CompletedAt *time.Time `json:"completed_at,omitempty"`
}

type TodoList struct {
    Items      []TodoItem `json:"items"`
    nextID     int
    selectedID int
}
```

**Key Methods:**
- `AddItem(text string)`: Adds a new todo item
- `ToggleItem(index int)`: Toggles completion status
- `DeleteItem(index int)`: Removes an item
- `MoveSelectionUp/Down()`: Navigation methods

### Bubbletea Model (`internal/todo/tui.go`)

Implements the Bubbletea pattern:

```go
type Model struct {
    TodoList    *TodoList
    Mode        AppMode
    InputText   string
    Width       int
    Height      int
    Styles      Styles
}
```

**Bubbletea Methods:**
- `Init() tea.Cmd`: Initialize the model
- `Update(tea.Msg) (tea.Model, tea.Cmd)`: Handle messages
- `View() string`: Render the interface

**Application Modes:**
- `ModeList`: Normal todo list view
- `ModeAdd`: Adding new todo items

### WebSocket Handler (`internal/websocket/handler.go`)

Manages real-time communication:

```go
type TerminalSession struct {
    conn     *websocket.Conn
    model    *todo.Model
    send     chan []byte
    done     chan bool
    mutex    sync.RWMutex
}
```

**Key Functions:**
- `HandleWebSocket()`: Upgrades HTTP to WebSocket
- `readPump()`: Handles incoming messages
- `writePump()`: Sends outgoing messages
- `processKeyEvent()`: Converts web events to model updates

### Frontend (`web/static/`)

**HTML Structure:**
- Terminal container with header
- Content area for rendered output
- Help section with keyboard shortcuts

**CSS Features:**
- Terminal window styling
- Responsive design
- ANSI color support (placeholder)
- Monospace font handling

**JavaScript Functionality:**
- WebSocket connection management
- Keyboard event capture
- Terminal rendering
- Auto-reconnection

## Message Protocol

### Client to Server

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

### Server to Client

```json
{
  "type": "render",
  "data": {
    "content": "📝 Todo List\nTasks: 1 completed, 3 total\n...",
    "mode": 0
  }
}
```

## Styling System

The application uses Lipgloss for terminal styling:

```go
type Styles struct {
    Title       lipgloss.Style
    Subtitle    lipgloss.Style
    TodoItem    lipgloss.Style
    Selected    lipgloss.Style
    Completed   lipgloss.Style
    Input       lipgloss.Style
    Help        lipgloss.Style
    StatusBar   lipgloss.Style
}
```

## Testing Strategy

### TUI Testing with tmux

```bash
# Start tmux session
tmux new-session -d -s test -x 80 -y 24

# Run application
tmux send-keys -t test './tui-app' Enter

# Capture screenshot
tmux capture-pane -t test -p > screenshot.txt

# Send key events
tmux send-keys -t test Space  # Toggle todo
tmux send-keys -t test 'a'    # Add mode
```

### Web Testing

1. Start web server
2. Navigate to localhost:8080
3. Test keyboard interactions
4. Verify WebSocket communication
5. Check responsive design

## Extension Points

### Adding New Features

1. **New Todo Operations**
   ```go
   // Add to TodoList
   func (tl *TodoList) ArchiveCompleted() {
       // Implementation
   }
   ```

2. **New UI Modes**
   ```go
   const (
       ModeList AppMode = iota
       ModeAdd
       ModeEdit  // New mode
   )
   ```

3. **Persistence**
   ```go
   func (tl *TodoList) SaveToFile(filename string) error {
       // JSON serialization
   }
   
   func LoadFromFile(filename string) (*TodoList, error) {
       // JSON deserialization
   }
   ```

### WebSocket Extensions

Add new message types:

```go
switch msg.Type {
case "keypress":
    // Existing handler
case "mouse_click":
    // New handler for mouse events
case "resize":
    // Existing handler
}
```

### Frontend Enhancements

1. **ANSI Color Support**
   - Parse ANSI escape sequences
   - Convert to HTML/CSS classes
   - Implement color themes

2. **Mobile Improvements**
   - Touch gesture support
   - Virtual keyboard optimization
   - Responsive layout adjustments

## Performance Considerations

### Memory Management

- Use channels with appropriate buffer sizes
- Implement proper cleanup in WebSocket handlers
- Avoid memory leaks in long-running sessions

### Concurrency

- WebSocket handlers run in separate goroutines
- Use mutexes to protect shared state
- Implement graceful shutdown

### Network Optimization

- Minimize message frequency
- Compress large renders if needed
- Implement reconnection logic

## Debugging

### TUI Debugging

```bash
# Run with debug output
go run ./cmd/tui 2> debug.log

# Use tmux for isolation
tmux new-session -d -s debug
```

### WebSocket Debugging

```javascript
// Browser console
const ws = new WebSocket('ws://localhost:8080/ws');
ws.onmessage = (event) => {
    console.log('Received:', JSON.parse(event.data));
};
```

### Go Debugging

```go
// Add logging
log.Printf("Processing key event: %+v", keyEvent)

// Use delve debugger
dlv debug ./cmd/web
```

## Deployment

### Local Development

```bash
make dev  # Start development server
```

### Production Considerations

1. **Security**
   - Implement authentication if needed
   - Use HTTPS/WSS in production
   - Validate all input

2. **Scalability**
   - Consider session management
   - Implement load balancing
   - Add monitoring

3. **Configuration**
   - Environment variables
   - Configuration files
   - Feature flags

## Contributing Guidelines

1. **Code Style**
   - Follow Go conventions
   - Use gofmt and golint
   - Write clear comments

2. **Testing**
   - Add unit tests for new features
   - Test both TUI and web interfaces
   - Include integration tests

3. **Documentation**
   - Update README for user-facing changes
   - Update this guide for developer changes
   - Include code examples

## Future Enhancements

### Planned Features

1. **Data Persistence**
   - File-based storage
   - Database integration
   - Cloud synchronization

2. **Advanced UI**
   - Multiple todo lists
   - Categories and tags
   - Due dates and reminders

3. **Collaboration**
   - Multi-user support
   - Real-time collaboration
   - Shared todo lists

4. **Mobile App**
   - Native mobile applications
   - Offline support
   - Push notifications

### Technical Improvements

1. **Performance**
   - Optimize rendering
   - Implement virtual scrolling
   - Add caching

2. **Accessibility**
   - Screen reader support
   - Keyboard navigation improvements
   - High contrast themes

3. **Internationalization**
   - Multi-language support
   - Localized date formats
   - RTL text support

