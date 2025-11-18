# Mento Services Manager - TUI Application

A comprehensive Terminal User Interface (TUI) application for managing multiple services (Identity Server, Frontend/Vite, and Mento Worker) with real-time monitoring, logging, and configuration management.

## Features

### 🎯 Core Functionality
- **Service Management**: Start, stop, and restart individual services or all at once
- **Real-time Monitoring**: Live CPU and memory usage tracking for each service
- **Process Management**: View PIDs, ports, and service status at a glance
- **Log Aggregation**: Centralized logging with filtering by service
- **Configuration Viewer**: Display environment variables and configuration with secret masking
- **Interactive Navigation**: Keyboard-driven interface with intuitive controls

### 📊 Screens

#### 1. Dashboard
- Overview of all services with their status
- CPU and memory usage per service
- Quick actions for service management
- Service selection with visual highlighting
- Uptime tracking

#### 2. Log Viewer
- Real-time log streaming from all services
- Tab-based filtering (Identity, Frontend, Worker, All)
- Auto-scroll functionality
- Line count tracking
- Timestamp and service name display

#### 3. Configuration Viewer
- Environment sources display
- Database configuration (with password masking)
- OAuth credentials (with secret masking)
- Service configuration details
- Scrollable viewport for long configs

#### 4. Help Screen
- Comprehensive keyboard shortcuts
- Context-sensitive help
- Feature descriptions

#### 5. Command-Line Help System
- Access help documentation from the command line
- Configuration YAML reference guide
- Service management tutorials
- Usage examples and best practices

## Architecture

### Project Structure
```
mento-tui/
├── cmd/
│   └── main.go                 # Application entry point
├── internal/
│   ├── models/
│   │   └── models.go           # Data structures and types
│   ├── services/
│   │   └── manager.go          # Service lifecycle management
│   └── ui/
│       ├── app.go              # Main application model
│       ├── dashboard.go        # Dashboard screen
│       ├── logviewer.go        # Log viewer screen
│       ├── config.go           # Configuration viewer
│       ├── help.go             # Help screen
│       └── styles.go           # UI styling with Lipgloss
├── mock-binaries/
│   ├── identity-server.go      # Mock Identity Server
│   ├── frontend.go             # Mock Frontend/Vite server
│   └── worker.go               # Mock Mento Worker
├── screenshots/                # TUI screenshots
└── go.mod                      # Go module dependencies
```

### Technology Stack
- **Framework**: [Bubble Tea](https://github.com/charmbracelet/bubbletea) - TUI framework
- **UI Components**: [Bubbles](https://github.com/charmbracelet/bubbles) - TUI components
- **Styling**: [Lipgloss](https://github.com/charmbracelet/lipgloss) - Terminal styling
- **Process Monitoring**: [gopsutil](https://github.com/shirou/gopsutil) - System and process utilities
- **CLI Framework**: [Glazed](https://github.com/go-go-golems/glazed) - Command-line interface framework
- **Language**: Go 1.18+

## Installation

### Prerequisites
- Go 1.18 or higher
- Linux/Unix environment (tested on Ubuntu 22.04)
- Terminal with ANSI color support

### Build from Source
```bash
# Clone or navigate to the project directory
cd mento-tui

# Download dependencies
go mod tidy

# Build mock binaries
go build -o mock-binaries/identity-server mock-binaries/identity-server.go
go build -o mock-binaries/frontend mock-binaries/frontend.go
go build -o mock-binaries/worker mock-binaries/worker.go

# Build main application
go build -o mento-tui cmd/main.go
```

## Configuration

Mento-tui uses a YAML configuration file to define services and their settings. By default, it looks for `mento-tui.yaml` in the current directory.

### Configuration File Setup

1. Copy the example configuration:
```bash
cp mento-tui.yaml.example mento-tui.yaml
```

2. Edit `mento-tui.yaml` to match your environment:
```yaml
services:
  - name: "Identity Server"
    ports: [8083]
    binary_path: "./mock-binaries/identity-server"
    working_directory: "./identity-service"     # optional
    args: ["--port", "8083", "--debug"]         # or string: "--port 8083 --debug"
    env_vars:
      - "IDENTITY_SERVICE_PORT=8083"
    log_buffer_size: 1000                       # optional

  - name: "Frontend (Vite)"
    ports: [5173]
    binary_path: "./mock-binaries/frontend"
    args: ["--host", "0.0.0.0", "--port", "5173"]
    env_vars:
      - "VITE_PORT=5173"

  - name: "Mento Worker"
    ports: [8082, 9090]                         # multiple ports supported
    binary_path: "./mock-binaries/worker"
    args: "--config worker.yaml --verbose"      # string format also supported
    env_vars:
      - "MENTO_SERVICE_PORT=8082"

global:
  working_directory: "."      # default PWD for services (optional)
  log_buffer_size: 10000      # default global buffer size (optional)
```

### Configuration Options

#### Service Configuration
- **name** (required): Service display name
- **ports** (required): List of ports the service uses (preferred) or **port** (single port, deprecated)
- **binary_path** (required): Path to the service binary
- **working_directory** (optional): Working directory for the service process
- **args** (optional): Command-line arguments as a list `["--arg", "value"]` or string `"--arg value"`
- **env_vars** (optional): List of environment variables in `KEY=value` format
- **log_buffer_size** (optional): Per-service log buffer size (defaults to global or 1000)

#### Global Configuration
- **working_directory** (optional): Default working directory for services without explicit PWD
- **log_buffer_size** (optional): Default log buffer size (defaults to 10000)

### Using a Custom Config File

Specify a different configuration file with the `--config` flag:
```bash
./mento-tui --config /path/to/custom-config.yaml
```

## Usage

### Starting the Application

**Note**: This is a TUI (Terminal User Interface) application. For best results, run it in a terminal multiplexer like `tmux` or `screen`, or in a dedicated terminal window.

```bash
# Using the compiled binary
./mento-tui

# Or using go run
go run cmd/main.go

# With a custom config file
./mento-tui --config ./my-config.yaml

# Or with go run
go run cmd/main.go --config ./my-config.yaml
```

**Running in tmux** (recommended):
```bash
tmux new-session -d -s mento-tui 'go run cmd/main.go'
tmux attach -t mento-tui
```

### Keyboard Shortcuts

#### Global Keys
- `q` or `Ctrl+C` - Quit application
- `h` or `?` - Show help screen
- `Esc` - Go back / Return to dashboard

#### Dashboard
- `↑`/`k`, `↓`/`j` - Navigate between services
- `Enter` - Start selected service (if stopped)
- `r` - Restart selected service
- `s` - Stop selected service
- `a` - Start all services
- `x` - Stop all services
- `t` - View tail logs
- `c` - View configuration
- `e` - View environment

#### Log Viewer
- `Tab` - Switch between service tabs
- `1-4` - Jump to specific tab (Identity/Frontend/Worker/All)
- `↑`/`k`, `↓`/`j` - Scroll logs
- `g` - Go to top
- `G` - Go to bottom

#### Configuration Viewer
- `↑`/`k`, `↓`/`j` - Scroll configuration

## Help System

Mento-tui includes a comprehensive help system accessible from the command line. Access help documentation using:

```bash
# Show all available help topics
mento-tui help

# Show specific help topic
mento-tui help config-yaml-reference  # Configuration file reference
mento-tui help usage                   # Command usage guide
mento-tui help service-management      # Service management guide
mento-tui help getting-started         # Getting started tutorial

# Show command help
mento-tui --help
```

The help system provides:
- **Configuration Reference**: Complete documentation of the YAML configuration file format
- **Usage Guide**: Command-line options and usage patterns
- **Service Management Guide**: Detailed guide to using the TUI dashboard
- **Getting Started Tutorial**: Step-by-step setup instructions

## Mock Services

The application includes three mock services that simulate real service behavior:

### Identity Server (Port 8083)
- HTTP server with authentication endpoints
- WebSocket connection simulation
- Periodic activity logging
- OAuth credential management

### Frontend/Vite (Port 5173)
- HTTP server serving static content
- Hot Module Replacement (HMR) simulation
- Component compilation logging
- Development server behavior

### Mento Worker (Port 8082)
- HTTP server with health endpoints
- Background job processing simulation
- Database query logging with timing
- Document processing and workflow execution
- Calendar event syncing

## Features in Detail

### Service Management
The service manager handles the complete lifecycle of each service:
- **Process spawning**: Uses `exec.Cmd` to start binaries
- **Log capture**: Captures stdout and stderr via pipes
- **Process monitoring**: Tracks CPU and memory usage every 2 seconds
- **Graceful shutdown**: Sends SIGTERM before force kill
- **Status tracking**: Real-time status updates (Stopped, Starting, Running, Failed)

### Log Aggregation
Logs are collected from all services and stored in a circular buffer:
- **Buffer size**: 10,000 lines globally, 1,000 per service
- **Filtering**: View logs by service or all combined
- **Auto-scroll**: Automatically scroll to latest logs
- **Timestamps**: All logs include precise timestamps
- **Service tagging**: Each log line is tagged with its source service

### Configuration Management
Configuration viewer displays environment variables with security features:
- **Secret masking**: Passwords and tokens are automatically masked
- **Source tracking**: Shows which config files were loaded
- **Categorization**: Groups configs by type (Database, OAuth, Service)
- **Read-only**: Current implementation is view-only (edit coming soon)

### UI/UX Design
- **Responsive layout**: Adapts to terminal size
- **Visual hierarchy**: Uses borders, colors, and spacing effectively
- **Status indicators**: Emoji icons for quick status recognition (✅ ⭕ ⏳ ❌)
- **Selected state**: Double borders highlight selected items
- **Color coding**: Different colors for different states and elements

## Testing

The application has been thoroughly tested with:
- ✅ Starting individual services
- ✅ Starting all services at once
- ✅ Stopping individual services
- ✅ Stopping all services
- ✅ Restarting services
- ✅ Service status monitoring
- ✅ Log viewing and filtering
- ✅ Configuration display
- ✅ Navigation between screens
- ✅ Keyboard shortcuts
- ✅ Port conflict detection
- ✅ Process cleanup on exit

## Development

### Adding a New Service
1. Create a new mock binary in `mock-binaries/`
2. Add service definition to `mento-tui.yaml`:
```yaml
services:
  - name: "New Service"
    ports: [9000]
    binary_path: "./mock-binaries/new-service"
    args: ["--port", "9000"]
    env_vars:
      - "SERVICE_PORT=9000"
```

### Adding a New Screen
1. Create a new file in `internal/ui/`
2. Implement the `tea.Model` interface
3. Add screen type to `app.go`
4. Add navigation logic

### Customizing Styles
Edit `internal/ui/styles.go` to customize colors and styling:
```go
ColorPrimary   = lipgloss.Color("#7D56F4")
ColorSecondary = lipgloss.Color("#00BFFF")
ColorSuccess   = lipgloss.Color("#00FF00")
```

## Troubleshooting

### Services won't start
- Check if ports are already in use: `lsof -i :8083 -i :5173 -i :8082`
- Ensure mock binaries are executable: `chmod +x mock-binaries/*`
- Check binary paths in service definitions

### Logs not appearing
- Verify services are actually running (check PIDs)
- Check if log buffers are being filled (may take a few seconds)
- Ensure services are writing to stdout/stderr

### UI rendering issues
- Ensure terminal supports ANSI colors
- Try resizing terminal window
- Check terminal size is at least 80x24

## Future Enhancements

- [ ] Search functionality in log viewer
- [ ] Log filtering by keyword/regex
- [ ] Configuration editing
- [ ] Service dependency management
- [ ] Custom service commands
- [ ] Export logs to file
- [ ] Service health checks
- [ ] Alert notifications
- [ ] Performance graphs
- [ ] Theme customization

## License

This is a demonstration/prototype application built for testing TUI concepts.

## Credits

Built with:
- [Bubble Tea](https://github.com/charmbracelet/bubbletea) by Charm
- [Lipgloss](https://github.com/charmbracelet/lipgloss) by Charm
- [Bubbles](https://github.com/charmbracelet/bubbles) by Charm
- [gopsutil](https://github.com/shirou/gopsutil) by shirou

## Screenshots

See the `screenshots/` directory for captured screens of all functionality.
