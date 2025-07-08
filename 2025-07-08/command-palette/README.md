# VSCode-Style Command Palette with Bubbletea

A terminal-based chat REPL application featuring a VSCode-style command palette built with Charm Bracelet's Bubbletea framework.

## Features

- **Interactive Chat REPL**: Type messages and interact with a chat interface
- **Command Palette**: VSCode-style overlay command palette (Ctrl+P)
- **Fuzzy Search**: fzf-style fuzzy matching for command names
- **Command Registration**: Easy system for registering new commands
- **Overlay UI**: Command palette overlays the main interface
- **Bubbletea Messages**: Uses Bubbletea's message system for communication
- **Tmux Compatible**: Tested and validated in tmux environment

## Dependencies

- Go 1.23+
- `github.com/charmbracelet/bubbletea` - TUI framework
- `github.com/rmhubbert/bubbletea-overlay` - Overlay functionality
- `github.com/sahilm/fuzzy` - Fuzzy string matching

## Installation

1. Clone or download the project
2. Install dependencies:
   ```bash
   go mod tidy
   ```
3. Build the application:
   ```bash
   go build -o chat-repl
   ```

## Usage

### Running the Application

```bash
./chat-repl
```

### Basic Controls

- **Type messages**: Simply type and press Enter to send messages
- **Open Command Palette**: Press `Ctrl+P`
- **Navigate Commands**: Use arrow keys or `Ctrl+J/K`
- **Execute Command**: Press Enter
- **Close Palette**: Press Escape or `Ctrl+P` again
- **Quit Application**: Press `Ctrl+C` or `q`

### Available Commands

- `help` - Show help information
- `clear` - Clear chat messages
- `echo` - Echo a test message
- `time` - Show current time
- `date` - Show current date
- `about` - Show application information
- `theme` - Change application theme
- `quit` - Exit the application

### Direct Commands

You can also use direct commands by typing them in the chat:
- `/help` - Show help
- `/clear` - Clear messages
- `/quit` - Exit application

## Architecture

### Main Components

1. **main.go**: Main application with chat REPL and integration
2. **palette.go**: Command palette implementation with fuzzy search

### Key Features Implementation

#### Command Registration
```go
cp.RegisterCommand("commandName", "Description", func() tea.Cmd {
    return func() tea.Msg {
        return CommandExecutedMsg{Command: "commandName", Data: "optional data"}
    }
})
```

#### Fuzzy Search
Uses the `sahilm/fuzzy` library to provide fzf-style command filtering:
- Type partial command names
- Commands are filtered in real-time
- Matches are ranked by relevance

#### Overlay System
The command palette appears as an overlay over the main chat interface:
- Centered positioning
- Maintains background visibility
- Clean visual separation

#### Message Communication
Uses Bubbletea's message system for command execution:
- Commands send `CommandExecutedMsg` messages
- Main application handles command results
- Clean separation of concerns

## Screenshots

The application has been tested in tmux and screenshots are available:

1. **Basic Interface**: Shows the initial chat REPL
2. **Command Palette**: Shows the overlay command palette
3. **Fuzzy Search**: Demonstrates command filtering
4. **Command Execution**: Shows command results in chat
5. **Final State**: Shows multiple interactions

## Development

### Adding New Commands

To add a new command, register it in the `NewCommandPalette()` function:

```go
cp.RegisterCommand("mycommand", "My command description", func() tea.Cmd {
    return func() tea.Msg {
        return CommandExecutedMsg{
            Command: "mycommand",
            Data:    "optional data",
        }
    }
})
```

Then handle the command in the main application's `Update` method:

```go
case CommandExecutedMsg:
    switch msg.Command {
    case "mycommand":
        // Handle your command
        m.messages = append(m.messages, "System: My command executed!")
    }
```

### Customizing Styles

Styles are defined using Lipgloss and can be customized in both files:
- Chat interface styles in `main.go`
- Command palette styles in `palette.go`

## Testing

The application has been thoroughly tested in tmux:
- Command palette overlay functionality
- Fuzzy search filtering
- Command execution and message handling
- Keyboard navigation and controls

## License

This project is created as a demonstration of Bubbletea capabilities and command palette implementation patterns.

## Author

Created by Manus AI Assistant as a demonstration of VSCode-style command palette implementation using Charm Bracelet's Bubbletea framework.

