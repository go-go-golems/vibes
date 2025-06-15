# Documentation for Goja JavaScript REPL with Enhanced Features

## Overview

This project implements a JavaScript REPL (Read-Eval-Print Loop) in Go using the Goja JavaScript engine, with a beautiful terminal UI powered by Bubbletea. The REPL includes advanced features such as code wrapping, multiline input support, and tmux integration for editor spawning and console log redirection.

## Features

### Core Features
- Full JavaScript evaluation using Goja
- Beautiful terminal UI with Bubbletea
- Code wrapping for better readability
- Multiline input support using Ctrl+J
- Slash commands for various operations

### Tmux Integration
- Session management with dedicated windows
- Editor spawning for JavaScript variables and files
- Console log redirection to a dedicated pane
- Vim integration for editing and inserting code

## Usage

### Running the REPL

```bash
./repl
```

### Basic JavaScript

Enter JavaScript code to evaluate it:

```javascript
2 + 2
"Hello, " + "World!"
const x = 10; x * 2
```

### Multiline Input

Use Ctrl+J to enter multiline mode:

```javascript
function calculateArea(radius) {  // Press Ctrl+J after this line
  // Calculate the area of a circle  // Press Ctrl+J after this line
  return Math.PI * radius * radius;  // Press Ctrl+J after this line
}  // Press Enter after this line
// Press Enter on an empty line to execute
```

### Slash Commands

The REPL supports various slash commands:

- `/help` - Display help information
- `/clear` - Clear the screen
- `/history` - Show command history
- `/quit` or `/exit` - Exit the REPL

### Tmux Integration Commands

- `/tmux start` - Start a new tmux session with REPL, editor, and log windows
- `/tmux kill` - Kill the current tmux session
- `/edit variable [name]` - Edit a JavaScript variable in the editor window
- `/edit file [path]` - Edit a file in the editor window
- `/log view` - Switch to the log window
- `/log send [message]` - Send a message to the log window
- `/log return` - Return to the main REPL window
- `/vim [content]` - Spawn vim, edit code, and insert it into the REPL on exit

## Implementation Details

### Project Structure

```
goja-repl/
├── cmd/
│   └── repl/         # Main application entry point
├── internal/
│   ├── engine/       # Goja JavaScript engine integration
│   ├── ui/           # Bubbletea TUI components
│   ├── commands/     # Slash command implementation
│   └── tmux/         # Tmux integration
├── examples/         # Example code and usage documentation
├── demos/            # Generated demo GIFs
└── vhs/              # VHS scripts for demo creation
```

### Key Components

1. **JavaScript Engine (internal/engine/engine.go)**
   - Wraps the Goja JavaScript runtime
   - Handles evaluation of JavaScript code
   - Provides variable access and console.log redirection

2. **Terminal UI (internal/ui/model.go)**
   - Implements the Bubbletea Model interface
   - Handles user input and rendering
   - Supports code wrapping and multiline input

3. **Command Registry (internal/commands/commands.go)**
   - Manages slash commands
   - Provides extensible command interface

4. **Tmux Integration (internal/tmux/tmux.go)**
   - Manages tmux sessions, windows, and panes
   - Provides editor spawning and log redirection

5. **Vim Integration (internal/commands/vim_command.go)**
   - Spawns vim in a tmux pane
   - Captures edited code and inserts it into the REPL

## Requirements

- Go 1.20+
- tmux (for tmux integration features)
- vim (or another editor set via $EDITOR)

## License

MIT
