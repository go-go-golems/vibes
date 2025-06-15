# Goja JavaScript REPL with Bubbletea and Tmux Integration

This project implements a JavaScript REPL (Read-Eval-Print Loop) in Go using the Goja JavaScript engine, Bubbletea for the terminal UI, and tmux integration for advanced features.

## Features

- Full JavaScript evaluation using Goja
- Beautiful terminal UI with Bubbletea
- Slash commands for various operations
- Tmux integration for advanced terminal features:
  - Editor spawning for JavaScript variables and files
  - Console log redirection to dedicated panes
  - Session management

## Installation

1. Ensure you have Go 1.20+ installed
2. Clone the repository
3. Install dependencies:

```bash
go mod tidy
```

4. Build the REPL:

```bash
go build -o repl cmd/repl/main.go
```

## Usage

### Basic REPL

Run the REPL:

```bash
./repl
```

Enter JavaScript code to evaluate it:

```javascript
2 + 2
"Hello, " + "World!"
const x = 10; x * 2
```

### Slash Commands

The REPL supports various slash commands:

- `/help` - Display help information
- `/clear` - Clear the screen
- `/history` - Show command history
- `/quit` or `/exit` - Exit the REPL

### Tmux Integration

The REPL includes tmux integration for advanced features:

- `/tmux start` - Start a new tmux session with REPL, editor, and log windows
- `/tmux kill` - Kill the current tmux session
- `/edit variable [name]` - Edit a JavaScript variable in the editor window
- `/edit file [path]` - Edit a file in the editor window
- `/log view` - Switch to the log window
- `/log send [message]` - Send a message to the log window
- `/log return` - Return to the main REPL window

## Examples

See the [examples](examples/) directory for usage examples.

## Demo GIFs

- [Basic Usage](demos/basic_usage.gif) - Demonstrates basic REPL functionality
- [Tmux Features](demos/tmux_features.gif) - Demonstrates tmux integration features

## Requirements

- Go 1.20+
- tmux (for tmux integration features)
- An editor (vim by default, or set via $EDITOR environment variable)

## License

MIT
