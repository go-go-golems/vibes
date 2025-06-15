# Research Notes

## Goja JavaScript Engine

Goja is an ECMAScript 5.1 implementation in pure Go with emphasis on standard compliance and performance.

### Key Features:
- Full ECMAScript 5.1 support (including regex and strict mode)
- Passes nearly all tc39 tests for implemented features
- Capable of running Babel, TypeScript compiler, and other ES5 code
- Sourcemaps support
- Most of ES6 functionality (work in progress)
- No cgo dependencies, pure Go implementation
- Minimum required Go version is 1.20

### Usage Example:
```go
vm := goja.New()
v, err := vm.RunString("2 + 2")
if err != nil {
    panic(err)
}
if num := v.Export().(int64); num != 4 {
    panic(num)
}
```

### Integration Notes:
- A single instance of goja.Runtime can only be used by one goroutine at a time
- Multiple instances can be created but object values cannot be passed between runtimes
- Go values can be passed to JS using Runtime.ToValue() method
- JS values can be exported to Go using Value.Export() or Runtime.ExportTo() methods
- JS functions can be called from Go using AssertFunction()

### Important Considerations:
- No built-in setTimeout()/setInterval() (not part of ECMAScript standard)
- These would need to be implemented by the hosting application
- Some limitations with WeakMap implementation due to Go runtime constraints

### GitHub Repository:
- https://github.com/dop251/goja
- 6.2k stars, actively maintained

## Bubbletea TUI Framework

Bubbletea is a powerful terminal UI framework in Go based on The Elm Architecture.

### Key Features:
- Based on functional design paradigms of The Elm Architecture
- Well-suited for simple and complex terminal applications
- Supports inline, full-window, or mixed terminal applications
- Includes framerate-based renderer, mouse support, focus reporting
- Has 32.2k stars on GitHub, actively maintained

### Architecture Components:
1. **Model**: Describes the application state (any type, usually a struct)
2. **Init**: Function that returns an initial command for the application to run
3. **Update**: Function that handles incoming events and updates the model
4. **View**: Function that renders the UI based on data in the model

### Usage Example:
```go
package main

import (
    "fmt"
    tea "github.com/charmbracelet/bubbletea"
)

type model struct {
    choices  []string
    cursor   int
    selected map[int]struct{}
}

func initialModel() model {
    return model{
        choices:  []string{"Option 1", "Option 2", "Option 3"},
        selected: make(map[int]struct{}),
    }
}

func (m model) Init() tea.Cmd {
    return nil
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
    switch msg := msg.(type) {
    case tea.KeyMsg:
        switch msg.String() {
        case "ctrl+c", "q":
            return m, tea.Quit
        // Handle other key presses
        }
    }
    return m, nil
}

func (m model) View() string {
    // Return UI as a string
    return "UI representation"
}

func main() {
    p := tea.NewProgram(initialModel())
    if _, err := p.Run(); err != nil {
        fmt.Printf("Error: %v", err)
    }
}
```

### Additional Resources:
- Bubbles: A library of common UI components for Bubble Tea
- Examples and tutorials available in the GitHub repository
- Video tutorials available

### GitHub Repository:
- https://github.com/charmbracelet/bubbletea
- 32.2k stars, actively maintained

## VHS Terminal GIF Recorder

VHS is a tool for generating terminal GIFs as code, allowing for integration testing and demoing CLI tools.

### Key Features:
- Create terminal GIFs using a simple scripting language
- Record terminal sessions and convert them to tape files
- Publish GIFs to vhs.charm.sh for easy sharing
- SSH server for remote access and execution
- Support for multiple output formats (GIF, MP4, WebM, PNG sequence)
- 16.6k stars on GitHub, actively maintained

### Tape File Commands:
- `Output <path>`: Specify file output location and format
- `Require <program>`: Specify required programs for tape file
- `Set <Setting> Value`: Configure recording settings (font size, dimensions, etc.)
- `Type "<characters>"`: Emulate typing
- `Left`, `Right`, `Up`, `Down`: Arrow key commands
- `Backspace`, `Enter`, `Tab`, `Space`: Special key commands
- `Ctrl[+Alt][+Shift]+<char>`: Modifier key combinations
- `Sleep <time>`: Wait for a specified duration
- `Wait[+Screen][+Line] /regex/`: Wait for specific conditions
- `Hide`/`Show`: Control command visibility in output
- `Screenshot`: Capture the current frame
- `Copy`/`Paste`: Clipboard operations
- `Source`: Include commands from another tape file
- `Env <Key> Value`: Set environment variables

### Usage Example:
```
# Where should we write the GIF?
Output demo.gif

# Set up a 1200x600 terminal with 46px font.
Set FontSize 46
Set Width 1200
Set Height 600

# Type a command in the terminal.
Type "echo 'Welcome to VHS!'"

# Pause for dramatic effect...
Sleep 500ms

# Run the command by pressing enter.
Enter

# Admire the output for a bit.
Sleep 5s
```

### Installation Requirements:
- Requires `ttyd` and `ffmpeg` to be installed and available on PATH
- Available via package managers (brew, pacman, nix, scoop)
- Docker image available
- Installable via Go: `go install github.com/charmbracelet/vhs@latest`

### GitHub Repository:
- https://github.com/charmbracelet/vhs
- 16.6k stars, actively maintained
