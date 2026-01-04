# Neovim Bubble Tea Plugin

A Neovim plugin that allows you to run interactive Bubble Tea TUI applications directly within Neovim buffers.

## Features

- 🎯 Interactive demo with multiple TUI components
- 📝 Text input handling within Neovim
- 📊 Progress bars and real-time updates
- 🎨 Styled components using Lipgloss
- ⌨️ Full keyboard navigation support

## Installation

### Using vim-plug

Add this to your `init.vim` or `init.lua`:

```vim
Plug 'path/to/nvim-bubbletea-plugin'
```

### Manual Installation

1. Clone this repository to your Neovim plugin directory:
   ```bash
   git clone https://github.com/your-username/nvim-bubbletea-plugin ~/.config/nvim/pack/plugins/start/nvim-bubbletea-plugin
   ```

2. The plugin will automatically build the Go binary when first used.

## Usage

### Commands

- `:BubbleTeaDemo` - Start the interactive Bubble Tea demo
- `:BubbleTeaStop` - Stop the running TUI application

### Key Bindings

- `<leader>bt` - Start Bubble Tea demo
- `<leader>bs` - Stop Bubble Tea TUI

### Navigation

Within the TUI:
- `↑/k` - Move up
- `↓/j` - Move down
- `Enter` - Select item
- `Esc` - Go back/exit
- `q` - Quit application

## Demo Features

1. **Interactive Demo** - Overview of plugin capabilities
2. **Text Input Example** - Demonstrates text input handling
3. **Progress Bar Demo** - Shows real-time progress updates
4. **Style Showcase** - Examples of styled components
5. **Quit** - Exit the application

## Requirements

- Neovim 0.5+
- Go 1.19+ (for building the binary)

## How It Works

The plugin consists of two main components:

1. **Lua Interface** (`lua/bubbletea/init.lua`) - Handles Neovim integration
2. **Go Binary** (`cmd/bubbletea-tui/main.go`) - Implements the Bubble Tea TUI

When you run `:BubbleTeaDemo`, the Lua script:
1. Builds the Go binary (if needed)
2. Creates a new Neovim buffer
3. Starts the Go binary as a terminal job
4. Displays the TUI within the buffer

## Development

To modify the TUI application:

1. Edit `cmd/bubbletea-tui/main.go`
2. Rebuild with `go build -o bubbletea-tui ./cmd/bubbletea-tui`
3. Restart Neovim or run `:BubbleTeaDemo` again

## License

MIT License

