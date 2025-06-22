# Conversation Manager

A prototype terminal-based UI for managing conversation history, built with Go and Bubble Tea. This application provides an intuitive interface for browsing, searching, filtering, and previewing conversations.

## Features

- **📋 Conversation List**: Browse conversations organized by date with emoji indicators
- **🔍 Real-time Search**: Search through conversation titles and content
- **🏷️ Smart Filtering**: Filter by date range, tags, and AI models
- **👁️ Live Preview**: Preview conversation content without leaving the list
- **⌨️ Keyboard Navigation**: Vim-style navigation with intuitive shortcuts
- **🎨 Beautiful UI**: Clean, responsive interface with proper styling

## Architecture

The application follows a modular Bubble Tea architecture with separate models for different UI components:

- **MainModel**: Root coordinator managing all submodels
- **ConversationListModel**: Displays conversations grouped by date
- **SearchModel**: Handles search input and results
- **FilterRowModel**: Manages filter buttons and options
- **PreviewModel**: Shows conversation content in a split view
- **StatusModel**: Displays help text and status messages

## Installation

### Prerequisites

- Go 1.21 or later
- Terminal with 256-color support

### Build from Source

```bash
# Clone the repository
git clone <repository-url>
cd conversation-manager

# Build the application
go mod tidy
go build -o conversation-manager ./cmd/conversation-manager

# Run the application
./conversation-manager
```

## Usage

### Keyboard Shortcuts

#### Navigation
- `↑/k` - Move up
- `↓/j` - Move down
- `g` - Go to top
- `G` - Go to bottom
- `Enter` - Open conversation (placeholder)

#### Search
- `/` - Start search
- `Ctrl+U` - Clear search
- `Esc` - Exit search mode

#### Preview
- `Space` - Toggle preview panel
- `↑/↓` - Scroll in preview
- `PgUp/PgDn` - Page up/down in preview

#### Filters
- `f` - Toggle filter panel
- `←/→` - Navigate filter categories
- `↑/↓` - Navigate filter options
- `Space` - Toggle filter
- `c` - Clear current category
- `Enter` - Apply filters

#### General
- `q` - Quit application
- `Ctrl+C` - Force quit

### Data Format

Conversations are stored as YAML files in the `data/conversations/` directory. Each file should follow this structure:

```yaml
id: "conv-001"
title: "Conversation Title"
created_at: "2025-06-22T14:34:00Z"
last_updated: "2025-06-22T14:45:00Z"
tags: ["code", "react", "typescript"]
model: "Claude Sonnet 4"
metadata:
  project: "dashboard"
  difficulty: "intermediate"
messages:
  - id: "msg-001"
    parent_id: null
    role: "user"
    content: "Message content here..."
    timestamp: "2025-06-22T14:34:00Z"
  - id: "msg-002"
    parent_id: "msg-001"
    role: "assistant"
    content: "Response content here..."
    timestamp: "2025-06-22T14:35:00Z"
```

### Tag Colors

The application uses emoji indicators for different conversation types:

- 🔴 `code` - Programming and development
- 🟠 `writing` - Creative writing and content
- 🟡 `analysis` - Data analysis and research
- 🟢 `creative` - Creative projects
- 🔵 `q&a` - Questions and answers
- 🟣 `other` - Default for other tags

## Demo Videos

The `demos/` directory contains VHS recordings demonstrating key features:

- `demo-navigation.gif` - Basic navigation and browsing
- `demo-preview.gif` - Preview panel functionality
- `demo-search.gif` - Search and filtering

## Development

### Project Structure

```
conversation-manager/
├── cmd/conversation-manager/    # Main application entry point
├── pkg/
│   ├── models/                 # Data models and structures
│   ├── ui/                     # Bubble Tea UI components
│   └── data/                   # Data management and loading
├── data/conversations/         # Sample conversation data
├── demos/                      # VHS demo recordings
└── docs/                       # Additional documentation
```

### Adding New Features

1. **New UI Components**: Add to `pkg/ui/` and integrate with `MainModel`
2. **Data Models**: Extend `pkg/models/` for new data structures
3. **Message Types**: Add to `pkg/ui/messages.go` for inter-model communication

### Testing

```bash
# Run the application with sample data
./conversation-manager

# Test specific features:
# - Navigation: Use j/k to move, g/G for top/bottom
# - Search: Press / and type queries
# - Preview: Press Space to toggle preview
# - Filters: Press f to open filter panel
```

## Technical Details

### Dependencies

- **Bubble Tea**: Terminal UI framework
- **Lipgloss**: Styling and layout
- **YAML**: Configuration and data parsing

### Performance

- Lazy loading of conversation content
- Efficient search indexing
- Responsive UI updates
- Memory-conscious design for large datasets

### Compatibility

- Works on Linux, macOS, and Windows
- Requires terminal with 256-color support
- Optimized for 80x24 minimum terminal size

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Built with [Bubble Tea](https://github.com/charmbracelet/bubbletea) by Charm
- Inspired by modern terminal applications like `fzf` and `lazygit`
- Demo recordings created with [VHS](https://github.com/charmbracelet/vhs)

