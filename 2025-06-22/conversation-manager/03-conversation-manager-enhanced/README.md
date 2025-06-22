# Conversation Manager

A prototype terminal-based conversation history manager built with Go and Bubble Tea, featuring advanced tag filtering and date range search capabilities.

## Features

### 🔍 **Advanced Search**
- **Tag Search**: `tag:python`, `tag:react` - Search by specific tags
- **Date Range Search**: `range:today`, `after:yesterday`, `before:2024-06-01`
- **Content Search**: `title:dashboard`, `content:api`, `model:gpt-4`
- **Combined Search**: Mix tags, dates, and content for precise results
- **Smart Suggestions**: Real-time suggestions with tag counts and history
- **Search Modes**: Dedicated tag and date range search modes

### 🏷️ **Intelligent Tag System**
- **Auto-categorization**: Automatic tag categorization with visual indicators
- **Tag Categories**: Code 🔴, Writing 🟠, Analysis 🟡, Creative 🟢, Q&A 🔵, Other 🟣
- **Tag Statistics**: Usage counts and popularity tracking
- **Tag Filtering**: Advanced filtering with AND/OR operators

### 📅 **Comprehensive Date Filtering**
- **Predefined Ranges**: today, yesterday, this-week, last-month, etc.
- **Relative Ranges**: last-7-days, last-30-days, last-N-weeks
- **Absolute Dates**: 2024-06-22, 2024-06-01 to 2024-06-22
- **Date Operators**: after:, before:, on:, range:
- **Date Statistics**: Conversation counts by time period

### 🎨 **Modern Terminal UI**
- **Split-screen Preview**: Live conversation preview with scrolling
- **Keyboard Navigation**: Vim-style shortcuts (j/k, /, f, space)
- **Visual Indicators**: Color-coded tags and status indicators
- **Responsive Layout**: Adapts to terminal size
- **Focus Management**: Clear visual focus indicators

### 📊 **Data Management**
- **YAML Storage**: Human-readable conversation format
- **Efficient Loading**: Fast startup with lazy loading
- **Search Indexing**: Optimized search with relevance scoring
- **Data Validation**: Robust error handling and validation

## Installation

### Prerequisites
- Go 1.21 or later
- Terminal with 256-color support

### Build from Source
```bash
git clone <repository-url>
cd conversation-manager
go mod tidy
go build -o conversation-manager ./cmd/conversation-manager
```

## Usage

### Basic Usage
```bash
./conversation-manager
```

### Keyboard Shortcuts

#### Navigation
- `j/k` or `↑/↓` - Navigate conversations
- `Space` - Toggle preview panel
- `Enter` - Select conversation
- `q` - Quit application

#### Search
- `/` - Enter search mode
- `Ctrl+T` - Tag search mode
- `Ctrl+D` - Date range search mode
- `Ctrl+U` - Clear search
- `Tab` - Show suggestions
- `Esc` - Exit search

#### Filtering
- `f` - Toggle filter panel
- `←/→` - Navigate filter categories
- `↑/↓` - Navigate filter options
- `Space` - Toggle filter
- `Esc` - Exit filter mode

### Search Examples

#### Tag Search
```
tag:python              # Find Python conversations
tag:react tag:css       # Find React AND CSS conversations
```

#### Date Range Search
```
range:today             # Today's conversations
range:last-week         # Last week's conversations
after:2024-06-01        # After June 1st, 2024
before:yesterday        # Before yesterday
on:2024-06-22          # On specific date
```

#### Combined Search
```
tag:python range:last-week          # Python conversations from last week
title:api tag:nodejs after:yesterday # API-related Node.js conversations since yesterday
content:machine learning range:last-30-days # ML content from last 30 days
```

#### Advanced Search
```
model:gpt-4 tag:analysis            # GPT-4 analysis conversations
title:dashboard tag:react range:today # React dashboard conversations from today
```

## Data Format

Conversations are stored as YAML files in the `data/conversations/` directory:

```yaml
id: "conv-001"
title: "React Dashboard Design Help"
created_at: 2024-06-22T09:30:00Z
last_updated: 2024-06-22T14:45:00Z
tags: ["code", "react", "typescript", "ui"]
model: "gpt-4"
messages:
  - id: "msg-001"
    role: "user"
    content: "How do I create a responsive dashboard in React?"
    timestamp: 2024-06-22T09:30:00Z
  - id: "msg-002"
    role: "assistant"
    content: "Here's how to create a responsive React dashboard..."
    timestamp: 2024-06-22T09:30:15Z
```

## Project Structure

```
conversation-manager/
├── cmd/conversation-manager/    # Main application entry point
├── pkg/
│   ├── models/                 # Data models and business logic
│   │   ├── conversation.go     # Conversation and message models
│   │   ├── date_range.go      # Date range parsing and filtering
│   │   └── tag_manager.go     # Tag management and categorization
│   ├── data/                  # Data access layer
│   │   └── manager.go         # Data loading and filtering
│   └── ui/                    # User interface components
│       ├── main.go           # Main UI coordinator
│       ├── search.go         # Enhanced search with tags and dates
│       ├── conversation_list.go # Conversation list display
│       ├── filter_row.go     # Filter controls
│       ├── preview.go        # Conversation preview
│       ├── tag_filter.go     # Tag filtering UI
│       ├── status.go         # Status bar
│       ├── styles.go         # UI styling
│       └── messages.go       # Inter-component messages
├── data/conversations/        # Sample conversation data
├── demos/                    # VHS demo recordings
└── docs/                     # Documentation
```

## Development

### Adding New Conversations
1. Create a new YAML file in `data/conversations/`
2. Follow the conversation format shown above
3. Restart the application to load new data

### Extending Search
The search system is modular and can be extended:
- Add new search modifiers in `pkg/data/manager.go`
- Implement new date range patterns in `pkg/models/date_range.go`
- Add new tag categories in `pkg/models/conversation.go`

### UI Customization
- Modify styles in `pkg/ui/styles.go`
- Add new UI components following the Bubble Tea pattern
- Update keyboard shortcuts in respective model files

## Demo Videos

The `demos/` directory contains VHS recordings showing:
- `demo-navigation.gif` - Basic navigation and browsing
- `demo-preview.gif` - Preview panel functionality
- `demo-search-fixed.gif` - Search functionality
- `demo-tag-search-simple.gif` - Tag search features
- `demo-date-search.gif` - Date range search features

## Architecture

### Bubble Tea Models
- **MainModel**: Coordinates all submodels and handles global state
- **SearchModel**: Handles search input with tag and date range support
- **ConversationListModel**: Displays filtered conversation list
- **FilterRowModel**: Manages filter controls
- **PreviewModel**: Shows conversation details
- **StatusModel**: Displays status and help information

### Data Flow
1. User input → UI models
2. UI models → Messages
3. MainModel coordinates → Data manager
4. Data manager → Filtered results
5. Results → UI updates

### Search Pipeline
1. Parse search query for modifiers (tag:, range:, etc.)
2. Extract and validate date ranges
3. Apply filters in order: dates → tags → content
4. Score and rank results by relevance
5. Return sorted results with match highlights

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
- Styled with [Lip Gloss](https://github.com/charmbracelet/lipgloss)
- Demos created with [VHS](https://github.com/charmbracelet/vhs)
- Inspired by modern conversation management needs

