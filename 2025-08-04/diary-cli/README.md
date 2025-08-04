# Diary CLI

A powerful command-line tool for managing diary entries in Obsidian markdown files with support for the Tasks plugin.

## Features

- **Multiple Entry Types**: TIL (Today I Learned), thoughts, activities, links, and todos
- **Interactive UI**: Beautiful forms using [huh](https://github.com/charmbracelet/huh) for guided entry creation
- **Multiple Output Formats**: Default markdown, enhanced markdown, or Obsidian Tasks format
- **Dual Output Support**: Human-readable display or structured data (JSON, CSV, etc.)
- **Visual Editor Integration**: Open your preferred editor for detailed entries
- **Smart Date Parsing**: Natural language dates like "today", "yesterday", "last friday"
- **Obsidian Integration**: Seamless integration with Obsidian and the Tasks plugin
- **Powerful Querying**: List, search, and filter entries with flexible options

## Quick Start

### Installation

1. Download the `diary` binary for your platform
2. Place it in your PATH or use it directly
3. Initialize your diary:

```bash
# In your Obsidian vault or any directory
diary init

# Or specify a path
diary init /path/to/your/vault
```

### Basic Usage

```bash
# Add entries
diary add til "Go interfaces are satisfied implicitly"
diary add thought "Architecture considerations for microservices"
diary add did "Completed authentication system"
diary add link "https://example.com" --title "Interesting Article"

# Create todos
diary todo add "Review pull requests" --priority high
diary todo add "Write documentation" --due tomorrow

# Query entries
diary list                           # Recent entries
diary search "authentication"        # Search content
diary show today                     # Today's entries

# Interactive mode
diary add                           # Guided entry creation
diary todo                          # Guided todo creation
```

## Entry Types

### TIL (Today I Learned)
Capture new knowledge, insights, or discoveries:
```bash
diary add til "CSS Grid can replace most flexbox layouts"
```

### Thoughts
Record ideas, reflections, or mental notes:
```bash
diary add thought "Microservices might be overkill for our current scale"
```

### Activities (Did)
Document what you accomplished or experienced:
```bash
diary add did "Completed user authentication system"
```

### Links
Save interesting resources with context:
```bash
diary add link "https://go.dev/blog/interfaces" --title "Go Interfaces Explained"
```

### Todos
Create actionable tasks with Obsidian Tasks integration:
```bash
diary todo add "Prepare presentation" --priority high --due "next friday"
```

## Output Formats

### Default Format
Simple, readable markdown:
```markdown
## TIL: Go interfaces are satisfied implicitly

*Added: 2025-08-04 09:31*
```

### Enhanced Markdown Format
Rich metadata for documentation:
```markdown
## TIL: Go interfaces are satisfied implicitly
**Type:** til  
**Date:** 2025-08-04 09:31  

---
```

### Obsidian Tasks Format
Compatible with the Tasks plugin:
```markdown
- [ ] **TIL**: Go interfaces are satisfied implicitly #toProcess #til
  - Added: 2025-08-04 09:31
```

## Obsidian Integration

The diary CLI creates files that work seamlessly with Obsidian:

### File Structure
```
YourVault/
├── Logs/
│   ├── README.md
│   ├── 2025-08-04.md
│   ├── 2025-08-05.md
│   └── 2025/
│       └── 08/
└── (your other Obsidian files)
```

### Tasks Plugin Queries
Use these in your Obsidian notes:

```tasks
not done
(description includes #toProcess)
created after last week
sort by created reverse
```

## Advanced Features

### Structured Output
Export data for analysis or automation:
```bash
diary list --output json
diary search "project" --output csv --fields type,content,date
```

### Visual Editor Integration
```bash
diary add til --editor
diary config editor vim
```

### Flexible Querying
```bash
diary list --type til --since "last month" --limit 20
diary search "authentication" --since "last week"
diary show "last friday"
```

## Configuration

Check and modify settings:
```bash
diary config                                    # Show all settings
diary config vault_path /path/to/vault          # Set vault path
diary config default_limit 20                   # Set default limit
diary config editor vim                          # Set preferred editor
```

## Development

### Building from Source

```bash
git clone <repository>
cd diary-cli
go mod tidy
go build -o diary
```

### Project Structure

```
diary-cli/
├── main.go                 # Application entry point
├── pkg/
│   ├── commands/          # Command implementations
│   ├── config/            # Configuration management
│   ├── doc/               # Documentation files
│   ├── storage/           # Markdown file handling
│   ├── types/             # Data structures
│   └── ui/                # Interactive forms
└── README.md
```

## Dependencies

- [Glazed](https://github.com/go-go-golems/glazed) - CLI framework with dual output support
- [Cobra](https://github.com/spf13/cobra) - CLI library
- [Huh](https://github.com/charmbracelet/huh) - Interactive forms
- [Natural Date](https://github.com/tj/go-naturaldate) - Date parsing

## License

[Add your license here]

## Contributing

[Add contribution guidelines here]

---

Built with ❤️ for productivity and knowledge management.

