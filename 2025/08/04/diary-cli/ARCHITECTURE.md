# Diary CLI Architecture and Design

## Overview

The Diary CLI is a command-line tool for managing diary entries in Obsidian markdown files with support for the Tasks plugin. It provides an interactive interface for adding entries, querying existing entries, and managing todos.

## Application Structure

```
diary-cli/
├── main.go                 # Application entry point
├── go.mod                  # Go module dependencies
├── README.md              # Project documentation
├── INSTALL.md             # Installation instructions
├── pkg/                   # Core application packages
│   ├── commands/          # Command implementations
│   ├── config/            # Configuration management
│   ├── doc/               # Documentation system
│   ├── storage/           # Data storage layer
│   ├── types/             # Type definitions
│   └── ui/                # User interface components
└── Logs/                  # Diary entry storage
```

## Main.go Analysis

### Entry Point and Initialization

The `main.go` file serves as the application entry point and follows a clean, modular architecture:

```go
func main() {
    // 1. Initialize configuration
    cfg, err := config.Load()
    
    // 2. Setup help system
    helpSystem := help.NewHelpSystem()
    
    // 3. Load documentation
    doc.AddDocToHelpSystem(helpSystem)
    
    // 4. Create commands
    addCmd := commands.NewAddCommand(cfg)
    todoCmd := commands.NewTodoCommand(cfg)
    // ... other commands
    
    // 5. Build glaze commands with middleware
    listCobraCmd, err := cli.BuildCobraCommand(listCmd, ...)
    
    // 6. Add commands to root
    rootCmd.AddCommand(addCmd)
    // ... other commands
    
    // 7. Execute
    rootCmd.ExecuteContext(context.Background())
}
```

### Key Design Patterns

1. **Dependency Injection**: Configuration is passed to all command constructors
2. **Command Pattern**: Each operation is encapsulated in a separate command
3. **Middleware Pattern**: Glaze framework provides middleware for command processing
4. **Factory Pattern**: Commands are created through factory functions

## Core Components

### 1. Configuration System (`pkg/config/`)

The configuration system is the foundation of the diary CLI application, managing all user preferences and application settings through a centralized YAML-based configuration file. The configuration is loaded at application startup and provides essential paths, formatting options, and behavioral settings that control how the application interacts with the user's Obsidian vault and manages diary entries.

**File**: `pkg/config/config.go`

**Key Functions**:
- `Load()` - Loads configuration from YAML file or creates default
- `DefaultConfig()` - Returns default configuration
- `Save()` - Persists configuration to file
- `GetLogsDir()` - Returns logs directory path
- `GetTodayFile()` - Returns today's diary file path

**Configuration Structure**:
```go
type Config struct {
    VaultPath    string `yaml:"vault_path"`
    LogsPath     string `yaml:"logs_path"`
    DateFormat   string `yaml:"date_format"`
    DefaultLimit int    `yaml:"default_limit"`
    Editor       string `yaml:"editor,omitempty"`
}
```

**Configuration File Location**:
- Default: `~/.config/diary-cli/config.yaml`
- Created automatically on first run with sensible defaults

**Default Configuration Values**:
```yaml
vault_path: ~/obsidian-vault
logs_path: Logs
date_format: 2006-01-02
default_limit: 10
editor: ""  # Uses $EDITOR environment variable
```

**Configuration Management**:
- Auto-creation of config file with defaults
- YAML-based configuration for readability
- Environment-aware editor detection
- Path resolution for vault and logs directories

### 2. Command System (`pkg/commands/`)

The command system implements the core functionality of the diary CLI through a collection of specialized commands, each responsible for a specific aspect of diary management. Each command follows a consistent factory pattern, receiving configuration as a dependency and providing both interactive and direct command-line interfaces. The commands are built using the Cobra framework and integrate with the Glazed middleware system for enhanced functionality and output formatting.

Each command is implemented as a separate file with a factory function:

#### Add Command (`pkg/commands/add.go`)
- **Function**: `NewAddCommand(cfg *config.Config) *cobra.Command`
- **Purpose**: Creates new diary entries
- **Modes**: Interactive (form-based) and direct (command-line)
- **Entry Types**: TIL, thought, activity, link
- **Formats**: default, markdown, task

#### Todo Command (`pkg/commands/todo.go`)
- **Function**: `NewTodoCommand(cfg *config.Config) *cobra.Command`
- **Purpose**: Manages todo items
- **Features**: Add, complete, list todos

#### List Command (`pkg/commands/list.go`)
- **Function**: `NewListCommand(cfg *config.Config) *cobra.Command`
- **Purpose**: Lists diary entries
- **Features**: Filtering, formatting, pagination

#### Search Command (`pkg/commands/search.go`)
- **Function**: `NewSearchCommand(cfg *config.Config) *cobra.Command`
- **Purpose**: Searches diary entries
- **Features**: Text search, date filtering

#### Show Command (`pkg/commands/show.go`)
- **Function**: `NewShowCommand(cfg *config.Config) *cobra.Command`
- **Purpose**: Displays specific entries
- **Features**: Detailed view, formatting options

#### Append Command (`pkg/commands/append.go`)
- **Function**: `NewAppendCommand(cfg *config.Config) *cobra.Command`
- **Purpose**: Appends content to existing entries

#### Config Command (`pkg/commands/config.go`)
- **Function**: `NewConfigCommand(cfg *config.Config) *cobra.Command`
- **Purpose**: Manages application configuration

#### Init Command (`pkg/commands/init.go`)
- **Function**: `NewInitCommand(cfg *config.Config) *cobra.Command`
- **Purpose**: Initializes diary structure

### 3. Storage Layer (`pkg/storage/`)

The storage layer is responsible for all data persistence operations, handling the reading and writing of diary entries to and from markdown files within the user's Obsidian vault. This layer abstracts the file system operations and provides a consistent interface for entry management, supporting multiple output formats and ensuring data integrity through proper file handling and directory management.

**File**: `pkg/storage/markdown.go`

**Key Functions**:
- `NewMarkdownStorage(cfg *config.Config) *MarkdownStorage`
- `AddEntry(entry *types.DiaryEntry) error`
- `GetEntries(date time.Time) ([]*types.DiaryEntry, error)`
- `SearchEntries(query string) ([]*types.DiaryEntry, error)`

### 4. Type System (`pkg/types/`)

The type system defines the core data structures and enums used throughout the application, providing type safety and consistency across all components. This system includes comprehensive entry types, formatting options, and priority levels that ensure data integrity and provide clear interfaces for the various subsystems that interact with diary entries.

**File**: `pkg/types/entry.go`

**Key Types**:
```go
type DiaryEntry struct {
    Type         EntryType
    Title        string
    Content      string
    Date         time.Time
    Format       Format
    SubtitleSlug string
    URL          string
}

type EntryType string
type Format string
```

### 5. UI Components (`pkg/ui/`)

The UI components provide the interactive interface layer of the application, offering user-friendly forms and prompts that simplify the entry creation and management process. These components leverage the Huh library to create rich terminal-based forms that guide users through complex operations while maintaining the flexibility of direct command-line input for power users.

**File**: `pkg/ui/forms.go`

**Key Functions**:
- `ShowAddForm() (*AddForm, error)` - Interactive form for adding entries
- `ShowTodoForm() (*TodoForm, error)` - Interactive form for todos

## Configuration System Deep Dive

The configuration system is the backbone of the diary CLI application, providing a centralized way to manage all application settings and user preferences. The configuration is designed to be both user-friendly and powerful, offering sensible defaults while allowing extensive customization for advanced users.

### Configuration Loading Process

The configuration loading process follows a robust pattern that ensures the application can always start, even if no configuration file exists. When the application starts, the `config.Load()` function is called from `main.go`, which performs several key operations:

1. **Path Resolution**: The function determines the configuration file path using `getConfigPath()`, which returns `~/.config/diary-cli/config.yaml` on most systems
2. **Existence Check**: If the configuration file doesn't exist, the system creates a default configuration using `DefaultConfig()`
3. **File Creation**: The default configuration is automatically saved to disk, ensuring the user has a working configuration file
4. **YAML Parsing**: If the file exists, it's read and parsed using the YAML library, with the parsed data merged into the default configuration structure

### Configuration Structure and Usage

The configuration structure is designed to be comprehensive yet simple, containing all the essential settings needed to customize the application behavior:

```go
type Config struct {
    VaultPath    string `yaml:"vault_path"`    // Path to Obsidian vault
    LogsPath     string `yaml:"logs_path"`     // Subdirectory for diary files
    DateFormat   string `yaml:"date_format"`   // Go date format string
    DefaultLimit int    `yaml:"default_limit"` // Default number of entries to show
    Editor       string `yaml:"editor,omitempty"` // Preferred text editor
}
```

Each configuration field serves a specific purpose in the application:

- **VaultPath**: Specifies the root directory of the user's Obsidian vault, where all diary files will be stored
- **LogsPath**: Defines the subdirectory within the vault where daily diary files are created (default: "Logs")
- **DateFormat**: Controls the format used for file names and date display (default: "2006-01-02")
- **DefaultLimit**: Sets the maximum number of entries to display in list and search results
- **Editor**: Specifies the preferred text editor for interactive editing (falls back to $EDITOR environment variable)

### Configuration Integration Throughout the Application

The configuration object is passed to all major components, ensuring consistent behavior across the application:

**Storage Layer Integration**: The `MarkdownStorage` uses configuration to determine file paths and formatting options. The `GetDateFile()` method constructs file paths using the configured vault and logs paths, while `formatEntry()` uses the date format for timestamp generation.

**Command System Integration**: All commands receive the configuration as a constructor parameter, allowing them to access user preferences for paths, limits, and formatting options. This enables commands to respect user settings for file locations and output formatting.

**UI Component Integration**: The UI components use configuration to determine editor preferences and default values for forms. The editor configuration is particularly important for the interactive editing features.

### Configuration File Management

The configuration system provides several utility functions for managing the configuration file:

- **`Save()`**: Persists the current configuration to disk, creating the configuration directory if it doesn't exist
- **`GetLogsDir()`**: Returns the full path to the logs directory by combining the vault path and logs path
- **`GetTodayFile()`**: Returns the path to today's diary file using the configured date format
- **`GetDateFile()`**: Returns the path to a specific date's diary file, useful for historical entry management

### Environment Variable Integration

The configuration system integrates with environment variables to provide additional flexibility:

- **Editor Detection**: If no editor is specified in the configuration, the system falls back to the `$EDITOR` environment variable
- **Path Expansion**: The system automatically expands user home directory (`~`) in paths
- **Cross-Platform Support**: The configuration system handles different path separators and home directory locations across operating systems

### Configuration Validation and Error Handling

The configuration system includes robust validation and error handling:

- **Type Safety**: The YAML structure is validated against the Go struct definition
- **Path Validation**: The system verifies that configured paths are accessible and creates directories as needed
- **Default Fallbacks**: If any configuration value is missing or invalid, the system falls back to sensible defaults
- **Graceful Degradation**: Configuration errors don't prevent the application from starting, allowing users to fix issues and restart

This comprehensive configuration system ensures that the diary CLI application can be easily customized to fit different user workflows while maintaining reliability and providing a smooth user experience.

## External Dependencies

### Core Dependencies

1. **Cobra** (`github.com/spf13/cobra`)
   - Command-line interface framework
   - Used for: Root command, subcommands, flags, help system

2. **Glazed** (`github.com/go-go-golems/glazed`)
   - CLI framework for building rich command-line applications
   - Used for: Command building, middleware, output formatting
   - Key APIs:
     - `cli.BuildCobraCommand()` - Converts glaze commands to cobra
     - `cli.CobraParserConfig` - Configuration for command parsing
     - `cli.CobraCommandDefaultMiddlewares` - Default middleware stack

3. **Huh** (`github.com/charmbracelet/huh`)
   - Terminal UI library for forms
   - Used for: Interactive forms, user input

4. **Natural Date** (`github.com/tj/go-naturaldate`)
   - Natural language date parsing
   - Used for: Parsing dates like "today", "yesterday", "2 days ago"

### Supporting Dependencies

- **YAML** (`gopkg.in/yaml.v3`) - Configuration file format
- **UUID** (`github.com/google/uuid`) - Unique identifier generation
- **Help System** (`github.com/go-go-golems/glazed/pkg/help`) - Documentation system

## Architecture Patterns

### 1. Layered Architecture

```
┌─────────────────┐
│   CLI Layer     │  (main.go, cobra commands)
├─────────────────┤
│  Command Layer  │  (pkg/commands/)
├─────────────────┤
│   Service Layer │  (pkg/storage/, pkg/ui/)
├─────────────────┤
│   Data Layer    │  (pkg/types/, markdown files)
└─────────────────┘
```

### 2. Command Pattern

Each operation is encapsulated in a separate command:
- Commands are created through factory functions
- Commands receive configuration as dependency
- Commands handle both interactive and direct modes

### 3. Middleware Pattern

Glaze framework provides middleware for:
- Input validation
- Output formatting
- Error handling
- Logging

### 4. Configuration Management

- YAML-based configuration
- Default configuration generation
- Environment-aware settings
- Editor integration support

## Key APIs and Functions

### Main Application Flow

1. **Configuration Loading**
   ```go
   cfg, err := config.Load()
   ```

2. **Help System Setup**
   ```go
   helpSystem := help.NewHelpSystem()
   doc.AddDocToHelpSystem(helpSystem)
   ```

3. **Command Creation**
   ```go
   addCmd := commands.NewAddCommand(cfg)
   ```

4. **Glaze Integration**
   ```go
   listCobraCmd, err := cli.BuildCobraCommand(listCmd,
       cli.WithParserConfig(cli.CobraParserConfig{
           ShortHelpLayers: []string{layers.DefaultSlug},
           MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
       }),
   )
   ```

### Command Factory Pattern

Each command follows this pattern:
```go
func NewCommandName(cfg *config.Config) *cobra.Command {
    var flags string
    
    cmd := &cobra.Command{
        Use:   "command",
        Short: "Description",
        RunE: func(cmd *cobra.Command, args []string) error {
            // Command implementation
        },
    }
    
    cmd.Flags().StringVarP(&flags, "flag", "f", "default", "description")
    return cmd
}
```



## Extension Points

The architecture supports several extension points that allow developers to extend the functionality without modifying core components. These extension points follow established patterns and interfaces, ensuring that new features integrate seamlessly with the existing codebase while maintaining the overall architectural integrity.

1. **New Entry Types**: Add new entry types in `pkg/types/`
2. **New Output Formats**: Extend formatting in command implementations
3. **New Storage Backends**: Implement storage interfaces
4. **New UI Components**: Add new forms in `pkg/ui/`
5. **New Commands**: Follow the command factory pattern

## Performance Considerations

The application is designed with performance in mind, implementing several strategies to ensure fast response times and efficient resource usage. These considerations balance user experience with system resource consumption, providing a responsive interface while maintaining the flexibility and functionality required for diary management.

1. **Lazy Loading**: Configuration and storage are initialized on demand
2. **Efficient Parsing**: Natural date parsing for user convenience
3. **Minimal Dependencies**: Only essential dependencies included
4. **Context Usage**: Proper context propagation for cancellation

## Security Considerations

The application implements several security measures to protect user data and ensure safe operation in various environments. These considerations address both data security and system security, providing a robust foundation for handling sensitive diary entries and maintaining system integrity.

1. **File Permissions**: Proper file permissions for configuration and logs
2. **Input Validation**: Validation of user inputs in commands
3. **Path Sanitization**: Safe handling of file paths
4. **Editor Integration**: Secure editor command execution

## Program Workflow and Data Flow

### 1. Entry Creation Flow

#### Interactive Mode Flow
```
User runs: diary add
    ↓
ShowAddForm() (pkg/ui/forms.go)
    ↓
User fills form (entry type, content, title, date, format)
    ↓
parseDate() - Natural date parsing ("today", "yesterday", "2 days ago")
    ↓
Create DiaryEntry struct (pkg/types/entry.go)
    ↓
MarkdownStorage.AddEntry() (pkg/storage/markdown.go)
    ↓
formatEntry() - Choose format (default/markdown/task)
    ↓
appendToFile() - Write to markdown file
    ↓
File written to: ~/obsidian-vault/Logs/YYYY-MM-DD.md
```

#### Direct Mode Flow
```
User runs: diary add til "Learned about Go interfaces"
    ↓
Parse command line arguments
    ↓
runDirectAdd() - Validate and process input
    ↓
Create DiaryEntry struct
    ↓
MarkdownStorage.AddEntry() (same as interactive)
```

### 2. Entry Formatting and Storage

#### Format Selection
The program supports three output formats:

**Default Format** (`formatDefaultEntry`):
```markdown
## Til: Learned about Go interfaces

*Added: 2025-01-06 14:30*

```

**Markdown Format** (`formatMarkdownEntry`):
```markdown
## Til: Learned about Go interfaces

**Type:** til  
**Date:** 2025-01-06 14:30  
**Tags:** go, interfaces  

---

```

**Task Format** (`formatTaskEntry`):
```markdown
- [ ] **TIL**: Learned about Go interfaces #toProcess #til
  - Added: 2025-01-06 14:30

```

#### File Structure
```
~/obsidian-vault/
└── Logs/
    ├── 2025-01-06.md
    ├── 2025-01-07.md
    └── 2025-01-08.md
```

Each daily file contains:
- Header with date and metadata
- "To Process" section for new entries
- "Processed" section for completed entries
- "Todos" section for task items

### 3. Entry Parsing and Retrieval

#### File Parsing Process
```
GetEntries() called
    ↓
filepath.Walk() - Scan all .md files in Logs directory
    ↓
parseEntriesFromFile() for each file
    ↓
Line-by-line parsing:
    - Task format: "- [ ] **TYPE**: content"
    - Markdown format: "## TYPE: content"
    - Todo format: "- [ ] content 📅 date"
    ↓
Create DiaryEntry structs with metadata
    ↓
Return filtered and sorted entries
```

#### Parsing Functions
- `parseTaskLine()` - Parses Obsidian Tasks format
- `parseMarkdownHeader()` - Parses markdown headers
- `parseEntryFromHeading()` - Advanced markdown parsing with Goldmark
- `parseTaskEntry()` - Parses task list items

#### Entry Metadata Extraction
```go
type DiaryEntry struct {
    Type         EntryType  // til, thought, did, link, todo
    Title        string     // Optional title
    Content      string     // Main content
    Subtitle     string     // Optional subtitle
    Date         time.Time  // Entry date
    Tags         []string   // Extracted tags
    File         string     // Source file path
    LineNum      int        // Line number in file
    SubtitleSlug string     // For organization
    Format       Format     // default, markdown, task
    Priority     Priority   // For todos: high, medium, low
    DueDate      *time.Time // For todos
    Completed    bool       // For todos
    TaskID       string     // Unique todo identifier
    URL          string     // For link entries
}
```

### 4. Search and Query Flow

#### Search Process
```
User runs: diary search "go interfaces"
    ↓
SearchCommand.RunE()
    ↓
MarkdownStorage.SearchEntries()
    ↓
GetEntries() - Get all entries
    ↓
Filter by query string (case-insensitive)
    ↓
Rank results by relevance
    ↓
Format output using Glaze middleware
    ↓
Display results in table/JSON/CSV format
```

#### List Process
```
User runs: diary list --type til --limit 5
    ↓
ListCommand.RunE()
    ↓
MarkdownStorage.GetEntries()
    ↓
Filter by entry type and date range
    ↓
Sort by date (newest first)
    ↓
Limit results
    ↓
Format using Glaze output formatters
```

### 5. Todo Management Flow

#### Todo Creation
```
User runs: diary todo add "Review PR #123"
    ↓
ShowTodoForm() - Interactive form
    ↓
Parse priority, due date, tags
    ↓
Create DiaryEntry with Type=EntryTypeTodo
    ↓
formatTaskEntry() - Format as Obsidian Tasks
    ↓
Write to daily file in "Todos" section
```

#### Todo Completion
```
User runs: diary todo complete "Review PR #123"
    ↓
Search for todo by description
    ↓
Update completed status
    ↓
Move from "Todos" to "Completed" section
    ↓
Update markdown file
```

### 6. Data Persistence Strategy

#### File Organization
- **Daily Files**: One markdown file per day (`YYYY-MM-DD.md`)
- **Section Structure**: Each file has organized sections
- **Metadata**: Entries include creation timestamps and tags
- **Backup**: Files are human-readable and version-control friendly

#### Entry Lifecycle
1. **Creation**: Entry added to "To Process" section
2. **Processing**: User can append content or modify
3. **Completion**: Entry moved to "Processed" section
4. **Archival**: Entries remain in daily files for reference

#### Error Handling
- **File I/O**: Graceful handling of missing files/directories
- **Parsing**: Skip malformed entries, continue processing
- **Validation**: Input validation before storage
- **Recovery**: Automatic file creation and directory setup

### 7. Integration Points

#### Obsidian Integration
- **File Format**: Standard markdown compatible with Obsidian
- **Tasks Plugin**: Task format supports Obsidian Tasks plugin
- **Tags**: Standard markdown tag format (`#tag`)
- **Links**: Internal and external link support

#### Editor Integration
- **External Editor**: Support for `$EDITOR` environment variable
- **Visual Editing**: Open entries in preferred editor
- **Content Parsing**: Parse editor output back into structured data

#### Output Formats
- **Human-Readable**: Default markdown output
- **Structured Data**: JSON/CSV for programmatic access
- **Enhanced Markdown**: Rich metadata and formatting
- **Task Format**: Obsidian Tasks plugin compatibility

This workflow provides a complete diary management system with flexible input methods, robust storage, and multiple output formats for different use cases. 