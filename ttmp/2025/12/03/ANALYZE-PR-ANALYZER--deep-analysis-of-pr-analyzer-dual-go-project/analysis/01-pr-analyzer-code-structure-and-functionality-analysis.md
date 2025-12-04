---
Title: PR Analyzer Code Structure and Functionality Analysis
Ticket: ANALYZE-PR-ANALYZER
Status: active
Topics:
    - analysis
    - go
    - cli
DocType: analysis
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2025-07-29/pr-analyzer-dual/README.md
      Note: Project documentation and usage guide
    - Path: 2025-07-29/pr-analyzer-dual/cmd/analyze/analyze.go
      Note: Command group setup for analyze commands
    - Path: 2025-07-29/pr-analyzer-dual/cmd/analyze/function_history_dual.go
      Note: Dual-mode function history command for tracking function evolution
    - Path: 2025-07-29/pr-analyzer-dual/cmd/analyze/functions.go
      Note: Non-dual functions command (legacy/alternative implementation)
    - Path: 2025-07-29/pr-analyzer-dual/cmd/get/commits.go
      Note: Non-dual commits command (legacy/alternative implementation)
    - Path: 2025-07-29/pr-analyzer-dual/cmd/get/context.go
      Note: Non-dual context command (legacy/alternative implementation)
    - Path: 2025-07-29/pr-analyzer-dual/cmd/get/context_dual.go
      Note: Dual-mode context command for file change analysis
    - Path: 2025-07-29/pr-analyzer-dual/cmd/get/diff.go
      Note: Non-dual diff command (legacy/alternative implementation)
    - Path: 2025-07-29/pr-analyzer-dual/cmd/get/diff_dual.go
      Note: Dual-mode diff command for retrieving PR diffs
    - Path: 2025-07-29/pr-analyzer-dual/cmd/get/file_history.go
      Note: Non-dual file history command (legacy/alternative implementation)
    - Path: 2025-07-29/pr-analyzer-dual/cmd/get/file_history_dual.go
      Note: Dual-mode file history command for commit tracking
    - Path: 2025-07-29/pr-analyzer-dual/cmd/get/get.go
      Note: Command group setup for get commands
    - Path: 2025-07-29/pr-analyzer-dual/go.mod
      Note: Go module dependencies and versions
ExternalSources: []
Summary: 'Comprehensive analysis of pr-analyzer-dual Go project: architecture, components, control flow, symbols, and dual-mode command structure'
LastUpdated: 2025-12-03T12:59:57.486832926-05:00
---


# PR Analyzer Code Structure and Functionality Analysis

## Executive Summary

The `pr-analyzer-dual` project is a Go CLI tool for analyzing GitHub pull requests using tree-sitter for Go code parsing and the glazed framework for structured output. The project implements a "dual-mode" command architecture that supports both human-readable markdown output and structured data formats (JSON, CSV, YAML, etc.) through the same command interface.

## Project Overview

**Location:** `vibes/2025-07-29/pr-analyzer-dual/`

**Purpose:** Analyze GitHub pull requests to extract:
- Commit history
- File diffs
- Changed functions (using tree-sitter AST parsing)
- Function history
- File context and statistics

**Key Technologies:**
- Go 1.24.2+
- Cobra CLI framework
- Glazed framework (dual-mode commands)
- Tree-sitter (Go parser)
- GitHub API (go-github v66)

## Project Structure

```
pr-analyzer-dual/
├── main_dual.go              # Entry point, command setup
├── cmd/
│   ├── get/                  # Data retrieval commands
│   │   ├── commits.go        # Non-dual commits command
│   │   ├── commits_dual.go    # Dual-mode commits command
│   │   ├── context.go        # Non-dual context command
│   │   ├── context_dual.go   # Dual-mode context command
│   │   ├── diff.go           # Non-dual diff command
│   │   ├── diff_dual.go      # Dual-mode diff command
│   │   ├── file_history.go   # Non-dual file history command
│   │   ├── file_history_dual.go  # Dual-mode file history command
│   │   └── get.go            # Command group setup
│   └── analyze/              # Code analysis commands
│       ├── functions.go      # Non-dual functions command
│       ├── functions_dual.go # Dual-mode functions command
│       ├── function_history_dual.go  # Function history analysis
│       └── analyze.go        # Command group setup
├── internal/
│   ├── github/
│   │   └── client.go         # GitHub API client wrapper
│   ├── treesitter/
│   │   └── parser.go         # Tree-sitter Go parser wrapper
│   └── analysis/
│       ├── diff.go           # Unified diff parser
│       └── function_analysis.go  # Function change detection
├── pkg/
│   └── doc/                  # Embedded documentation
│       ├── doc.go            # Help system integration
│       ├── overview.md
│       ├── examples/
│       ├── topics/
│       └── tutorials/
└── Documentation files (README.md, EXAMPLES.md, etc.)
```

## Architecture Components

### 1. Entry Point (`main_dual.go`)

**Purpose:** Sets up the Cobra command tree and integrates dual-mode commands.

**Key Functions:**
- `main()` - Initializes root command, creates `get` and `analyze` command groups
- Registers dual-mode commands using `cli.BuildCobraCommand()` with `WithDualMode(true)`
- Sets up help system with embedded documentation

**Control Flow:**
1. Create root `cobra.Command`
2. Create `get` and `analyze` command groups
3. For each subcommand:
   - Create dual command instance (e.g., `NewCommitsDualCommand()`)
   - Build Cobra command with dual-mode support
   - Add to parent command group
4. Setup help system with `doc.AddDocToHelpSystem()`
5. Execute root command

### 2. GitHub Client (`internal/github/client.go`)

**Type:** `Client` struct wrapping `*github.Client`

**Key Methods:**
- `NewClient()` - Creates authenticated or unauthenticated GitHub client
- `GetPullRequest()` - Retrieves PR metadata
- `GetPullRequestDiff()` - Gets unified diff for PR
- `GetPullRequestCommits()` - Lists all commits in PR (paginated)
- `GetFileCommits()` - Gets commit history for specific file
- `GetPullRequestFiles()` - Lists files changed in PR
- `GetFileContent()` - Retrieves file content at specific ref

**Authentication:** Checks `GITHUB_TOKEN` environment variable; falls back to unauthenticated client with rate limits.

**Pagination:** All list methods handle pagination automatically using `ListOptions{PerPage: 100}`.

### 3. Tree-sitter Parser (`internal/treesitter/parser.go`)

**Type:** `Parser` struct wrapping `*sitter.Parser`

**Key Types:**
- `Function` struct:
  - `Name`, `StartLine`, `EndLine`, `StartByte`, `EndByte`
  - `Signature`, `Body`, `Receiver`
  - `IsExported` (boolean)

**Key Methods:**
- `NewParser()` - Creates parser with Go language support
- `ParseCode()` - Parses source code to AST
- `ExtractFunctions()` - Extracts all function/method declarations
- `FindFunctionAtLine()` - Finds function containing a line number
- `GetChangedFunctions()` - Identifies functions modified by changed lines

**AST Traversal:**
- Recursive `traverseNode()` function searches for `function_declaration` and `method_declaration` nodes
- Extracts function signatures, bodies, receivers using tree-sitter field names
- Handles both functions and methods (with receivers)

### 4. Diff Analysis (`internal/analysis/diff.go`)

**Key Types:**
- `DiffLine` - Represents a single diff line (`add`, `remove`, `context`)
- `FileDiff` - Represents changes to one file

**Key Functions:**
- `ParseDiff()` - Parses unified diff format using regex patterns
- `GetChangedLines()` - Returns line numbers that were added/modified
- `GetStats()` - Calculates added/removed/modified line counts

**Regex Patterns:**
- File headers: `^diff --git a/(.*) b/(.*)$`
- Hunk headers: `^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@`
- Line types: `+` (add), `-` (remove), ` ` (context)

### 5. Function Analysis (`internal/analysis/function_analysis.go`)

**Key Types:**
- `FileChange` - Represents file changes with line statistics

**Key Functions:**
- `ParseDiffForAnalysis()` - Converts diff to `FileChange` list
- `IsFunctionChanged()` - Checks if function contains changed lines
- `GetGoFiles()` - Filters to only `.go` files

**Logic:** Determines function changes by checking if any changed line number falls within function's line range (`StartLine` to `EndLine`).

## Command Architecture: Dual-Mode Pattern

### Dual-Mode Command Structure

Each command implements **two interfaces**:
1. `cmds.BareCommand` - Human-readable markdown output (`Run()`)
2. `cmds.GlazeCommand` - Structured data output (`RunIntoGlazeProcessor()`)

**Example Pattern (from `commits_dual.go`):**

```go
type CommitsDualCommand struct {
    *cmds.CommandDescription
}

type CommitsDualSettings struct {
    Owner    string `glazed.parameter:"owner"`
    Repo     string `glazed.parameter:"repo"`
    PRNumber int    `glazed.parameter:"pr-number"`
}

// Human-readable output
func (c *CommitsDualCommand) Run(ctx, parsedLayers) error {
    // Print markdown-formatted output
}

// Structured output
func (c *CommitsDualCommand) RunIntoGlazeProcessor(ctx, parsedLayers, gp) error {
    // Add rows to glazed processor for JSON/CSV/etc.
}
```

**Interface Assertions:**
```go
var _ cmds.BareCommand = &CommitsDualCommand{}
var _ cmds.GlazeCommand = &CommitsDualCommand{}
```

### Command Groups

#### `get` Commands (Data Retrieval)

1. **`get commits`** (`commits_dual.go`)
   - Retrieves commit list for PR
   - Output: SHA, message, author, date
   - Parameters: `owner`, `repo`, `pr-number`

2. **`get diff`** (`diff_dual.go`)
   - Retrieves unified diff
   - Output: Complete diff text
   - Parameters: `owner`, `repo`, `pr-number`

3. **`get context`** (`context_dual.go`)
   - Analyzes PR to show file context
   - Output: File paths, line stats, function counts, changed function names
   - Parameters: `owner`, `repo`, `pr-number`

4. **`get file-history`** (`file_history_dual.go`)
   - Gets commit history for specific file
   - Output: Commits affecting the file
   - Parameters: `owner`, `repo`, `file-path`

#### `analyze` Commands (Code Analysis)

1. **`analyze functions`** (`functions_dual.go`)
   - Extracts Go functions from changed files
   - Uses tree-sitter to parse AST
   - Output: Function names, signatures, line ranges, change status
   - Parameters:
     - `owner`, `repo`, `pr-number` (required)
     - `show-body` (bool, optional)
     - `only-changed` (bool, optional)

2. **`analyze function-history`** (`function_history_dual.go`)
   - Tracks function changes across commits
   - Shows evolution of functions over time
   - Parameters: `owner`, `repo`, `pr-number`, `file-path`, `function-name`

## Control Flow Examples

### Example 1: `get commits` Command Flow

```
User: pr-analyzer get commits --owner X --repo Y --pr-number 123
  ↓
main_dual.go: main()
  ↓
commitsCobraCmd.Execute()
  ↓
CommitsDualCommand.Run() OR RunIntoGlazeProcessor()
  ↓
github.NewClient()
  ↓
client.GetPullRequestCommits(ctx, owner, repo, prNumber)
  ↓
GitHub API: ListCommits (paginated)
  ↓
Format output (markdown OR structured rows)
```

### Example 2: `analyze functions` Command Flow

```
User: pr-analyzer analyze functions --owner X --repo Y --pr-number 123 --only-changed
  ↓
main_dual.go: main()
  ↓
functionsCobraCmd.Execute()
  ↓
FunctionsDualCommand.Run() OR RunIntoGlazeProcessor()
  ↓
client.GetPullRequestDiff() → unified diff string
  ↓
analysis.ParseDiffForAnalysis() → []FileChange
  ↓
For each .go file:
  ├─ client.GetFileContent() → source code
  ├─ parser.ExtractFunctions() → []Function
  └─ analysis.IsFunctionChanged() → mark changed functions
  ↓
Filter by --only-changed flag
  ↓
Format output (markdown OR structured rows)
```

## Key Symbols and Types

### Core Types

**GitHub Client:**
- `github.Client` - Wraps `*github.Client`
- Methods return `*github.PullRequest`, `[]*github.RepositoryCommit`, etc.

**Tree-sitter:**
- `treesitter.Parser` - Wraps `*sitter.Parser`
- `treesitter.Function` - Function metadata struct

**Diff Analysis:**
- `analysis.DiffLine` - Single diff line
- `analysis.FileDiff` - File-level diff
- `analysis.FileChange` - File change statistics

**Commands:**
- All dual commands embed `*cmds.CommandDescription`
- Settings structs use `glazed.parameter` tags

### Parameter Layer System

Commands use glazed's parameter layer system:
- `layers.DefaultSlug` - Default parameter layer
- `settings.NewGlazedParameterLayers()` - Creates standard glazed layer
- `parsedLayers.InitializeStruct()` - Extracts parameters into struct

## Dependencies

**Core Dependencies:**
- `github.com/go-go-golems/glazed` v0.6.6 - CLI framework
- `github.com/google/go-github/v66` v66.0.0 - GitHub API
- `github.com/smacker/go-tree-sitter` - Tree-sitter bindings
- `github.com/spf13/cobra` v1.9.1 - CLI framework
- `golang.org/x/oauth2` v0.25.0 - OAuth2 for GitHub auth

**Key Features:**
- Glazed provides dual-mode command support, output formatting, parameter layers
- go-github handles all GitHub API interactions
- Tree-sitter provides AST parsing for Go code

## File Organization Patterns

### Dual vs Non-Dual Commands

The project contains **both** dual-mode and non-dual versions of commands:
- `*_dual.go` files implement dual-mode (human-readable + structured)
- Non-dual files (e.g., `commits.go`) implement only structured output

**Current State:** `main_dual.go` uses only dual-mode commands. Non-dual commands appear to be legacy or alternative implementations.

### Command Naming Convention

- Dual commands: `*DualCommand` struct, `New*DualCommand()` constructor
- Settings: `*DualSettings` struct
- Both interfaces: `Run()` and `RunIntoGlazeProcessor()`

## Error Handling Patterns

1. **GitHub API Errors:** Wrapped with context: `fmt.Errorf("failed to get PR: %w", err)`
2. **Parsing Errors:** Gracefully skipped with error messages in output
3. **Missing Files:** Commands continue processing other files, report errors per-file
4. **Tree-sitter Errors:** Files with parse errors are skipped, not fatal

## Output Formats

### Human-Readable (BareCommand.Run())

- Markdown-formatted output
- Headers, lists, code blocks
- Emoji indicators (📝, 🔄, ❌)
- Summary statistics

### Structured (GlazeCommand.RunIntoGlazeProcessor())

- Rows added via `gp.AddRow(ctx, row)`
- Fields set using `types.MRP()` (MakeRowPair)
- Supports: JSON, CSV, YAML, table, markdown
- Controlled by `--output` flag

## Documentation System

**Embedded Documentation:** `pkg/doc/` uses `embed.FS` to include markdown files:
- `overview.md` - Project overview
- `examples/` - Usage examples
- `topics/` - Topic-specific docs
- `tutorials/` - Getting started guides

**Help Integration:** `doc.AddDocToHelpSystem()` loads embedded docs into glazed help system.

## Testing and Validation

**No test files found** in the analyzed directory structure. The project appears to rely on:
- Manual testing via CLI
- Integration with GitHub API
- Tree-sitter parsing validation

## Key Design Decisions

1. **Dual-Mode Architecture:** Enables same command to produce both human-readable and machine-readable output
2. **Separation of Concerns:** GitHub client, tree-sitter parser, and diff analysis are separate packages
3. **Graceful Degradation:** Missing files or parse errors don't stop entire analysis
4. **Pagination Handling:** All GitHub API list methods handle pagination automatically
5. **Parameter Layers:** Uses glazed's layer system for flexible parameter handling

## Potential Improvements

1. **Error Handling:** Could benefit from structured error types
2. **Testing:** Missing unit tests for core functionality
3. **Caching:** No caching of GitHub API responses or parsed ASTs
4. **Concurrency:** Sequential processing of files; could parallelize
5. **Configuration:** Hard-coded values (e.g., PerPage: 100) could be configurable

## Summary

The pr-analyzer-dual project demonstrates a well-structured Go CLI application using:
- **Modular architecture** with clear separation between GitHub API, parsing, and commands
- **Dual-mode commands** supporting both human and machine-readable output
- **Robust parsing** using tree-sitter for Go AST analysis
- **Comprehensive GitHub integration** with pagination and authentication support

The codebase follows Go best practices with clear type definitions, interface-based design, and error handling patterns.

