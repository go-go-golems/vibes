# PR Analyzer

A Go CLI utility that analyzes GitHub pull requests to compute percentage of language-related changes and cross-subsystem statistics, with support for custom glob patterns for categorization.

## Features

- **Language Analysis**: Computes percentage of changes by programming language
- **Cross-Subsystem Analysis**: Identifies commits that touch multiple subsystems (now includes `uncategorized`)
- **Custom Categorization**: Uses glob patterns (doublestar) to define semantic groups/subsystems
- **Multiple Output Formats**: Table, JSON, and YAML output
- **Commit/PR Analysis**: Analyze any commit (merge or non-merge) or a branch range
- **Exclude Patterns**: Filter out files using glob patterns
- **Configurable Logging**: `--log-level trace|debug|info|warn|error`

## Installation

### Prerequisites

- Go 1.21 or later
- Git repository access

### Build from Source

```bash
git clone <repository-url>
cd pr-analyzer
go mod tidy
go build -o pr-analyzer .
```

## Usage

### Basic Commands

```bash
# Analyze a specific commit (works for merge and non-merge commits)
pr-analyzer analyze --commit abc123def456

# Analyze branch differences
pr-analyzer analyze --pr-branch feature/new-api --base-branch main

# Use default category patterns
pr-analyzer analyze --commit abc123def456 --use-defaults

# Custom categories
pr-analyzer analyze --commit abc123def456 --categories "frontend:frontend/**,*.css,*.js;backend:backend/**,*.go"

# Exclude files
pr-analyzer analyze --commit abc123def456 --excludes "*.md,docs/**"

# JSON output
pr-analyzer analyze --commit abc123def456 --output json

# Increase verbosity
pr-analyzer analyze --commit abc123def456 --log-level debug
```

### Command Line Options

#### Global Flags
- `--repo-path string` / `--repo`: Path to git repository (default ".")
- `--output string`: Output format: table, json, yaml (default "table")
- `--config string`: Path to config file
- `--log-level string`: Log level: trace, debug, info, warn, error (default "info")
- `--db-path string`: Path to sqlite database file (default `pr-analyzer.sqlite`)

#### Analyze Command Flags
- `--pr-branch string`: Branch to analyze as PR (required unless using --commit)
- `--base-branch string`: Base branch to compare against (default "main")
- `--commit string`: Specific commit to analyze (merge or non-merge)
- `--categories string`: Custom categories in format 'name1:pattern1,pattern2;name2:pattern3'
- `--excludes string`: Comma-separated exclude patterns
- `--use-defaults`: Use default category patterns
- `--save-to-db`: Save analysis result to sqlite database (uses `--db-path`)

### SQLite Mode

Initialize the database, save analyses, and query aggregates across many commits/PRs.

```bash
# Initialize schema (creates file if needed)
pr-analyzer db init --db-path ./pr-stats.sqlite

# Analyze and save a commit
pr-analyzer analyze --commit abc123 --use-defaults --save-to-db --db-path ./pr-stats.sqlite

# Aggregate languages across all saved analyses
pr-analyzer db languages --db-path ./pr-stats.sqlite

# Aggregate systems across all saved analyses
pr-analyzer db systems --db-path ./pr-stats.sqlite
```

### Category Patterns

Categories are defined using glob patterns (via `github.com/bmatcuk/doublestar/v4`) supporting `**`, `*`, `?`, character classes and alternations.
Format:
```
"category1:pattern1,pattern2;category2:pattern3,pattern4"
```
Examples:
- `frontend:frontend/**,*.css,*.js`
- `backend:{api,services}/**,*.go`
- `tests:**/*_test.*`

#### Default Categories

When using `--use-defaults`, the following categories are applied:

- **frontend**: `frontend/**`, `web/**`, `ui/**`, `client/**`, `*.html`, `*.css`, `*.js`, `*.jsx`, `*.ts`, `*.tsx`, `*.vue`, `*.svelte`
- **backend**: `backend/**`, `server/**`, `api/**`, `services/**`, `*.go`, `*.py`, `*.java`, `*.rb`, `*.php`, `*.rs`
- **database**: `database/**`, `db/**`, `migrations/**`, `*.sql`, `*.db`
- **config**: `config/**`, `configs/**`, `*.yaml`, `*.yml`, `*.json`, `*.toml`, `*.ini`, `*.conf`, `*.cfg`
- **docs**: `docs/**`, `documentation/**`, `*.md`, `*.rst`, `*.txt`, `README*`
- **tests**: `test/**`, `tests/**`, `*_test.*`, `*Test.*`, `*.test.*`
- **build**: `build/**`, `scripts/**`, `Makefile`, `*.mk`, `Dockerfile*`, `docker-compose*`, `*.sh`, `*.bat`, `*.ps1`

### Output Formats

#### Table Format (Default)
Human-readable tables showing:
- PR information summary (includes repository path and merge commit metadata when applicable)
- Language statistics with percentages
- Cross-system analysis metrics (includes `uncategorized`)
- Most touched systems
- System co-occurrence matrix
- Categories configuration

#### JSON Format
Structured JSON output suitable for programmatic processing:
```json
{
  "pr_info": {
    "repo_path": "/abs/path/to/repo",
    "base_branch": "main",
    "pr_branch": "feature/x",
    "commit": "abc123...",
    "merge_commit": "deadbeef...",
    "merge_author_name": "Alice",
    "merge_author_email": "alice@example.com",
    "merge_author_date": "2025-08-22T10:00:00Z",
    "merge_committer_name": "Bob",
    "merge_committer_email": "bob@example.com",
    "merge_committer_date": "2025-08-22T10:05:00Z",
    "merge_summary": "Merge branch 'feature/x'",
    "total_files": 10,
    "total_lines": 250,
    "total_commits": 3
  },
  "language_stats": [...],
  "cross_system_stats": {...},
  "commits": [...],
  "categories": {...}
}
```

#### YAML Format
YAML output for configuration-friendly processing.

## Architecture

The tool is built with a modular architecture:

- **cmd/**: Cobra CLI commands
- **internal/analysis/**: Core analysis logic
- **internal/git/**: Git repository operations
- **internal/output/**: Output formatters

### Key Components

1. **Language Detection**: File extension-based language identification
2. **Category Matching**: Glob pattern matching for file categorization
3. **Git Analysis**: Commit diff analysis and merge commit processing
4. **Statistics Calculation**: Language percentages and cross-system metrics

## Testing

Run tests:
```bash
go test ./...
```

Manual validation with a fake repo:
```bash
# Example: analyze specific commit and save
pr-analyzer analyze --commit <commit-hash> --use-defaults --save-to-db --db-path ./pr-stats.sqlite
# Query aggregates
pr-analyzer db languages --db-path ./pr-stats.sqlite
pr-analyzer db systems --db-path ./pr-stats.sqlite
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

[Add your license here]

## Support

For issues and questions, please use the GitHub issue tracker.

