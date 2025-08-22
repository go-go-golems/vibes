# PR Analyzer

A Go CLI utility that analyzes GitHub pull requests to compute percentage of language-related changes and cross-subsystem statistics, with support for custom glob patterns for categorization.

## Features

- **Language Analysis**: Computes percentage of changes by programming language
- **Cross-Subsystem Analysis**: Identifies commits that touch multiple subsystems
- **Custom Categorization**: Uses glob patterns to define semantic groups/subsystems
- **Multiple Output Formats**: Table, JSON, and YAML output
- **Merge Commit Analysis**: Analyzes specific merge commits to understand PR impact
- **Branch Comparison**: Compares branches to analyze differences
- **Exclude Patterns**: Filter out files using glob patterns

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
# Analyze a merge commit
pr-analyzer analyze --merge-commit abc123def456

# Analyze branch differences
pr-analyzer analyze --pr-branch feature/new-api --base-branch main

# Use default category patterns
pr-analyzer analyze --merge-commit abc123def456 --use-defaults

# Custom categories
pr-analyzer analyze --merge-commit abc123def456 --categories "frontend:frontend/**,*.css,*.js;backend:backend/**,*.go"

# Exclude files
pr-analyzer analyze --merge-commit abc123def456 --excludes "*.md,docs/**"

# JSON output
pr-analyzer analyze --merge-commit abc123def456 --output json
```

### Command Line Options

#### Global Flags
- `--repo-path string`: Path to git repository (default ".")
- `--output string`: Output format: table, json, yaml (default "table")
- `--config string`: Path to config file

#### Analyze Command Flags
- `--pr-branch string`: Branch to analyze as PR (required unless using --merge-commit)
- `--base-branch string`: Base branch to compare against (default "main")
- `--merge-commit string`: Specific merge commit to analyze
- `--categories string`: Custom categories in format 'name1:pattern1,pattern2;name2:pattern3'
- `--excludes string`: Comma-separated exclude patterns
- `--use-defaults`: Use default category patterns

### Category Patterns

Categories are defined using glob patterns. The format is:
```
"category1:pattern1,pattern2;category2:pattern3,pattern4"
```

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
- PR information summary
- Language statistics with percentages
- Cross-system analysis metrics
- Most touched systems
- System co-occurrence matrix
- Categories configuration

#### JSON Format
Structured JSON output suitable for programmatic processing:
```json
{
  "pr_info": {
    "merge_commit": "abc123...",
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

## Examples

### Example 1: Analyze Frontend Changes
```bash
pr-analyzer analyze --merge-commit abc123 --categories "ui:frontend/**,*.css,*.js;api:backend/**,*.go"
```

Output shows:
- 85% of changes in JavaScript/CSS (UI)
- 15% of changes in Go (API)
- Cross-system rate: 100% (touches both UI and API)

### Example 2: Backend-Only Changes
```bash
pr-analyzer analyze --merge-commit def456 --use-defaults
```

Output shows:
- 70% Go, 30% SQL
- Single-system commits: 2, Multi-system commits: 0
- Cross-system rate: 0%

### Example 3: Full-Stack Integration
```bash
pr-analyzer analyze --merge-commit ghi789 --use-defaults --excludes "*.md"
```

Output shows multiple systems touched with co-occurrence matrix.

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

The repository includes a comprehensive test suite with a fake repository containing:

- Multiple feature branches
- Merge commits simulating pull requests
- Various file types and languages
- Cross-system changes

Run tests:
```bash
cd testdata/fake-repo
# Test various merge commits
../../pr-analyzer analyze --merge-commit <commit-hash> --use-defaults
```

## Validation Results

The tool has been validated with the test repository showing:

1. **Frontend-only PR**: 0% cross-system rate, CSS/JS language detection
2. **Backend-only PR**: 0% cross-system rate, Go/SQL language detection  
3. **Full-stack PR**: 100% cross-system rate, multiple languages and systems

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

