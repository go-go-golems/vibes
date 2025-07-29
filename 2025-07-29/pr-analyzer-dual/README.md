# PR Analyzer

A comprehensive Go CLI tool for analyzing GitHub pull requests using tree-sitter and glazed, with features to get diffs, context on files/functions, commit history, and code analysis capabilities.

## Features

- **GitHub API Integration**: Retrieve PR data, diffs, commits, and file history
- **Tree-sitter Go Parsing**: Analyze Go code structure and extract function information
- **Glazed Framework**: Dual command structure with readable and structured output
- **Comprehensive Analysis**: Identify changed functions, analyze code context, and track modifications
- **Multiple Output Formats**: Table, JSON, CSV, YAML, and more
- **Authentication Support**: Works with GitHub tokens for higher rate limits

## Installation

### Prerequisites

- Go 1.21 or later
- Git
- GCC (for tree-sitter compilation)

### Build from Source

```bash
git clone <repository-url>
cd pr-analyzer
go mod tidy
go build -o pr-analyzer .
```

### Environment Setup

For authenticated GitHub API access (recommended):

```bash
export GITHUB_TOKEN=your_github_token_here
```

## Quick Start

### Basic Usage

```bash
# Get commits for a PR
./pr-analyzer get commits --owner go-go-golems --repo geppetto --pr-number 181

# Get the diff for a PR
./pr-analyzer get diff --owner go-go-golems --repo geppetto --pr-number 181

# Analyze functions changed in a PR
./pr-analyzer analyze functions --owner go-go-golems --repo geppetto --pr-number 181 --only-changed

# Get context on files and functions
./pr-analyzer get context --owner go-go-golems --repo geppetto --pr-number 181
```

### Output Formats

```bash
# Table format (default)
./pr-analyzer get commits --owner owner --repo repo --pr-number 123

# JSON format
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 --output json

# CSV format
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 --output csv

# Select specific fields
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 --fields sha,message,author_name
```

## Commands

### `get` Commands

#### `get commits`
Retrieves the list of commits in a pull request.

```bash
./pr-analyzer get commits --owner OWNER --repo REPO --pr-number NUMBER
```

**Output Fields:**
- `owner`, `repo`, `pr_number`: Repository information
- `sha`: Commit SHA
- `message`: Commit message
- `author_name`, `author_email`: Author information
- `commit_date`: When the commit was made

#### `get diff`
Retrieves the unified diff for a pull request.

```bash
./pr-analyzer get diff --owner OWNER --repo REPO --pr-number NUMBER
```

**Output Fields:**
- `owner`, `repo`, `pr_number`: Repository information
- `diff`: The complete unified diff

#### `get context`
Analyzes the PR diff to provide context on affected files and functions.

```bash
./pr-analyzer get context --owner OWNER --repo REPO --pr-number NUMBER
```

**Output Fields:**
- `file_path`: Path to the modified file
- `lines_added`, `lines_removed`, `lines_modified`: Change statistics
- `total_functions`: Total functions in the file
- `changed_functions`: Number of functions that were modified
- `changed_function_names`: Names of the changed functions

#### `get file-history`
Retrieves the commit history for a specific file.

```bash
./pr-analyzer get file-history --owner OWNER --repo REPO --file-path PATH
```

**Output Fields:**
- `file_path`: Path to the file
- `sha`: Commit SHA
- `message`: Commit message
- `author_name`, `author_email`: Author information
- `commit_date`: When the commit was made
- `committer_name`, `committer_email`: Committer information
- `committer_date`: When the commit was committed

### `analyze` Commands

#### `analyze functions`
Analyzes Go functions affected by pull request changes using tree-sitter.

```bash
./pr-analyzer analyze functions --owner OWNER --repo REPO --pr-number NUMBER [OPTIONS]
```

**Options:**
- `--show-body`: Include function body in output
- `--only-changed`: Show only functions that were changed in the PR

**Output Fields:**
- `file_path`: Path to the file containing the function
- `function_name`: Name of the function
- `receiver`: Method receiver (for methods)
- `start_line`, `end_line`: Line numbers where the function is defined
- `is_exported`: Whether the function is exported (public)
- `is_changed`: Whether the function was modified in the PR
- `signature`: Function signature
- `body`: Function body (if `--show-body` is used)

## Use Cases

### Code Review Scenarios

#### 1. Understanding What Changed
```bash
# Get an overview of the PR
./pr-analyzer get context --owner go-go-golems --repo geppetto --pr-number 181

# See which functions were modified
./pr-analyzer analyze functions --owner go-go-golems --repo geppetto --pr-number 181 --only-changed
```

#### 2. Tracking Function History
```bash
# See when a specific file was last modified
./pr-analyzer get file-history --owner go-go-golems --repo geppetto --file-path pkg/steps/ai/factory.go

# Get the commits in a PR to understand the development process
./pr-analyzer get commits --owner go-go-golems --repo geppetto --pr-number 181
```

#### 3. Detailed Function Analysis
```bash
# Get function signatures and bodies for changed functions
./pr-analyzer analyze functions --owner go-go-golems --repo geppetto --pr-number 181 --only-changed --show-body

# Export to JSON for further processing
./pr-analyzer analyze functions --owner go-go-golems --repo geppetto --pr-number 181 --output json > analysis.json
```

### Integration with Other Tools

#### CI/CD Pipeline
```bash
# Generate a report for the PR
./pr-analyzer get context --owner $GITHUB_REPOSITORY_OWNER --repo $GITHUB_REPOSITORY --pr-number $PR_NUMBER --output json > pr-analysis.json

# Check if critical functions were modified
./pr-analyzer analyze functions --owner $GITHUB_REPOSITORY_OWNER --repo $GITHUB_REPOSITORY --pr-number $PR_NUMBER --only-changed --fields function_name | grep -E "(main|init|critical)"
```

#### Code Review Automation
```bash
# Generate a summary for code reviewers
./pr-analyzer get context --owner owner --repo repo --pr-number 123 --fields file_path,changed_functions,changed_function_names --output markdown > review-summary.md
```

## Advanced Usage

### Field Selection and Filtering

```bash
# Select specific fields
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 --fields sha,message,author_name

# Filter out certain fields
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 --filter commit_date,committer_email

# Sort by field
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 --sort-by commit_date
```

### Output Customization

```bash
# Use templates for custom output
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 --template "{{.sha}}: {{.message}}"

# Output to file
./pr-analyzer get diff --owner owner --repo repo --pr-number 123 --output-file pr-123.diff

# Stream output for large results
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 --stream
```

## Architecture

### Components

1. **GitHub Client** (`internal/github/client.go`): Handles GitHub API interactions
2. **Tree-sitter Parser** (`internal/treesitter/parser.go`): Parses Go code and extracts functions
3. **Diff Analysis** (`internal/analysis/diff.go`): Parses and analyzes unified diffs
4. **Commands** (`cmd/`): Glazed-based command implementations

### Design Principles

- **Modular Architecture**: Clear separation between GitHub API, tree-sitter, and command logic
- **Glazed Integration**: Leverages glazed framework for consistent CLI experience
- **Error Handling**: Graceful degradation when files are missing or inaccessible
- **Performance**: Efficient parsing and minimal memory usage

## Troubleshooting

### Common Issues

#### Rate Limiting
If you encounter rate limiting errors:
1. Set up a GitHub token: `export GITHUB_TOKEN=your_token`
2. Use authenticated requests for higher limits

#### Tree-sitter Parsing Errors
If Go files fail to parse:
1. Ensure the file is valid Go syntax
2. Check if the file is too large (tree-sitter has memory limits)
3. Files with syntax errors will be skipped gracefully

#### Missing Files
If files are not found:
1. Verify the PR number and repository are correct
2. Check if files were deleted or renamed in the PR
3. Ensure you have access to the repository

### Debug Mode

For debugging, use the `--print-parsed-parameters` flag to see how parameters are being processed:

```bash
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 --print-parsed-parameters
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Submit a pull request

## License

[Add your license information here]

## Acknowledgments

- [Glazed](https://github.com/go-go-golems/glazed) - CLI framework
- [go-tree-sitter](https://github.com/smacker/go-tree-sitter) - Tree-sitter Go bindings
- [go-github](https://github.com/google/go-github) - GitHub API client

