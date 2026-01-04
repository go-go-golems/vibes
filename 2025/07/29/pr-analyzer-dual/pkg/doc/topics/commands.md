---
Title: Command Reference - Complete Guide to PR Analyzer Commands
Slug: commands-reference
Short: Complete reference for all pr-analyzer-dual commands with parameters, examples, and output formats
Topics:
  - commands
  - reference
  - get
  - analyze
  - parameters
Commands:
  - get commits
  - get context
  - get diff
  - get file-history
  - analyze functions
  - analyze function-history
Flags:
  - owner
  - repo
  - pr-number
  - file-path
  - function-name
  - show-body
  - only-changed
  - max-commits
IsTopLevel: true
IsTemplate: false
ShowPerDefault: true
SectionType: GeneralTopic
---

# Command Reference

## Command Structure and Philosophy

The pr-analyzer-dual tool organizes commands into logical groups that reflect different types of analysis workflows. Each command follows consistent parameter patterns and supports both human-readable and structured output modes, ensuring predictable behavior across the entire tool.

All commands require GitHub repository information (`--owner`, `--repo`) and most work with pull request numbers (`--pr-number`), creating a uniform interface for GitHub-based analysis.

## Get Commands

Get commands focus on retrieving and presenting information from GitHub pull requests. They provide the foundational data that other tools and workflows can build upon.

### get commits

Retrieves comprehensive commit information for a pull request, including authorship, timestamps, and commit messages.

**Required Parameters:**
- `--owner` - GitHub repository owner (user or organization name)
- `--repo` - GitHub repository name
- `--pr-number` - Pull request number to analyze

**Output Fields:**
- `owner`, `repo`, `pr_number` - Repository identification
- `sha` - Full commit SHA hash
- `message` - Complete commit message
- `author_name`, `author_email` - Commit author information
- `commit_date` - ISO 8601 timestamp of commit

**Example Usage:**
```bash
# Human-readable output
./pr-analyzer-dual get commits --owner microsoft --repo vscode --pr-number 12345

# JSON output for automation
./pr-analyzer-dual get commits --owner microsoft --repo vscode --pr-number 12345 \
    --with-glaze-output --output json

# CSV with specific fields
./pr-analyzer-dual get commits --owner microsoft --repo vscode --pr-number 12345 \
    --with-glaze-output --output csv --fields sha,author_name,commit_date
```

### get context

Analyzes pull request changes to provide file-level context with function statistics. This command bridges the gap between raw diffs and structured code analysis.

**Required Parameters:**
- `--owner` - GitHub repository owner
- `--repo` - GitHub repository name  
- `--pr-number` - Pull request number to analyze

**Output Fields:**
- `file_path` - Relative path to the modified file
- `lines_added`, `lines_removed`, `lines_modified` - Line change statistics
- `total_functions` - Total number of functions found in the file
- `changed_functions` - Count of functions that were modified
- `changed_function_names` - Comma-separated list of changed function names

**Example Usage:**
```bash
# Overview of all changed files
./pr-analyzer-dual get context --owner golang --repo go --pr-number 54321

# Table format for structured review
./pr-analyzer-dual get context --owner golang --repo go --pr-number 54321 \
    --with-glaze-output --output table
```

### get diff

Retrieves the complete unified diff for a pull request, providing the raw change information that other analysis tools can process.

**Required Parameters:**
- `--owner` - GitHub repository owner
- `--repo` - GitHub repository name
- `--pr-number` - Pull request number to analyze

**Output Fields:**
- `owner`, `repo`, `pr_number` - Repository identification
- `diff` - Complete unified diff content

**Example Usage:**
```bash
# Human-readable diff with formatting
./pr-analyzer-dual get diff --owner kubernetes --repo kubernetes --pr-number 98765

# Raw diff for processing
./pr-analyzer-dual get diff --owner kubernetes --repo kubernetes --pr-number 98765 \
    --with-glaze-output --output json | jq -r '.diff'
```

### get file-history

Retrieves commit history for a specific file, enabling temporal analysis of how files have evolved.

**Required Parameters:**
- `--owner` - GitHub repository owner
- `--repo` - GitHub repository name
- `--file-path` - Path to the file within the repository

**Output Fields:**
- `file_path` - Path to the tracked file
- `sha`, `message` - Commit identification and description
- `author_name`, `author_email` - Commit author information
- `commit_date`, `committer_date` - Temporal information

**Example Usage:**
```bash
# File evolution history
./pr-analyzer-dual get file-history --owner docker --repo docker \
    --file-path cmd/docker/main.go

# Recent changes in CSV format
./pr-analyzer-dual get file-history --owner docker --repo docker \
    --file-path cmd/docker/main.go --with-glaze-output --output csv
```

## Analyze Commands

Analyze commands use tree-sitter parsing to provide deep insights into Go code structure and changes. These commands focus on function-level analysis and are particularly valuable for understanding the impact of code changes.

### analyze functions

Performs comprehensive function-level analysis of Go files changed in a pull request, using tree-sitter for accurate parsing and change detection.

**Required Parameters:**
- `--owner` - GitHub repository owner
- `--repo` - GitHub repository name
- `--pr-number` - Pull request number to analyze

**Optional Parameters:**
- `--show-body` - Include function body source code in output (default: false)
- `--only-changed` - Filter results to show only functions that were modified (default: false)

**Output Fields:**
- `file_path` - File containing the function
- `function_name` - Function identifier
- `receiver` - Method receiver type (for methods, empty for functions)
- `start_line`, `end_line` - Function boundary line numbers
- `is_exported` - Whether the function is publicly accessible
- `is_changed` - Whether this function was modified in the pull request
- `signature` - Complete function signature
- `body` - Function source code (when `--show-body` is enabled)

**Example Usage:**
```bash
# All functions in changed files
./pr-analyzer-dual analyze functions --owner hashicorp --repo terraform --pr-number 11111

# Only changed functions with source code
./pr-analyzer-dual analyze functions --owner hashicorp --repo terraform --pr-number 11111 \
    --only-changed --show-body

# Structured output for processing
./pr-analyzer-dual analyze functions --owner hashicorp --repo terraform --pr-number 11111 \
    --with-glaze-output --output json --fields function_name,is_changed,signature
```

### analyze function-history

Tracks the evolution of a specific function over time, showing how it has changed across commits. This command is particularly useful for understanding the development history of critical functions.

**Required Parameters:**
- `--owner` - GitHub repository owner
- `--repo` - GitHub repository name
- `--file-path` - Path to the Go file containing the function
- `--function-name` - Name of the function to track

**Optional Parameters:**
- `--max-commits` - Maximum number of commits to analyze (default: 20)
- `--show-body` - Include function body in output (default: false)

**Output Fields:**
- `owner`, `repo`, `file_path`, `function_name` - Identification fields
- `commit_number`, `commit_sha`, `commit_date` - Commit context
- `author`, `message` - Commit metadata
- `function_found` - Whether the function exists in this commit
- `receiver`, `start_line`, `end_line` - Function location details
- `is_exported`, `signature`, `function_type` - Function characteristics
- `body` - Function source code (when `--show-body` is enabled)
- `error`, `parse_error` - Error information if parsing fails

**Example Usage:**
```bash
# Function evolution over time
./pr-analyzer-dual analyze function-history --owner prometheus --repo prometheus \
    --file-path cmd/prometheus/main.go --function-name main

# Detailed history with source code
./pr-analyzer-dual analyze function-history --owner prometheus --repo prometheus \
    --file-path cmd/prometheus/main.go --function-name main \
    --max-commits 10 --show-body

# Structured data for analysis
./pr-analyzer-dual analyze function-history --owner prometheus --repo prometheus \
    --file-path cmd/prometheus/main.go --function-name main \
    --with-glaze-output --output json
```

## Global Options and Behaviors

### Authentication
All commands support both authenticated and anonymous GitHub API access. Set the `GITHUB_TOKEN` environment variable for authenticated access, which provides higher rate limits and access to private repositories.

### Error Handling
Commands gracefully handle common error conditions:
- Invalid repository or pull request numbers
- Network connectivity issues
- GitHub API rate limiting
- Parse errors in Go source code
- Missing files or functions

### Output Consistency
All commands follow consistent output patterns:
- Human-readable mode provides formatted, contextual information
- Structured mode offers machine-readable data with consistent field names
- Error messages are clear and actionable
- Long-running operations provide progress feedback where appropriate

For more details on output mode selection and formatting options, see:
```
glaze help dual-mode
```
