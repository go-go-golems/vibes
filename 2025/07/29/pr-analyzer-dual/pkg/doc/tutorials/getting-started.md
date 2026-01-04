---
Title: Getting Started with PR Analyzer Dual
Slug: getting-started
Short: Step-by-step tutorial to start analyzing GitHub pull requests with PR Analyzer Dual
Topics:
  - tutorial
  - getting-started
  - setup
  - installation
Commands:
  - pr-analyzer
  - get
  - analyze
Flags:
  - owner
  - repo
  - pr-number
IsTopLevel: true
IsTemplate: false
ShowPerDefault: true
SectionType: Tutorial
---

# Getting Started Tutorial

## Prerequisites

Before using PR Analyzer, ensure you have:

1. **Go 1.21 or later** - Required for building the tool
2. **Git** - For repository operations
3. **GCC** - Required for tree-sitter compilation
4. **GitHub Token** (optional but recommended) - For higher API rate limits

## Installation

### Step 1: Clone and Build
```bash
git clone <repository-url>
cd pr-analyzer-dual
go mod tidy
go build -o pr-analyzer .
```

### Step 2: Setup Authentication (Optional)
```bash
export GITHUB_TOKEN=your_github_token_here
```

Without a token, you'll be limited to 60 requests per hour. With authentication, you get 5,000 requests per hour.

## Your First Analysis

Let's analyze a real pull request to understand the tool's capabilities.

### Step 1: Get PR Overview
```bash
./pr-analyzer get context --owner go-go-golems --repo geppetto --pr-number 181
```

This command shows:
- Which files were modified
- How many lines were added/removed
- Number of functions affected
- Names of changed functions

### Step 2: Examine Specific Changes
```bash
./pr-analyzer analyze functions --owner go-go-golems --repo geppetto --pr-number 181 --only-changed
```

This shows:
- Detailed function-level changes
- Function signatures
- Whether functions are exported (public)
- Line numbers where changes occurred

### Step 3: Review Commit History
```bash
./pr-analyzer get commits --owner go-go-golems --repo geppetto --pr-number 181
```

This provides:
- Chronological list of commits
- Commit messages and authors
- SHA hashes for reference

## Understanding Output Modes

### Human-Readable Output (Default)
The default output is formatted in markdown for easy reading:

```markdown
# Function Analysis for PR #181

**Repository:** go-go-golems/geppetto
**Filter:** Only showing changed functions

## 📁 pkg/steps/ai/factory.go

### 🔄 NewStepFactory
- **Type:** Function
- **Lines:** 45-67
- **Exported:** true
- **Status:** Changed in this PR
```

### Structured Output
Add `--output json` for machine processing:

```json
[
  {
    "owner": "go-go-golems",
    "repo": "geppetto", 
    "pr_number": 181,
    "file_path": "pkg/steps/ai/factory.go",
    "function_name": "NewStepFactory",
    "start_line": 45,
    "end_line": 67,
    "is_exported": true,
    "is_changed": true
  }
]
```

## Common Use Cases

### Code Review Workflow
1. **Start with context** to understand the scope of changes
2. **Focus on changed functions** to see what actually modified
3. **Review commit history** to understand the development process

```bash
# Complete review workflow
./pr-analyzer get context --owner $OWNER --repo $REPO --pr-number $PR
./pr-analyzer analyze functions --owner $OWNER --repo $REPO --pr-number $PR --only-changed
./pr-analyzer get commits --owner $OWNER --repo $REPO --pr-number $PR
```

### Automation Integration
Use structured output for CI/CD pipelines:

```bash
# Generate analysis report
./pr-analyzer get context --owner $OWNER --repo $REPO --pr-number $PR --output json > analysis.json

# Check for critical function changes
./pr-analyzer analyze functions --owner $OWNER --repo $REPO --pr-number $PR --only-changed --fields function_name | grep -E "(main|init)"
```

## Next Steps

1. **Explore advanced options** - Try `--show-body` to see function implementations
2. **Use field selection** - Filter output with `--fields` for specific data
3. **Try different formats** - Experiment with CSV, YAML output formats
4. **Integrate with tools** - Use with jq, awk, or scripting languages
5. **Set up automation** - Add to your CI/CD pipeline for automatic PR analysis

## Troubleshooting

### Rate Limiting
```bash
# Set up authentication
export GITHUB_TOKEN=your_token

# Verify it works
./pr-analyzer get commits --owner octocat --repo Hello-World --pr-number 1
```

### File Parsing Issues
Some files may not parse successfully. The tool handles this gracefully:
- Syntax errors in Go files are skipped
- Non-Go files are ignored for function analysis
- Missing files (deleted in PR) are handled appropriately

### Getting Help
```bash
# Command-specific help
./pr-analyzer get commits --help
./pr-analyzer analyze functions --help

# Full parameter details
./pr-analyzer get commits --print-parsed-parameters
```
