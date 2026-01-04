---
Title: Basic Usage Examples
Slug: basic-usage
Short: Essential examples for common PR analysis tasks
Topics:
  - examples
  - basic-usage
  - get
  - analyze
Commands:
  - get commits
  - get context
  - analyze functions
Flags:
  - owner
  - repo
  - pr-number
IsTopLevel: true
IsTemplate: false
ShowPerDefault: true
SectionType: Example
---

# Basic Usage Examples

## Getting Started

### Authentication Setup
```bash
export GITHUB_TOKEN=your_github_token_here
```

### Basic PR Analysis
```bash
# Get overview of PR changes
./pr-analyzer get context --owner go-go-golems --repo geppetto --pr-number 181

# List all commits in PR
./pr-analyzer get commits --owner go-go-golems --repo geppetto --pr-number 181

# Analyze changed functions
./pr-analyzer analyze functions --owner go-go-golems --repo geppetto --pr-number 181 --only-changed
```

## Output Format Examples

### JSON for Automation
```bash
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 --output json > commits.json
```

### CSV for Spreadsheets
```bash
./pr-analyzer analyze functions --owner owner --repo repo --pr-number 123 --output csv > functions.csv
```

### Field Selection
```bash
# Only show specific fields
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 --fields sha,message,author_name

# Sort by specific field
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 --sort-by commit_date
```

## Common Workflows

### Code Review Preparation
```bash
# Get summary of changes
./pr-analyzer get context --owner $OWNER --repo $REPO --pr-number $PR

# Focus on changed functions only
./pr-analyzer analyze functions --owner $OWNER --repo $REPO --pr-number $PR --only-changed --show-body
```

### CI/CD Integration
```bash
# Generate structured data for pipeline
./pr-analyzer get context --owner $GITHUB_REPOSITORY_OWNER --repo $GITHUB_REPOSITORY --pr-number $PR_NUMBER --output json > pr-analysis.json

# Check for changes to critical functions
./pr-analyzer analyze functions --owner $OWNER --repo $REPO --pr-number $PR --only-changed --fields function_name | grep -E "(main|init|critical)"
```

### File History Tracking
```bash
# Track changes to specific file
./pr-analyzer get file-history --owner owner --repo repo --file-path pkg/important/module.go

# Get detailed diff for the PR
./pr-analyzer get diff --owner owner --repo repo --pr-number 123
```
