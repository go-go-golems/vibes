# MDM Usage Examples

This document provides practical examples of using the Markdown Document Manager (MDM) CLI tool.

## Command Output Examples

### 1. Listing Files

```bash
$ mdm list examples/
PATH                         TITLE                                         TAGS                            PROJECT                 STATUS     MODIFIED
api-documentation.md         REST API Documentation                        api,documentation,rest,back...  user-management-system  published  2025-08-14
troubleshooting-guide.md     System Troubleshooting Guide                  troubleshooting,support,deb...  system-operations       final      2024-08-13
meeting-notes.md             Weekly Engineering Standup - August 14, 2024  meeting,standup,engineering...  team-coordination       final      2024-08-14
project-roadmap.md           Q4 2024 Product Roadmap                       planning,roadmap,product,st...  product-roadmap         draft      2024-08-14
transformer-architecture.md  Transformer Architecture Deep Dive            ai,machine-learning,transfo...  ai-research             draft      2024-08-12
react-component-library.md   React Component Library Design System         react,components,design-sys...  web-dev                 final      2024-08-14
```

### 2. Searching by Tags

```bash
$ mdm search --tags planning,roadmap
PATH                TITLE                    TAGS                                       PROJECT          STATUS  MODIFIED
project-roadmap.md  Q4 2024 Product Roadmap  planning,roadmap,product,strategy,q4-2024  product-roadmap  draft   2024-08-14
```

### 3. File Information

```bash
$ mdm info examples/projects/ai-research/transformer-architecture.md
File: examples/projects/ai-research/transformer-architecture.md
Title: Transformer Architecture Deep Dive
Description: Detailed analysis of the Transformer neural network architecture and its applications
Tags: ai, machine-learning, transformers, attention, nlp
Category: research
Project: ai-research
Repository: https://github.com/research-team/transformer-analysis
Branch: feature/attention-mechanisms
Status: draft
Priority: critical
Version: 0.3
Author: Dr. Sarah Chen
Contributors: Prof. Michael Rodriguez, Alex Kim
Language: markdown
Format: research-paper
Template: academic
Created: 2024-07-20 14:00:00
Modified: 2024-08-12 11:45:00
Last Used: 2024-08-14 09:15:00
File Size: 7706 bytes
Content Length: 6705 characters
Related Files: attention-mechanisms.md, bert-analysis.md, gpt-evolution.md
Dependencies: pytorch-implementation.py, attention-visualization.ipynb
References: https://arxiv.org/abs/1706.03762, https://arxiv.org/abs/1810.04805
```

### 4. Statistics Query

```bash
$ mdm query --directory examples/ --query stats
Total Files: 6
Total Size: 63542 bytes
Total Content: 57976 characters
Average Size: 10590 bytes
Average Content: 9662 characters
Files with Tags: 6
Files with Projects: 6
Files with Authors: 6
```

### 5. Tag Analysis

```bash
$ mdm query --directory examples/ --query tags
TAG               COUNT
v2.1              1
machine-learning  1
nlp               1
product           1
strategy          1
standup           1
support           1
team-sync         1
typescript        1
weekly            1
```

### 6. Project Statistics

```bash
$ mdm query --directory examples/ --query projects
PROJECT                 FILES  TOTAL_SIZE  AVG_SIZE
product-roadmap         1      11450       11450
ai-research             1      7706        7706
web-dev                 1      11130       11130
user-management-system  1      3194        3194
system-operations       1      16961       16961
team-coordination       1      13101       13101
```

## Workflow Examples

### Documentation Team Workflow

#### 1. Daily Documentation Review

```bash
# Check recent activity
mdm query --query recent

# Find draft documents
mdm search --status draft

# Review high-priority items
mdm search --priority critical
```

#### 2. Publishing Workflow

```bash
# Find documents ready for review
mdm search --status review

# Update status after review
mdm update api-guide.md --status published --add-tags reviewed,v2.1

# Verify publication
mdm info api-guide.md
```

#### 3. Maintenance Tasks

```bash
# Find stale documentation
mdm query --query stale

# Update last_used for accessed files
mdm info important-doc.md --touch

# Clean up old drafts
mdm search --status draft --tags old
```

### Development Team Workflow

#### 1. Project Documentation

```bash
# List all project documentation
mdm search --project web-app

# Find API documentation
mdm search --tags api --project web-app

# Check documentation coverage
mdm query --query projects
```

#### 2. Code Review Process

```bash
# Find documents needing review
mdm search --status review --project current-sprint

# Update after code review
mdm update feature-spec.md --status final --add-tags implemented

# Track implementation status
mdm search --tags implemented --project current-sprint
```

#### 3. Release Preparation

```bash
# Find all release documentation
mdm search --tags release,v2.0

# Update version information
mdm update changelog.md --version 2.0 --status published

# Generate release notes
mdm search --tags release --show-content
```

### Research Team Workflow

#### 1. Paper Management

```bash
# Find research papers
mdm search --category research

# Track paper status
mdm search --format research-paper --status draft

# Find papers by author
mdm search --author "Dr. Smith"
```

#### 2. Collaboration Tracking

```bash
# Find collaborative documents
mdm search --contributors "Prof. Johnson"

# Check recent research activity
mdm query --query recent --directory research/

# Analyze research topics
mdm query --query tags --directory research/
```

#### 3. Publication Pipeline

```bash
# Find papers ready for submission
mdm search --status final --category research

# Update submission status
mdm update paper.md --status submitted --add-tags conference-2024

# Track publication timeline
mdm search --tags submitted,accepted,published
```

## Advanced Use Cases

### 1. Content Migration

```bash
# Find old format documents
mdm search --format old-template

# Update to new template
for file in $(mdm search --format old-template --format json | jq -r '.[].path'); do
    mdm update "$file" --format new-template --add-tags migrated
done

# Verify migration
mdm search --tags migrated
```

### 2. Quality Assurance

```bash
# Find documents without proper metadata
mdm search --status ""
mdm search --priority ""
mdm search --project ""

# Check for missing descriptions
mdm list --format json | jq '.[] | select(.metadata.description == "")'

# Validate tag consistency
mdm query --query tags | grep -E "(api|API|Api)"
```

### 3. Reporting and Analytics

```bash
# Generate team productivity report
echo "# Team Productivity Report"
echo "## Recent Activity"
mdm query --query recent

echo "## Project Status"
mdm query --query projects

echo "## Tag Usage"
mdm query --query tags

echo "## Author Contributions"
mdm query --query authors
```

### 4. Automated Workflows

#### Update Script Example

```bash
#!/bin/bash
# update-docs.sh - Automated documentation maintenance

# Update last_used for accessed files
find docs/ -name "*.md" -atime -1 -exec mdm update {} --touch \;

# Find and report stale files
echo "Stale files (not modified in 30 days):"
mdm query --query stale --directory docs/

# Update status for reviewed files
for file in $(mdm search --tags reviewed --status draft --format json | jq -r '.[].path'); do
    mdm update "$file" --status final --remove-tags reviewed
done

# Generate daily report
mdm query --query stats --directory docs/ > daily-report.txt
```

#### Git Hook Integration

```bash
#!/bin/bash
# pre-commit hook to validate metadata

# Check for required metadata fields
for file in $(git diff --cached --name-only --diff-filter=A | grep '\.md$'); do
    if ! mdm info "$file" >/dev/null 2>&1; then
        echo "Error: $file missing required metadata"
        exit 1
    fi
done

# Update modified timestamp
for file in $(git diff --cached --name-only --diff-filter=M | grep '\.md$'); do
    mdm update "$file" --touch
    git add "$file"
done
```

## Integration Examples

### 1. CI/CD Pipeline

```yaml
# .github/workflows/docs.yml
name: Documentation Validation
on: [push, pull_request]

jobs:
  validate-docs:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Setup Go
        uses: actions/setup-go@v2
        with:
          go-version: 1.21
      - name: Build MDM
        run: go build -o mdm
      - name: Validate Documentation
        run: |
          # Check all markdown files have metadata
          ./mdm list --format json | jq '.[] | select(.metadata.title == "")' | jq -e 'length == 0'
          # Generate documentation report
          ./mdm query --query stats > docs-report.txt
      - name: Upload Report
        uses: actions/upload-artifact@v2
        with:
          name: docs-report
          path: docs-report.txt
```

### 2. VS Code Integration

```json
// .vscode/tasks.json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "MDM: List Files",
            "type": "shell",
            "command": "mdm",
            "args": ["list"],
            "group": "build",
            "presentation": {
                "echo": true,
                "reveal": "always",
                "focus": false,
                "panel": "shared"
            }
        },
        {
            "label": "MDM: Search by Tags",
            "type": "shell",
            "command": "mdm",
            "args": ["search", "--tags", "${input:tags}"],
            "group": "build"
        }
    ],
    "inputs": [
        {
            "id": "tags",
            "description": "Enter tags to search for",
            "default": "api,documentation",
            "type": "promptString"
        }
    ]
}
```

### 3. Makefile Integration

```makefile
# Makefile
.PHONY: docs-stats docs-validate docs-update

docs-stats:
	@echo "Documentation Statistics:"
	@mdm query --query stats
	@echo "\nProject Breakdown:"
	@mdm query --query projects

docs-validate:
	@echo "Validating documentation metadata..."
	@mdm list --format json | jq '.[] | select(.metadata.title == "")' | jq -e 'length == 0' || (echo "Error: Files missing titles" && exit 1)
	@echo "All files have required metadata ✓"

docs-update:
	@echo "Updating documentation timestamps..."
	@find docs/ -name "*.md" -exec mdm update {} --touch \;
	@echo "Updated last_used timestamps ✓"

docs-report:
	@echo "# Documentation Report" > docs-report.md
	@echo "Generated: $(shell date)" >> docs-report.md
	@echo "" >> docs-report.md
	@echo "## Statistics" >> docs-report.md
	@mdm query --query stats >> docs-report.md
	@echo "" >> docs-report.md
	@echo "## Recent Files" >> docs-report.md
	@mdm query --query recent >> docs-report.md
```

## Tips and Tricks

### 1. Efficient Searching

```bash
# Use multiple criteria for precise results
mdm search --project web-app --status final --tags api

# Search content with context
mdm search --content "authentication" --show-content

# Find files by date range (using shell)
mdm list --format json | jq '.[] | select(.metadata.modified > "2024-08-01")'
```

### 2. Batch Operations

```bash
# Update multiple files with same project
for file in $(mdm search --project old-name --format json | jq -r '.[].path'); do
    mdm update "$file" --project new-name
done

# Add tags to all API documentation
mdm search --tags api --format json | jq -r '.[].path' | xargs -I {} mdm update {} --add-tags documented
```

### 3. Custom Queries

```bash
# Find files without recent activity
mdm list --format json | jq '.[] | select(.metadata.last_used == null or .metadata.last_used < "2024-07-01")'

# Count files by status
mdm list --format json | jq 'group_by(.metadata.status) | map({status: .[0].metadata.status, count: length})'

# Find largest files
mdm list --format json | jq 'sort_by(.size) | reverse | .[0:5] | .[] | {path: .path, size: .size}'
```

These examples demonstrate the flexibility and power of the MDM tool for various documentation and project management workflows.

