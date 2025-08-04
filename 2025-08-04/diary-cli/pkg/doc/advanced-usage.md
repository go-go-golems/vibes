---
Title: Advanced Usage and Tips
Slug: advanced-usage
Short: Advanced features, tips, and workflow optimization
Topics:
- advanced
- tips
- workflow
- automation
SectionType: tutorial
---

# Advanced Usage and Tips

This guide covers advanced features and optimization techniques for power users of the diary CLI.

## Structured Output and Automation

### JSON Output for Scripting

Export diary data for analysis or integration:

```bash
# Export all entries as JSON
diary list --output json --limit 100

# Export specific types
diary list --type til --output json

# Search results as JSON
diary search "authentication" --output json
```

### CSV Export for Analysis

Create spreadsheets or import into other tools:

```bash
# Export with specific fields
diary list --output csv --fields type,content,date,tags

# Export search results
diary search "project" --output csv --fields type,title,content,file
```

### Automation Scripts

Create shell scripts for common workflows:

```bash
#!/bin/bash
# daily-review.sh - Generate daily summary

echo "# Daily Review - $(date +%Y-%m-%d)"
echo
echo "## Today's Entries"
diary show today

echo
echo "## Pending Todos"
diary todo list

echo
echo "## Recent TILs"
diary list --type til --limit 5
```

## Advanced Search and Filtering

### Complex Queries

Combine multiple filters for precise results:

```bash
# TIL entries from last month
diary list --type til --since "last month" --limit 50

# High priority todos
diary todo list --priority high

# Recent entries with specific content
diary search "microservices" --since "last week"
```

### Date Range Queries

Use natural language date parsing:

```bash
diary show "last friday"
diary list --since "3 weeks ago"
diary search "authentication" --since "last month"
```

## Editor Integration

### Visual Editor Workflows

Configure your preferred editor:

```bash
# Set default editor
diary config editor vim
diary config editor "code --wait"
diary config editor "subl --wait"
```

Use editor mode for complex entries:

```bash
# Open editor for detailed entry
diary add til --editor

# Create todo with detailed description
diary todo add --editor
```

### Editor Templates

Create templates for consistent entry formatting:

```bash
# ~/.diary-templates/til-template.md
# TIL Entry Template
# Date: $(date)
# Topic: 

## What I Learned


## Context


## Why It Matters


## Related Resources

```

## Batch Operations

### Multiple Entries

Add multiple related entries efficiently:

```bash
# Project retrospective
diary add thought "Sprint went well overall" --title "Sprint Retro"
diary add did "Completed user stories 1-5" --title "Sprint Retro"
diary add til "Learned about React hooks" --title "Sprint Retro"
diary add link "https://reactjs.org/docs/hooks-intro.html" --title "React Hooks"
```

### Import from Other Sources

Convert existing notes or logs:

```bash
# From a text file
while IFS= read -r line; do
    diary add thought "$line" --title "Imported Notes"
done < notes.txt

# From a CSV
awk -F',' 'NR>1 {print $2}' data.csv | while read entry; do
    diary add til "$entry"
done
```

## Configuration Management

### Multiple Vaults

Manage different vaults for different purposes:

```bash
# Work vault
diary config vault_path ~/work-vault

# Personal vault  
diary config vault_path ~/personal-vault

# Project-specific vault
diary config vault_path ~/projects/my-project/docs
```

### Environment-Specific Settings

Use different configurations for different contexts:

```bash
# Development environment
export DIARY_CONFIG=~/.diary-dev.yaml
diary init ~/dev-notes

# Production documentation
export DIARY_CONFIG=~/.diary-prod.yaml
diary init ~/prod-docs
```

## Integration with Other Tools

### Git Integration

Track diary changes with version control:

```bash
# Initialize git in logs directory
cd ~/vault/Logs
git init
git add .
git commit -m "Initial diary setup"

# Daily commit script
#!/bin/bash
cd ~/vault/Logs
git add .
git commit -m "Daily update: $(date +%Y-%m-%d)"
```

### Backup Automation

Automated backup strategies:

```bash
#!/bin/bash
# backup-diary.sh

VAULT_PATH=$(diary config vault_path)
BACKUP_DIR=~/diary-backups/$(date +%Y-%m-%d)

mkdir -p "$BACKUP_DIR"
cp -r "$VAULT_PATH/Logs" "$BACKUP_DIR/"

# Compress and upload to cloud storage
tar -czf "$BACKUP_DIR.tar.gz" "$BACKUP_DIR"
# rclone copy "$BACKUP_DIR.tar.gz" remote:diary-backups/
```

### Calendar Integration

Sync with calendar applications:

```bash
# Export todos with due dates
diary todo list --output json | jq -r '.[] | select(.due_date) | "\(.due_date): \(.content)"'

# Create calendar events from diary entries
diary list --output json --since today | jq -r '.[] | "\(.date): \(.type) - \(.content)"'
```

## Performance Optimization

### Large Vault Management

For vaults with many entries:

```bash
# Limit search scope
diary search "term" --since "last month"

# Use specific file patterns
diary list --limit 20

# Archive old entries
mkdir -p ~/vault/Logs/archive/2024
mv ~/vault/Logs/2024-*.md ~/vault/Logs/archive/2024/
```

### Indexing and Search

Improve search performance:

```bash
# Create index of common terms
diary list --output json | jq -r '.[].content' | tr ' ' '\n' | sort | uniq -c | sort -nr > ~/diary-index.txt

# Use grep for fast text search
grep -r "authentication" ~/vault/Logs/
```

## Workflow Automation

### Cron Jobs

Automate regular diary tasks:

```bash
# Daily reminder to add entries
0 18 * * * /usr/local/bin/diary todo add "Review today's accomplishments"

# Weekly review
0 9 * * 1 /home/user/scripts/weekly-review.sh

# Monthly backup
0 2 1 * * /home/user/scripts/backup-diary.sh
```

### Shell Aliases

Create shortcuts for common operations:

```bash
# Add to ~/.bashrc or ~/.zshrc
alias dt='diary todo add'
alias dl='diary list'
alias ds='diary search'
alias da='diary add'
alias dtil='diary add til'
alias dthought='diary add thought'
alias ddid='diary add did'
```

### Custom Functions

Advanced shell functions:

```bash
# Quick daily standup
standup() {
    echo "## Yesterday"
    diary show yesterday --type did
    echo
    echo "## Today's Plan"
    diary todo list
    echo
    echo "## Blockers"
    diary search "blocked\|blocker\|issue" --since "last week"
}

# Learning summary
learning_summary() {
    local period=${1:-"last week"}
    echo "# Learning Summary - $period"
    diary list --type til --since "$period" --output json | \
        jq -r '.[] | "- \(.content)"'
}
```

## Troubleshooting and Debugging

### Verbose Output

Enable detailed logging:

```bash
diary --log-level debug list
diary --log-level trace search "term"
```

### Configuration Debugging

Check configuration issues:

```bash
# Verify configuration
diary config

# Test file access
diary config vault_path
ls -la "$(diary config vault_path)"

# Check permissions
diary config logs_dir
ls -la "$(diary config logs_dir)"
```

### Data Validation

Verify data integrity:

```bash
# Check for malformed entries
diary list --output json | jq empty

# Validate markdown syntax
find ~/vault/Logs -name "*.md" -exec markdown-lint {} \;

# Check for duplicate IDs
diary todo list --output json | jq -r '.[].task_id' | sort | uniq -d
```

These advanced techniques help you build sophisticated workflows and integrate the diary CLI into larger productivity systems.

