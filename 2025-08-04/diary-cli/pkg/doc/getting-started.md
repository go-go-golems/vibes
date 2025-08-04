---
Title: Getting Started with Diary CLI
Slug: getting-started
Short: Quick start guide for the diary CLI tool
Topics:
- tutorial
- basics
SectionType: tutorial
---

# Getting Started with Diary CLI

The diary CLI tool helps you manage diary entries in Obsidian markdown files with support for the Tasks plugin. This guide will get you up and running quickly.

## Installation

1. Download the diary binary for your platform
2. Place it in your PATH or use it directly
3. Initialize your diary in an existing directory or Obsidian vault

## Quick Start

### 1. Initialize Your Diary

First, navigate to your Obsidian vault or create a new directory:

```bash
# In your Obsidian vault
diary init

# Or specify a path
diary init /path/to/your/vault
```

This creates:
- Configuration file (`~/.diary-config.yaml`)
- Logs directory structure
- Today's diary file
- Sample README

### 2. Add Your First Entry

Add a "Today I Learned" entry:

```bash
diary add til "Go interfaces are satisfied implicitly"
```

Add other types of entries:

```bash
diary add thought "Architecture considerations for microservices"
diary add did "Completed authentication system implementation"
diary add link "https://example.com" --title "Interesting Article"
```

### 3. Create Todos

Create todos that integrate with Obsidian Tasks plugin:

```bash
diary todo add "Review pull requests"
diary todo add "Write documentation" --priority high --due tomorrow
```

### 4. Query Your Entries

List recent entries:

```bash
diary list
diary list --limit 20
diary list --type til
```

Search for specific content:

```bash
diary search "authentication"
diary search "go interfaces" --type til
```

Show entries for a specific date:

```bash
diary show today
diary show yesterday
diary show 2025-08-04
```

## Entry Formats

The diary tool supports three output formats:

### Default Format (default)
Simple markdown format suitable for reading:

```markdown
## TIL: Go interfaces are satisfied implicitly

*Added: 2025-08-04 09:31*
```

### Enhanced Markdown Format (markdown)
Rich markdown with metadata:

```markdown
## TIL: Go interfaces are satisfied implicitly
**Type:** til  
**Date:** 2025-08-04 09:31  

---
```

### Obsidian Tasks Format (task)
Compatible with the Obsidian Tasks plugin:

```markdown
- [ ] **TIL**: Go interfaces are satisfied implicitly #toProcess #til
  - Added: 2025-08-04 09:31
```

## Configuration

Check your configuration:

```bash
diary config
```

Update configuration values:

```bash
diary config vault_path /path/to/vault
diary config default_limit 20
diary config editor vim
```

## Next Steps

- Explore interactive mode: `diary add` (without arguments)
- Try structured output: `diary list --output json`
- Set up your preferred editor: `diary config editor vim`
- Learn about advanced features in the other help topics

For more detailed information, see:
- `diary help entry-types` - Learn about different entry types
- `diary help formats` - Understand output formats
- `diary help obsidian-integration` - Obsidian-specific features

