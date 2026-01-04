---
id: reference-001
title: TTMP CLI Reference
document_type: reference
status: published
created: 2025-04-15
updated: 2025-04-20
owner: Samantha Programmer
tags:
  - reference
  - cli
  - ttmp
category: documentation
audience: developers
abstract: |
  Complete reference guide for the TTMP command-line interface,
  including all available commands, options, and examples.
see_also:
  - tutorial-001
  - specification-001
---

# TTMP CLI Reference

This reference guide provides detailed information about all commands and options available in the TTMP command-line interface.

## Global Options

The following options can be used with any TTMP command:

| Option | Description |
|--------|-------------|
| `--help` | Show help information |
| `--version` | Show version information |
| `--verbose` | Enable verbose output |
| `--config` | Specify custom config file location |

## Commands

### validate

Validates TTMP documents against the TTMP specification.

```bash
ttmp validate [options] <file-or-directory>
```

Options:

- `--recursive`: Recursively validate all files in directories
- `--strict`: Enable strict validation mode
- `--report`: Generate validation report file

Examples:

```bash
# Validate a single document
ttmp validate document.md

# Validate all documents in a directory
ttmp validate --recursive ./docs
```

### query

Query TTMP documents based on metadata and content.

```bash
ttmp query [options] <query-expression>
```

Options:

- `--dir`: Directory to search (default: current directory)
- `--recursive`: Recursively search subdirectories
- `--format`: Output format (json, yaml, text)
- `--keywords`: Additional keywords to search in content

Examples:

```bash
# Find all published tutorials
ttmp query "document_type='tutorial' && status='published'"

# Find documents by a specific author containing keywords
ttmp query "owner='John Smith'" --keywords "golang,performance"
```

### create

Create a new TTMP document with the specified metadata.

```bash
ttmp create [options]
```

Options:

- `--id`: Document ID
- `--title`: Document title
- `--type`: Document type
- `--status`: Document status
- `--owner`: Document owner
- `--tags`: Document tags (comma-separated)
- `--output`: Output file path

Examples:

```bash
# Create a new tutorial document
ttmp create --id tutorial-002 --title "Advanced TTMP Features" --type tutorial --status draft --owner "Jane Doe" --tags "advanced,ttmp,features" --output advanced-tutorial.md
```

### list

List TTMP documents in a directory.

```bash
ttmp list [options] [directory]
```

Options:

- `--recursive`: Recursively list documents in subdirectories
- `--format`: Output format (json, yaml, text)
- `--sort`: Sort field (id, title, type, status, date)

Examples:

```bash
# List all documents in the current directory
ttmp list

# List all documents sorted by date
ttmp list --recursive --sort date
```

### stats

Generate statistics about TTMP documents.

```bash
ttmp stats [options] [directory]
```

Options:

- `--recursive`: Include subdirectories
- `--format`: Output format (json, yaml, text)

Examples:

```bash
# Generate statistics for all documents
ttmp stats --recursive
```

## Query Syntax

The query command uses a simple expression language:

- Field comparisons: `field='value'`, `field!='value'`
- Numeric comparisons: `field>value`, `field<=value`
- Array containment: `field contains 'value'`
- Logical operators: `&&` (and), `||` (or), `!` (not)
- Grouping: `(expression)`

## Configuration File

You can customize TTMP behavior by creating a `.ttmp.yaml` file in your home directory or the current working directory:

```yaml
# TTMP Configuration
validate:
  strict: true
  ignore_patterns:
    - "*.draft.md"
    - "temp/*"

create:
  templates_dir: "~/.ttmp/templates"
  default_owner: "Your Name"
```

## Environment Variables

TTMP respects the following environment variables:

- `TTMP_CONFIG`: Path to the configuration file
- `TTMP_TEMPLATES`: Path to document templates directory
- `TTMP_DEFAULT_OWNER`: Default document owner