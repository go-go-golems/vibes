# TTMP Command-Line Interface Documentation

This document provides comprehensive documentation for the TTMP command-line interface, covering installation, usage, and examples.

## Installation

To install the TTMP command-line tool:

```bash
# Build from source
cd ttmp-go
go build -o ttmp ./cmd/ttmp

# Optional: Move to a directory in your PATH
sudo mv ttmp /usr/local/bin/
```

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

```
ttmp validate [options] <file-or-directory>
```

#### Options

- `--recursive`: Recursively validate all files in directories
- `--strict`: Enable strict validation mode
- `--report`: Generate validation report file

#### Examples

```bash
# Validate a single document
ttmp validate document.md

# Validate all documents in a directory
ttmp validate --recursive ./docs

# Generate a validation report
ttmp validate --report ./validation-report.json document.md
```

#### Validation Rules

The validator checks:

1. YAML frontmatter is correctly formatted
2. Required fields are present (id, title, document_type, status)
3. Field types match expected types
4. Document structure follows the TTMP specification

### query

Query TTMP documents based on metadata and content.

```
ttmp query [options] <query-expression>
```

#### Options

- `--dir`: Directory to search (default: current directory)
- `--recursive`: Recursively search subdirectories
- `--format`: Output format (json, yaml, text)
- `--keywords`: Additional keywords to search in content

#### Examples

```bash
# Find all published tutorials
ttmp query "document_type='tutorial' && status='published'"

# Find documents by a specific author containing keywords
ttmp query "owner='John Smith'" --keywords "golang,performance"

# Find documents with specific tags, output as JSON
ttmp query --format json "tags contains 'important'"
```

#### Query Syntax

- Field comparisons: `field='value'`, `field!='value'`
- Numeric comparisons: `field>value`, `field<=value`
- Array containment: `field contains 'value'`
- Logical operators: `&&` (and), `||` (or), `!` (not)
- Grouping: `(expression)`

### create

Create a new TTMP document with the specified metadata.

```
ttmp create [options]
```

#### Options

- `--id`: Document ID
- `--title`: Document title
- `--type`: Document type
- `--status`: Document status
- `--owner`: Document owner
- `--tags`: Document tags (comma-separated)
- `--output`: Output file path
- `--template`: Template to use (optional)

#### Examples

```bash
# Create a new tutorial document
ttmp create --id tutorial-002 --title "Advanced TTMP Features" --type tutorial --status draft --owner "Jane Doe" --tags "advanced,ttmp,features" --output advanced-tutorial.md

# Create a document using a template
ttmp create --template reference --id ref-001 --title "API Reference" --output api-reference.md
```

### list

List TTMP documents in a directory.

```
ttmp list [options] [directory]
```

#### Options

- `--recursive`: Recursively list documents in subdirectories
- `--format`: Output format (json, yaml, text)
- `--sort`: Sort field (id, title, type, status, date)

#### Examples

```bash
# List all documents in the current directory
ttmp list

# List all documents sorted by date
ttmp list --recursive --sort date

# Output document list as JSON
ttmp list --format json
```

#### Output Formats

- **text** (default): Human-readable tabular format
- **json**: JSON array of document metadata
- **yaml**: YAML array of document metadata

### stats

Generate statistics about TTMP documents.

```
ttmp stats [options] [directory]
```

#### Options

- `--recursive`: Include subdirectories
- `--format`: Output format (json, yaml, text)

#### Examples

```bash
# Generate statistics for all documents
ttmp stats --recursive

# Output statistics as JSON
ttmp stats --format json
```

#### Statistics Provided

- Total document count
- Document counts by type
- Document counts by status
- Metadata field usage statistics
- Average document size
- Creation/update date ranges

## Working with Templates

TTMP supports document templates for creating consistent documents.

### Default Templates

The system includes several default templates:

- **tutorial**: Template for tutorial documents
- **reference**: Template for reference documents
- **article**: Template for general articles

### Custom Templates

You can create custom templates in the `.ttmp/templates` directory in your home directory.

Example template file (`~/.ttmp/templates/proposal.md`):

```markdown
---
id: {{id}}
title: {{title}}
document_type: proposal
status: draft
owner: {{owner}}
created: {{now}}
tags:
  - proposal
---

# {{title}}

## Summary

[Brief summary of the proposal]

## Motivation

[Why this proposal is needed]

## Proposal

[Detailed proposal description]

## Implementation

[Implementation details]

## Alternatives Considered

[Alternative approaches]
```

To use this template:

```bash
ttmp create --template proposal --id prop-001 --title "New Feature Proposal" --owner "John Smith" --output proposal.md
```

## Configuration

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

### Environment Variables

TTMP respects the following environment variables:

- `TTMP_CONFIG`: Path to the configuration file
- `TTMP_TEMPLATES`: Path to document templates directory
- `TTMP_DEFAULT_OWNER`: Default document owner

## Integration Examples

### Validating Documents in CI Pipeline

```bash
#!/bin/bash
# Validate all Markdown documents in the docs directory
ttmp validate --recursive --strict ./docs
if [ $? -ne 0 ]; then
  echo "Validation failed!"
  exit 1
fi
```

### Generating a Documentation Index

```bash
#!/bin/bash
# Generate an index of all published documents
ttmp query "status='published'" --format json > published.json
```

### Bulk Creating Documents

```bash
#!/bin/bash
# Create multiple tutorial documents from a list
while IFS=, read -r id title owner; do
  ttmp create --id "$id" --title "$title" --type tutorial --status draft --owner "$owner" --output "docs/$id.md"
done < tutorials.csv
```

## Troubleshooting

### Common Issues

#### Parser Errors

If you encounter parser errors:

1. Check that the YAML frontmatter is correctly formatted
2. Ensure the frontmatter is enclosed by triple dashes (`---`)
3. Verify that the YAML is valid

#### Query Errors

If you encounter query errors:

1. Check the query syntax
2. Ensure that field names match those in your documents
3. Make sure string values are enclosed in single quotes

#### Exit Codes

The TTMP command-line tool uses the following exit codes:

- **0**: Success
- **1**: General error
- **2**: Validation error
- **3**: Query syntax error
- **4**: File not found
- **5**: Permission error