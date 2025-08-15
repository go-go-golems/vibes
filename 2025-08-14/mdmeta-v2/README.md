# MDMeta v2: Markdown Metadata Management CLI Tool

MDMeta is a comprehensive command-line interface tool built using the Glazed framework to manage markdown files with YAML metadata preambles. This version features proper Glazed framework integration with professional CLI patterns and multiple output formats.

## Features

- **Metadata Management**: Initialize, validate, and update YAML frontmatter in markdown files
- **Document Lifecycle**: Track creation, modification, and review timestamps
- **Professional CLI**: Built with Glazed framework for consistent output formatting
- **Multiple Output Formats**: JSON, CSV, YAML, and human-readable tables
- **Search and Query**: Find documents based on metadata criteria
- **Validation**: Schema compliance checking with auto-fix capabilities
- **CI/CD Ready**: Automated validation and compliance checking

## Installation

### From Source

```bash
git clone <repository-url>
cd mdmeta-v2
go build -o mdmeta ./cmd/mdmeta
```

### Prerequisites

- Go 1.24.6+ (required for Glazed framework)
- Git (for version control integration)

## Quick Start

### 1. Initialize Metadata

Add metadata to your existing markdown files:

```bash
# Initialize a single file
./mdmeta init --files doc.md --title "My Document"

# Initialize multiple files
./mdmeta init --files doc1.md,doc2.md,doc3.md

# Initialize recursively
./mdmeta init --files docs/ --recursive
```

### 2. List Documents

View your documents with metadata:

```bash
# List all documents (table format)
./mdmeta list --paths docs/

# JSON output
./mdmeta list --paths docs/ --output json

# CSV output for spreadsheets
./mdmeta list --paths docs/ --output csv

# Filter by status
./mdmeta list --paths docs/ --status draft
```

### 3. Validate Metadata

Ensure your metadata is compliant:

```bash
# Validate all documents
./mdmeta validate --paths docs/ --recursive

# Strict validation
./mdmeta validate --paths docs/ --strict

# Auto-fix common issues
./mdmeta validate --paths docs/ --fix
```

## Metadata Schema

MDMeta uses the `mdmeta/v1` schema:

```yaml
---
schema: mdmeta/v1
doc_id: ulid:01K2N7XNNV3E5YH8B5EZ3BGZ6Y
title: Test Document
status: draft
created_at: 2025-08-14T17:31:35.995346122-04:00
updated_at: 2025-08-14T17:31:35.995394002-04:00
path: test-docs/test.md
visibility: internal
data_class: none
---
```

## Commands

### init

Initialize metadata in markdown files.

```bash
mdmeta init --files [files...] [flags]

Flags:
  --files strings     Files or directories to initialize (required)
  --title string      Document title (defaults to filename)
  --status choice     Document status (draft, in_progress, review, final, archived) (default "draft")
  --recursive         Process directories recursively
  --force             Overwrite existing metadata
```

### list

List markdown documents with metadata.

```bash
mdmeta list --paths [paths...] [flags]

Flags:
  --paths strings         Paths to scan for markdown files (default ["."])
  --status choice         Filter by document status
  --owner string          Filter by document owner
  --tags strings          Filter by tags (comma-separated)
  --due-for-review        Show only documents due for review
  --recursive             Scan directories recursively (default true)
  --show-content          Include content preview
  --sort-field choice     Sort results by field (default "updated_at")
  --limit int             Limit number of results
```

### validate

Validate markdown metadata compliance.

```bash
mdmeta validate --paths [paths...] [flags]

Flags:
  --paths strings     Files or directories to validate (default ["."])
  --strict            Use strict validation mode
  --recursive         Process directories recursively (default true)
  --schema string     Required schema version (default "mdmeta/v1")
  --fix               Automatically fix common issues
```

## Output Formats

The Glazed framework provides multiple output formats:

- **table** (default): Human-readable table format
- **json**: JSON format for APIs and automation
- **csv**: CSV format for spreadsheets
- **yaml**: YAML format for configuration
- **markdown**: Markdown table format
- **html**: HTML table format

Use `--output <format>` to specify the

