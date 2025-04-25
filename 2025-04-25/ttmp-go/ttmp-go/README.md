# TTMP System in Golang

A complete implementation of the Tag-Tree Markdown Protocol (TTMP) system in Golang, providing both command-line and web interfaces for working with structured Markdown documents.

## What is TTMP?

TTMP (Tag-Tree Markdown Protocol) is a system that adds structured metadata to Markdown documents using YAML headers. It transforms working documents into queryable and maintainable knowledge artifacts.

Key features of TTMP:
- Structured metadata in YAML format
- Full Markdown content
- Validation of document structure
- Query capabilities based on metadata
- Ability to track relationships between documents

## Components

This repository includes:

1. **Core TTMP Library**: Go packages for parsing, validating, and processing TTMP documents
2. **Command-Line Interface**: A powerful CLI for interacting with TTMP documents
3. **Web Interface**: A browser-based interface for navigating and working with TTMP documents

## Installation

To install the TTMP system:

```bash
# Clone the repository
git clone https://github.com/yourusername/ttmp-go.git
cd ttmp-go

# Build the command-line tool
go build -o ttmp ./cmd/ttmp

# Build the web server
go build -o ttmp-server ./cmd/ttmp-server
```

## Command-Line Usage

The TTMP command-line tool provides several commands:

### Validating Documents

```bash
# Validate a single document
./ttmp validate path/to/document.md

# Validate all documents in a directory
./ttmp validate --recursive ./docs
```

### Querying Documents

```bash
# Find all published tutorials
./ttmp query "document_type='tutorial' && status='published'"

# Find documents by a specific author containing keywords
./ttmp query "owner='John Smith'" --keywords "golang,performance"
```

### Creating Documents

```bash
# Create a new tutorial document
./ttmp create --id tutorial-002 --title "Advanced TTMP Features" --type tutorial --status draft --owner "Jane Doe" --tags "advanced,ttmp,features" --output advanced-tutorial.md
```

### Listing Documents

```bash
# List all documents in the current directory
./ttmp list

# List all documents sorted by date
./ttmp list --recursive --sort date
```

### Generating Statistics

```bash
# Generate statistics for all documents
./ttmp stats --recursive
```

## Web Interface

The TTMP web interface provides a user-friendly way to navigate and work with TTMP documents.

### Starting the Web Server

```bash
./ttmp-server --path /path/to/documents --port 8080
```

Then open your browser to http://localhost:8080 to access the web interface.

### Web Interface Features

- File tree visualization for navigating document hierarchies
- Document listing with metadata display
- Detailed document view with content and metadata presentation
- Query functionality for searching documents
- Document validation tool
- Statistics dashboard for collection insights

For more information, see the [Web Interface Documentation](docs/web_interface.md).

## Document Structure

A TTMP document consists of:

1. A YAML frontmatter section enclosed between triple-dashes (`---`)
2. Markdown content following the frontmatter

Example:

```markdown
---
id: sample-doc-001
title: Sample TTMP Document
document_type: article
status: draft
owner: John Smith
tags:
  - example
  - sample
---

# Sample TTMP Document

This is the content of the document...
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.