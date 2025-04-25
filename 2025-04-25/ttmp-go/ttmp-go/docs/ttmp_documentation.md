# TTMP System Documentation

This comprehensive documentation covers the Tag-Tree Markdown Protocol (TTMP) system implemented in Golang, including both the command-line interface and web interface.

## Table of Contents

1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Command-Line Interface](#command-line-interface)
   - [Validation](#validation)
   - [Querying](#querying)
   - [Creating Documents](#creating-documents)
   - [Listing Documents](#listing-documents)
   - [Statistics](#statistics)
4. [Web Interface](#web-interface)
   - [Document Explorer](#document-explorer)
   - [Document View](#document-view)
   - [Query Interface](#query-interface)
   - [Validation Tool](#validation-tool)
   - [Statistics Dashboard](#statistics-dashboard)
5. [Document Structure](#document-structure)
   - [Required Fields](#required-fields)
   - [Optional Fields](#optional-fields)
6. [Query Syntax](#query-syntax)
7. [API Reference](#api-reference)
8. [Implementation Details](#implementation-details)
9. [Configuration Options](#configuration-options)
10. [Troubleshooting](#troubleshooting)

## Introduction

The Tag-Tree Markdown Protocol (TTMP) is a system that adds structured metadata to Markdown documents using YAML headers. It transforms regular Markdown documents into queryable knowledge artifacts that can be easily managed, searched, and organized.

**Key features of TTMP:**
- Structured metadata in YAML format
- Full Markdown content
- Validation of document structure
- Query capabilities based on metadata
- Ability to track relationships between documents

This Golang implementation provides a complete solution for working with TTMP documents, including both a command-line interface for automation and scripting, and a web interface for interactive use.

## Installation

### Prerequisites

- Go 1.16 or later
- Git (for installation from source)

### Installation from Source

```bash
# Clone the repository
git clone https://github.com/yourusername/ttmp-go.git
cd ttmp-go

# Build the command-line tool
go build -o ttmp ./cmd/ttmp

# Build the web server
go build -o ttmp-server ./cmd/ttmp-server

# Optional: Install to a directory in your PATH
sudo mv ttmp ttmp-server /usr/local/bin/
```

### Installation via Go Install

```bash
go install github.com/yourusername/ttmp-go/cmd/ttmp@latest
go install github.com/yourusername/ttmp-go/cmd/ttmp-server@latest
```

## Command-Line Interface

The TTMP command-line interface (CLI) provides a comprehensive set of commands for working with TTMP documents.

### Global Options

The following options can be used with any TTMP command:

| Option | Description |
|--------|-------------|
| `--help` | Show help information |
| `--version` | Show version information |
| `--verbose` | Enable verbose output |
| `--config` | Specify custom config file location |

### Validation

The `validate` command validates TTMP documents against the TTMP specification.

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

### Querying

The `query` command allows you to search for documents based on their metadata and content.

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

### Creating Documents

The `create` command generates new TTMP documents with the specified metadata.

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

### Listing Documents

The `list` command displays TTMP documents in a directory.

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

### Statistics

The `stats` command generates statistics about TTMP documents.

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

## Web Interface

The TTMP web interface provides a user-friendly way to navigate and work with TTMP documents.

### Starting the Web Server

```bash
ttmp-server --path /path/to/documents --port 8080
```

Then open your browser to http://localhost:8080 to access the web interface.

### Document Explorer

The Document Explorer is the main view of the web interface, displaying:

1. **File Tree View**: A hierarchical view of all documents, organized by directory
2. **Document List**: A card-based view showing document metadata

This view allows you to:
- Navigate through your document hierarchy
- Search for documents by title, content, or metadata
- See key metadata at a glance
- Select documents for detailed viewing

### Document View

When you select a document, the Document View displays:

1. The document title
2. Key metadata displayed in an easy-to-read format
3. The full document content with markdown rendering and syntax highlighting
4. A "View All Metadata" button for accessing the complete metadata set

Features:
- Syntax highlighting for code blocks
- Formatted rendering of markdown content
- Intuitive display of metadata
- Modal dialog for viewing all metadata fields

### Query Interface

The Query interface allows you to search for documents using:

1. Metadata queries using the TTMP query syntax
2. Content keywords for full-text search

Example queries:
- `document_type='tutorial' && status='published'`
- `owner='Scout' || tags contains 'reference'`

### Validation Tool

The Validation tool allows you to:

1. Paste TTMP document content for validation
2. See validation results with detailed error messages
3. Test document compliance with the TTMP specification

### Statistics Dashboard

The Statistics dashboard provides insights about your document collection:

1. Total document count
2. Document type distribution with pie chart visualization
3. Metadata usage statistics

This view helps you understand:
- The overall size of your document collection
- Which document types are most common
- How consistently metadata fields are being used

## Document Structure

A TTMP document consists of two parts:

1. A YAML front matter section enclosed between triple-dashes (`---`)
2. Markdown content following the front matter

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

### Required Fields

Every TTMP document must include at least the following metadata fields:

- `id`: A unique identifier for the document
- `title`: The document's title
- `document_type`: The type of document (e.g., article, tutorial, reference)
- `status`: The document's current status (e.g., draft, review, published)

### Optional Fields

There are many optional metadata fields you can include:

- `created`: Document creation date (ISO 8601 format)
- `updated`: Last update date (ISO 8601 format)
- `owner`: Document owner or author
- `tags`: List of relevant tags
- `category`: Document category
- `audience`: Intended audience
- `abstract`: Brief summary of the document
- `see_also`: Related documents
- `concepts`: Key concepts covered
- `source_files`: Source code or other files referenced
- `longevity`: Expected document lifetime

## Query Syntax

The TTMP query syntax allows you to search for documents based on their metadata.

### Field Comparisons

- Equality: `field='value'`
- Inequality: `field!='value'`
- Numeric comparisons: `field>value`, `field<=value`

### Array Operations

- Contains: `field contains 'value'`
- Not contains: `!(field contains 'value')`

### Logical Operators

- AND: `&&`
- OR: `||`
- NOT: `!`

### Grouping

You can use parentheses to group expressions:

```
(field1='value1' || field2='value2') && field3='value3'
```

### Examples

```
# Find all published tutorials
document_type='tutorial' && status='published'

# Find documents by a specific author with specific tags
owner='John Smith' && (tags contains 'advanced' || tags contains 'tutorial')

# Find documents updated since a specific date
updated>'2022-01-01'
```

## API Reference

The TTMP system exposes the following Go packages that you can use in your own applications:

### Model Package

`github.com/yourusername/ttmp-go/pkg/model`

Contains the core data structures for representing TTMP documents:

- `TTMPDocument`: Represents a complete TTMP document
- `DocumentCollection`: Represents a collection of TTMP documents

### Parser Package

`github.com/yourusername/ttmp-go/pkg/parser`

Provides functionality for parsing TTMP documents:

- `NewTTMPParser()`: Creates a new parser instance
- `ParseFile(path string)`: Parses a TTMP document from a file
- `ParseString(content string)`: Parses a TTMP document from a string

### Validator Package

`github.com/yourusername/ttmp-go/pkg/validator`

Provides functionality for validating TTMP documents:

- `NewValidator()`: Creates a new validator instance
- `Validate(doc *model.TTMPDocument)`: Validates a TTMP document

### Query Package

`github.com/yourusername/ttmp-go/pkg/query`

Provides functionality for querying TTMP documents:

- `QueryDocuments(docs []*model.TTMPDocument, query string, keywords []string)`: Searches for documents matching a query

### Server Package

`github.com/yourusername/ttmp-go/pkg/server`

Provides the web server implementation:

- `NewServer(basePath string)`: Creates a new web server instance
- `Start(addr string)`: Starts the web server

## Implementation Details

The TTMP system is implemented in Golang with the following design principles:

1. **Modularity**: The system is divided into separate packages for different responsibilities
2. **Extensibility**: The architecture allows for easy extension with new features
3. **Robustness**: Comprehensive error handling ensures reliable operation
4. **Performance**: Efficient algorithms for parsing and querying

Key implementation details:

- YAML parsing using the `gopkg.in/yaml.v2` package
- Query parsing and evaluation using a custom expression parser
- Web server implemented using the standard `net/http` package and `gorilla/mux` router
- Frontend implemented with HTML5, CSS3, JavaScript, Bootstrap 5, Chart.js, and Highlight.js

## Configuration Options

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

#### Web Server Issues

If you encounter web server issues:

1. Check that the port is not already in use
2. Ensure the document path exists and is readable
3. Check the server logs for error messages

### Getting Help

If you encounter issues not covered in this documentation:

1. Check the [GitHub issues](https://github.com/yourusername/ttmp-go/issues)
2. Report new issues with detailed error information
3. Join the TTMP community on [Discussions](https://github.com/yourusername/ttmp-go/discussions)