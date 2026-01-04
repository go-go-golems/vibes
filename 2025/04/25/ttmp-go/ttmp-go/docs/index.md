# TTMP System Documentation

Welcome to the comprehensive documentation for the TTMP (Tag-Tree Markdown Protocol) system implemented in Golang. This documentation covers all aspects of the system, including installation, usage, and API reference.

## What is TTMP?

TTMP (Tag-Tree Markdown Protocol) is a system that adds structured metadata to Markdown documents using YAML headers. It transforms regular Markdown documents into queryable knowledge artifacts that can be easily managed, searched, and organized.

Key features of TTMP:
- Structured metadata in YAML format
- Full Markdown content
- Validation of document structure
- Query capabilities based on metadata
- Ability to track relationships between documents

## Documentation Sections

### Core Documentation

- [**Complete System Documentation**](ttmp_documentation.md) - Comprehensive documentation covering all aspects of the TTMP system
- [**README**](../README.md) - Quick overview and getting started guide

### Command-Line Interface

- [**CLI Documentation**](cli_documentation.md) - Detailed documentation for the TTMP command-line interface

### Web Interface

- [**Web Interface Documentation**](web_interface.md) - Documentation for the TTMP web interface
- [**Web Interface Screenshots**](web_screenshots.md) - Screenshots of the TTMP web interface in action

### API Reference

- [**API Reference**](api_reference.md) - Complete reference for the TTMP Go API

## Quick Start

### Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/ttmp-go.git
cd ttmp-go

# Build the command-line tool
go build -o ttmp ./cmd/ttmp

# Build the web server
go build -o ttmp-server ./cmd/ttmp-server
```

### Command-Line Usage

```bash
# Validate a document
./ttmp validate document.md

# Query documents
./ttmp query "document_type='tutorial' && status='published'"

# Create a new document
./ttmp create --id doc-001 --title "New Document" --type article --status draft --output document.md
```

### Web Interface

```bash
# Start the web server
./ttmp-server --path /path/to/documents --port 8080
```

Then open your browser to http://localhost:8080 to access the web interface.

## Sample Document

Here's a simple example of a TTMP document:

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
created: 2022-01-01T00:00:00Z
updated: 2022-01-02T00:00:00Z
category: documentation
audience: developers
abstract: A sample TTMP document demonstrating the format
---

# Sample TTMP Document

This is a sample document that demonstrates the TTMP format. 

## What is TTMP?

TTMP adds structured metadata to Markdown documents using YAML headers.

## Why use TTMP?

TTMP makes your documents more searchable, manageable, and interconnected.
```

## Reference Links

- [Golang Documentation](https://golang.org/doc/)
- [YAML Specification](https://yaml.org/spec/)
- [Markdown Guide](https://www.markdownguide.org/)

## Contributing

We welcome contributions to the TTMP system! Please see the [README](../README.md) for information on how to contribute.