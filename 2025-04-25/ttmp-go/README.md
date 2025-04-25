# TTMP: Timestamped Tagged Markdown Preamble

A Golang implementation of the TTMP (Timestamped Tagged Markdown Preamble) system for adding structured metadata to Markdown documents.

## What is TTMP?

TTMP is a system for adding structured metadata to markdown documents. It transforms working documents into "first-class citizens" that can be queried, analyzed, and maintained programmatically.

Key features include:
- Structured YAML headers (preambles) at the beginning of markdown files
- Metadata fields for document identity, time tracking, relationships to code and other documents
- Standardized format for organizing and managing documentation
- Tools for validating and processing TTMP documents

## Installation

```bash
go get github.com/ttmp/ttmp-go
```

Or download the latest release from the Releases page.

## Usage

### Creating a New TTMP Document

```bash
ttmp create --id example-doc --title "Example Document" --tags example,documentation \
  --owner "@username" --type guide document.md
```

### Validating TTMP Documents

```bash
ttmp validate document.md
```

Or validate all documents in a directory:

```bash
ttmp validate ./docs --recursive
```

### Querying TTMP Documents

Find documents with a specific tag:

```bash
ttmp query ./docs --tag documentation
```

Find documents by owner:

```bash
ttmp query ./docs --owner "@username"
```

Find stale documents:

```bash
ttmp query ./docs --stale
```

### Listing TTMP Documents

List all documents in a directory:

```bash
ttmp list ./docs
```

With sorting and filtering:

```bash
ttmp list ./docs --recursive --sort updated --status draft
```

## TTMP Document Structure

A TTMP document consists of two parts:
1. A YAML frontmatter at the beginning of the file, enclosed between `---` markers
2. The document content in Markdown format

Example:

```md
---
id: example-doc
title: "Example Document"
document_type: guide
longevity: long
owner: "@username"
created: 2025-04-23
updated: 2025-04-23
status: draft
tags: [documentation, example]
source_files:
  - src/main.go
  - src/utils.go
---

# Example Document

This is the content of the document...
```

## License

MIT