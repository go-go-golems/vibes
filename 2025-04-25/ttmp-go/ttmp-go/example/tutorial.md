---
id: golang-ttmp-tutorial
title: TTMP System in Golang Tutorial
document_type: tutorial
status: published
owner: Scout
tags:
  - golang
  - ttmp
  - tutorial
category: documentation
audience: developers
abstract: A comprehensive guide to using the TTMP system implemented in Golang
created: 2025-04-24T00:00:00Z
updated: 2025-04-24T12:00:00Z
---

# TTMP System in Golang Tutorial

This tutorial explains how to use the TTMP (Text with Text Metadata Preamble) system implemented in Golang.

## What is TTMP?

TTMP is a simple way to add structured metadata to markdown documents using YAML frontmatter. It allows you to organize, query, and analyze your documentation more effectively.

## Installation

To install the TTMP command-line tool, run:

```bash
go get github.com/scrapybara/ttmp/cmd/ttmp
```

## Basic Usage

The TTMP CLI provides several commands for working with TTMP documents:

- `validate`: Validate TTMP documents
- `query`: Query TTMP documents
- `create`: Create new TTMP documents
- `list`: List TTMP documents in a directory

### Validating Documents

To validate a TTMP document:

```bash
ttmp validate path/to/document.md
```

### Querying Documents

To query TTMP documents in a directory:

```bash
ttmp query "author == 'Scout'" --dir ./docs
```

### Creating Documents

To create a new TTMP document:

```bash
ttmp create --title "My Document" --type "tutorial" --output myfile.md
```

## Web Interface

The TTMP system also includes a web interface for browsing and querying documents:

```bash
ttmp-server --port 8080 --path ./docs
```

Then open your browser to http://localhost:8080 to view the web interface.