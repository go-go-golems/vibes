---
id: ttmp-example
title: "Example TTMP Document"
document_type: guide
longevity: long
owner: "@example-user"
created: 2025-04-23
updated: 2025-04-23
status: draft
tags: [documentation, example, ttmp]
source_files:
  - cmd/ttmp/main.go
  - pkg/model/document.go
see_also:
  - 01-design-yaml-ttmp-preamble.md
schema_version: 1.0
review_cycle: 90d
abstract: >
  This is an example TTMP document demonstrating the structure and format used
  in the TTMP system. It shows how metadata is organized in the YAML frontmatter.
---

# Example TTMP Document

## Introduction

This is an example TTMP document that demonstrates how structured metadata is included in the YAML frontmatter.

## What is TTMP?

TTMP (Timestamped Tagged Markdown Preamble) is a system for adding structured metadata to markdown documents. It transforms working documents into "first-class citizens" that can be queried, analyzed, and maintained programmatically.

The key elements of the TTMP system include:

1. A structured YAML header (preamble) at the beginning of markdown files
2. Metadata fields for document identity, time tracking, relationships to code and other documents
3. A standardized format for organizing and managing documentation
4. The ability to validate and process these documents programmatically

## Using TTMP

TTMP documents are designed to be both human-readable and machine-processable. The frontmatter provides a structured way to include metadata while the main content follows standard markdown formatting.

By using TTMP, teams can:

- Track document relationships
- Monitor document freshness
- Build dashboards for documentation
- Ensure documentation stays in sync with code
- Create knowledge graphs based on document connections

This enables a more systematic approach to documentation management in software projects.