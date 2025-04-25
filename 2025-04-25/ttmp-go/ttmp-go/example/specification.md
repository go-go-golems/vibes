---
id: ttmp-specification
title: TTMP File Format Specification
document_type: specification
status: approved
owner: Scrapybara Team
tags:
  - specification
  - documentation
  - format
category: reference
audience: developers
abstract: Technical specification for the TTMP file format and processing rules
created: 2025-01-10T00:00:00Z
updated: 2025-04-23T15:30:00Z
see_also:
  - golang-ttmp-tutorial.md
---

# TTMP File Format Specification

This document specifies the TTMP (Text with Text Metadata Preamble) file format and processing rules.

## Overview

TTMP is a file format that combines structured metadata with markdown content. The metadata is stored as YAML frontmatter at the beginning of the file, separated by triple dashes (`---`).

## Metadata Format

The metadata section must be valid YAML and should be placed at the beginning of the file, enclosed by triple dashes.

Example:

```yaml
---
id: document-id
title: Document Title
tags:
  - tag1
  - tag2
---
```

## Required Fields

The following fields are required in the metadata section:

- `id`: A unique identifier for the document
- `title`: The title of the document
- `document_type`: The type of document (e.g., specification, tutorial)

## Optional Fields

The following fields are optional:

- `tags`: An array of tags
- `created`: Creation date (ISO 8601 format)
- `updated`: Last update date (ISO 8601 format)
- `status`: Document status (e.g., draft, published, approved)
- `owner`: Document owner or author
- `audience`: Target audience
- `abstract`: Brief summary of the document
- `category`: Category or section
- `see_also`: Related documents

## Content Format

The content section follows the metadata section and should be written in Markdown format. 

## Processing Rules

1. The YAML frontmatter must be at the beginning of the file.
2. The YAML frontmatter must be enclosed by triple dashes (`---`).
3. The YAML must be valid.
4. The content must follow the metadata section.

## Implementation Guidelines

Implementations should:

1. Parse the YAML frontmatter to extract metadata
2. Validate required fields
3. Process the markdown content separately
4. Support querying documents based on metadata
5. Support validation of document structure and content