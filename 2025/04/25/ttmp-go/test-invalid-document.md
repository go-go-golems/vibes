---
id: invalid@document
type: invalid-type
title: Document with Validation Errors
# missing created field
modified: 2025-05-01T10:00:00Z
tags:
  - this-is-a-valid-tag
  - invalid tag with spaces
links:
  - url: 
    title: Link without URL
  - type: missing-url-and-title
---
# Invalid Document

This document contains several validation errors:

1. The ID contains an invalid character (@)
2. The type is not one of the allowed types
3. The created field is missing
4. One of the tags contains spaces
5. One of the links is missing a URL
6. One of the links is missing a title

This document should fail validation with clear error messages.