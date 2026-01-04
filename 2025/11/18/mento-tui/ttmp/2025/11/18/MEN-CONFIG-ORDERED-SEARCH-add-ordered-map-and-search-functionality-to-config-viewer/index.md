---
Title: Add ordered map and search functionality to config viewer
Ticket: MEN-CONFIG-ORDERED-SEARCH
Status: active
Topics:
    - ui
    - config
    - search
DocType: index
Intent: long-term
Owners: []
RelatedFiles:
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/models/models.go
      Note: Config struct needs to change map[string]string to OrderedMap
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/config.go
      Note: Needs modification to use OrderedMap and add search functionality
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/logviewer.go
      Note: Reference implementation for search mode pattern
ExternalSources: []
Summary: Add ordered map for stable environment variable ordering and search functionality to filter env vars in config viewer
LastUpdated: 2025-11-18T10:03:15.805602155-05:00
---






# Add ordered map and search functionality to config viewer

## Overview

Add two improvements to the config viewer:
1. **Ordered Map**: Replace `map[string]string` with an ordered map implementation so environment variables maintain consistent display order
2. **Search Functionality**: Add search mode (similar to log viewer) to filter environment variables by key or value

**Current Problem**: Go maps don't preserve insertion order, causing environment variables to appear in different orders on each render. Additionally, there's no way to search/filter environment variables.

**Solution**: Use `elliotchance/orderedmap` library for ordered storage and implement search mode using the same pattern as log viewer.

## Key Links

- **Design Document**: [Analysis: Ordered Map and Search for Config Viewer](./design-doc/01-analysis-ordered-map-and-search-for-config-viewer.md) - Detailed analysis of implementation approach
- **Related Files**: See frontmatter RelatedFiles field
- **External Sources**: See frontmatter ExternalSources field

## Status

Current status: **active**

## Topics

- ui
- config
- search

## Tasks

See [tasks.md](./tasks.md) for the current task list.

## Changelog

See [changelog.md](./changelog.md) for recent changes and decisions.

## Structure

- design/ - Architecture and design documents
- reference/ - Prompt packs, API contracts, context summaries
- playbooks/ - Command sequences and test procedures
- scripts/ - Temporary code and tooling
- various/ - Working notes and research
- archive/ - Deprecated or reference-only artifacts
