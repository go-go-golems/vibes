---
Title: Fix search mode keyboard input routing
Ticket: MEN-SEARCH-INPUT-FIX
Status: active
Topics:
    - ui
    - keyboard-input
    - bugfix
DocType: index
Intent: long-term
Owners: []
RelatedFiles:
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/app.go
      Note: Modified Update method to delegate to child screens first
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/logviewer.go
      Note: Search mode handlers now receive keys correctly due to parent-level routing fix
ExternalSources: []
Summary: Fixed keyboard input routing in search mode by reversing message routing order - child screens now handle keys before global handlers
LastUpdated: 2025-11-18T10:00:05.653284482-05:00
---





# Fix search mode keyboard input routing

## Overview

Fix keyboard input routing in search mode. Global key handlers in `app.go` intercept ESC, Enter, and other keys before they reach the search input component in the log viewer, preventing proper search functionality.

**Root Cause**: Message routing order processes global keys before delegating to child screens, breaking when child screens need to handle the same keys in different contexts (like search mode).

**Solution**: Reverse routing order - delegate to child screen first, then handle global keys only if not consumed by the child screen.

## Key Links

- **Bug Report**: [04-search-mode-keyboard-input-routing-global-keys-intercept-search-input.md](../../MEN-CONFIG-YAML-make-service-binaries-configurable-via-yaml-file/bug-report/04-search-mode-keyboard-input-routing-global-keys-intercept-search-input.md) - Original bug report with detailed analysis
- **Related Files**: See frontmatter RelatedFiles field
- **External Sources**: See frontmatter ExternalSources field

## Status

Current status: **active**

## Topics

- ui
- keyboard-input
- bugfix

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
