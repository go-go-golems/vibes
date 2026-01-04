---
Title: Convert mento-tui to Glazed Command with Help System
Ticket: MEN-GLAZED-CONVERT
Status: active
Topics:
    - glaze
    - help-system
    - tui
    - cli
DocType: index
Intent: long-term
Owners: []
RelatedFiles: []
ExternalSources: []
Summary: ""
LastUpdated: 2025-11-18T10:25:14.519940588-05:00
---


# Convert mento-tui to Glazed Command with Help System

## Overview

This ticket converts mento-tui from a simple flag-based CLI application to a Glazed command with a comprehensive help system.

**Current State**: 
- mento-tui uses a simple `flag`-based CLI entry point (`cmd/main.go`)
- No help system beyond basic `--help` flag
- Configuration file (`mento-tui.yaml`) has no documentation or reference

**Goal**: 
- Convert mento-tui to a Glazed bare command (since it's a TUI application)
- Implement a help system using Glazed's help infrastructure
- Create help pages documenting the configuration YAML format and structure
- Enable users to access help via `mento-tui help` command

**Key Requirements**:
1. Implement `BareCommand` interface from Glazed (since TUI doesn't produce structured output)
2. Set up Glazed help system with markdown documentation
3. Create help pages for:
   - Configuration YAML reference
   - Command usage and examples
   - Service management guide
4. Integrate help system with Cobra root command
5. Maintain backward compatibility with existing `--config` flag

## Key Links

- **Related Files**: See frontmatter RelatedFiles field
- **External Sources**: See frontmatter ExternalSources field

## Status

Current status: **active**

## Topics

- glaze
- help-system
- tui
- cli

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
