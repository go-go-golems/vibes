---
Title: Imagelayout template preview uses backend computation
Ticket: IMAGELAYOUT-PREVIEW
Status: active
Topics:
    - imagelayout
    - frontend
    - ux
DocType: index
Intent: short-term
Owners: []
RelatedFiles:
    - Path: ../../../../../../../../../zine-layout/pkg/serve/layout_preview_routes.go
      Note: POST /projects/{id}/image-layout/preview handler
    - Path: ../../../../../../../../../zine-layout/pkg/services/layout.go
      Note: PreviewLayout helper reusing InputsFromRequest
    - Path: ../../../../../../../../../zine-layout/web/src/api.ts
      Note: RTK mutation previewLayoutRequest
    - Path: ../../../../../../../../../zine-layout/web/src/views/tabs/ImageLayoutsTab.tsx
      Note: Preview overlay renders backend canvas/target rects
    - Path: 2025/11/29/photobook-app/2025/11/30/IMAGELAYOUT-PREVIEW-imagelayout-template-preview-uses-backend-computation/playbook/01-preview-backend-geometry.md
      Note: Playbook covering curl + UI steps
ExternalSources: []
Summary: ""
LastUpdated: 2025-12-01T00:20:00-05:00
---


# Imagelayout template preview uses backend computation

## Overview

<!-- Provide a brief overview of the ticket, its goals, and current status -->

## Key Links

- **Related Files**: See frontmatter RelatedFiles field
- **External Sources**: See frontmatter ExternalSources field

## Status

Current status: **active**

## Topics

- imagelayout
- frontend
- ux

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
