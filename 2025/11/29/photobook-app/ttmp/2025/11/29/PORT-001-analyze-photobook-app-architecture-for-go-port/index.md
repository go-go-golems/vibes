---
Title: Analyze photobook app architecture for Go port
Ticket: PORT-001
Status: active
Topics:
    - backend
    - frontend
DocType: index
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2025/11/29/photobook-app/server/_core/index.ts
      Note: API server entry Express+tRPC OAuth
    - Path: 2025/11/29/photobook-app/server/index.ts
      Note: Static-only server used by build start missing APIs
    - Path: 2025/11/29/photobook-app/ttmp/2025/11/29/PORT-001-analyze-photobook-app-architecture-for-go-port/reference/01-current-architecture-and-data-flow.md
      Note: Reference summary of current app flows
ExternalSources: []
Summary: Doc'd current Node/tRPC + MySQL/storage flows, noted frontend is local-only and build omits API server
LastUpdated: 2025-11-29T20:20:00-05:00
---



# Analyze photobook app architecture for Go port

## Overview

- Node/Express + tRPC backend (OAuth auth, photo + PDF job routes, Forge storage proxy) exists in `server/_core`, but production build/start only serves static files via `server/index.ts` (API not wired).
- MySQL via Drizzle stores users/photos/pdfJobs; PDF worker polls every 10 s and uploads finished PDFs to storage proxy.
- React frontend is currently local-only (object URLs, drag/drop slideshow); helpers for Manus OAuth/Maps exist but are unused.
- Reference doc captures routes, data stores, auth flow, and integration gaps to unblock Go port planning.

## Key Links

- Reference: [Current architecture and data flow](./reference/01-current-architecture-and-data-flow.md)
- **Related Files**: See frontmatter RelatedFiles field
- **External Sources**: See frontmatter ExternalSources field

## Status

Current status: **active**

## Topics

- backend
- frontend

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
