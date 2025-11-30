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
    - Path: 2025/11/29/photobook-app/client/src/pages/Home.tsx
      Note: Frontend implementation with tRPC integration
    - Path: 2025/11/29/photobook-app/server/_core/cookies.ts
      Note: Cookie configuration logic (httpOnly
    - Path: 2025/11/29/photobook-app/server/_core/env.ts
      Note: Environment variable configuration and validation
    - Path: 2025/11/29/photobook-app/server/_core/index.ts
      Note: API server entry Express+tRPC OAuth
    - Path: 2025/11/29/photobook-app/server/_core/trpc.ts
      Note: tRPC configuration with superjson transformer and procedure types (public/protected/admin)
    - Path: 2025/11/29/photobook-app/server/_core/vite.ts
      Note: Vite dev server integration and static file serving
    - Path: 2025/11/29/photobook-app/server/index.ts
      Note: Static-only server used by build start missing APIs
    - Path: 2025/11/29/photobook-app/ttmp/2025/11/29/PORT-001-analyze-photobook-app-architecture-for-go-port/reference/01-current-architecture-and-data-flow.md
      Note: Reference summary of current app flows
    - Path: 2025/11/29/photobook-app/ttmp/2025/11/29/PORT-001-analyze-photobook-app-architecture-for-go-port/reference/02-debate-format-and-candidates.md
      Note: Debate candidate profiles and rules
    - Path: 2025/11/29/photobook-app/ttmp/2025/11/29/PORT-001-analyze-photobook-app-architecture-for-go-port/reference/03-debate-questions.md
      Note: 12 debate questions exploring migration trade-offs
    - Path: 2025/11/29/photobook-app/ttmp/2025/11/29/PORT-001-analyze-photobook-app-architecture-for-go-port/reference/04-debate-synthesis-and-decisions.md
      Note: Synthesis of all debate rounds with final architectural decisions
    - Path: 2025/11/29/photobook-app/ttmp/2025/11/29/PORT-001-analyze-photobook-app-architecture-for-go-port/reference/debate-round-1-3-foundational-architecture.md
      Note: Combined debate round exploring foundational architecture decisions with multiple argument rounds
    - Path: 2025/11/29/photobook-app/ttmp/2025/11/29/PORT-001-analyze-photobook-app-architecture-for-go-port/reference/debate-round-11-12-alignment-mvp.md
      Note: Debate round covering configuration management (Glazed vs simple env vars) and MVP checklist definition
    - Path: 2025/11/29/photobook-app/ttmp/2025/11/29/PORT-001-analyze-photobook-app-architecture-for-go-port/reference/debate-round-4-7-architecture-mechanics.md
      Note: Combined debate round exploring job queue abstraction
    - Path: 2025/11/29/photobook-app/ttmp/2025/11/29/PORT-001-analyze-photobook-app-architecture-for-go-port/reference/debate-round-8-10-mechanics.md
      Note: Debate round covering PDF generation library choice
ExternalSources: []
Summary: Comprehensive documentation of Node/tRPC + MySQL/storage architecture, frontend-backend integration, PDF worker implementation, and identified gaps for Go port
LastUpdated: 2025-11-29T20:20:00-05:00
---









# Analyze photobook app architecture for Go port

## Overview

This ticket documents the complete architecture of the photobook application to enable a successful port from Node.js/TypeScript to Go. The photobook app allows users to upload images, reorder them via drag-and-drop, and generate PDF photobooks. The current implementation uses a Node/Express backend with tRPC for type-safe APIs, MySQL for persistence, and a Forge storage proxy for blob storage.

**Current State**: The application has a fully functional backend API server in `server/_core/index.ts` that handles OAuth authentication, photo management, and PDF job creation. However, there's a critical production build issue: the build process bundles `server/index.ts` instead, which only serves static files and omits all API routes. The React frontend is fully integrated with the backend via tRPC hooks, contrary to earlier documentation that suggested it was local-only.

**Data Layer**: MySQL database (accessed via Drizzle ORM) stores three main entities: users (with OAuth integration), photos (with ordering and storage references), and PDF jobs (tracking generation status). A background worker polls for pending PDF jobs every 10 seconds, downloads photos, generates PDFs using jsPDF and canvas, and uploads completed PDFs to the storage proxy.

**Purpose of This Ticket**: This workspace contains comprehensive documentation of the current architecture, including detailed API contracts, data flows, authentication mechanisms, PDF generation algorithms, and identified gaps. This documentation serves as the source of truth for planning the Go port, ensuring no behaviors or contracts are lost during migration.

## Key Links

**Start Here**: If you're new to this project, begin with the [Current architecture and data flow](./reference/01-current-architecture-and-data-flow.md) reference document. This comprehensive guide walks through every component of the system, from authentication flows to PDF generation algorithms, written specifically for someone coming up to speed on the codebase.

**Related Files**: The frontmatter RelatedFiles field lists all key source files that implement the documented architecture. These files are linked bidirectionally—you can find documentation from code and code from documentation. Key files include the API server entrypoint, tRPC configuration, frontend implementation, and various core modules.

**External Sources**: Any external documentation, API contracts, or reference materials are listed in the ExternalSources field. Currently, the architecture is documented entirely within this workspace.

## Status

Current status: **active**

This ticket is actively being worked on. The architecture documentation is comprehensive and up-to-date, covering all major components of the system. The next steps involve planning the Go port implementation based on this documented architecture.

## Topics

- **backend**: All server-side components including API routes, database access, authentication, storage integration, and background workers
- **frontend**: React client application with tRPC integration, drag-and-drop photo management, and slideshow interface

## Tasks

See [tasks.md](./tasks.md) for the current task list. The tasks track the progress of documenting the architecture and planning the Go port. Key completed items include mapping the frontend-backend integration (which is fully wired via tRPC) and capturing comprehensive architecture details. Remaining tasks focus on production build fixes and planning Go-specific implementations.

## Changelog

See [changelog.md](./changelog.md) for recent changes and decisions. The changelog tracks the evolution of this documentation effort, including major expansions of the architecture reference document with implementation details, frontend integration analysis, and gap identification.

## Structure

This ticket workspace follows a standard docmgr structure for organizing documentation:

- **design/** - Architecture and design documents exploring Go port options and implementation strategies
- **reference/** - Comprehensive reference documentation including the main architecture guide (`01-current-architecture-and-data-flow.md`) that serves as the primary onboarding document
- **playbooks/** - Command sequences and test procedures (not yet populated)
- **scripts/** - Temporary code and tooling (not yet populated)
- **various/** - Working notes and research (not yet populated)
- **archive/** - Deprecated or reference-only artifacts (not yet populated)

The reference directory contains the most important document: a 700+ line comprehensive guide to the current architecture that covers everything from environment variables to PDF generation algorithms.
