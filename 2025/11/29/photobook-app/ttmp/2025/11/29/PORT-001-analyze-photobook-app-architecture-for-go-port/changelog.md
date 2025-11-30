# Changelog

## 2025-11-29

- Initial workspace created
- Added reference doc summarizing current backend/frontend flows and gaps for Go port

## 2025-11-29

Linked key backend/frontend files to docs using docmgr relate

## 2025-11-29

- Expanded reference doc for new contributors with architecture, flows, env vars, and known gaps

## 2025-11-29

Deepened architecture reference with endpoint contracts, flows, env requirements, and newcomer guidance


## 2025-11-29

Added Go migration design options (SQLite/disk vs job-runner abstraction) with REST contracts and auth/storage plans


## 2025-11-29

Fleshed out minimal Go monolith design (tRPC-compatible API, SQLite schema, storage/auth interfaces, worker flow, Go signatures)


## 2025-11-30

Expanded architecture reference document with comprehensive implementation details: frontend-backend integration (tRPC hooks), PDF worker algorithm (jsPDF+canvas, aspect-fit calculations), OAuth session flow (JWT signing/verification, cookie config), storage API contracts, tRPC configuration, and detailed gap analysis. Documented that frontend is fully integrated (contrary to earlier notes) and identified missing features (PDF job polling, job history UI).


## 2025-11-30

Expanded Go backend migration design document with comprehensive explanatory paragraphs for each section. Added detailed context to Executive Summary, Problem Statement, Proposed Solution, Design Decisions, Alternatives Considered, Implementation Plan, Open Questions, and all Detailed Design sections. Document now provides full context for newcomers understanding the migration strategy, technical decisions, and implementation approach.


## 2025-11-30

Created debate framework infrastructure: candidate profiles and 12 debate questions for exploring Go backend migration options


## 2025-11-30

Created combined debate round 1-3 covering foundational architecture: tRPC vs REST, SQLite+disk vs production backends, in-process vs separate worker. Multiple rounds of argumentation with data-driven evidence from codebase analysis.


## 2025-11-30

Updated debate round 1-3 to include RTK Query as REST option. Added analysis showing RTK Query bridges DX gap between tRPC and REST—similar hooks/caching/types with standard HTTP protocol. Updated arguments to reflect moderate refactoring effort vs zero changes.


## 2025-11-30

Created debate round 4-7 covering architecture and mechanics: job queue abstraction, Go project layout, authentication strategy, and file serving approach. Incorporated decisions: tRPC compatibility and single binary.


## 2025-11-30

Created debate rounds 8-10 (mechanics) and 11-12 (configuration/MVP). Explored PDF generation (gofpdf), migrations (Goose), error handling (zerolog+pkg/errors), configuration (Glazed vs simple env vars), and MVP checklist. Incorporated Glazed framework analysis for configuration management.


## 2025-11-30

Created debate synthesis document summarizing all 12 debate rounds with final architectural decisions. Documented technology stack, project structure, key interfaces, implementation phases, and open questions.


## 2025-11-30

Created comprehensive implementation guide for interns with project context, design patterns, decisions, and getting started instructions. Created 25 implementation tasks organized by phase (infrastructure, auth, photos, PDFs, integration).

