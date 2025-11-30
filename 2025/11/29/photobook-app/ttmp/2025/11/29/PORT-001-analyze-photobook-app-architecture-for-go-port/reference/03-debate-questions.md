---
Title: Debate questions for Go backend migration
Ticket: PORT-001
Status: active
Topics:
    - backend
    - architecture
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
ExternalSources: []
Summary: 12 debate questions exploring Go backend migration options, mapped to candidates
LastUpdated: 2025-11-30T00:00:00-05:00
---

# Debate Questions for Go Backend Migration

## Overview

This document defines 12 debate questions that will explore the key architectural decisions for porting the photobook application backend from Node.js/TypeScript to Go. Each question builds on previous ones, moving from foundational decisions to detailed implementation choices.

**Question Flow**: Foundation → Architecture → Mechanics → Alignment

## Question Progression

### Foundation Questions (Rounds 1-3)

#### Round 1: Should we maintain tRPC compatibility or switch to pure REST?

**Question**: Given that this is a fresh project (no backwards compatibility concerns), should we maintain tRPC-compatible JSON-over-HTTP endpoints, or switch to pure REST? What are the trade-offs?

**Primary Candidates**:
- Jordan "The Feature Engineer" Kim (argues for tRPC compatibility - frontend integration)
- `client/src/pages/Home.tsx` (argues for minimal frontend changes)
- Alex "The Pragmatist" Chen (argues for REST simplicity)
- `server/_core/index.ts` (provides current API surface evidence)

**Key Trade-offs to Surface**:
- Frontend refactoring effort vs backend simplicity
- Type safety mechanisms (tRPC types vs OpenAPI)
- Developer experience (tRPC hooks vs fetch/axios)
- Protocol complexity (tRPC batching vs REST endpoints)

**Decision Points**:
- If tRPC-compatible: How much of tRPC protocol do we need to implement?
- If REST: What's the migration path for frontend?

---

#### Round 2: Should we start with SQLite+disk or design for MySQL/S3 from day one?

**Question**: Since this is a fresh project, should we start with the simplest possible implementation (SQLite + filesystem storage) and add pluggable interfaces, or design for production backends (MySQL/Postgres + S3) from the start?

**Primary Candidates**:
- Alex "The Pragmatist" Chen (argues for SQLite+disk first)
- Sam "The Architect" Rivera (argues for production-ready design)
- Morgan "The Infrastructure Engineer" Taylor (argues for production considerations)
- `drizzle/schema.ts` (provides schema complexity evidence)

**Key Trade-offs to Surface**:
- Local development experience (zero deps vs docker compose)
- Implementation speed (simple vs comprehensive)
- Refactoring risk (adding abstractions later vs upfront)
- Production deployment complexity

**Decision Points**:
- If SQLite+disk: How pluggable should interfaces be?
- If production-first: What's the local dev experience?

---

#### Round 3: Should the worker run in-process or as a separate process?

**Question**: Should the PDF worker run as a goroutine within the API server process, or as a separate binary/process? What are the operational and architectural implications?

**Primary Candidates**:
- Morgan "The Infrastructure Engineer" Taylor (argues for separate process)
- Alex "The Pragmatist" Chen (argues for in-process simplicity)
- `server/pdfWorker.ts` (provides current worker behavior evidence)
- Sam "The Architect" Rivera (argues for abstraction that supports both)

**Key Trade-offs to Surface**:
- Resource isolation (CPU/memory for API vs worker)
- Deployment complexity (one binary vs two)
- Scaling flexibility (scale API and worker independently)
- Local development simplicity

**Decision Points**:
- If in-process: How do we handle resource contention?
- If separate: How do we share database/storage config?

---

### Architecture Questions (Rounds 4-7)

#### Round 4: What level of job queue abstraction do we need?

**Question**: Should we implement a formal job queue abstraction (Enqueue/Claim/Complete interfaces) that could support external queues later, or use simple database operations (UPDATE ... WHERE status='pending')?

**Primary Candidates**:
- Sam "The Architect" Rivera (argues for formal abstraction)
- Alex "The Pragmatist" Chen (argues for simple DB operations)
- `server/pdfWorker.ts` (provides current job processing evidence)
- Morgan "The Infrastructure Engineer" Taylor (argues for operational needs)

**Key Trade-offs to Surface**:
- Code complexity (abstractions vs direct DB calls)
- Future flexibility (upgrade to Redis/RabbitMQ later)
- Current needs (database-backed is sufficient)
- Testing complexity (mocking interfaces vs DB fixtures)

**Decision Points**:
- If abstraction: What's the minimal interface?
- If direct DB: How do we handle future queue needs?

---

#### Round 5: How should we structure the Go project layout?

**Question**: What's the optimal Go project structure? Should we follow standard layout (cmd/, internal/, pkg/) or something simpler? How do we organize handlers, services, repositories, and storage?

**Primary Candidates**:
- Sam "The Architect" Rivera (argues for standard layout)
- "The New Hire" (argues for simplicity and clarity)
- `go.mod` (provides ecosystem conventions)
- Alex "The Pragmatist" Chen (argues for minimal structure)

**Key Trade-offs to Surface**:
- Onboarding complexity (standard vs custom)
- Separation of concerns (layers vs flat)
- Import visibility (internal/ vs pkg/)
- Package boundaries (domain-driven vs layer-driven)

**Decision Points**:
- What's the minimal structure that enables growth?
- How do we organize domain logic (photos, pdfs, auth)?

---

#### Round 6: What authentication strategy should we implement?

**Question**: Should we implement email/password only, or include OAuth providers from the start? How do we structure auth to support both without over-engineering?

**Primary Candidates**:
- Alex "The Pragmatist" Chen (argues for email/password only)
- Jordan "The Feature Engineer" Kim (argues for OAuth UX benefits)
- `server/_core/index.ts` (provides current OAuth implementation evidence)
- "The New Hire" (argues for simplicity)

**Key Trade-offs to Surface**:
- Implementation complexity (simple vs comprehensive)
- User experience (password vs OAuth)
- Security considerations (password hashing vs OAuth tokens)
- Extensibility (adapter pattern vs hardcoded)

**Decision Points**:
- If email/password only: How extensible should it be?
- If OAuth included: Which providers? How pluggable?

---

#### Round 7: How should we handle file serving and storage URLs?

**Question**: For disk-based storage, should we serve files directly via HTTP endpoints with auth middleware, or generate signed URLs with expiration? What are the security and operational implications?

**Primary Candidates**:
- Morgan "The Infrastructure Engineer" Taylor (argues for signed URLs)
- Alex "The Pragmatist" Chen (argues for direct serving simplicity)
- `server/storage.ts` (provides current storage proxy evidence)
- Sam "The Architect" Rivera (argues for interface consistency)

**Key Trade-offs to Surface**:
- Security (time-limited access vs session-based)
- Implementation complexity (HMAC signing vs middleware)
- Multi-user scenarios (URL sharing prevention)
- Local dev experience (simple vs production-like)

**Decision Points**:
- If direct serving: How do we prevent URL sharing?
- If signed URLs: What's the expiration policy? How do we handle clock skew?

---

### Mechanics Questions (Rounds 8-10)

#### Round 8: How should we handle PDF generation?

**Question**: What Go library should we use for PDF generation? Should we match the current jsPDF+canvas approach exactly, or can we improve it? How do we handle image loading and rendering?

**Primary Candidates**:
- `server/pdfWorker.ts` (provides current PDF generation algorithm evidence)
- `go.mod` (provides Go PDF library ecosystem analysis)
- Sam "The Architect" Rivera (argues for library choice and abstraction)
- Alex "The Pragmatist" Chen (argues for simplest working solution)

**Key Trade-offs to Surface**:
- Library maturity (gofpdf vs unidoc vs others)
- Feature parity (A4 portrait, 10mm margins, aspect-fit)
- Performance (memory usage, rendering speed)
- License considerations (commercial vs open source)

**Decision Points**:
- Which library best matches current behavior?
- How do we abstract PDF generation for testing?

---

#### Round 9: How should we handle database migrations?

**Question**: What migration tool should we use (Goose, migrate, custom)? How do we version schema changes? Should migrations be embedded in the binary or external files?

**Primary Candidates**:
- `drizzle/schema.ts` (provides current schema structure)
- Sam "The Architect" Rivera (argues for migration strategy)
- "The New Hire" (argues for developer ergonomics)
- `go.mod` (provides migration tool ecosystem)

**Key Trade-offs to Surface**:
- Tool maturity and maintenance
- SQL vs Go-based migrations
- Rollback capabilities
- Developer workflow (run migrations vs auto-migrate)

**Decision Points**:
- What's the simplest migration approach?
- How do we handle schema changes during development?

---

#### Round 10: How should we structure error handling and logging?

**Question**: What error handling patterns should we use? How do we structure logging for observability? Should we use structured logging (zerolog, zap) or standard library?

**Primary Candidates**:
- Morgan "The Infrastructure Engineer" Taylor (argues for structured logging)
- Sam "The Architect" Rivera (argues for error handling patterns)
- `server/pdfWorker.ts` (provides current error handling evidence)
- Alex "The Pragmatist" Chen (argues for simplicity)

**Key Trade-offs to Surface**:
- Logging library choice (zerolog vs zap vs log/slog)
- Error wrapping (pkg/errors vs fmt.Errorf)
- Structured fields (context, request IDs, user IDs)
- Log levels and filtering

**Decision Points**:
- What's the minimal logging that enables debugging?
- How do we handle errors in API responses vs worker logs?

---

### Alignment Questions (Rounds 11-12)

#### Round 11: How should we handle configuration and environment variables?

**Question**: How should we structure configuration? Environment variables only, or config files? How do we validate required settings? Should we use a config library or custom validation?

**Primary Candidates**:
- "The New Hire" (argues for clarity and validation)
- Morgan "The Infrastructure Engineer" Taylor (argues for production config patterns)
- `server/_core/env.ts` (provides current env validation evidence)
- Alex "The Pragmatist" Chen (argues for simple env vars)

**Key Trade-offs to Surface**:
- Configuration library vs custom code
- Validation timing (startup vs runtime)
- Default values and local dev experience
- Secret management (env vars vs vault)

**Decision Points**:
- What's the minimal config approach?
- How do we handle secrets in production?

---

#### Round 12: What's the minimal viable implementation checklist?

**Question**: Given all previous debates, what's the absolute minimum we need to ship a working Go backend? What can we defer? What's the MVP feature set?

**Primary Candidates**:
- Alex "The Pragmatist" Chen (argues for minimal MVP)
- Jordan "The Feature Engineer" Kim (argues for frontend compatibility)
- `client/src/pages/Home.tsx` (provides frontend requirements)
- `server/_core/index.ts` (provides current feature set)

**Key Trade-offs to Surface**:
- Core features vs nice-to-haves
- Implementation order (auth → photos → PDFs?)
- Testing requirements (unit tests vs integration tests)
- Documentation needs

**Decision Points**:
- What's the smallest working system?
- What's the implementation order?
- What can we ship without?

---

## Question Dependencies

```
Round 1 (tRPC vs REST) 
  ↓
Round 2 (SQLite vs Production DB)
  ↓
Round 3 (In-process vs Separate Worker)
  ↓
Round 4 (Job Queue Abstraction)
  ↓
Round 5 (Project Layout)
  ↓
Round 6 (Auth Strategy)
  ↓
Round 7 (File Serving)
  ↓
Round 8 (PDF Generation)
  ↓
Round 9 (Migrations)
  ↓
Round 10 (Error Handling)
  ↓
Round 11 (Configuration)
  ↓
Round 12 (MVP Checklist)
```

Each round builds on previous decisions, but candidates can reference earlier rounds when making arguments.

