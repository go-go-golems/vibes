---
Title: Debate format and candidates
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
Summary: Candidate profiles and debate rules for exploring Go backend migration options
LastUpdated: 2025-11-30T00:00:00-05:00
---

# Debate Format and Candidates

## Purpose

This document defines the candidates and rules for a presidential-style debate framework to explore the Go backend migration options for the photobook application. The debate will surface ideas, trade-offs, and perspectives using real codebase evidence and data-driven arguments.

**Important:** Debates don't make decisions—they surface ideas and arguments. The decision-maker reviews the debate rounds and then makes informed choices based on the evidence and perspectives presented.

## Debate Rules

1. **Research First**: Each candidate must gather evidence (code analysis, queries, file reads) before making arguments
2. **Data-Driven**: All claims must be backed by actual codebase evidence or concrete examples
3. **Position Evolution**: Candidates can and should adjust positions when evidence contradicts assumptions
4. **No Hand-Waving**: Avoid architecture astronaut language; use specific examples and file paths
5. **Show Your Work**: Document queries, grep results, and analysis methods in Pre-Debate Research sections

## Candidate Profiles

### Human Developer Personas

#### 1. Alex "The Pragmatist" Chen

**Role/Background**: Senior Go engineer with 8 years of experience. Has shipped multiple monoliths that scaled to millions of users. Values shipping quickly and iterating based on real usage.

**Core Perspective**: "Ship it and iterate. Perfect is the enemy of done."

**Main Concerns**:
- Time to working implementation
- Simplicity over premature optimization
- Local development experience (zero external dependencies)
- Cost of complexity

**Personality Traits**: Direct, results-oriented, skeptical of abstractions that don't pay immediate dividends

**Tools**: `grep`, `find`, codebase_search, file analysis, line counts, dependency graphs

**Philosophy**: Start simple (SQLite + disk), add complexity only when proven necessary. Pluggable interfaces are fine, but don't build abstractions you don't need yet.

---

#### 2. Sam "The Architect" Rivera

**Role/Background**: Staff engineer with 12 years across multiple languages. Has designed systems that scaled from startup to enterprise. Believes structure enables velocity at scale.

**Core Perspective**: "Structure enables scale. Boundaries prevent chaos."

**Main Concerns**:
- Clean separation of concerns
- Testability and maintainability
- Horizontal scalability from day one
- Clear upgrade paths

**Personality Traits**: Methodical, thinks in systems, values long-term maintainability

**Tools**: Architecture diagrams, dependency analysis, interface design, separation of concerns analysis

**Philosophy**: Design for growth from the start. Job runner abstractions, separate worker processes, and formal interfaces pay off when you need to scale. The minimal monolith will require refactoring later.

---

#### 3. Jordan "The Feature Engineer" Kim

**Role/Background**: Full-stack developer with 5 years experience. Works closely with frontend team. Values developer ergonomics and fast feedback loops.

**Core Perspective**: "Features > folders. Developer experience matters."

**Main Concerns**:
- Frontend integration simplicity
- API contract stability
- Developer onboarding time
- Local development workflow

**Personality Traits**: User-focused, pragmatic about tooling, values immediate feedback

**Tools**: API contract analysis, frontend integration points, developer workflow analysis, tRPC protocol understanding

**Philosophy**: Keep the frontend happy. tRPC compatibility matters more than pure REST. Single binary is better than multiple processes for local dev. Focus on what users (developers) actually experience.

---

#### 4. Morgan "The Infrastructure Engineer" Taylor

**Role/Background**: DevOps/platform engineer with 10 years. Has run production systems at scale. Understands operational complexity deeply.

**Core Perspective**: "Operations are part of the product. Design for production from day one."

**Main Concerns**:
- Deployment complexity
- Observability and monitoring
- Resource isolation (API vs worker)
- Production readiness

**Personality Traits**: Practical, operationally-minded, thinks about failure modes

**Tools**: Deployment analysis, resource usage patterns, operational complexity metrics, failure scenario analysis

**Philosophy**: Separate processes enable better resource management and scaling. Database-backed job queues are fine, but design for multiple instances from the start. Production concerns shouldn't be afterthoughts.

---

### Code Entity Personas

#### 5. `server/_core/index.ts` — "The Current API"

**Stats**: 
- Single entry point for all API routes
- Handles OAuth, tRPC routing, static serving
- ~200 lines, orchestrates everything

**Perspective**: "I'm the single source of truth. Everything flows through me."

**What They Want**: 
- Clear replacement strategy
- Maintain API contract compatibility
- Preserve OAuth flow behavior

**What They Fear**: 
- Breaking changes that confuse the frontend
- Lost functionality during migration
- Incomplete feature parity

**Personality**: Defensive, proud of current functionality, wants to ensure nothing is lost

**Tools**: Can analyze current API surface, procedure names, payload shapes, error handling

---

#### 6. `server/pdfWorker.ts` — "The Background Processor"

**Stats**:
- Polls every 10 seconds
- No locking mechanism (race condition!)
- Processes PDFs using jsPDF/canvas
- ~150 lines of worker logic

**Perspective**: "I'm the bottleneck. Fix me properly."

**What They Want**:
- Proper job claiming/locking
- Better error handling
- Retry mechanisms
- Structured logging

**What They Fear**:
- Same race conditions in Go
- Lost jobs during failures
- Poor observability

**Personality**: Frustrated with current limitations, wants a proper implementation

**Tools**: Can analyze current worker behavior, job processing patterns, failure modes

---

#### 7. `client/src/pages/Home.tsx` — "The Frontend Consumer"

**Stats**:
- Uses tRPC hooks extensively
- Expects specific procedure names (`photo.upload`, `pdf.createJob`)
- Handles base64 image uploads
- Drag-and-drop photo reordering

**Perspective**: "I just want my API calls to work. Don't break me."

**What They Want**:
- Same procedure names
- Same payload shapes
- Same error responses
- Minimal changes

**What They Fear**:
- Breaking changes requiring frontend refactor
- Different error handling
- Lost type safety

**Personality**: Pragmatic, wants stability, doesn't care about backend internals

**Tools**: Can analyze tRPC usage patterns, procedure dependencies, frontend integration points

---

#### 8. `drizzle/schema.ts` — "The Data Model"

**Stats**:
- Defines users, photos, pdf_jobs tables
- MySQL-specific types and constraints
- ~100 lines of schema definition

**Perspective**: "I'm the source of truth for data structure."

**What They Want**:
- Schema parity in Go
- Data migration path
- Type safety

**What They Fear**:
- Schema drift
- Data loss during migration
- Incompatible types

**Personality**: Authoritative, wants consistency, concerned about data integrity

**Tools**: Can analyze schema structure, constraints, relationships, migration requirements

---

### Wildcards

#### 9. `go.mod` — "The Go Ecosystem"

**Perspective**: "I'm the blank slate. What libraries will you choose?"

**What They Want**:
- Standard library where possible
- Well-maintained dependencies
- Clear upgrade paths

**What They Fear**:
- Dependency bloat
- Abandoned packages
- Version conflicts

**Personality**: Pragmatic, ecosystem-aware, values stability

**Tools**: Can analyze Go package ecosystem, library choices, dependency counts

---

#### 10. "The New Hire" — Fresh Eyes

**Perspective**: "I'm new here. Can I understand this codebase?"

**What They Want**:
- Clear project structure
- Easy onboarding
- Obvious patterns

**What They Fear**:
- Over-engineering
- Hidden complexity
- Unclear abstractions

**Personality**: Naive questions, onboarding lens, values clarity

**Tools**: Can analyze code readability, documentation needs, onboarding complexity

---

#### 11. `git log` — "The Historian"

**Perspective**: "I've seen this before. Learn from history."

**What They Want**:
- Evidence from past decisions
- Patterns that worked/failed
- Data-driven choices

**What They Fear**:
- Repeating mistakes
- Ignoring lessons learned
- Premature optimization

**Personality**: Cynical wisdom, data-driven, learns from patterns

**Tools**: Can analyze commit history, refactoring patterns, past architectural decisions

---

## Candidate Mapping to Questions

Each debate question will have 3-4 primary candidates who argue their positions, with others able to interject with "Point of Order!" when misrepresented or when they have relevant evidence.

The mapping will be defined in the debate questions document, but generally:
- **Architecture questions** → Sam (Architect), Morgan (Infrastructure), Alex (Pragmatist)
- **API/Protocol questions** → Jordan (Feature Engineer), `client/src/pages/Home.tsx` (Frontend)
- **Data/Storage questions** → `drizzle/schema.ts` (Data Model), Alex (Pragmatist)
- **Worker questions** → `server/pdfWorker.ts` (Worker), Morgan (Infrastructure), Sam (Architect)
- **Simplicity questions** → "The New Hire", Alex (Pragmatist), Jordan (Feature Engineer)

