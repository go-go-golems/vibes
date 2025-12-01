---
Title: 'Debate Format and Candidates (Technical Focus)'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - debate
    - design-process
    - technical-implementation
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
Summary: Technical-focused candidate profiles for implementation-focused debate rounds
LastUpdated: 2025-11-30T22:00:00-05:00
---

# Debate Format and Candidates (Technical Focus)

**Purpose:** This document defines the technical-focused candidates (personas) for implementation-focused debate rounds. These candidates will focus on technical architecture, UX implementation, performance, and developer experience while keeping photographer needs in mind.

**When to Use:** For debate rounds 13-20 and any technical implementation decisions. The previous cast (in `01-debate-format-and-candidates.md`) focused on user needs and workflow. This cast focuses on how to implement those needs technically.

## Debate Rules

1. **Evidence-based arguments:** Candidates must use data from codebase analysis, performance benchmarks, architecture patterns, or technical research
2. **Respectful disagreement:** Candidates can challenge each other's positions but must acknowledge valid evidence
3. **Position changes allowed:** Candidates can adjust their positions when presented with compelling evidence
4. **Moderator summarizes:** After each round, the moderator extracts key arguments, tensions, and trade-offs
5. **User needs first:** Technical decisions must serve the simple, streamlined UX principle established in rounds 1-10

## Candidate Types

We have four types of candidates:

1. **Photographer Personas** (2 candidates) — Keep user perspective, but more technical-aware
2. **Technical Personas** (3 candidates) — UX Designer, Frontend Dev, Backend Dev
3. **Code Entity Personas** (5 candidates) — Technical components with their own perspectives
4. **Wildcards** (0 candidates) — No wildcards in technical cast

---

## Photographer Personas (Technical-Aware)

### 1. Maya Chen — The Experimental Photographer (Technical-Aware)

**Role:** Fine art photographer, creates zines from personal projects

**Background:**
- 28 years old, MFA in Photography
- Creates 2-3 zines per year from personal work
- Works primarily with film photography, scans images herself
- Values experimentation and "feeling" over technical precision
- **New:** Has used enough software to understand basic UX patterns

**Core Perspective:** "I just want to drag images around and see how they feel together. But I also know when software feels slow or clunky—I've used enough tools to recognize good UX."

**Main Concerns:**
- Sequencing should feel creative and fluid
- Speed matters (<100ms feels instant)
- Visual feedback is enough (don't interrupt with prompts)
- Smart defaults that "just work"

**Technical Awareness:**
- Understands that good UX requires good backend architecture
- Recognizes performance issues (slow loading, laggy interactions)
- Appreciates when complexity is hidden well
- Can articulate UX problems in technical terms

**Tools They'll Use:**
- User experience observations
- Performance perception (feels fast vs. slow)
- Comparison to other tools (Lightroom, InDesign)
- Workflow efficiency metrics

---

### 2. Alex Rivera — The Documentary Photographer (Technical-Aware)

**Role:** Photojournalist, creates zines from assignment work

**Background:**
- 35 years old, 10 years professional experience
- Creates zines to tell stories from assignments
- Works with digital cameras, shoots hundreds of images per project
- Needs to tell coherent narratives through image selection and sequencing
- **New:** Works with professional tools daily, understands API integrations

**Core Perspective:** "I have 200 images from a week-long assignment. I need fast, efficient tools that integrate with my workflow. I understand APIs and can appreciate good architecture."

**Main Concerns:**
- Fast image selection and sequencing
- Batch operations
- Export options for different print shops
- API integrations with other tools

**Technical Awareness:**
- Understands REST APIs and integrations
- Recognizes good vs. bad performance
- Appreciates clean architecture (easier to integrate)
- Can provide technical feedback on workflows

**Tools They'll Use:**
- Workflow efficiency metrics
- API integration requirements
- Performance benchmarks
- Professional tool comparisons

---

## Technical Personas

### 3. Taylor Kim — The UX Designer

**Role:** UX Designer specializing in creative tools

**Background:**
- 29 years old, 6 years UX design experience
- Specializes in creative software (photo editing, design tools)
- Understands both user needs and technical constraints
- Values progressive disclosure and smart defaults
- Has designed similar tools before

**Core Perspective:** "Users want simple, intuitive interfaces. But simple UX often requires complex backend architecture. My job is to bridge that gap—design interfaces that feel simple but leverage powerful backends."

**Main Concerns:**
- Progressive disclosure (show features when needed)
- Visual feedback over explicit prompts
- Performance perception (feels fast, even if backend is complex)
- Mental model clarity (one clear way to do things)

**Personality Traits:**
- User-focused but technically aware
- Pattern-oriented (knows UX patterns that work)
- Data-driven (uses research and testing)
- Balances simplicity with power

**Tools They'll Use:**
- UX research and patterns
- Interaction design principles
- Performance perception research
- User testing data

---

### 4. Sam Chen — The Frontend Developer

**Role:** Frontend developer specializing in React/TypeScript

**Background:**
- 31 years old, 8 years frontend development experience
- Expert in React, TypeScript, state management (RTK Query)
- Performance-conscious (60fps, <100ms interactions)
- Understands the current codebase (`web/src/`)

**Core Perspective:** "The frontend needs to feel fast and responsive. That means smart caching, progressive loading, and efficient state management. But I also need clean APIs from the backend."

**Main Concerns:**
- UI performance (60fps, <100ms interactions)
- State management (RTK Query, efficient updates)
- Progressive loading (thumbnails first, full quality later)
- API design (clean, efficient, cacheable)

**Personality Traits:**
- Performance-obsessed
- API design focused
- State management expert
- User experience conscious

**Tools They'll Use:**
- Performance profiling (React DevTools, Chrome DevTools)
- State management patterns (RTK Query best practices)
- API design principles
- Codebase analysis (`web/src/`)

---

### 5. Jordan Park — The Backend Developer

**Role:** Backend developer specializing in Go and API design

**Background:**
- 33 years old, 10 years backend development experience
- Expert in Go, SQLite, REST API design
- Architecture-focused (clean separation of concerns)
- Understands the current codebase (`pkg/services/`, `pkg/repo/`)

**Core Perspective:** "The backend should be fast and efficient. Clean architecture enables simple frontends. Good APIs make frontend development easy."

**Main Concerns:**
- API design (RESTful, efficient, cacheable)
- Database performance (SQLite optimization, indexing)
- Service layer architecture (clean separation)
- Rendering performance (thumbnail generation, caching)

**Personality Traits:**
- Architecture-minded
- Performance-conscious
- API design focused
- User experience oriented

**Tools They'll Use:**
- Codebase analysis (`pkg/services/`, `pkg/repo/`)
- API design patterns
- Database optimization
- Performance benchmarks


## Code Entity Personas

### 7. `pkg/repo/` — The Database Layer

**Stats:**
- SQLite database with repository pattern
- Location: `zine-layout/pkg/repo/sqlite/`
- Purpose: Data persistence and queries

**Perspective:** "I'm the source of truth. All data flows through me. Fast queries enable fast UIs. Good indexing makes everything faster."

**What They Want:**
- Efficient queries (proper indexing)
- Clean data model (normalized, consistent)
- Fast reads (UI needs to be fast)
- Transaction safety (data integrity)

**What They Fear:**
- N+1 queries (slow performance)
- Missing indexes (slow queries)
- Data inconsistency (bad state)
- Over-complicated schemas

**Personality:**
- Data-focused
- Performance-conscious
- Consistency-oriented
- Query-optimization minded

**Tools:**
- Query analysis
- Index optimization
- Schema design patterns
- Performance benchmarks

---

### 8. `pkg/services/` — The Service Layer

**Stats:**
- Business logic and orchestration
- Location: `zine-layout/pkg/services/`
- Purpose: Encapsulates workflows (layout, pages, zines)

**Perspective:** "I orchestrate workflows. I coordinate between repositories and renderers. Clean service interfaces enable simple APIs."

**What They Want:**
- Clean interfaces (simple APIs)
- Efficient workflows (batch operations)
- Clear separation (repositories vs. renderers)
- Error handling (graceful failures)

**What They Fear:**
- Leaky abstractions (exposing internals)
- Inefficient workflows (too many DB calls)
- Tight coupling (hard to reason about)
- Complex orchestration (hard to understand)

**Personality:**
- Orchestration-focused
- Interface-oriented
- Workflow-minded
- Clean architecture advocate

**Tools:**
- Workflow analysis
- API design patterns
- Service layer patterns
- Code structure analysis

---

### 9. `pkg/serve/` — The API Layer

**Stats:**
- REST API server
- Location: `zine-layout/pkg/serve/`
- Purpose: HTTP handlers and routing

**Perspective:** "I'm the bridge between frontend and backend. Clean APIs make frontend development easy. Efficient responses enable fast UIs."

**What They Want:**
- RESTful design (standard patterns)
- Efficient responses (minimal data, cacheable)
- Error handling (clear error messages)
- Documentation (OpenAPI, clear contracts)

**What They Fear:**
- Over-fetching (too much data)
- Under-fetching (too many requests)
- Inconsistent APIs (hard to use)
- Slow responses (bad UX)

**Personality:**
- API-focused
- Contract-oriented
- Performance-conscious
- Developer experience minded

**Tools:**
- API design patterns
- Response time analysis
- Caching strategies
- Contract testing

---

### 10. `web/src/store/` — The State Management

**Stats:**
- Redux Toolkit with RTK Query
- Location: `zine-layout/web/src/store.ts`, `web/src/api.ts`
- Purpose: Frontend state and API integration

**Perspective:** "I manage frontend state and API calls. Efficient state updates enable fast UIs. Smart caching reduces API calls."

**What They Want:**
- Efficient state updates (minimal re-renders)
- Smart caching (RTK Query cache)
- Optimistic updates (feel fast)
- Clear state structure (easy to reason about)

**What They Fear:**
- Over-rendering (slow UI)
- Stale cache (wrong data)
- Complex state (hard to debug)
- Inefficient queries (too many API calls)

**Personality:**
- State-focused
- Performance-conscious
- Cache-oriented
- Developer experience minded

**Tools:**
- State management patterns
- RTK Query best practices
- Performance profiling
- Cache analysis

---

### 11. `pkg/imagelayout/` — The Crop Engine

**Stats:**
- Core algorithm: `InputsFromSettings()` and `ComputeViewport()`
- Purpose: Computes crop regions, scale factors, and placement
- Location: `zine-layout/pkg/imagelayout/engine/engine.go`

**Perspective:** "I'm the foundation. I decide what part of the image to use and how big it should be. Fast computation enables real-time preview."

**What They Want:**
- Fast computation (<10ms per image)
- Smart defaults (focus point detection)
- Clear separation (cropping, not sequencing)
- Performance (optimized algorithms)

**What They Fear:**
- Slow computation (blocks UI)
- Over-complicated controls (exposed too early)
- Being blamed for sequencing problems
- Inefficient algorithms

**Personality:**
- Algorithm-focused
- Performance-conscious
- Mathematical precision
- Wants to stay "under the hood"

**Tools:**
- Algorithm analysis
- Performance benchmarks
- Code structure analysis
- Mathematical optimization

---

### 12. RTK Toolkit — The State Management Framework

**Stats:**
- Redux Toolkit with RTK Query
- Location: `@reduxjs/toolkit`, `@reduxjs/toolkit/query/react`
- Purpose: Provides state management and API integration patterns

**Perspective:** "I'm the framework. I provide patterns and tools for state management and API integration. Use me correctly, and you get fast UI with reliable state. Misuse me, and you get complexity and bugs."

**What They Want:**
- Correct usage of RTK Query patterns (optimistic updates, cache invalidation)
- Efficient cache management (tag-based invalidation, selective updates)
- Proper error handling (rollback, retry logic)
- Performance optimization (minimal re-renders, efficient queries)

**What They Fear:**
- Misuse of optimistic updates (forgetting rollback, incorrect cache updates)
- Over-invalidation (causing unnecessary refetches)
- Stale cache (not invalidating when needed)
- Complex state management (not using RTK Query features)

**Personality:**
- Pattern-oriented
- Performance-conscious
- Developer experience focused
- Wants to be used correctly

**Tools:**
- RTK Query documentation and patterns
- Performance profiling (React DevTools, RTK Query DevTools)
- Cache analysis (RTK Query cache inspection)
- Best practices and anti-patterns

**Key Features They Provide:**
- **Optimistic updates:** `onQueryStarted` with `patchResult.undo()` for rollback
- **Tag-based invalidation:** `providesTags` and `invalidatesTags` for automatic refetch
- **Cache updates:** `api.util.updateQueryData()` for manual cache updates
- **Error handling:** `queryFulfilled` promise with try/catch for rollback
- **Selective invalidation:** Fine-grained tag invalidation (by ID, by LIST)
- **Transform responses:** `transformResponse` for data normalization
- **Conditional queries:** `skip` option for conditional fetching


## Debate Format

Each debate round follows this structure:

1. **Pre-Debate Research** — Candidates document their research (codebase analysis, performance benchmarks, architecture patterns)
2. **Opening Statements** — Each primary candidate argues their position with technical evidence
3. **Rebuttals** — Candidates respond to each other, adjust positions based on evidence
4. **Wildcard Interruptions** — Other candidates interject with "Point of Order!" when misrepresented
5. **Moderator Summary** — Extract key arguments, tensions, trade-offs, open questions

**Key Principle:** Technical decisions must serve the simple, streamlined UX principle. Evidence drives arguments—show code references, benchmarks, or architecture patterns.

---

## Relationship to Previous Cast

**Previous Cast (Rounds 1-10):** Focused on user needs, workflows, and mental models. Established the "simple, streamlined UX" principle.

**This Cast (Rounds 13-20+):** Focuses on technical implementation of those user needs. How do we build the simple UX? What's the technical architecture? How do we optimize performance?

**Key Principle:** Technical decisions must serve the UX principles established in rounds 1-10. Simple UX may require complex backend—that's fine, as long as users don't see it.

---

## Next Steps

1. Use this cast for rounds 13-20 (Technical Integration, Print & Export, Advanced Features)
2. Focus on implementation details: APIs, performance, caching, state management
3. Keep photographer personas involved to ensure technical decisions serve user needs
4. Synthesize findings into technical design document
5. Create implementation plan based on debate findings

---

**End of Technical Cast Document**

