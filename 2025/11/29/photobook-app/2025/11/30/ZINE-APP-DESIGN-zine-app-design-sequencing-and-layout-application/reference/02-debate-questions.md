---
Title: 'Debate Questions'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - debate
    - design-process
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
Summary: All 20 validation questions mapped to primary debate candidates
LastUpdated: 2025-11-30T15:00:00-05:00
---

# Debate Questions

**Purpose:** This document lists all 20 validation questions for the zine app design debate, mapped to primary candidates who will argue each question. Questions are organized by theme and build from foundational to detailed.

## Question Progression

Questions are designed to build on each other:
1. **Foundation (Q1-Q4):** Sequencing experience and user workflow
2. **Supporting Features (Q5-Q8):** Cropping and layout (secondary concerns)
3. **Workflow & Mental Model (Q9-Q12):** How users think about the app
4. **Technical Integration (Q13-Q15):** Implementation concerns
5. **Print & Export (Q16-Q18):** Final output requirements
6. **Advanced Features (Q19-Q20):** Progressive disclosure

---

## Sequencing Experience

### Q1: How do photographers currently sequence images?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for creative, intuitive workflow
- **Alex Rivera** (Documentary Photographer) — Argues for efficient, professional workflow
- **The New Hire** — Questions assumptions about current workflows

**Secondary Participants:**
- Sam Taylor (can interject with technical feasibility)
- Riley Park (can add print shop perspective)

**Question Details:**
- Do they use physical prints on a wall?
- Digital tools (Lightroom, Bridge)?
- Mental/notebook planning?
- **Why:** Understanding current workflow helps design the sequencing interface.

**Debate Round:** `reference/debate-round-01-how-do-photographers-sequence.md`

---

### Q2: What makes a sequence "feel right"?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for visual flow and "feeling"
- **Alex Rivera** (Documentary Photographer) — Argues for narrative progression
- **Jordan Kim** (Designer/Photographer) — Argues for design principles

**Question Details:**
- Visual flow (color, composition)?
- Narrative progression?
- Emotional rhythm?
- **Why:** The app should support whatever makes sequences feel good, not impose a structure.

**Debate Round:** `reference/debate-round-02-what-makes-sequence-feel-right.md`

---

### Q3: How important is real-time preview vs. speed?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for instant feel over perfect preview
- **Sam Taylor** (Software Developer) — Argues for technical trade-offs
- **`web/src/`** (The Frontend) — Argues for UI performance

**Question Details:**
- Should sequencing feel instant (even if preview is lower quality)?
- Or is high-quality preview worth a slight delay?
- **Why:** Performance trade-offs affect user experience.

**Debate Round:** `reference/debate-round-03-preview-vs-speed.md`

---

### Q4: Do photographers want to see spreads (two pages) or single pages when sequencing?

**Primary Candidates:**
- **Alex Rivera** (Documentary Photographer) — Argues for spread view (narrative flow)
- **Maya Chen** (Experimental Photographer) — Argues for simplicity (single page)
- **`pkg/pagelayout/`** (The Page Renderer) — Argues for technical implications

**Question Details:**
- Spread view shows how pages work together
- Single page view is simpler
- **Why:** Affects the primary interface design.

**Debate Round:** `reference/debate-round-04-spreads-vs-single-pages.md`

---

## Cropping and Layout (Supporting Features)

### Q5: When do photographers think about cropping?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for "after sequencing" (don't interrupt flow)
- **Jordan Kim** (Designer/Photographer) — Argues for "during sequencing" (see how it fits)
- **`pkg/imagelayout/`** (The Crop Engine) — Argues for technical workflow

**Question Details:**
- Before sequencing (pre-crop images)?
- During sequencing (crop to fit template)?
- After sequencing (fine-tune for final layout)?
- **Why:** Determines when cropping UI appears (or stays hidden).

**Debate Round:** `reference/debate-round-05-when-to-think-about-cropping.md`

---

### Q6: How much control do photographers want over cropping?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for fully automatic (smart defaults)
- **Jordan Kim** (Designer/Photographer) — Argues for fine-grained control (hidden by default)
- **`pkg/imagelayout/`** (The Crop Engine) — Argues for algorithmic capabilities

**Question Details:**
- Fully automatic (smart defaults)?
- One-click presets (fit, fill, center)?
- Fine-grained control (but hidden by default)?
- **Why:** Balances "just works" with user control.

**Debate Round:** `reference/debate-round-06-cropping-control-level.md`

---

### Q7: Should page templates be visible during sequencing?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for hidden templates (focus on sequence)
- **Jordan Kim** (Designer/Photographer) — Argues for visible templates (see layout)
- **`pkg/pagelayout/`** (The Page Renderer) — Argues for template system design

**Question Details:**
- Show template placeholders (so users see layout)?
- Hide templates (focus purely on sequence)?
- Toggle between views?
- **Why:** Templates affect visual feedback during sequencing.

**Debate Round:** `reference/debate-round-07-template-visibility.md`

---

### Q8: How do photographers discover they need to adjust cropping/layout?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for visual feedback (implicit)
- **Jordan Kim** (Designer/Photographer) — Argues for explicit prompts
- **Sam Taylor** (Software Developer) — Argues for progressive disclosure patterns

**Question Details:**
- Visual feedback (image doesn't fit well)?
- Explicit prompts ("This image needs cropping")?
- Manual exploration (advanced mode)?
- **Why:** Determines how to surface supporting features without interrupting flow.

**Debate Round:** `reference/debate-round-08-discovering-cropping-needs.md`

---

## Workflow and Mental Model

### Q9: What is the primary mental model?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for "arrange photos in order" (sequence-first)
- **Jordan Kim** (Designer/Photographer) — Argues for "fill pages with photos" (layout-first)
- **Alex Rivera** (Documentary Photographer) — Argues for "tell a story with images" (narrative-first)
- **The New Hire** — Questions which model is most intuitive

**Question Details:**
- "Arrange photos in order" (sequence-first)?
- "Fill pages with photos" (layout-first)?
- "Tell a story with images" (narrative-first)?
- **Why:** The primary model should match how photographers think.

**Debate Round:** `reference/debate-round-09-primary-mental-model.md`

---

### Q10: How do photographers want to iterate?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for quick A/B testing (many sequences)
- **Alex Rivera** (Documentary Photographer) — Argues for iterative refinement (one sequence)
- **Sam Taylor** (Software Developer) — Argues for technical implementation (undo/redo, versioning)

**Question Details:**
- Try many sequences quickly (A/B testing)?
- Refine one sequence carefully (iterative refinement)?
- Both (quick exploration, then careful refinement)?
- **Why:** Affects undo/redo, versioning, and comparison features.

**Debate Round:** `reference/debate-round-10-iteration-patterns.md`

---

### Q11: When do photographers think about print specifications?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for "never" (smart defaults)
- **Riley Park** (Print Shop Owner) — Argues for upfront (avoid problems)
- **`pkg/zinelayout/`** (The Imposition Engine) — Argues for technical workflow

**Question Details:**
- Upfront (set paper size, page count before sequencing)?
- After sequencing (export options)?
- Never (smart defaults handle it)?
- **Why:** Determines when print settings appear in the workflow.

**Debate Round:** `reference/debate-round-11-when-to-think-about-print-specs.md`

---

### Q12: How do photographers want to save/share work?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for local + export PDFs
- **Alex Rivera** (Documentary Photographer) — Argues for cloud sync (professional workflow)
- **Sam Taylor** (Software Developer) — Argues for technical implementation (data model)

**Question Details:**
- Save projects locally?
- Cloud sync?
- Export PDFs for sharing?
- Share preview links?
- **Why:** Affects data model and sharing features.

**Debate Round:** `reference/debate-round-12-save-share-workflow.md`

---

## UX Flow and State Management

### Q13: How should the UI flow between sequencing, layout, and page composition?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for simple, linear flow
- **Taylor Kim** (UX Designer) — Argues for clear navigation and workflow
- **Sam Chen** (Frontend Developer) — Argues for state management patterns

**Question Details:**
- Linear workflow (sequence → layout → pages)?
- Tab-based navigation (separate tabs for each step)?
- Contextual navigation (show next step when ready)?
- **Why:** Affects how photographers move through the workflow and understand progress.

**Debate Round:** `reference/debate-round-13-ui-workflow-flow.md`

---

### Q14: Which APIs should be called when, and how should state sync?

**Primary Candidates:**
- **Sam Chen** (Frontend Developer) — Argues for RTK Query patterns and cache invalidation
- **Jordan Park** (Backend Developer) — Argues for API design and response structure
- **`web/src/store/`** (State Management) — Argues for efficient state updates

**Question Details:**
- When to fetch sequences vs. assets vs. layouts?
- How to handle cache invalidation (tags, refetch)?
- Optimistic updates vs. wait for server response?
- **Why:** Affects UI responsiveness and data consistency between frontend and backend.

**Debate Round:** `reference/debate-round-14-api-usage-and-state-sync.md`

---

### Q15: How should the frontend handle optimistic updates and error recovery?

**Primary Candidates:**
- **Taylor Kim** (UX Designer) — Argues for immediate feedback and graceful error handling
- **Sam Chen** (Frontend Developer) — Argues for RTK Query optimistic updates
- **`web/src/store/`** (State Management) — Argues for state consistency

**Question Details:**
- Optimistic updates (update UI immediately, rollback on error)?
- Wait for server response (slower, but guaranteed consistency)?
- How to handle errors (toast notifications, inline errors, rollback)?
- **Why:** Affects perceived performance and user confidence in the app.

**Debate Round:** `reference/debate-round-15-optimistic-updates-and-errors.md`

---

## UX+API for Core Workflows

### Q16: What is the UX and API pattern for sequencing?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for simple drag-and-drop sequencing
- **Taylor Kim** (UX Designer) — Argues for clear UI patterns and feedback
- **Sam Chen** (Frontend Developer) — Argues for RTK Query mutations and cache invalidation
- **Jordan Park** (Backend Developer) — Argues for efficient API design

**Question Details:**
- How should drag-and-drop reordering work (optimistic updates, batch API calls)?
- Which API endpoints are called when (reorder items, add item, delete item)?
- How should state sync (RTK Query cache invalidation, optimistic updates)?
- What UI feedback is needed (loading states, success/error messages)?
- **Why:** Sequencing is the core workflow—needs to feel fast and reliable.

**Debate Round:** `reference/debate-round-16-sequencing-ux-api.md`

---

### Q17: What is the UX and API pattern for assigning image layout templates?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for simple template selection
- **Taylor Kim** (UX Designer) — Argues for clear template preview and application flow
- **Sam Chen** (Frontend Developer) — Argues for efficient API calls and state management
- **Jordan Park** (Backend Developer) — Argues for batch operations and API design
- **`pkg/imagelayout/`** (The Crop Engine) — Argues for layout computation workflow

**Question Details:**
- How should templates be selected (list, preview, search)?
- How should templates be applied (single image, batch, sequence)?
- Which APIs are called (create LaidOutImage, batch create, preview)?
- How should state sync (cache invalidation, optimistic updates)?
- What UI feedback is needed (preview, loading, success/error)?
- **Why:** Template assignment bridges sequencing and page layout—needs clear UX and efficient APIs.

**Debate Round:** `reference/debate-round-17-image-layout-ux-api.md`

---

### Q18: What is the UX and API pattern for creating and assigning page templates?

**Primary Candidates:**
- **Jordan Kim** (Designer/Photographer) — Argues for template creation and reuse
- **Taylor Kim** (UX Designer) — Argues for clear template creation and assignment flow
- **Sam Chen** (Frontend Developer) — Argues for template CRUD and state management
- **Jordan Park** (Backend Developer) — Argues for template API design
- **`pkg/pagelayout/`** (The Page Renderer) — Argues for template system workflow

**Question Details:**
- How should templates be created (wizard, form, presets)?
- How should templates be assigned (single page, batch, sequence)?
- Which APIs are called (create PageTemplate, create LaidOutPage, batch operations)?
- How should state sync (cache invalidation, template preview)?
- What UI feedback is needed (template preview, page preview, loading states)?
- **Why:** Page templates are reusable—needs clear creation flow and efficient assignment.

**Debate Round:** `reference/debate-round-18-page-layout-ux-api.md`

---

## Advanced Features (Progressive Disclosure)

### Q19: What advanced features should be available but hidden?

**Primary Candidates:**
- **Jordan Kim** (Designer/Photographer) — Argues for custom templates, fine-grained controls
- **Maya Chen** (Experimental Photographer) — Argues for keeping it simple
- **Sam Taylor** (Software Developer) — Argues for progressive disclosure patterns

**Question Details:**
- Custom page templates?
- Fine-grained cropping controls?
- Typography/text overlays?
- Color adjustments?
- **Why:** Power users need features, but they shouldn't clutter the primary interface.

**Debate Round:** `reference/debate-round-19-advanced-features.md`

---

### Q20: How should advanced features be discovered?

**Primary Candidates:**
- **Sam Taylor** (Software Developer) — Argues for contextual hints + settings menu
- **Jordan Kim** (Designer/Photographer) — Argues for keyboard shortcuts (power users)
- **The New Hire** — Questions discoverability patterns

**Question Details:**
- Contextual hints ("Want more control? Click here")?
- Settings menu?
- Keyboard shortcuts?
- **Why:** Affects discoverability without overwhelming new users.

**Debate Round:** `reference/debate-round-20-feature-discovery.md`

---

## Debate Schedule

**Round 1 (Foundation):** Q1-Q4 — Sequencing experience
**Round 2 (Supporting):** Q5-Q8 — Cropping and layout
**Round 3 (Workflow):** Q9-Q12 — Mental model and iteration
**Round 4 (UX Flow):** Q13-Q15 — UI workflow flow, API usage, optimistic updates
**Round 5 (Core Workflows):** Q16-Q18 — Sequencing UX+API, Image Layout UX+API, Page Layout UX+API
**Round 6 (Advanced):** Q19-Q20 — Progressive disclosure

Each round includes:
1. Pre-Debate Research (candidates gather evidence)
2. Opening Statements (candidates argue positions)
3. Rebuttals (candidates respond and adjust)
4. Moderator Summary (extract key arguments and trade-offs)
