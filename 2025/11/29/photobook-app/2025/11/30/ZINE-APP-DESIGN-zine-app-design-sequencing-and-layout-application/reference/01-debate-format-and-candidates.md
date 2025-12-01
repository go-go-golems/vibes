---
Title: 'Debate Format and Candidates'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - debate
    - design-process
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
Summary: Candidate profiles and debate rules for exploring zine app design decisions
LastUpdated: 2025-11-30T15:00:00-05:00
---

# Debate Format and Candidates

**Purpose:** This document defines the candidates (personas) who will participate in the debate rounds and establishes the rules of engagement for exploring design decisions for the zine sequencing and layout application.

## Debate Rules

1. **Evidence-based arguments:** Candidates must use data from research, codebase analysis, or user research to support claims
2. **Respectful disagreement:** Candidates can challenge each other's positions but must acknowledge valid evidence
3. **Position changes allowed:** Candidates can adjust their positions when presented with compelling evidence
4. **Moderator summarizes:** After each round, the moderator extracts key arguments, tensions, and trade-offs
5. **No predetermined outcomes:** Debates explore options; decisions come after synthesis

## Candidate Types

We have three types of candidates:

1. **Human Developer/User Personas** (5 candidates) — Represent different user perspectives and developer concerns
2. **Code Entity Personas** (3 candidates) — Personify actual code modules with their own perspectives
3. **Wildcards** (2 candidates) — Meta or external perspectives

---

## Human Developer/User Personas

### 1. Maya Chen — The Experimental Photographer

**Role:** Fine art photographer, creates zines from personal projects

**Background:**
- 28 years old, MFA in Photography
- Creates 2-3 zines per year from personal work
- Works primarily with film photography, scans images herself
- Values experimentation and "feeling" over technical precision
- Uses Instagram to share work, wants physical zines for exhibitions

**Core Perspective:** "I just want to drag images around and see how they feel together. I don't want to think about pixels or margins until I'm happy with the story."

**Main Concerns:**
- Sequencing should feel creative and fluid
- Technical details should stay hidden until needed
- Speed matters more than perfect preview quality
- Visual flow and narrative feel

**Personality Traits:**
- Intuitive, creative-first
- Impatient with technical complexity
- Values "feeling" over precision
- Wants tools that get out of the way

**Tools They'll Use:**
- User research data
- Workflow observations
- Analogies to physical sequencing (prints on wall)

---

### 2. Alex Rivera — The Documentary Photographer

**Role:** Photojournalist, creates zines from assignment work

**Background:**
- 35 years old, 10 years professional experience
- Creates zines to tell stories from assignments
- Works with digital cameras, shoots hundreds of images per project
- Needs to tell coherent narratives through image selection and sequencing
- Values speed and efficiency but also narrative control

**Core Perspective:** "I have 200 images from a week-long assignment. I need to find the 16 that tell the story, put them in the right order, and get them printed. I don't want to fight with software."

**Main Concerns:**
- Fast image selection and sequencing
- Visual feedback on narrative flow
- Spread preview (two-page view)
- Batch operations
- Export options for different print shops

**Personality Traits:**
- Efficiency-focused
- Narrative-driven
- Professional workflow needs
- Wants tools that support storytelling

**Tools They'll Use:**
- Workflow efficiency metrics
- Professional tool comparisons
- Narrative structure analysis

---

### 3. Jordan Kim — The Designer/Photographer Hybrid

**Role:** Graphic designer who also creates photo zines

**Background:**
- 32 years old, BFA in Graphic Design
- Works as freelance designer, creates personal zine projects
- Understands typography, layout, print production
- Values both creative experimentation and technical precision
- Creates zines for clients and personal work

**Core Perspective:** "I want to experiment freely, but when I find something that works, I need to be able to refine it precisely. And I want to reuse layouts across projects."

**Main Concerns:**
- Creative freedom for experimentation
- Template system for reusable layouts
- Fine-grained control when needed (but hidden by default)
- Typography and design tools
- Export options matching print shop specs

**Personality Traits:**
- Balances creativity with precision
- Template-oriented thinking
- Client-focused
- Wants progressive disclosure

**Tools They'll Use:**
- Design system analysis
- Template reuse patterns
- Print production requirements

---

### 4. Sam Taylor — The Software Developer

**Role:** Full-stack developer building the zine application

**Background:**
- 30 years old, 8 years software development experience
- Works on both frontend (React/TypeScript) and backend (Go)
- Understands the technical stack (imagelayout, pagelayout, zinelayout)
- Values clean architecture and maintainable code
- Needs to balance user needs with technical constraints

**Core Perspective:** "We have powerful layout engines, but users shouldn't need to know they exist. Sequencing should feel like arranging photos on a wall, not configuring software."

**Main Concerns:**
- Build intuitive UI that hides technical complexity
- Make sequencing feel fast and fluid
- Provide smart defaults that "just work"
- Allow power users to access advanced features when needed
- Ensure export quality matches print requirements

**Personality Traits:**
- Technical pragmatist
- Performance-conscious
- Architecture-minded
- User-focused despite technical background

**Tools They'll Use:**
- Codebase analysis (grep, codebase_search)
- Performance measurements
- Architecture patterns
- API design principles

---

### 5. Riley Park — The Print Shop Owner

**Role:** Small print shop specializing in zines and artist books

**Background:**
- 45 years old, 15 years in print production
- Works with artists and photographers regularly
- Understands print specifications and binding requirements
- Values files that "just work" without manual fixes
- Runs a small operation, needs efficiency

**Core Perspective:** "I need files that are print-ready. The right size, the right resolution, the right color space. If the app can generate that automatically, everyone wins."

**Main Concerns:**
- Export formats matching print shop requirements
- Automatic imposition (page ordering for folding)
- Print specification presets
- Clear documentation on export options
- Validation before export (warn about potential issues)

**Personality Traits:**
- Production-focused
- Specification-oriented
- Efficiency-minded
- Wants to minimize back-and-forth

**Tools They'll Use:**
- Print specification standards
- Production workflow analysis
- File format requirements

---

## Code Entity Personas

### 6. `pkg/imagelayout/` — The Crop Engine

**Stats:**
- Core algorithm: `InputsFromSettings()` and `ComputeViewport()`
- Purpose: Computes crop regions, scale factors, and placement
- Location: `zine-layout/pkg/imagelayout/engine/engine.go`

**Perspective:** "I'm the foundation. I decide what part of the image to use and how big it should be. Users shouldn't need to understand my math, but they should trust my defaults."

**What They Want:**
- Smart defaults that "just work"
- Clear separation of concerns (I handle cropping, not sequencing)
- Performance (fast computation for real-time preview)

**What They Fear:**
- Being exposed too early (users shouldn't see cropping UI during sequencing)
- Over-complicated positioning controls
- Being blamed for sequencing problems

**Personality:**
- Precise, mathematical
- Proud of algorithmic elegance
- Wants to stay "under the hood"

**Tools:**
- Algorithm analysis
- Performance benchmarks
- Code structure analysis

---

### 7. `pkg/pagelayout/` — The Page Renderer

**Stats:**
- Core function: `RenderPage()`
- Purpose: Renders cropped/scaled images onto physical pages
- Location: `zine-layout/pkg/pagelayout/renderer/renderer.go`

**Perspective:** "I take images and put them on pages. I handle margins, spreads, borders, and variants. Templates are my domain—users should pick templates, not configure every detail."

**What They Want:**
- Template-based workflow
- Clear separation: sequencing happens before rendering
- Variant generation (thumbnails, spreads) handled automatically

**What They Fear:**
- Users configuring every pixel manually
- Sequencing logic mixed with rendering logic
- Performance issues from rendering too often

**Personality:**
- Template-oriented
- Production-focused
- Wants clear boundaries

**Tools:**
- Template system analysis
- Rendering performance metrics
- Variant generation patterns

---

### 8. `pkg/zinelayout/` — The Imposition Engine

**Stats:**
- Core function: `CreateOutputImage()`
- Purpose: Arranges pages on print sheets for folding
- Location: `zine-layout/pkg/zinelayout/layout.go`

**Perspective:** "I'm the final step. I take rendered pages and arrange them so folding produces the right order. Users shouldn't think about me until export time."

**What They Want:**
- Automatic imposition based on page count
- Print-ready output
- Clear separation: I run at export, not during sequencing

**What They Fear:**
- Users trying to sequence pages thinking about imposition
- Manual imposition configuration
- Being exposed too early in workflow

**Personality:**
- Final-step focused
- Print production oriented
- Wants to stay invisible until needed

**Tools:**
- Imposition algorithm analysis
- Print format requirements
- Export workflow patterns

---

## Wildcards

### 9. `web/src/` — The Frontend

**Stats:**
- React + TypeScript frontend
- Location: `zine-layout/web/src/`
- Current state: Basic UI exists, needs sequencing interface

**Perspective:** "I'm the user's window into the system. I need to feel fast, responsive, and intuitive. Don't make me expose technical complexity—make sequencing feel magical."

**What They Want:**
- Fast, responsive UI
- Progressive disclosure
- Real-time preview (but optimized)
- Drag-and-drop sequencing

**What They Fear:**
- Slow rendering blocking UI
- Exposing too many technical options
- Complex state management

**Personality:**
- User experience focused
- Performance conscious
- Wants simplicity

**Tools:**
- UI/UX patterns
- Performance analysis
- State management patterns

---

### 10. The New Hire — Fresh Eyes

**Role:** Developer new to the project

**Background:**
- Just joined the team
- Doesn't know the codebase yet
- Asks naive but important questions

**Perspective:** "I don't know how things work yet, but I can see when things are confusing. If I can't understand the workflow, users won't either."

**What They Want:**
- Clear mental models
- Obvious workflows
- Good documentation
- Intuitive defaults

**What They Fear:**
- Hidden complexity
- Unclear boundaries
- Magic that breaks unexpectedly

**Personality:**
- Curious, naive
- Questions assumptions
- Onboarding lens

**Tools:**
- Fresh perspective
- Onboarding experience
- Documentation gaps

---

## Debate Format

Each debate round follows this structure:

1. **Pre-Debate Research** — Candidates document their research (queries, code analysis, data gathering)
2. **Opening Statements** — Each primary candidate argues their position with evidence
3. **Rebuttals** — Candidates respond to each other, adjust positions based on evidence
4. **Wildcard Interruptions** — Other candidates interject with "Point of Order!" when misrepresented
5. **Moderator Summary** — Extract key arguments, tensions, trade-offs, open questions

**Key Principle:** Evidence drives arguments. Candidates must show their work—mention the grep query, code reference, or user research that supports their claim.

---

## Next Steps

1. Map questions to primary candidates (see `debate-questions.md`)
2. Conduct research → write debate rounds for each question
3. Synthesize findings into design document
4. Create RFC with decisions
