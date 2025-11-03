---
Title: RFC – Structured TTMP Documentation for LLM Workflows
Status: draft
Ticket: MEN-000
Topics:
- documentation
- llm-workflow
- process
Created: 2025-10-16
---

# RFC: Structured TTMP Documentation for LLM Workflows

## 1. Purpose
This RFC proposes a consistent structure, metadata model, and supporting tooling for intermediate documents stored under `ttmp/`. The goal is to make ticket-focused LLM collaboration more reliable for the core team without losing the flexibility that has made ad-hoc notes useful.

## 2. Background and Current State
The `ttmp/` directory has become the primary scratch space for LLM-assisted work. Recent tickets such as `ttmp/2025-10-15/01-chat-backend.md` and the October 13th collection (`ttmp/2025-10-13/08-path-standardization-summary.md`, `ttmp/2025-10-13/07-webchat-storybook-testing-guide.md`) show how much context accumulates in a few hours. They also highlight recurring friction:
- **No entry point per ticket.** Directories rarely contain an `index.md`, so contributors must skim multiple files before understanding scope or canonical decisions.
- **Inconsistent metadata.** Most files lack YAML frontmatter, making it difficult to filter by topic, maturity, or ownership, and impossible to automate migration into long-lived docs.
- **Context drift.** There is no dedicated place to summarize active code touchpoints or link to upstream tickets, so LLM prompts repeat discovery work that could be cached once.
- **Prompt reuse gaps.** Prompts sit next to exploratory transcripts (for example `ttmp/2025-10-13/03-how-agentic-workflows-work-in-go.md`), making it hard to extract clean prompt packs.
- **Search burden.** Files such as `ttmp/2025-10-13/11-timeline-architecture-summary.md` and `ttmp/2025-10-13/15-unified-timeline-redux-store.md` contain rich references but have no metadata, so discoverability still depends on grep.

## 3. Goals and Non-Goals
- **Goals**
  - Reduce slop by establishing a predictable file layout and baseline metadata for every ticket.
  - Keep enough freedom for engineers to add bespoke docs or folders.
  - Enable tooling that scaffolds new tickets, assigns topics, and surfaces related source files.
- **Non-Goals**
  - Define a governance committee or archival cadence (to be decided later).
  - Mandate a single writing style beyond referencing existing glazed guidance.

## 4. Proposed Directory Structure
Each active initiative lives under `ttmp/MEN-XXX-<slug>/`. Ticket numbers keep parity with product tracking while the slug provides human context.

```
ttmp/
  MEN-3475-chat-backend/
    index.md
    various/
      01-context.md
      02-decisions.md
    design/
      draft-architecture.md
    reference/
      01-handoff.md
      02-refinement.md
    playbooks/
      curl-smoke.md
    scripts/
      verify-ws.sh
      README.md
    tasks.md
    changelog.md
    archive/
      spike-investigation.md
```

- `index.md` – canonical landing page (required). Summarizes ticket intent, status, key links, hot topics, and open tasks.
- `various/` – scratch or meeting notes (`ttmp/2025-10-13/02-mapping-out-the-different-databases-used-for-the-services.md` is a good pattern). Rename freely (`working-notes/`, `research/`) when it clarifies contents.
- `design/` – structured proposals, architecture docs, or decisions; e.g., `ttmp/2025-10-13/11-timeline-architecture-summary.md`.
- `reference/` – reusable prompt packs or API summaries, formatted per glazed writing guidance (compare `ttmp/2025-10-13/07-webchat-storybook-testing-guide.md`).
- `playbooks/` – command sequences, scripts, and manual test procedures.
- `scripts/` – temporary code, SQL snippets, or REPL transcripts associated with the ticket. Files may include `.sh`, `.sql`, `.go`, with a short `README.md` summarizing their purpose.
- `tasks.md` – canonical task list for the ticket; keep checkboxes machine-readable.
- `changelog.md` – running log of decisions, til, and "what changed" entries to keep LLMs anchored.
- `archive/` – optional space for deprecated or reference-only artifacts to keep the main view lean.

Directories are suggestions, not enforced; contributors may add folders (for example `experiments/` or `prototype/`) if they keep metadata intact. When a workflow prefers kanban-style tracking (e.g., the storyboard documents on 2025-10-13), the ticket can introduce `tracking/` with tables or embed structured task lists directly inside `index.md`.
Within each subdirectory, name files with incremental prefixes (`01-`, `02-`, `03-`) to keep natural ordering for both humans and LLM ingestion; reuse the pattern already present in `ttmp/2025-10-13`.

## 5. Metadata Schema
Every Markdown (and Prompt) file starts with YAML frontmatter. Two fields are required (`topics`, `status`); others are optional but recommended for richer automation.

```yaml
---
Title: Normalize chat API paths
Ticket: MEN-3475
Status: active            # draft | active | review | archived
Topics:
- chat
- backend
DocType: design-doc       # valid values defined in doc/vocabulary.yaml
Intent: short-term        # short-term | long-term | throwaway
Owners:
- manuel
RelatedFiles:
- go/pkg/mento/http/web-chat/register.go
- web/src/store/api/chatApi.ts
Summary: >
  Tracks the API remapping work to align backend and frontend chat endpoints.
LastUpdated: 2025-10-16
---
```

- **Status (required):** default `draft`, then `active`, `review`, or `archived`. This gives lightweight lifecycle signaling without a heavy process.
- **Topics (required):** references to controlled vocabulary (section 7). Use lowercase slugs.
- **DocType:** guides tooling and should map to entries in `doc/vocabulary.yaml` (for example `index`, `design-doc`, `reference`, `working-note`, `tutorial`, `playbook`, `task-list`, `log`, `script`). Tooling will warn when a ticket lacks an `index`.
- **Intent:** indicates expected lifespan (`short-term`, `long-term`, `throwaway`), helping decide what graduates into permanent docs.
- **RelatedFiles:** list of repository paths relevant to the doc; CLI can populate via heuristics.
- **Summary / LastUpdated:** optional but improve scanability and diffing.

## 6. Ticket Landing Page Contract
`index.md` anchors the ticket. It should reference files, symbols, or other context that is relevant to the ticket.

## 7. Topic Dictionary
Maintain controlled vocabularies at `doc/vocabulary.yaml`. Example snippet:

```yaml
topics:
  - slug: chat
    description: Chat backend and frontend surfaces
  - slug: observability
    description: Logging, metrics, tracing, and alerting work
  - slug: llm-workflow
    description: Tooling and processes for collaborating with LLMs
docTypes:
  - slug: index
    description: Canonical entry point for the ticket
  - slug: design-doc
    description: Structured rationale and architecture notes
  - slug: reference
    description: Prompt cheat-sheets, API contracts, or context summaries
  - slug: tutorial
    description: Step-by-step or repeatable walkthroughs
  - slug: playbook
    description: Command lists and operational validation steps
  - slug: task-list
    description: Markdown checklists that track execution
  - slug: log
    description: Timeline or changelog entries
  - slug: script
    description: Temporary code or tooling kept under version control
intent:
  - slug: short-term
    description: Expected to live only for the ticket’s duration
  - slug: long-term
    description: Candidate for promotion into docs
  - slug: throwaway
    description: Ephemeral scratchpad or spike
```

- CLI commands (`ttmp vocab list topics`, `ttmp vocab edit docTypes`, `ttmp vocab add intent`) manage the dictionary for each choice list field.
- Tooling validates metadata values (topics, doc types, intent) against the dictionary and suggests matches on typos.
- Tickets may mix multiple topics but should stay concise (ideally three or fewer).

## 8. Doc Types and Conventions
- **index:** required single source of truth per ticket (one per directory). Only this doc should have `DocType: index`.
- **working-note:** free-form logs or meeting notes; encourage short summaries at the top for LLM ingestion.
- **design-doc:** structured proposals with executive summary and decision sections.
- **reference:** curated prompts, API contracts, or quick-look tables – aim for copy/paste-ready context.
- **tutorial:** step-by-step workflows or reusable prompt playbooks. Follow `glazed/pkg/doc/topics/how-to-write-good-documentation-pages.md` for structure.
- **playbook:** command lists, cURL scripts, or manual test steps. Include environment assumptions and exit criteria.
- **log:** running changelog or incident timeline; prefer reverse chronological order.
- **task-list:** canonical TODO with checkboxes and owners.
- **script:** Markdown wrappers inside `scripts/` that describe executable scratch files stored nearby.

DocType feeds tooling to assemble prompt packs or run ticket-level health checks.

## 9. Workflow Guidance
1. **Scaffold:** run `ttmp init MEN-3475 --title "LLM chat API cleanup" --topics chat,llm-workflow`. The command creates the directory, populates `index.md`, `tasks.md`, `changelog.md`, and optional folder stubs (`design/`, `reference/`, `playbooks/`, `scripts/`) with metadata that matches the vocabulary file. When invoked without a ticket argument, `ttmp init` should derive the ticket identifier from the current branch name (pattern `MEN-\d+-.*`) before falling back to prompting the user.
2. **Attach context early:** use `ttmp relate --files go/pkg/mento/http/web-chat/register.go web/src/store/api/chatApi.ts` to populate `RelatedFiles` on `index.md` and any `design-doc` that leans on the same code. The command can pull recent git history to suggest files.
3. **Capture working notes:** add ad-hoc fragments via `ttmp add working-note "storyboard sync"`; the template keeps summaries and metadata consistent while allowing free-form exploration.
4. **Document prompts as references:** when a prompt stabilizes, run `ttmp add reference "websocket triage"` which opens a pre-filled Markdown file under `reference/` with fields for goal, context chunk list, and call-to-action for the LLM.
5. **Maintain scripts:** store scratch helpers in `scripts/` with short README entries. CLI option `ttmp add script verify-ws` can scaffold a `scripts/verify-ws.sh` and `scripts/verify-ws.md` pair.
6. **Log decisions:** after notable outcomes, append entries to `changelog.md` via `ttmp log add "Aligned backend routes with frontend"` to keep the LLM consistent.
7. **Close-out:** when a ticket wraps, set `Status: archived`, ensure reusable `tutorial` or `reference` docs are linked from longer-lived documentation, and run `ttmp doctor --ticket MEN-3475` to confirm metadata hygiene.

The workflow keeps the process “guardrailed but flexible” by combining required metadata with optional sections.

## 10. CLI Tooling Proposal
Implement a CLI (Go) exposed as `ttmp`. Suggested verbs and flags:

- `ttmp init <ticket> --title <str> --topics <comma> [--owners manuel,alex] [--intent short-term]`
  - Creates ticket directory, stubs `index.md`, `tasks.md`, `changelog.md`, `.gitkeep` for optional folders, and injects metadata.
- `ttmp add working-note|design-doc|reference|tutorial|playbook|task-list|script <name> [--ticket MEN-3475] [--topics ...]`
  - Generates new docs from templates stored under `ttmp/_templates`.
- `ttmp relate --ticket MEN-3475 --files path1 path2`
  - Updates `RelatedFiles` for `index.md` (or specified doc). Optionally integrates with `rg` or `git grep` to suggest matches via `--suggest "feature flag"`.
- `ttmp meta update --doc reference/01-handoff.md --field Status --value review`
  - Edits metadata fields (status, intent, topics, owners) programmatically; supports multi-doc selection via `--ticket`, `--doc-type`, or `--all`.
- `ttmp vocab list topics|docTypes|intent`
  - Manages `doc/vocabulary.yaml`; editing may invoke `$EDITOR`.
- `ttmp vocab assign --ticket MEN-3475 --doc reference/01-handoff.md --topics chat,llm-workflow`
  - Ensures documents stay in sync after manual edits.
- `ttmp list tickets|docs --ticket MEN-3475 [--format table]`
  - Streams ticket summaries through Glazed presenters (see `glazed/pkg/doc/tutorials/build-first-command.md`) for tabular, JSON, or Markdown output.
- `ttmp doctor --ticket MEN-3475`
  - Runs health checks: missing `index.md`, docs without required metadata, unknown topics, stale `LastUpdated` (>14 days), missing `Status`.
- `ttmp search --ticket MEN-3475 --topics chat --files`
  - Uses heuristics (recent commit history, glob patterns) to recommend `RelatedFiles`.

CLI should be idempotent and safe to re-run. Templates live in `ttmp/_templates/`, and reusable guidance per doc type or task resides in `ttmp/_guidelines/` so commands like `ttmp guidelines reference` can emit quick reminders for LLM context.

