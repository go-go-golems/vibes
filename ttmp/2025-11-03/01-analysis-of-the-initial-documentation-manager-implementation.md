---
Title: Analysis – Initial Documentation Manager (CLI) vs RFC
Status: draft
Topics:
- documentation
- llm-workflow
- process
Created: 2025-11-03
---

# Analysis: docmgr CLI vs RFC – Scope, Status, Divergences, and Recommendations

This document provides a deep-dive review of the intern-built CLI at `vibes/2025-11-03/doc-manager/docmgr` against the RFC in `vibes/2025-11-03/doc-manager/rfc.md`. The HTTP server found in the same module is purposefully out of scope for this analysis.

## 1) Purpose and Scope
- Evaluate current CLI feature-set and structure against the RFC’s proposed `ttmp` workflow, directory layout, and metadata model.
- Identify divergences, missing capabilities, and potential pitfalls.
- Catalog the relevant files, types, and functions for further development.
- Provide concrete, prioritized recommendations to move toward a production-ready implementation.

## 2) Current CLI Overview
- Module: `vibes/2025-11-03/doc-manager/docmgr`
- Entrypoint: `cmd/docmgr/main.go`
- Commands implemented:
  - `init`: scaffold a workspace under a configurable root (default `docs/active/<TICKET>-<slug>`)
  - `list`: list workspaces under `docs/active`
  - `add`: add new docs for select types (design-doc, reference, playbook)
  - `import file`: import a local file into `sources/local` and record metadata
  - `doctor`: validate presence/fields of ticket-level `index.md`

### Build and Sanity Checks
- Building required adding the module to the root go.work:
  - `go work use vibes/2025-11-03/doc-manager/docmgr`
- Executed successfully:
  - `docmgr --help`, `docmgr list --root test-workspace/docs`, `docmgr doctor --root test-workspace/docs`
- Observed correct listing and basic validations on the provided sample workspace.

## 3) Relevant Files, Symbols, Responsibilities

- CLI Entrypoint
  - `cmd/docmgr/main.go`
    - Registers commands created via Glazed wrappers: `NewInitCommand`, `NewListCommand`, `NewAddCommand`, `NewDoctorCommand`, `NewImportFileCommand`.

- Commands
  - `pkg/commands/init.go`
    - Type: `InitCommand`, `InitSettings`
    - Core actions:
      - Creates `docs/active/<TICKET>-<slug>/` with subdirs: `design/`, `reference/`, `playbooks/`, `scripts/`, `sources/`, `.meta/` (no `various/`, `archive/`, `tasks.md`, `changelog.md`).
      - Writes `index.md` with frontmatter modeled by `models.Document`.
      - Writes a basic `README.md`.
    - Helper: `writeDocumentWithFrontmatter(path, *models.Document, content)`.

  - `pkg/commands/add.go`
    - Type: `AddCommand`, `AddSettings`
    - Supports `--doc-type` of: `design-doc`, `reference`, `playbook` only (no `working-note`, `tutorial`, `task-list`, `log`, `script`).
    - Places new doc in the matching subdir; inherits metadata (topics, owners, status) from workspace `index.md`.

  - `pkg/commands/list.go`
    - Type: `ListCommand`, `ListSettings`
    - Scans `docs/active` and reads each `index.md` frontmatter using `frontmatter.Parse`.
    - Emits rows with `ticket`, `title`, `status`, `topics`, `path`, `last_updated`.

  - `pkg/commands/doctor.go`
    - Type: `DoctorCommand`, `DoctorSettings`
    - Checks for `index.md` and validates presence of required fields (`Title`, `Ticket`, `Status`, `Topics`).
    - Does not validate vocabulary, staleness, or directory/file presence beyond `index.md`.

  - `pkg/commands/import_file.go`
    - Type: `ImportFileCommand`, `ImportFileSettings`
    - Copies a local file into `sources/local/`, appends entry to `.meta/sources.yaml` (array of `ExternalSource`).
    - Adds a `local:<filename>` entry to `index.md` `ExternalSources` if missing.
    - Utilities: `findTicketDirectory`, `appendSourceMetadata`, simple frontmatter splitting routine.

- Models
  - `pkg/models/document.go`
    - `Document` frontmatter model:
      - Title, Ticket, Status, Topics, DocType, Intent, Owners, RelatedFiles, ExternalSources, Summary, LastUpdated.
    - `Vocabulary` and `VocabItem` types exist but are not wired into validation.

## 4) Comparison to RFC: Divergences and Gaps

### 4.1 Directory Layout
- RFC standard: `ttmp/MEN-XXX-<slug>/` with required `index.md` and recommended subdirs:
  - `various/`, `design/`, `reference/`, `playbooks/`, `scripts/`, optional `archive/`; plus sibling files `tasks.md`, `changelog.md`.
- Current CLI: `docs/active/<TICKET>-<slug>/` with: `design/`, `reference/`, `playbooks/`, `scripts/`, `sources/`, `.meta/` only.
- Divergences:
  - Root naming: `docs/active` vs RFC’s `ttmp/` root.
  - Missing: `various/`, `archive/`, `tasks.md`, `changelog.md` creation.
  - Extra: `sources/` and `.meta/` present (not specified in RFC, but potentially useful).

### 4.2 Metadata Schema
- RFC frontmatter (required): `Status`, `Topics`. Recommended: `DocType`, `Intent`, `Owners`, `RelatedFiles`, `Summary`, `LastUpdated`.
- Current CLI’s `Document` model includes all of the above plus `ExternalSources`. Keys use capitalized YAML names (e.g., `Title`, `Ticket`, `Status`).
- Divergences:
  - Vocabulary validation absent (no enforcement against `doc/vocabulary.yaml`).
  - `ExternalSources` is not in RFC schema; it’s an extension in this implementation.

### 4.3 Commands and Workflow
- RFC proposed CLI: `ttmp` with verbs including `init`, `add <docType>`, `relate`, `meta update`, `vocab list|edit|add|assign`, `list tickets|docs`, `doctor`, `search`.
- Current CLI (docmgr): `init`, `list`, `add`, `import file`, `doctor`.
- Missing vs RFC:
  - `add` doc types: `working-note`, `tutorial`, `task-list`, `log`, `script`.
  - `relate` (manage `RelatedFiles`), `meta update` (frontmatter editing from CLI).
  - Vocabulary commands: `vocab list|edit|add`, `vocab assign`.
  - `list docs` vs `list tickets` distinction (current `list` lists workspaces/tickets only).
  - `search` (only present in the excluded server, not in the CLI).
  - `init` branch-derived ticket inference (RFC: derive MEN-#### from current branch).
  - Templates under `ttmp/_templates` and guidelines under `ttmp/_guidelines`.

### 4.4 Idempotency and Safety
- RFC: “CLI should be idempotent and safe to re-run.”
- Current behavior:
  - `init` will overwrite `index.md` and `README.md` unconditionally if they exist (uses `os.Create` without prior existence checks). This is not idempotent and risks data loss.
  - `add` correctly fails if file already exists.
  - `import file` appends to `.meta/sources.yaml` and updates `index.md` safely for `ExternalSources` de-dup.

### 4.5 Conventions and Naming
- RFC: Encourage `01-`, `02-` prefixes for file ordering within subdirs.
- Current: `add` generates `slug.md` without numeric prefixes.
- RFC: Only the ticket landing page should have `DocType: index`.
- Current: Not enforced.

### 4.6 Health Checks (doctor)
- RFC: doctor should check missing `index.md`, invalid metadata values (against vocabulary), unknown topics/doc types, stale `LastUpdated` (>14 days), missing `Status`, missing `index`, etc.
- Current: Validates presence of fields on `index.md` only, no vocabulary validation, no staleness, no per-subdir checks, no enforcement of required `index.md` uniqueness.

### 4.7 Controlled Vocabulary
- RFC: `doc/vocabulary.yaml` with `topics`, `docTypes`, `intent` and commands to manage it.
- Current: Types defined (`Vocabulary`, `VocabItem`) but unused; no loaders, no validation, no commands.

### 4.8 Ticket Landing Page Contract
- RFC: `index.md` should be the canonical entry point; contain status, key links, hot topics, open tasks; standardized.
- Current: `index.md` exists with correct fields, but:
  - No canonical tasks list (`tasks.md` or embedded checklists).
  - No changelog integration.
  - No “RelatedFiles” population helpers.

## 5) Observed Runtime Behavior (Test Workspace)
- Commands executed:
  - `docmgr list --root vibes/2025-11-03/doc-manager/test-workspace/docs`
  - `docmgr doctor --root vibes/2025-11-03/doc-manager/test-workspace/docs`
- Results:
  - `list` shows three workspaces (DOC-1001, MEN-3412, MEN-3475) with expected metadata.
  - `doctor` reports “All checks passed” on each, given minimal checks.
- Sample frontmatter observed in `MEN-3475-chat-backend-normalization/index.md` matches current `Document` struct and includes `ExternalSources` entries for local files.

## 6) Technical Notes and Caveats
- Workspace build setup: repository-wide go.work did not include the module by default; added via `go work use` to build within the monorepo.
- Implementation uses Glazed for command registration and I/O; this is fine and likely beneficial long term.
- The presence of `.meta/` and `sources/` is a useful extension, but should be reconciled with RFC (document how they fit the model and whether they remain optional or standardized).

## 7) Summary of Gaps vs RFC (Prioritized)
1) Idempotency: Prevent `init` from overwriting existing `index.md`/`README.md`.
2) Directory structure parity: create `various/`, `archive/`, `tasks.md`, `changelog.md` per RFC.
3) Command coverage:
   - Add missing doc types (`working-note`, `tutorial`, `task-list`, `log`, `script`).
   - Implement `relate`, `meta update`, vocabulary management (`vocab list|edit|add|assign`).
   - Add `list docs` and `search` in the CLI (not just server).
   - Derive ticket from branch name in `init` when not provided.
4) Vocabulary validation: load and validate against `doc/vocabulary.yaml`.
5) Doctor: validate vocabulary values, staleness (>14 days), required structure, and enforce `DocType: index` uniqueness.
6) Conventions: adopt `01-`, `02-` prefixes in generated filenames within subdirs.
7) Root paths: align with RFC (`ttmp/` root) or make root strategy configurable and documented.

## 8) Recommendations and Next Steps
- Align scaffolding with RFC. Extend `init` to:
  - Create RFC directories and files, including `various/`, `tasks.md`, `changelog.md`, optional `archive/`.
  - Respect idempotency: do not overwrite existing files unless `--force` is set.
  - Optional: generate `.gitkeep` in empty subdirs.
- Expand `add` to support RFC doc types with appropriate subdir mapping and metadata defaults.
- Introduce `relate` to manage `RelatedFiles` on `index.md` and selected docs; support heuristics (git history, globbing) for suggestions.
- Add `meta update` for frontmatter editing (Status, Intent, Topics, Owners, etc.).
- Implement `vocab` commands and wire `doc/vocabulary.yaml` validation in `init`, `add`, and `doctor`.
- Add `list docs` and `search` commands to the CLI; unify behavior with any future API.
- Enforce naming conventions and indexing prefixes in generated files.
- Branch-derived ticket inference in `init` (MEN-\d+ pattern), falling back to explicit `--ticket`.
- Keep `sources/` and `.meta/` as optional but documented features; specify how they integrate with RFC (e.g., captured in `ExternalSources`).

## 9) File/Function Reference (for implementers)
- `cmd/docmgr/main.go`: command registration via Glazed → Cobra.
- `pkg/commands/init.go`: workspace scaffolding; `writeDocumentWithFrontmatter`.
- `pkg/commands/add.go`: add doc with limited doc types.
- `pkg/commands/list.go`: list workspaces under `root/active`.
- `pkg/commands/doctor.go`: basic checks on `index.md` fields.
- `pkg/commands/import_file.go`: local file import, `.meta/sources.yaml`, `ExternalSources` update; helper `findTicketDirectory`.
- `pkg/models/document.go`: frontmatter schema; `Vocabulary` types (currently unused).

## 10) Decision: Prototype vs Foundation
Given the scope and quality:
- As-is, this is a solid prototype and partial foundation. The CLI builds, runs, and demonstrates key flows (scaffold, list, add, import, doctor) with coherent metadata handling.
- However, to meet RFC expectations for production use, we must close the gaps listed above—particularly idempotency, directory layout parity, vocabulary validation, and full command coverage.

---

Appendix A – Code Anchors (paths only; see source for details)
- Entrypoint: `vibes/2025-11-03/doc-manager/docmgr/cmd/docmgr/main.go`
- Commands: `vibes/2025-11-03/doc-manager/docmgr/pkg/commands/*.go`
- Models: `vibes/2025-11-03/doc-manager/docmgr/pkg/models/document.go`
- RFC: `vibes/2025-11-03/doc-manager/rfc.md`

