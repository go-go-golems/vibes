---
Title: Sober Review, Guide, and Design Plan – Documentation Manager
Ticket: MEN-000
Status: draft
Topics:
- documentation
- llm-workflow
- process
DocType: design-doc
Intent: long-term
Owners:
- manuel
RelatedFiles:
- vibes/2025-11-03/doc-manager/docmgr/cmd/docmgr/main.go
- vibes/2025-11-03/doc-manager/docmgr/pkg/commands/init.go
- vibes/2025-11-03/doc-manager/docmgr/pkg/commands/add.go
- vibes/2025-11-03/doc-manager/docmgr/pkg/commands/list.go
- vibes/2025-11-03/doc-manager/docmgr/pkg/commands/doctor.go
- vibes/2025-11-03/doc-manager/docmgr/pkg/commands/import_file.go
- vibes/2025-11-03/doc-manager/docmgr/pkg/models/document.go
- vibes/2025-11-03/doc-manager/rfc.md
Summary: >
  Clear-eyed assessment of the current CLI, a pragmatic guide for daily use, and a
  phased design plan (P0/P1/P2) to align with the RFC while maintaining a steady
  path toward a production-ready system.
LastUpdated: 2025-11-03
---

# Sober Review, Implementation Guide, and Design Plan

This document distills the current state of the documentation manager, provides a practical usage guide, and lays out a concrete plan to evolve it into a production-grade tool aligned with the RFC.

## 1) Executive Summary
- The CLI works and demonstrates key flows: `init`, `list`, `add` (subset), `import file`, `doctor`.
- Gaps vs RFC: directory layout (`ttmp/` root, missing `various/`, `tasks.md`, `changelog.md`, optional `archive/`), limited doc types, no vocabulary validation or commands, no `relate` or `meta update`, minimal `doctor` checks, non-idempotent `init`.
- Plan: Address idempotency and scaffolding parity first (P0), add RFC commands and validations (P1), and extend with search, heuristics, and conventions enforcement (P2).

## 2) Current State (Concise)
- Root: configurable; defaults to `docs/active/<TICKET>-<slug>/` (RFC expects `ttmp/MEN-XXX-<slug>/`).
- Metadata: frontmatter keyed off `models.Document` (Title, Ticket, Status, Topics, DocType, Intent, Owners, RelatedFiles, ExternalSources, Summary, LastUpdated).
- Commands implemented:
  - `init` (scaffold), `list` (tickets), `add` (design-doc|reference|playbook), `import file` (to `sources/local`), `doctor` (basic checks only).
- Missing commands (RFC §10): `relate`, `meta update`, `vocab ...`, `list docs`, `search` (present in server but out-of-scope here), branch-derived ticket inference.

## 3) Practical Usage Guide (Current CLI)
- Initialize workspace
  - `docmgr init MEN-3475 --title "Chat API cleanup" --topics chat,llm-workflow --root ttmp`
  - Note: currently writes to `root/active`, not `ttmp/MEN-XXX-<slug>` by default; use `--root` to control base path.
- Add documents
  - `docmgr add --ticket MEN-3475 --doc-type design-doc --title "Draft Architecture" --root ttmp`
  - Supported types: design-doc, reference, playbook.
- Import a local source
  - `docmgr import file --ticket MEN-3475 --file ./specs/ws.md --name "WS Spec" --root ttmp`
- List tickets
  - `docmgr list --root ttmp`
- Basic health checks
  - `docmgr doctor --root ttmp`

Caveats
- `init` overwrites existing `index.md`/`README.md` (not idempotent).
- No vocabulary validation; typos in `Topics` and `DocType` go unnoticed.
- No `tasks.md`, `changelog.md`, `various/`, `archive/` scaffolding.
- No `relate` or `meta update` commands yet.

## 4) Design Principles
- Safety first: Idempotent operations by default; destructive actions behind `--force`.
- RFC alignment: Prefer RFC structure and vocabulary; allow configuration for legacy layouts.
- Single source of truth: Frontmatter remains canonical; workspace-wide metadata can be denormalized (e.g., `.meta/`) but must be derived and validated.
- Extensibility: Add new doc types and behaviors via registries rather than forks.
- Observability: Clear, structured output; stable field names for downstream tools.

## 5) Phased Plan (P0 → P2)

### P0 – Stabilize and Align the Basics (sprint-ready)
- Idempotency for `init`
  - Check existence of `index.md` and `README.md`; refuse to overwrite unless `--force`.
  - Preserve existing frontmatter and content when re-running `init` without `--force`.
- RFC scaffolding parity
  - Create `various/`, `tasks.md`, `changelog.md`, optional `archive/`.
  - Keep `scripts/`, `reference/`, `design/`, `playbooks/`, `sources/`, `.meta/` (document their roles).
- Root strategy
  - Default to `ttmp/` while allowing `--root` to honor legacy `docs/active` layouts.
- Conventions
  - Ticket landing page: enforce single `DocType: index` in each workspace.
  - Optional file prefixes (`01-`, `02-`) behind a flag or config.
- Minimal doctor expansion
  - Validate required fields exist; ensure `index.md` uniqueness; warn if `LastUpdated` > 14 days.

### P1 – RFC Command Coverage and Validation
- Vocabulary
  - Load `doc/vocabulary.yaml`; add `vocab list|edit|add|assign` commands.
  - Validate `Topics`, `DocType`, and `Intent` in `init`, `add`, and `doctor`.
- `relate`
  - `ttmp relate --ticket MEN-XXXX --files ...` to update `RelatedFiles` in `index.md` (and optionally in a target doc).
  - `--suggest` leveraging git history and ripgrep; interactive acceptance (non-interactive modes supported by flags).
- `meta update`
  - `ttmp meta update --doc <path> --field <key> --value <val>`; multi-doc selection by `--ticket` or `--doc-type`.
- Listing
  - `ttmp list tickets` and `ttmp list docs` with Glazed presenters (table, json, markdown).
- Ticket inference in `init`
  - Derive ticket from branch name `MEN-\d+` when `--ticket` omitted; fallback to prompt or error.

### P2 – Power Features and Enforcement
- `search` in the CLI
  - Filter by topic, type, ticket, and substring; reuse server logic but keep it in CLI.
- Conventions enforcement
  - Enforce numeric prefixes (optional strict mode); flag non-conforming filenames.
- Doctor deep checks
  - Unknown topics/doc types, stale workspaces, missing required subdirs/files, multiple `index.md`s, bad `DocType` usage.
- Templates and guidelines
  - `ttmp/_templates/` for doc scaffolds per type; `ttmp/_guidelines/` for quick LLM-ready reminders.
- Config file
  - Optional `.ttmp.yaml` at repo root: root path, defaults (owners, intent), filename prefix policy, doc-type registry toggles.

## 6) Command Specifications (CLI)
- `ttmp init <ticket?> --title <str> --topics <comma> [--owners ...] [--intent ...] [--root ...] [--force]`
  - Derive `<ticket>` when omitted.
  - Create RFC-aligned layout; avoid overwrites unless `--force`.
- `ttmp add <doc-type> --ticket <id> --title <str> [--root ...] [--prefix auto|off]`
  - Supported types: index, working-note, design-doc, reference, tutorial, playbook, task-list, log, script.
- `ttmp relate --ticket <id> --files <paths...> [--doc <path>] [--suggest <query>]`
  - Update `RelatedFiles`; `--suggest` sources include git and ripgrep.
- `ttmp meta update --doc <path> --field <key> --value <val>`
  - Supports multi-doc selection via `--ticket`, `--doc-type`.
- `ttmp vocab list|edit|add|assign ...`
  - Operate on `doc/vocabulary.yaml`; validate on write.
- `ttmp list tickets|docs [--format table|json|markdown] [--ticket ...] [--status ...] [--topics ...]`
- `ttmp doctor --ticket <id?> [--all] [--strict]`
  - Checks: required fields; vocabulary; staleness; layout sanity; unique `index`.
- `ttmp search [--ticket ...] [--topic ...] [--type ...] [--q ...]`

## 7) Migration Strategy (docs/active → ttmp/)
- Non-destructive migration command
  - `ttmp migrate --from docs/active --to ttmp --dry-run`
  - Map `docs/active/<TICKET>-<slug>` → `ttmp/<TICKET>-<slug>`.
  - Preserve timestamps; verify frontmatter parses; generate report.
- Compatibility
  - Allow `--root` to continue operating in legacy trees for a deprecation window.
- Backups and rollbacks
  - Archive old path as `<dir>.bak` or guard via `--force` to move.

## 8) Testing and Validation
- Unit tests
  - Frontmatter read/write; idempotent `init` behavior; vocabulary loading and validation.
- Integration tests
  - Temp workspace end-to-end: `init → add → relate → meta update → doctor → list`.
- Golden tests
  - File scaffolding content (frontmatter blocks and standard section headers).
- CLI presenters
  - Verify table/json/markdown output schemas remain stable.

## 9) Operational Guidance
- Logging
  - Keep debug logs; add structured fields for command name, ticket, path.
- Performance
  - Avoid deep directory scans; scope operations per ticket; cache vocabulary.
- DX (Developer Experience)
  - Helpful error messages; `--dry-run` modes; clear warnings for non-idempotent actions.

## 10) Risks and Mitigations
- Data loss via overwrites → default-safe `init`, backups, `--force` required.
- Vocabulary drift → central file with validation + doctor checks.
- Layout fragmentation → explicit config + default to RFC.
- Backward compatibility → long-lived `--root` override and migration tooling.

## 11) Milestones & Timeline
- P0 (1–2 days): Idempotent `init`, RFC scaffolding parity, minimal doctor expansion, `ttmp` as default root (configurable).
- P1 (3–5 days): Vocabulary commands + validation, `relate`, `meta update`, `list docs`, ticket inference.
- P2 (3–5 days): CLI `search`, strict conventions, deep doctor, templates/guidelines, optional config file.

## 12) Work Plan Checklist
- [x] Make `init` idempotent; add `--force` and preserve content.
- [x] Scaffold `various/`, `tasks.md`, `changelog.md`, optional `archive/`.
- [x] Default root to `ttmp/`; support legacy via `--root`.
- [x] Expand `doctor` with staleness and unique `index` checks.
- [x] Implement vocabulary loader and `vocab list|add`.
- [ ] Add `relate` with `--suggest` (git + ripgrep heuristics).
- [ ] Add `meta update` for frontmatter edits.
- [ ] Split `list` into `list tickets|docs` with presenters.
- [ ] Implement ticket inference from branch name.
- [ ] Implement CLI `search` parity with server.
- [ ] Add filename prefix policy and optional enforcement.
- [ ] Create `ttmp/_templates/` and `ttmp/_guidelines/` scaffolds.
- [ ] Build migration command `migrate --dry-run` and a report.
- [ ] Add unit/integration/golden tests for all above.

---

## 13) Detailed Context and Implementation Notes

### Background and Motivation
The documentation manager addresses a real pain point in LLM-assisted development workflows: managing the growing collection of intermediate documents, context notes, design decisions, and external references that accumulate during ticket work. The RFC (see `vibes/2025-11-03/doc-manager/rfc.md`) emerged from observing friction in the existing `ttmp/` directory usage, where:
- No standard entry point made it hard to onboard collaborators
- Inconsistent metadata prevented automation and filtering
- Context drift forced repeated discovery work
- Prompt reuse was difficult without clean separation

The intern's prototype demonstrates that the core concept is sound and implementable using existing tools (Glazed, Cobra, frontmatter parsing).

### Architecture Overview
The CLI is built on Glazed, a Go framework that provides:
- Structured command definitions with typed parameters (`pkg/cmds/parameters`)
- Multiple output formatters (table, JSON, YAML, CSV) via "presenters" (`pkg/middlewares`)
- Layer-based parameter resolution (flags, config files, env vars) (`pkg/cmds/layers`)

This architecture choice is excellent for our use case because:
1. Output flexibility: downstream tools (LLMs, scripts, UIs) can consume JSON; humans get tables
2. Parameter validation and help generation are declarative
3. Middleware hooks for logging, validation, dry-run modes
4. Consistent error handling and context passing

**Key Files:**
- `cmd/docmgr/main.go` (lines 1-142): Root command setup; registers subcommands via `cli.BuildCobraCommand`
- Each command in `pkg/commands/*.go` implements `cmds.GlazeCommand` interface with `RunIntoGlazeProcessor`

### Current Implementation Deep-Dive

#### Init Command (`pkg/commands/init.go`)
**Purpose:** Scaffold a new ticket workspace with standard directory structure and metadata.

**Current behavior (lines 73-161):**
- Constructs path: `root/active/<TICKET>-<slug>` (line 86)
- Creates directories (lines 89-103): `design/`, `reference/`, `playbooks/`, `scripts/`, `sources/`, `.meta/`
- Writes `index.md` with frontmatter (lines 106-123) using `models.Document`
- Writes basic `README.md` (lines 127-150)
- Returns structured result via Glazed row (lines 153-160)

**Issues identified:**
1. **Non-idempotent:** Uses `os.Create(path)` at line 164 in `writeDocumentWithFrontmatter`, which truncates existing files
2. **Missing RFC directories:** No `various/`, `archive/`, `tasks.md`, `changelog.md`
3. **Root path mismatch:** Creates `root/active/` not `root/<TICKET>/` directly as RFC expects

**Fix strategy (P0):**
- Add existence checks before `os.Create`; use `os.Open` + read + merge for existing frontmatter
- Add missing directory and file scaffolding per RFC section 4 (lines 34-68)
- Make `root` default to `ttmp` instead of `docs`; keep `--root` for override

**Code references:**
- `InitCommand` struct (line 20-23): Add `Force bool` field
- `InitSettings` struct (line 26-31): Add `Force` parameter definition
- `writeDocumentWithFrontmatter` (lines 163-191): Refactor to check existence and preserve

#### Add Command (`pkg/commands/add.go`)
**Purpose:** Create new documents within an existing workspace.

**Current behavior (lines 73-148):**
- Finds ticket directory via `findTicketDirectory` (helper in `import_file.go` lines 171-195)
- Maps doc type to subdirectory (lines 90-100): only supports `design-doc`, `reference`, `playbook`
- Inherits metadata from workspace `index.md` (lines 113-132)
- Correctly checks for existing file (lines 108-110) and refuses to overwrite

**Issues identified:**
1. **Limited doc types:** Missing `working-note`, `tutorial`, `task-list`, `log`, `script` per RFC section 8
2. **No subdirectory mapping for missing types:** `working-note` → `various/`, `task-list` → root, etc.
3. **No file prefix support:** RFC section 4 suggests `01-`, `02-` for ordering

**Fix strategy (P1):**
- Extend switch statement (lines 91-100) with all RFC doc types
- Add parameter `--prefix auto|off|<number>` to control file naming
- Create `getNextPrefix(dir)` helper to scan existing files and suggest next number

**Code references:**
- `AddCommand.RunIntoGlazeProcessor` (lines 73-148): Extend doc type switch
- Add `pkg/commands/helpers.go` with `getNextPrefix(dir string) (string, error)`

#### List Command (`pkg/commands/list.go`)
**Purpose:** List all ticket workspaces with metadata.

**Current behavior (lines 67-126):**
- Scans `root/active/` (lines 77-85)
- Reads `index.md` frontmatter for each directory (lines 92-101)
- Filters by ticket and status if specified (lines 104-109)
- Emits rows with ticket, title, status, topics, path, last_updated (lines 111-122)

**Issues identified:**
1. **Only lists tickets, not individual docs:** RFC wants both `list tickets` and `list docs`
2. **Limited filtering:** No filter by topics (plural match), doc type, or date ranges

**Fix strategy (P1):**
- Split into two subcommands: `list tickets` and `list docs`
- `list docs` scans subdirs and aggregates all documents with frontmatter
- Add filter parameters: `--topics` (comma-separated, any-match), `--updated-since`, `--updated-before`

**Code references:**
- Create `pkg/commands/list_tickets.go` and `pkg/commands/list_docs.go`
- Share `readDocumentFrontmatter` helper (currently at line 128-142)

#### Doctor Command (`pkg/commands/doctor.go`)
**Purpose:** Validate workspace health and metadata correctness.

**Current behavior (lines 64-172):**
- Scans `root/active/` (lines 74-82)
- Checks for `index.md` existence (lines 92-105)
- Validates frontmatter parses (lines 108-121)
- Checks required fields: Title, Ticket, Status, Topics (lines 129-155)
- Reports issues or "All checks passed" (lines 157-167)

**Issues identified (RFC section 10, line 189-190):**
1. **No vocabulary validation:** Can't detect typos in Topics or invalid DocType values
2. **No staleness check:** LastUpdated > 14 days should warn
3. **No structure validation:** Missing subdirs, multiple `index.md`, wrong DocType usage
4. **No cross-document checks:** Related files don't exist, external sources metadata inconsistent

**Fix strategy:**
- P0: Add staleness check (compare `LastUpdated` to `time.Now()`)
- P0: Check for duplicate `index.md` files in subdirectories
- P1: Load `doc/vocabulary.yaml` and validate Topics/DocType/Intent against it
- P2: Validate `RelatedFiles` actually exist in repo, check subdirectory presence

**Code references:**
- `DoctorCommand.RunIntoGlazeProcessor` (lines 64-172): Add validation logic
- Add `pkg/commands/vocabulary.go` with `LoadVocabulary() (*models.Vocabulary, error)`
- `models.Vocabulary` (lines 22-44 in `models/document.go`): Already defined, needs loader

#### Import File Command (`pkg/commands/import_file.go`)
**Purpose:** Copy external files into workspace `sources/` and track metadata.

**Current behavior (lines 73-169):**
- Copies file to `sources/local/` (lines 95-116)
- Creates `ExternalSource` metadata entry (lines 119-128)
- Appends to `.meta/sources.yaml` (line 126 calls `appendSourceMetadata` at lines 197-219)
- Updates `index.md` `ExternalSources` field (lines 131-158)

**Issues identified:**
1. **`.meta/sources.yaml` not in RFC:** This is an extension; should document its purpose
2. **Duplication:** External sources in both `.meta/` and frontmatter
3. **No URL/git import:** Only supports local files

**Fix strategy:**
- P0: Document `.meta/` directory purpose in README or RFC addendum
- P1: Add `import url` and `import git` subcommands for HTTP and git-based sources
- Consider: Make `.meta/sources.yaml` the single source of truth; `ExternalSources` in frontmatter becomes derived

**Code references:**
- `ImportFileCommand.RunIntoGlazeProcessor` (lines 73-169)
- Add `pkg/commands/import_url.go` and `pkg/commands/import_git.go` for P1

### Models and Data Schema

#### Document Model (`pkg/models/document.go`)
**Core metadata structure (lines 8-20):**
```go
type Document struct {
    Title           string    `yaml:"Title" json:"title"`
    Ticket          string    `yaml:"Ticket" json:"ticket"`
    Status          string    `yaml:"Status" json:"status"`       // draft|active|review|archived
    Topics          []string  `yaml:"Topics" json:"topics"`       // controlled vocabulary
    DocType         string    `yaml:"DocType" json:"docType"`     // index|design-doc|reference|etc
    Intent          string    `yaml:"Intent" json:"intent"`       // short-term|long-term|throwaway
    Owners          []string  `yaml:"Owners" json:"owners"`
    RelatedFiles    []string  `yaml:"RelatedFiles" json:"relatedFiles"`
    ExternalSources []string  `yaml:"ExternalSources" json:"externalSources"`
    Summary         string    `yaml:"Summary" json:"summary"`
    LastUpdated     time.Time `yaml:"LastUpdated" json:"lastUpdated"`
}
```

**Design notes:**
- Uses capitalized YAML keys (Go convention); RFC examples use lowercase
- `time.Time` serializes to RFC3339 in YAML/JSON
- No validation methods; validation is external (should be added)

**Vocabulary Model (lines 22-34):**
```go
type Vocabulary struct {
    Topics      []VocabItem `yaml:"topics" json:"topics"`
    DocTypes    []VocabItem `yaml:"docTypes" json:"docTypes"`
    SourceTypes []VocabItem `yaml:"sourceTypes" json:"sourceTypes"`
    Lifecycle   []VocabItem `yaml:"lifecycle" json:"lifecycle"`
}

type VocabItem struct {
    Slug        string `yaml:"slug" json:"slug"`
    Description string `yaml:"description" json:"description"`
}
```

**Enhancement needed (P1):**
- Add `Validate() error` method to `Document`
- Add `ValidateAgainstVocabulary(vocab *Vocabulary) error`
- Add `IsStale(maxAge time.Duration) bool`
- Create `LoadVocabulary(path string) (*Vocabulary, error)` function

### Glazed Integration Points

The CLI leverages Glazed's capabilities but could do more:

**Current usage:**
- Parameter definitions with types and defaults (`parameters.NewParameterDefinition`)
- Row-based output (`types.NewRow`, `gp.AddRow`)
- Command descriptions (`cmds.NewCommandDescription`)

**Underutilized features:**
- Middleware: could add validation, dry-run, logging middlewares
- Layers: could support config files (`.ttmp.yaml`) for defaults
- Formatters: support --output json|yaml|csv|markdown (currently defaults to table)
- Help system: could add examples and extended help per command

**Enhancement opportunities (P2):**
- Add `DryRunMiddleware` that intercepts file writes
- Add `LoggingMiddleware` for structured command execution logs
- Create `TTMPConfigLayer` to load `.ttmp.yaml` with defaults
- Document `--output` flag usage in help text

### RFC Alignment Summary

**RFC Section → Implementation Status:**

| RFC Section | Topic | Status | Priority |
|-------------|-------|--------|----------|
| §4 | Directory structure (`ttmp/`, `various/`, `tasks.md`, etc.) | Partial (missing 4 items) | P0 |
| §5 | Metadata schema | Complete (+ ExternalSources extension) | ✓ |
| §6 | Landing page contract | Basic (no tasks/changelog) | P0 |
| §7 | Topic dictionary | Types exist, no loader/commands | P1 |
| §8 | Doc types | 3 of 8 implemented | P1 |
| §9 | Workflow guidance | init/add/import work, relate/meta/log missing | P1 |
| §10 | CLI tooling (init, add, relate, meta, vocab, list, doctor, search) | 5 of 9 command groups | P1-P2 |

**Key divergences documented in detail:**
- Root path: `docs/active/` vs `ttmp/` (cosmetic, configurable)
- `.meta/` directory: extension not in RFC (useful, should formalize)
- `ExternalSources` field: extension not in RFC (integrates with import)
- Server: built but out of scope; consider extracting shared logic to library

### Testing Strategy Details

**Unit tests (per command):**
```
pkg/commands/init_test.go:
  - TestInitCreatesDirectories
  - TestInitIdempotent (P0)
  - TestInitWithForceOverwrites (P0)
  - TestInitPreservesExistingFrontmatter (P0)

pkg/commands/add_test.go:
  - TestAddAllDocTypes (P1)
  - TestAddWithPrefix (P1)
  - TestAddRefusesOverwrite

pkg/commands/doctor_test.go:
  - TestDoctorDetectsMissingFields
  - TestDoctorValidatesVocabulary (P1)
  - TestDoctorDetectsStaleness (P0)

pkg/models/document_test.go:
  - TestDocumentValidate (P1)
  - TestDocumentIsStale (P0)
```

**Integration tests:**
```
test/integration/init_to_doctor_test.go:
  - Create temp dir
  - Run init, add multiple docs, import file
  - Run doctor, verify all checks pass
  - Modify metadata, verify doctor catches issues

test/integration/vocabulary_test.go: (P1)
  - Create workspace with invalid topics
  - Run doctor --strict
  - Verify vocabulary violations reported
```

**Golden file tests:**
```
test/golden/init/index.md.golden
test/golden/init/README.md.golden
test/golden/add/design-doc.md.golden
  - Compare scaffolded files to golden templates
  - Detect unintended changes in structure
```

### Performance Considerations

**Current bottlenecks (at scale):**
1. `list` scans entire `root/active/` and parses all `index.md` files (O(n) workspaces)
2. `doctor` does the same plus validates each workspace (O(n) workspaces × O(m) docs)
3. `findTicketDirectory` (used by add, import) scans all workspaces to match ticket (O(n))

**Optimizations (P2 or later):**
- Add `--ticket` scope to list/doctor to avoid full scan
- Consider `.ttmp/index.json` cache: maps ticket → path, updated on init
- For large repos, use git ls-files or fd instead of filepath.Walk
- Lazy-load vocabulary (singleton, cached after first load)

**Acceptable performance targets:**
- `init`: < 100ms (create dirs + write 2 files)
- `add`: < 200ms (find ticket + read index + write doc)
- `list`: < 500ms for 100 workspaces
- `doctor --all`: < 2s for 100 workspaces × 10 docs each

### Edge Cases and Error Handling

**Cases to handle explicitly:**

1. **Concurrent init on same ticket:**
   - Risk: Two processes create same workspace simultaneously
   - Mitigation: Atomic directory creation; lock file in workspace; or accept last-write-wins

2. **Corrupted frontmatter:**
   - Risk: YAML parse error breaks commands
   - Current: Commands skip or error out
   - Improvement: `doctor` should report parse errors; add `fix` subcommand to attempt repairs

3. **Missing dependencies (git, rg):**
   - Risk: `relate --suggest` fails if git/rg not installed
   - Mitigation: Graceful degradation; check for tool presence and skip feature with warning

4. **Unicode in filenames/slugs:**
   - Risk: Non-ASCII chars in titles create invalid filenames
   - Mitigation: Slugify with unicode normalization and transliteration

5. **Very long ticket paths:**
   - Risk: Filesystem path limits (255 chars on most systems)
   - Mitigation: Truncate slug if necessary; warn user

6. **RelatedFiles with absolute vs relative paths:**
   - Risk: Paths break when repo moves
   - Mitigation: Enforce relative-to-repo-root; add `--absolute` flag if needed

### Migration from Current to RFC-Aligned Structure

**Scenario:** Existing workspaces in `docs/active/` need to move to `ttmp/`.

**Migration command specification:**
```
ttmp migrate --from docs/active --to ttmp [--dry-run] [--ticket <id>]
```

**Algorithm:**
1. Scan source directory for workspaces (dirs with `index.md`)
2. For each workspace:
   - Parse `index.md` frontmatter to get Ticket
   - Construct target path: `to/<Ticket>-<slug>/`
   - Check target doesn't exist (or `--force`)
   - Create missing directories (`various/`, `tasks.md`, etc.) for RFC parity
   - Copy all files preserving timestamps
   - Update any internal references (relative paths in RelatedFiles?)
   - Validate target workspace with `doctor`
3. Generate report: migrated count, errors, warnings
4. If `--dry-run`, print report but don't write

**Implementation notes:**
- Use `os.Rename` if source and target on same filesystem (atomic)
- Fall back to copy+delete if cross-filesystem
- Consider backup: `cp -a source source.bak` before migration

---

This plan keeps momentum, reduces risk (idempotency first), and methodically brings the tool into alignment with the RFC while retaining useful extensions (`sources/`, `.meta/`). It is designed to be delivered in small, reviewable increments that can ship value continuously.

## References and Further Reading

- **RFC:** `vibes/2025-11-03/doc-manager/rfc.md` - Original specification (196 lines)
- **Analysis:** `vibes/ttmp/2025-11-03/01-analysis-of-the-initial-documentation-manager-implementation.md` - Detailed gap analysis
- **Debate:** `vibes/ttmp/2025-11-03/02-presidential-debate-documentation-manager.md` - Design tensions explored
- **Glazed docs:** `glazed/pkg/doc/tutorials/build-first-command.md` - Glazed command tutorial
- **Glazed layers:** `glazed/pkg/doc/topics/layers-guide.md` - Configuration layers guide
- **Test workspace:** `vibes/2025-11-03/doc-manager/test-workspace/docs/active/` - Sample data for testing

