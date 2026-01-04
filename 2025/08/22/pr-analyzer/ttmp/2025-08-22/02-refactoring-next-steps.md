## Refactoring Plan: DB CLI modularization, comprehensive filters, and developer handoff

This document gives context, design decisions, and precise next steps to finish the database-backed analysis and querying features. It is intended for a developer new to this repository to pick up and continue.

### 1) Purpose and scope
- Provide a clear path to reorganize the DB-related CLI commands for maintainability.
- Expose comprehensive, reusable DB filter flags across commands.
- Ensure a coherent UX for storing and querying per-PR (or per-commit) analyses.

### 2) Current state (as of 2025-08-22)
- Analysis core:
  - `--commit` supports both merge and non-merge commits via `Analyzer.AnalyzeCommit`.
  - Branch-range analysis remains available via `--pr-branch`/`--base-branch`.
  - Cross-system statistics now include `uncategorized`.
  - Categories use doublestar globbing with `internal/analysis/categories.go`.
- Logging: Zerolog wired; `--log-level` flag implemented.
- Repository discovery: walks parent directories to find `.git` or bare repo.
- Output: table, JSON, YAML across analysis and stats.
- SQLite persistence:
  - Pure-Go driver `modernc.org/sqlite`.
  - Schema: tables `prs`, `languages`, `system_touch`, `system_matrix` (see `internal/db/sqlite.go`).
  - Insert path: `--save-to-db` in `analyze` writes a record (one per analyzed commit/PR) with per-language and per-system stats.
  - Aggregates: `db languages`, `db systems` provide overview across stored analyses.
  - Listing: `db prs` lists stored analyses; `db summary` summarizes counts.
- Filters:
  - Storage supports rich filtering through `internal/db/sqlite.go` (since/until, author, committer, repo substring, has-merge/no-merge, language, system, min/max files/lines, order, desc, limit, offset).
  - Reusable flag helper created at `pkg/dbfilters/filters.go` with `AddFlags()` and `FromCmdFlags()`.
  - CLI currently wires only a subset (since, author) for `db prs`/`db summary` to keep builds green.

### 3) Final desired state
- Modular CLI structure for DB commands: one file per command for clarity.
- All DB subcommands (`db prs`, `db summary`, `db languages`, `db systems`) expose a consistent set of filter flags (as applicable) using `pkg/dbfilters` helpers.
- Clean separation of concerns:
  - Root/cobra setup minimal.
  - Query logic remains in `internal/db/sqlite.go`.
  - Flag parsing is centralized.
- Updated README with the new flags and examples.

### 4) Detailed next steps

#### A. Reorganize DB CLI commands
- Goal: move each DB subcommand into its own source file, still in package `cmd` to avoid package/import cycles.
- Recommended file layout (package name remains `cmd`):
  - `cmd/db_init.go` – defines `db init`
  - `cmd/db_languages.go` – defines `db languages`
  - `cmd/db_systems.go` – defines `db systems`
  - `cmd/db_prs.go` – defines `db prs`
  - `cmd/db_summary.go` – defines `db summary`
- Each file:
  - Declares an `init()` that attaches its command to the shared parent `dbCmd`.
  - To avoid circular references, declare a `var dbCmd *cobra.Command` in a small `cmd/db_root.go` (package `cmd`) that initializes the parent group and is referenced by subcommand files.
  - Remove the inlined DB command definitions from `cmd/root.go` once the new files are in place.

#### B. Centralize and expose comprehensive filters
- Use `pkg/dbfilters` in DB commands:
  - Call `dbfilters.AddFlags(cmd)` in each DB subcommand that supports filtering.
  - Parse with `f, err := dbfilters.FromCmdFlags(cmd)` and convert to `internal/db.Filters` (type alias already set up).
- Apply filters to commands:
  - `db prs`: all filters make sense (time, author, committer, repo, has-merge/no-merge, language, system, size bounds, order, desc, limit, offset).
  - `db summary`: same filter set as `db prs` (but returns summary counts).
  - `db languages` and `db systems`: optionally support time/author/committer/repo/has-merge filters before aggregates. Two options:
    1) Keep simple (no filters) for performance and clarity, or
    2) Add filtered variants that first select PR IDs matching filters and aggregate over those (requires new functions in `internal/db` to aggregate by a filtered PR set). If you choose (2), add helper queries: `AggregateLanguagesFiltered(ctx, f Filters)` and `AggregateSystemsFiltered(ctx, f Filters)` using `WHERE pr_id IN (SELECT id FROM prs WHERE …)`.

#### C. Update `internal/db/sqlite.go` if adopting filtered aggregates
- Add functions:
  - `AggregateLanguagesFiltered(ctx context.Context, f Filters) ([]… , error)`
  - `AggregateSystemsFiltered(ctx context.Context, f Filters) ([]… , error)`
- Implementation pattern:
  - Reuse the `Filters` SQL fragment/args construction used in `ListPRs`/`Summary` to build a `SELECT id FROM prs WHERE …` subquery.
  - Join/subselect in aggregates to restrict to matching PRs.

#### D. Wire flags into commands
- `db prs`:
  - Use `dbfilters.AddFlags(cmd)` and `FromCmdFlags`.
  - Call `store.ListPRs(ctx, f)` and render results.
- `db summary`:
  - Same as above but call `store.Summary(ctx, f)`.
- If implementing filtered aggregates:
  - `db languages`: `store.AggregateLanguagesFiltered(ctx, f)`.
  - `db systems`: `store.AggregateSystemsFiltered(ctx, f)`.

#### E. README and help text
- Add a “Filtering” section with examples:
  - Since/until windows (`--since`, `--until` RFC3339) with author/email filters.
  - Merge-only or non-merge-only queries (`--has-merge`/`--no-merge`).
  - Language/system constrained queries.
  - Size/scope filters (`--min-files/--max-files`, `--min-lines/--max-lines`).
  - Sorting and pagination (`--order-by analyzed_at|files|lines`, `--desc`, `--limit`, `--offset`).

### 5) Context and background
- The tool analyzes Git changes to show language composition and cross-system coupling per PR or commit.
- Storing per-analysis results enables longitudinal insights (e.g., how often PRs cross subsystem boundaries, which languages are frequently co-touched).
- The DB schema is normalized enough for simple aggregates; it intentionally avoids storing per-file rows to keep the DB size manageable.
- We use `modernc.org/sqlite` for easy distribution (no CGO).
- Reserved words caveat: we renamed `commit` column to `commit_hash` to avoid SQL parsing issues.

### 6) Acceptance criteria (final state)
- Codebase organization:
  - DB commands live in dedicated `cmd/db_*.go` files (or under a single `cmd/db_root.go` + subcommand files), package `cmd`.
  - Root command is clean; only attaches the DB group.
- CLI UX:
  - `pr-analyzer db prs` exposes all filters from `pkg/dbfilters` and renders results.
  - `pr-analyzer db summary` exposes all filters and renders counts.
  - Optional: `db languages` and `db systems` gain filtered variants if implemented.
- Documentation:
  - README updated with filter flags and examples.
- Build & tests:
  - `go build ./...` and `go test ./...` green.

### 7) Implementation checklist (suggested)
- [ ] Create `cmd/db_root.go` (package `cmd`) with `var dbCmd *cobra.Command` and attach to `rootCmd`.
- [ ] Create `cmd/db_init.go` and move `db init` there.
- [ ] Create `cmd/db_prs.go` and implement list with `dbfilters.AddFlags/FromCmdFlags` → `store.ListPRs`.
- [ ] Create `cmd/db_summary.go` and implement summary with `dbfilters` → `store.Summary`.
- [ ] Decide on filtered aggregates; if yes:
  - [ ] Add `AggregateLanguagesFiltered` and `AggregateSystemsFiltered`.
  - [ ] Create `cmd/db_languages.go` and `cmd/db_systems.go` to use filtered variants.
- [ ] Remove inlined DB commands from `cmd/root.go`.
- [ ] Update README with all filter flags and examples.
- [ ] Sanity test: insert a few analyses; run filtered queries; validate outputs.

### 8) Notes on testing
- Unit tests:
  - `pkg/dbfilters`: flag parsing edge cases (conflicting `--has-merge/--no-merge`, invalid time formats).
  - `internal/db`: query builders for `ListPRs`/`Summary` and any filtered aggregates (use temp sqlite files).
- Integration tests:
  - Run `analyze --commit … --save-to-db` on the test repo and assert DB rows.
  - Query with various filters and assert row counts and ordering.

### 9) Risks & mitigations
- Schema drift: If any column naming changes, add migrations or document destructive re-init.
- Performance: For very large datasets, add indices as needed (e.g., on `analyzed_at`, `total_files`, `total_lines`).
- Flag sprawl: Keep shared filters centralized (`pkg/dbfilters`) to avoid duplication.

### 10) References
- Analysis flow and algorithms: see `ttmp/2025-08-22/01-pr-analysis-algorithms.md`.
- Storage code: `internal/db/sqlite.go`.
- Filter helper: `pkg/dbfilters/filters.go`.
- CLI: `cmd/analyze.go`, `cmd/root.go` (to be refactored as described).

---

Save any further design notes as `ttmp/2025-08-22/0X-XXX.md` (e.g., `03-db-filtered-aggregates.md`) as you proceed.
