# docmgr verbs — LLM-friendly output design (2025-11-04)

Purpose: classify all `docmgr` verbs by their output characteristics and propose which should be normal writer commands vs dual commands (human-readable + structured glaze output) to improve LLM consumption.

Guiding rules:
- Normal writer command: verbs that do not yield multi-row structured data; print concise human-readable text.
- Dual command: verbs that produce lists/multi-rows or benefit from a simplified text mode; keep the current structured glaze output and add a human-friendly textual mode. Implement using dual mode as in the glazed tutorial (toggle flag like `--with-glaze-output`).

## Inventory and recommendations

Below, each verb lists: what it does today, output characteristics, and the proposed type with a text-mode spec for dual commands.

### Top-level

- `init` (file: `pkg/commands/init.go`)
  - Summary: Scaffold a new ticket workspace and seed files.
  - Output: Single row with ticket, path, title, status.
  - Recommendation: Normal writer
  - Writer text: “Created workspace <TICKET> at <PATH> — title: <TITLE>.”

- `add` (file: `pkg/commands/add.go`)
  - Summary: Create a new document under a ticket (resolves subdir by `DocType`).
  - Output: Single row (ticket, doc_type, title, path, status).
  - Recommendation: Normal writer
  - Writer text: “Added <DOC_TYPE> ‘<TITLE>’ for <TICKET> → <PATH>.”

- `doctor` (file: `pkg/commands/doctor.go`)
  - Summary: Validate workspaces; emits per-issue rows and OK rows.
  - Output: Multi-row (one per issue or OK per ticket), optional fail-on.
  - Recommendation: Dual command
  - Text mode:
    - Print a short summary first: “Checked N tickets: W warnings, E errors; stale>={stale-after}: K.”
    - Then per-ticket lines only when issues exist: “<TICKET> [warning] unknown_topics: a,b” etc. Hide “All checks passed” lines unless `--verbose`.

- `status` (file: `pkg/commands/status.go`)
  - Summary: Summarize tickets, staleness, and per-ticket counts; has `--summary-only`.
  - Output: Multi-row (per ticket) + one summary row.
  - Recommendation: Dual command
  - Text mode:
    - Always print summary: “root=<ROOT> tickets=<N> stale=<K> docs=<M> (design D / reference R / playbooks P).”
    - If not `--summary-only`, add per-ticket one-liners: “<TICKET> ‘<TITLE>’ status=<STATUS> stale=<true|false> docs=<count>.”

- `search` (file: `pkg/commands/search.go`)
  - Summary: Search content and metadata; `--files` suggests related files.
  - Output: Multi-row (results or suggested files with source+reason).
  - Recommendation: Dual command
  - Text mode:
    - For document results: “<path> — <title> [<ticket>] :: <snippet>”
    - For `--files`: “<file> — <reason> (source=<source>)”.

- `guidelines` (file: `pkg/commands/guidelines_cmd.go`)
  - Summary: List available guideline types or return the selected guideline content (as `content`).
  - Output: Either multi-row (types) or single row with full content.
  - Recommendation: Dual command
  - Text mode:
    - With `--list`: print each type on its own line.
    - With `--doc-type`: print the guideline markdown content as-is (no wrapping in structured fields).

- `relate` (file: `pkg/commands/relate.go`)
  - Summary: Manage `RelatedFiles` on a document or ticket index; can suggest and/or apply suggestions.
  - Output: Multi-row when only suggesting; single-row summary when applying.
  - Recommendation: Dual command
  - Text mode:
    - Suggest-only: list “<file> — <reason(s)>”.
    - Apply/update: “Updated <DOC|TICKET-INDEX>: +<added> −<removed> total=<total>.”

- `import file` (file: `pkg/commands/import_file.go`)
  - Summary: Copy a local file under `sources/local/` and record metadata.
  - Output: Single row (ticket, source_file, destination, type, status).
  - Recommendation: Normal writer
  - Writer text: “Imported <SOURCE> → <DEST> (ticket=<TICKET>, type=local).”

- `meta update` (file: `pkg/commands/meta_update.go`)
  - Summary: Update frontmatter field across one or many docs.
  - Output: Multi-row (one per updated or errored file).
  - Recommendation: Dual command
  - Text mode:
    - Print overall counts: “Updated <FIELD>=<VALUE> on N files; errors=E.”
    - Then list error lines if any: “ERROR <path>: <message>”.

- `changelog update` (file: `pkg/commands/changelog.go`, subcommand `update`)
  - Summary: Append a dated entry; can suggest files and optionally apply them.
  - Output: Multi-row when only suggesting; single summary row when written.
  - Recommendation: Dual command
  - Text mode:
    - Suggest-only: “<file> — <reason(s)> [note: <note(s)>].”
    - After append: “Updated changelog <FILE> on <DATE>, files=<count>.”

- `search` (already above, kept for completeness) → Dual

### `list` group

- `list tickets` (file: `pkg/commands/list_tickets.go`)
  - Summary: Enumerate ticket workspaces with filters.
  - Output: Multi-row.
  - Recommendation: Dual command
  - Text mode: “<ticket> ‘<title>’ status=<status> topics=<t1,t2> updated=<YYYY-MM-DD> path=<path>”.

- `list docs` (file: `pkg/commands/list_docs.go`)
  - Summary: Enumerate documents across tickets with filters.
  - Output: Multi-row.
  - Recommendation: Dual command
  - Text mode: “<ticket> <doc_type> ‘<title>’ status=<status> topics=<...> updated=<date> path=<relPath>”.

- Legacy/unused: `pkg/commands/list.go` defines a `list` that overlaps “tickets”. It is not registered in `cmd/docmgr/main.go`. Suggest either removing or wiring as an alias to `list tickets` (no functional change required in this pass).

### `tasks` group

- `tasks list` (file: `pkg/commands/tasks.go`)
  - Summary: Parse and list checkbox tasks from `tasks.md`.
  - Output: Multi-row.
  - Recommendation: Dual command
  - Text mode: “[#<index>] [x| ] <text> (file=<path>)”.

- `tasks add`
  - Output: Single row (file, status).
  - Recommendation: Normal writer
  - Writer text: “Added task to <FILE>.”

- `tasks check` / `tasks uncheck`
  - Output: Single row (file, status, id).
  - Recommendation: Normal writer
  - Writer text: “Marked task #<ID> as <checked|unchecked> in <FILE>.”

- `tasks edit`
  - Output: Single row (file, status, id).
  - Recommendation: Normal writer
  - Writer text: “Edited task #<ID> in <FILE>.”

- `tasks remove`
  - Output: Single row (file, status, id).
  - Recommendation: Normal writer
  - Writer text: “Removed task #<ID> from <FILE>.”

### `vocab` group

- `vocab list` (file: `pkg/commands/vocab_list.go`)
  - Summary: Print vocabulary entries (topics/docTypes/intent).
  - Output: Multi-row.
  - Recommendation: Dual command
  - Text mode: “<category>: <slug> — <description>”.

- `vocab add` (file: `pkg/commands/vocab_add.go`)
  - Output: Single row (category, slug, description, status).
  - Recommendation: Normal writer
  - Writer text: “Added <category> ‘<slug>’: <description>.”

## Implementation notes

- Dual-mode wiring: switch selected commands to dual mode toggle per glazed tutorial.
  - Build: `cli.BuildCobraCommand(<cmd>, cli.WithDualMode(true), cli.WithGlazeToggleFlag("with-glaze-output"), cli.WithParserConfig(...))`
  - Human mode implements `cmds.BareCommand.Run(ctx, parsedLayers)` and prints the text-mode described above.
  - Glaze mode keeps current `RunIntoGlazeProcessor` unchanged.
- Normal writer commands: implement only `cmds.BareCommand` and print concise messages; where existing code returns a single row today, port to text printing and, if helpful, also add a one-line row emission behind a flag for scripts (optional).
- Keep existing parameter parsing via `parsedLayers.InitializeStruct(layers.DefaultSlug, &Settings{})` in both modes.
- Do not remove existing structured outputs; dual-mode preserves machine-readable behavior and adds human-friendly text for LLM prompts and terminal use.

## Quick mapping

- Normal writer: `init`, `add`, `import file`, `tasks add|check|uncheck|edit|remove`, `vocab add`.
- Dual commands: `doctor`, `status`, `search`, `guidelines`, `relate`, `meta update`, `changelog update`, `list tickets`, `list docs`, `tasks list`, `vocab list`.
- Note: `pkg/commands/list.go` appears unused; consider aliasing or removing to avoid confusion.

## Examples (text mode sketches)

- doctor: “Checked 12 tickets: warnings=5 errors=1 stale>=14d=3.” followed by only problematic tickets.
- status: “root=ttmp tickets=12 stale=3 docs=84 (design 22 / reference 40 / playbooks 22).”
- search (docs): “MEN-4242/design/path-normalization.md — Path normalization strategy [MEN-4242] :: …snippet…”
- guidelines --doc-type design-doc: print the guideline markdown content directly.
- relate (suggest): “backend/chat/ws/manager.go — recent commit activity; content match: WebSocket; note: 
  observed in doc X”.

This plan keeps structured outputs intact where they are valuable for automation and adds human-friendly printouts where it improves LLM promptability and terminal UX.


