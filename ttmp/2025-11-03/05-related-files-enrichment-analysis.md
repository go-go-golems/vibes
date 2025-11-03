---
Title: Enriching RelatedFiles with per-file rationale (analysis and impact)
DocType: design-doc
Intent: long-term
Topics:
- docmgr
- architecture
Owners:
- manuel
Summary: >
  Introduce structured RelatedFiles entries that include a path and a human-readable
  rationale (“why this file matters”), while remaining backward compatible with the
  current string list format.
LastUpdated: 2025-11-03
---

# Goal

Enhance the `RelatedFiles` metadata so each entry carries a short rationale explaining why the file is related (for tickets and individual docs). This makes `index.md` and other docs richer and immediately useful to readers and LLMs.

Requirements:
- Backward compatible read of existing `RelatedFiles: [string, string, ...]`.
- Prefer writing structured entries going forward: a list of objects with `Path` and `Note`.
- Use suggestion reasons (from `search --files` and `relate --suggest`) as default notes when applying suggestions.

# Proposed Schema

Current (legacy):
```yaml
RelatedFiles:
- backend/chat/api/register.go
- web/src/store/api/chatApi.ts
```

New (preferred going forward):
```yaml
RelatedFiles:
- Path: backend/chat/api/register.go
  Note: Handles REST route registration; source of path normalization
- Path: web/src/store/api/chatApi.ts
  Note: Frontend integration point; aligns with backend path changes
```

Implementation detail: Represent `RelatedFiles` in code as a dedicated type with custom YAML marshal/unmarshal that accepts both sequences of scalars (legacy) and sequences of maps (new). Always marshal as sequence of maps.

```go
type RelatedFile struct {
    Path string `yaml:"Path" json:"path"`
    Note string `yaml:"Note,omitempty" json:"note,omitempty"`
}

type RelatedFiles []RelatedFile
```

# Impact Analysis (code and commands)

Data model
- `vibes/2025-11-03/doc-manager/docmgr/pkg/models/document.go`
  - Change `RelatedFiles []string` → `RelatedFiles RelatedFiles` (new type)
  - Add custom YAML marshal/unmarshal for `RelatedFiles` to support legacy lists

Frontmatter read/write helpers
- `pkg/commands/init.go` / `pkg/commands/add.go`
  - Initialize `RelatedFiles` to empty `RelatedFiles{}`
  - No other change needed for writing; YAML encoder will emit structured entries
- `pkg/commands/import_file.go`
  - Unchanged (doesn’t touch `RelatedFiles`)
- `writeDocumentWithFrontmatter` / `readDocumentFrontmatter` (in `init.go`)
  - Unchanged; rely on model-level YAML logic

Relate command (CRUD + suggestions)
- `pkg/commands/relate.go`
  - Update all read/write sites:
    - Iteration over `doc.RelatedFiles` → use `rf.Path`
    - De-duplication by path key
    - When `--apply-suggestions` is used, persist suggestion `reason` into `Note`
  - CLI flags (design proposal):
    - `--files path[,path...]` (as today, creates entries with empty `Note`)
    - `--file path --note "why"` (repeatable; adds/updates one with note)
    - `--remove-files path[,path...]` (as today)
    - Optional: `--from-csv path,note` in `--files` (pipe-delimited or JSON) later

Search command (reverse lookup + suggestions)
- `pkg/commands/search.go`
  - Reverse lookup filters (`--file`, `--dir`): adapt string comparisons to `rf.Path`
  - Suggestions from existing docs:
    - When surfacing “referenced by documents”, include `rf.Note` if present in the `reason` column (e.g., `referenced by documents; note: ...`)
  - Ripgrep/git heuristics untouched

Doctor command (validation)
- `pkg/commands/doctor.go`
  - Existence check: iterate `rf.Path`
  - Consider (optional, not required): warn on empty `Note` if a policy is enabled in config
  - `.docmgrignore` remains applied as today

Meta update command
- `pkg/commands/meta_update.go`
  - Today: `--field RelatedFiles --value path1,path2`
  - Keep backward-compatible setter: converts to structured with empty `Note`
  - For note edits, prefer `relate` sub-operations; consider future `--field RelatedFilesNote --value path|note`

Status command
- `pkg/commands/status.go`
  - Unaffected (doesn’t inspect `RelatedFiles` details)

Templates and Guidelines
- `pkg/commands/templates.go`
  - Keep `RelatedFiles: []` as default; optional example entries could be objects
- `pkg/commands/guidelines.go`
  - Update guidance to recommend short, meaningful notes for each related file

Documentation and Help
- `pkg/doc/docmgr-cli-guide.md`
  - Update `RelatedFiles` examples to show structured entries; document note usage
- `pkg/doc/how-to-setup.md` / `how-to-use.md`
  - Mention enriched `RelatedFiles` and that suggestions can fill `Note`
- Scenario scripts
  - Optional: demonstrate `relate --file ... --note ...` and `--apply-suggestions` populating notes

# Backward Compatibility Strategy

- Reading: support both `[]string` and `[]{Path, Note}` via custom YAML unmarshal on `RelatedFiles`.
- Writing: always serialize as objects; legacy readers will still parse `Path` strings but older tooling expecting scalars may need migration (acceptable per requirement).
- Doctor/search/relate/meta-update remain functional with legacy docs due to unmarshal bridge.

# Migration (optional follow-up)

Add an opt-in migration command later:
```bash
docmgr migrate related-files --root ttmp [--ticket MEN-1234] [--dry-run]
```
Transforms scalar lists into structured objects with empty `Note` (or infers notes from existing suggestion history if available).

# Implementation Plan (high-level)

1) Model and YAML bridge
   - Add `RelatedFile` and `RelatedFiles` types with YAML (un)marshal
   - Change `Document.RelatedFiles` type
2) Update commands to consume new type
   - `doctor.go`, `search.go`, `relate.go`, `meta_update.go`, `init.go`, `add.go`
   - Ensure no stray string assumptions remain
3) Extend relate CLI
   - Add `--file` (repeatable) + `--note` or accept `path|note` in `--files`
   - On `--apply-suggestions`, copy suggestion reason into `Note`
4) Surface notes where useful
   - Search suggestions: append `note: ...` to `reason` when present
5) Docs and examples
   - Update CLI guide and tutorials with structured examples and new flags
6) Tests
   - Golden frontmatter write, legacy read compatibility, reverse lookup, relate add/remove with notes, suggestion note propagation

# Risk and Mitigation

- Serialization changes: Ensure YAML bridge is robust (unit tests with both formats).
- UX for notes: Start minimal (`--file` + `--note`) and iterate; avoid overloading `--files` CSV parsing initially.
- Presenters: Ensure no presenter assumes `RelatedFiles` is a string list (audit table outputs).

# Files to Touch (summary)

- Models:
  - `pkg/models/document.go`
- Commands:
  - `pkg/commands/relate.go`
  - `pkg/commands/search.go`
  - `pkg/commands/doctor.go`
  - `pkg/commands/meta_update.go`
  - `pkg/commands/init.go` / `add.go`
  - `pkg/commands/templates.go` (optional example)
- Docs:
  - `pkg/doc/docmgr-cli-guide.md`, `pkg/doc/how-to-use.md`, `pkg/doc/how-to-setup.md`
- Scenario:
  - `glazed/ttmp/...` scripts to demonstrate note population via `relate`


