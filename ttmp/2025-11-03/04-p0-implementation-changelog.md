---
Title: P0 Implementation Changelog - Documentation Manager
Ticket: MEN-000
Status: active
Topics:
- documentation
- llm-workflow
- process
DocType: log
Intent: long-term
Owners:
- manuel
RelatedFiles:
- vibes/2025-11-03/doc-manager/docmgr/pkg/commands/init.go
- vibes/2025-11-03/doc-manager/docmgr/pkg/commands/list.go
- vibes/2025-11-03/doc-manager/docmgr/pkg/commands/doctor.go
- vibes/2025-11-03/doc-manager/docmgr/pkg/commands/add.go
- vibes/2025-11-03/doc-manager/docmgr/pkg/commands/import_file.go
- vibes/ttmp/2025-11-03/03-sober-review-and-design-plan.md
LastUpdated: 2025-11-03
Summary: >
  Implementation log for P0 stabilization tasks: idempotent init, RFC scaffolding parity,
  default root path alignment, and doctor command expansion. Documents changes, problems
  encountered, lessons learned, and important notes for future development.
LastUpdated: 2025-11-03
---

# P0 Implementation Changelog

## Overview

This document tracks the implementation of the first three P0 tasks from the design plan:
1. Making `init` idempotent with `--force` flag
2. Adding missing RFC directories/files (`various/`, `tasks.md`, `changelog.md`, `archive/`)
3. Changing default root from `docs` to `ttmp/`

**Date:** 2025-11-03  
**Status:** Completed  
**Next Steps:** Expand `doctor` command with staleness and unique `index` checks

---

## Changes Made

### 1. Idempotent `init` Command

**Files Modified:**
- `docmgr/pkg/commands/init.go`

**Changes:**
- Added `Force bool` field to `InitSettings` struct
- Added `--force` parameter definition with default `false`
- Created `writeFileIfNotExists()` helper function that skips writing if file exists (unless `force=true`)
- Updated `writeDocumentWithFrontmatter()` to accept `force bool` parameter
  - When `force=false` and file exists, function returns early (preserves existing content)
  - When `force=true`, overwrites existing files
- Updated calls to `writeDocumentWithFrontmatter()` in:
  - `init.go` (uses `settings.Force`)
  - `add.go` (uses `false` - new documents shouldn't overwrite)
  - `import_file.go` (uses `true` - updating index.md should overwrite)

**Behavior:**
- Running `docmgr init MEN-XXXX --title "..."` twice without `--force` now preserves existing files
- Running with `--force` overwrites all scaffolded files
- Idempotent behavior applies to: `index.md`, `README.md`, `tasks.md`, `changelog.md`

### 2. RFC Scaffolding Parity

**Files Modified:**
- `docmgr/pkg/commands/init.go`

**Changes:**
- Added `various/` directory to directory scaffolding list
- Added `archive/` directory to directory scaffolding list (optional per RFC)
- Added `tasks.md` file creation with basic structure:
  ```markdown
  # Tasks
  
  ## TODO
  
  - [ ] Add tasks here
  ```
- Added `changelog.md` file creation with initial entry:
  ```markdown
  # Changelog
  
  ## YYYY-MM-DD
  
  - Initial workspace created
  ```
- Updated `README.md` content to document new directories:
  - Added `various/` description: "Scratch or meeting notes, working notes"
  - Added `archive/` description: "Optional space for deprecated or reference-only artifacts"

**Directory Structure Created:**
```
ttmp/MEN-XXXX-slug/
  index.md
  README.md
  tasks.md
  changelog.md
  design/
  reference/
  playbooks/
  scripts/
  sources/
  .meta/
  various/
  archive/
```

### 3. Default Root Path Change

**Files Modified:**
- `docmgr/pkg/commands/init.go` (default: `"docs"` → `"ttmp"`)
- `docmgr/pkg/commands/list.go` (default: `"docs"` → `"ttmp"`)
- `docmgr/pkg/commands/doctor.go` (default: `"docs"` → `"ttmp"`)
- `docmgr/pkg/commands/add.go` (default: `"docs"` → `"ttmp"`)
- `docmgr/pkg/commands/import_file.go` (default: `"docs"` → `"ttmp"`)

**Changes:**
- Changed `parameters.WithDefault("docs")` to `parameters.WithDefault("ttmp")` in all commands
- All commands now default to `ttmp/` directory
- `--root` flag still allows overriding for legacy `docs/active` layouts

**Note:** The path structure still uses `root/active/<TICKET>-<slug>/` which maintains compatibility with existing commands that expect the `active/` subdirectory. Future work may align this to RFC's direct `ttmp/<TICKET>-<slug>/` structure.

---

## Problems Encountered

### 1. Compilation Errors After Signature Change

**Problem:** After updating `writeDocumentWithFrontmatter()` to accept `force bool` parameter, compilation failed because:
- `add.go` and `import_file.go` still called the function with old signature (3 args instead of 4)

**Solution:** Updated all call sites:
- `add.go`: Added `false` as fourth parameter (new documents shouldn't overwrite)
- `import_file.go`: Added `true` as fourth parameter (updating index.md should overwrite)

**Lesson:** When changing function signatures, use `grep` to find all call sites across the codebase before committing changes.

### 2. Incorrect Frontmatter Parsing Logic

**Problem:** Initially tried to merge existing frontmatter fields when `force=false`, but this was complex and not aligned with idempotent behavior (preserve means don't touch).

**Solution:** Simplified to early return when file exists and `force=false`. This aligns with true idempotency: re-running the same command produces the same result without side effects.

**Lesson:** Keep idempotent operations simple - "preserve" means "don't change", not "merge intelligently". Complex merging logic belongs in separate commands like `meta update`.

### 3. Unused Import After Refactoring

**Problem:** Removed frontmatter parsing logic but left `github.com/adrg/frontmatter` import, causing compilation error.

**Solution:** Removed unused import after simplifying the idempotent logic.

**Lesson:** Go's compiler catches unused imports, but it's good practice to clean up imports immediately after refactoring.

### 4. Initial Complex Merge Logic

**Problem:** First attempt at preserving frontmatter tried to merge fields intelligently (preserve existing if new is empty, etc.). This was overly complex and didn't match the requirement.

**Solution:** Simplified to "file exists? skip writing" - true idempotency.

**Lesson:** Read requirements carefully - "preserve existing frontmatter" means "don't overwrite", not "merge fields". Design plan explicitly said "refuse to overwrite unless `--force`".

---

## Lessons Learned

### 1. Package-Level Function Visibility

- Functions in the same package (`pkg/commands/`) can call each other's unexported functions
- `writeDocumentWithFrontmatter()` is unexported but used by `add.go` and `import_file.go`
- This is fine for internal helpers, but consider exporting if function might be useful elsewhere

### 2. Idempotency Simplicity

- Idempotent operations should be simple: "exists? skip" or "exists? overwrite if force"
- Complex merging logic belongs in update commands, not init commands
- Users can always use `--force` if they want to overwrite

### 3. Consistent Defaults Across Commands

- All commands that accept `--root` should use the same default value
- Changed defaults in 5 files - need to be careful about consistency
- Consider extracting default to a shared constant or config file

### 4. Testing Strategy

- Didn't write tests yet, but should add:
  - Test idempotent behavior (run init twice, verify files unchanged)
  - Test `--force` behavior (run init twice with force, verify files overwritten)
  - Test directory scaffolding (verify all directories created)
  - Test file creation (verify all files created with correct content)

---

## Important Notes for Future Development

### 1. Path Structure Consistency

**Current State:** Commands use `root/active/<TICKET>-<slug>/` structure  
**RFC Expectation:** `ttmp/<TICKET>-<slug>/` directly (no `active/` subdirectory)

**Decision Needed:** 
- Keep `active/` subdirectory for backward compatibility?
- Or migrate to RFC-aligned structure and update all commands?

**Recommendation:** Keep `active/` for now (P0), add migration command in P1/P2 to support both layouts.

### 2. `writeDocumentWithFrontmatter()` Usage

**Current Usage:**
- `init.go`: Uses `settings.Force` (user-controlled)
- `add.go`: Uses `false` (new documents, shouldn't overwrite)
- `import_file.go`: Uses `true` (updating index.md, should overwrite)

**Consider:** The function is used for different purposes:
- Creating new documents (should check existence)
- Updating existing documents (should overwrite)

**Future Refactor:** Consider separating into:
- `writeDocumentWithFrontmatter()` - always writes (for updates)
- `createDocumentWithFrontmatter()` - checks existence first (for new docs)

### 3. Frontmatter Merging

**Current:** No merging logic - idempotent means "preserve if exists"  
**Future:** `meta update` command (P1) will need merging logic to update specific fields

**Note:** Keep merging logic out of `init` - it belongs in update commands.

### 4. Default Root Configuration

**Current:** Hardcoded defaults in each command  
**Future:** Consider:
- Config file (`.ttmp.yaml`) for workspace-level defaults
- Environment variable (`TTMP_ROOT`) for user-level defaults
- Command-line flag (`--root`) for override

**Recommendation:** Add config file support in P2, keep hardcoded defaults for now.

### 5. Directory Creation Order

**Current:** All directories created in single loop  
**Note:** `archive/` is marked as "optional" in RFC but currently always created

**Future Consideration:** Make `archive/` truly optional (only create if needed, or add flag)

### 6. File Content Templates

**Current:** Basic templates for `tasks.md` and `changelog.md`  
**Future:** Consider:
- Template files in `ttmp/_templates/` directory (P2)
- Customizable templates per doc type
- Variables in templates (e.g., `{{TICKET}}`, `{{DATE}}`)

---

## Next Steps

1. **Expand `doctor` command** (P0)
   - Add staleness check (LastUpdated > 14 days warns)
   - Add unique `index.md` check (ensure only one per workspace)
   - Add validation for required fields

2. **Vocabulary System** (P1)
   - Load `doc/vocabulary.yaml`
   - Validate Topics, DocType, Intent against vocabulary
   - Add `vocab` commands

3. **Testing** (P0/P1)
   - Unit tests for idempotent behavior
   - Integration tests for full workflow
   - Golden tests for file scaffolding

---

## Files Changed Summary

```
docmgr/pkg/commands/init.go
  - Added Force field to InitSettings
  - Added --force parameter
  - Added various/ and archive/ directories
  - Added tasks.md and changelog.md creation
  - Updated writeDocumentWithFrontmatter() signature
  - Added writeFileIfNotExists() helper
  - Changed default root from "docs" to "ttmp"

docmgr/pkg/commands/list.go
  - Changed default root from "docs" to "ttmp"

docmgr/pkg/commands/doctor.go
  - Changed default root from "docs" to "ttmp"

docmgr/pkg/commands/add.go
  - Changed default root from "docs" to "ttmp"
  - Updated writeDocumentWithFrontmatter() call to include force=false

docmgr/pkg/commands/import_file.go
  - Changed default root from "docs" to "ttmp"
  - Updated writeDocumentWithFrontmatter() call to include force=true

vibes/ttmp/2025-11-03/03-sober-review-and-design-plan.md
  - Checked off completed items 1-3 in checklist
```

---

## Testing Recommendations

Before moving to next phase, consider testing:

1. **Idempotency Test:**
   ```bash
   docmgr init MEN-TEST --title "Test" --topics test
   # Verify files created
   docmgr init MEN-TEST --title "Test" --topics test
   # Verify files unchanged (no errors, timestamps same)
   docmgr init MEN-TEST --title "Test" --topics test --force
   # Verify files overwritten (timestamps updated)
   ```

2. **Directory Structure Test:**
   ```bash
   docmgr init MEN-TEST --title "Test"
   # Verify all directories exist:
   # - design/, reference/, playbooks/, scripts/, sources/, .meta/, various/, archive/
   ```

3. **Default Root Test:**
   ```bash
   docmgr init MEN-TEST --title "Test"
   # Verify workspace created in ttmp/active/MEN-TEST-test/
   docmgr init MEN-TEST --title "Test" --root docs
   # Verify workspace created in docs/active/MEN-TEST-test/
   ```

---

## Update: Doctor Command Expansion (2025-11-03)

### 4. Expanded Doctor Command Checks

**Files Modified:**
- `docmgr/pkg/commands/doctor.go`

**Changes:**
- Added `time` import for staleness calculations
- Added `findIndexFiles()` helper function that recursively searches for all `index.md` files in a workspace
- Added unique `index.md` check:
  - Scans entire workspace directory tree for `index.md` files
  - Warns if more than one `index.md` found (per RFC, should be only one per workspace)
  - Reports count and paths of all found `index.md` files
- Added staleness check:
  - Calculates days since `LastUpdated` field
  - Warns if `LastUpdated` is more than 14 days old
  - Reports exact number of days and formatted date
- Improved issue tracking:
  - Added `hasIssues` flag to track all checks
  - Only reports "All checks passed" if truly no issues from any check (multiple index, staleness, or missing fields)

**New Checks:**
1. **Multiple Index Check:**
   - Issue type: `multiple_index`
   - Severity: `warning`
   - Reports: count of index files, list of paths

2. **Staleness Check:**
   - Issue type: `stale`
   - Severity: `warning`
   - Threshold: 14 days
   - Reports: days since update, formatted last updated date

**Behavior:**
- Checks run for all workspaces (or filtered by `--ticket`)
- Each check reports issues independently
- "All checks passed" only shown if no issues from any check
- All checks are warnings (not errors) - they don't prevent operations

**Example Output:**
```
ticket    issue           severity  message                                    path
MEN-1234  multiple_index  warning   Multiple index.md files found (2)          /path/to/workspace
MEN-1234  stale           warning   LastUpdated is 16 days old (threshold: 14) /path/to/index.md
MEN-5678  none            ok        All checks passed                          /path/to/workspace
```

### Problems Encountered (Doctor Update)

**Problem:** Initial implementation reported "All checks passed" even when staleness or multiple index issues existed, because the logic only checked field validation.

**Solution:** Added `hasIssues` flag that tracks all checks, and only report success if `!hasIssues`.

**Lesson:** When adding new checks, ensure they're integrated into the overall success/failure logic, not just independent warnings.

### Files Changed Summary (Doctor Update)

```
docmgr/pkg/commands/doctor.go
  - Added time import
  - Added findIndexFiles() helper function
  - Added multiple index.md check
  - Added staleness check (14-day threshold)
  - Improved issue tracking logic
```

---

## Updated Status

**Completed Tasks:**
1. ✅ Make `init` idempotent with `--force` flag
2. ✅ Scaffold RFC-aligned directories/files (`various/`, `tasks.md`, `changelog.md`, `archive/`)
3. ✅ Default root to `ttmp/` with `--root` override
4. ✅ Expand `doctor` with staleness and unique `index` checks

**Next Steps:**
- Add `relate` command with `--suggest` (P1)
- Add `meta update` command (P1)
- Add vocabulary validation to `init` and `add` commands (P1)

---

## Update: Removed Backwards Compatibility & Vocabulary System (2025-11-03)

### 5. Removed `active/` Subdirectory

**Files Modified:**
- `docmgr/pkg/commands/init.go`
- `docmgr/pkg/commands/list.go`
- `docmgr/pkg/commands/doctor.go`
- `docmgr/pkg/commands/import_file.go`

**Changes:**
- Removed `active/` subdirectory from path structure
- Changed from `root/active/<TICKET>-<slug>/` to `root/<TICKET>-<slug>/` (RFC-aligned)
- Updated all commands to scan root directory directly
- Updated `findTicketDirectory()` helper to use new structure

**Rationale:**
- Green field project - no need for backwards compatibility
- Matches RFC structure exactly: `ttmp/MEN-XXX-<slug>/`
- Simpler, cleaner path structure

### 6. Vocabulary System Implementation

**Files Created:**
- `docmgr/pkg/commands/vocabulary.go` - Vocabulary loader and saver functions
- `docmgr/pkg/commands/vocab_list.go` - List vocabulary entries command
- `docmgr/pkg/commands/vocab_add.go` - Add vocabulary entry command

**Files Modified:**
- `docmgr/pkg/models/document.go` - Updated Vocabulary struct to match RFC (removed SourceTypes, Lifecycle; added Intent)
- `docmgr/cmd/docmgr/main.go` - Added vocab parent command and subcommands

**Implementation:**

1. **Vocabulary Loader (`vocabulary.go`):**
   - `LoadVocabulary()` - Searches for `doc/vocabulary.yaml` walking up from current directory
   - `SaveVocabulary()` - Saves vocabulary to `doc/vocabulary.yaml`, creates `doc/` if needed
   - `FindVocabularyPath()` - Finds path to vocabulary file
   - Returns empty vocabulary if file not found (graceful degradation)

2. **Vocab List Command (`vocab_list.go`):**
   - Lists vocabulary entries from `doc/vocabulary.yaml`
   - Supports filtering by category: `topics`, `docTypes`, `intent`
   - Lists all categories if no filter specified
   - Output: category, slug, description

3. **Vocab Add Command (`vocab_add.go`):**
   - Adds new vocabulary entry to specified category
   - Validates slug doesn't already exist
   - Creates `doc/` directory if needed
   - Finds repo root by walking up directory tree (checks for `.git`, `go.mod`, or `doc/`)

**Usage Examples:**
```bash
# List all vocabulary entries
docmgr vocab list

# List only topics
docmgr vocab list --category topics

# Add a new topic
docmgr vocab add --category topics --slug observability --description "Logging and metrics"

# Add a new doc type
docmgr vocab add --category docTypes --slug working-note --description "Free-form notes"
```

**Vocabulary Structure:**
- Matches RFC structure: `topics`, `docTypes`, `intent`
- Each entry has `slug` and `description`
- Stored in `doc/vocabulary.yaml` at repo root

### Problems Encountered

**Problem:** Vocabulary model had extra fields (`SourceTypes`, `Lifecycle`) not in RFC.

**Solution:** Updated model to match RFC exactly: `Topics`, `DocTypes`, `Intent` only.

**Problem:** Unused import in `vocab_list.go` after removing models import.

**Solution:** Removed unused import - LoadVocabulary() is in same package so no import needed.

### Files Changed Summary

```
docmgr/pkg/commands/init.go
  - Removed "active" from path: root/<TICKET>-<slug>/ instead of root/active/<TICKET>-<slug>/

docmgr/pkg/commands/list.go
  - Scan root directory directly instead of root/active/
  - Updated help text

docmgr/pkg/commands/doctor.go
  - Scan root directory directly instead of root/active/

docmgr/pkg/commands/import_file.go
  - Updated findTicketDirectory() to scan root directly

docmgr/pkg/models/document.go
  - Updated Vocabulary struct: removed SourceTypes, Lifecycle; added Intent

docmgr/pkg/commands/vocabulary.go (NEW)
  - Vocabulary loader and saver functions

docmgr/pkg/commands/vocab_list.go (NEW)
  - List vocabulary entries command

docmgr/pkg/commands/vocab_add.go (NEW)
  - Add vocabulary entry command

docmgr/cmd/docmgr/main.go
  - Added vocab parent command and subcommands
```

---

## Updated Status

**Completed Tasks:**
1. ✅ Make `init` idempotent with `--force` flag
2. ✅ Scaffold RFC-aligned directories/files (`various/`, `tasks.md`, `changelog.md`, `archive/`)
3. ✅ Default root to `ttmp/` with `--root` override
4. ✅ Expand `doctor` with staleness and unique `index` checks
5. ✅ Remove backwards compatibility (`active/` subdirectory)
6. ✅ Implement vocabulary loader and `vocab list|add` commands

**Next Steps:**
- Add `relate` command with `--suggest` (P1)
- Add `meta update` command (P1)
- Add vocabulary validation to `init` and `add` commands (P1)

