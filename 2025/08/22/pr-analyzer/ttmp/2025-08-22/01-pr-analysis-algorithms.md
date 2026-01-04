## PR Analyzer Algorithms: Overview and Implementation

This document explains how the PR analyzer computes its results and where the logic lives in the codebase.

### 1) Purpose and scope
- Provide a structured view into a PR’s makeup by language and by subsystem (categories).
- Compute cross-subsystem ("cross-system") signals to spot breadth of changes.
- Support analyzing either a base vs. head branch range or a single merge commit, and aggregate commit-level diffs into PR-level metrics.

### 2) What’s implemented where
- `internal/git/repository.go`:
  - Resolves branches/commits and enumerates commits in the PR range or a merge commit’s second-parent lineage.
  - Walks commit logs to compute the set difference between two commit graphs (head minus base).
  - Repository root discovery: opens the requested path; if it fails, walks parent directories to find `.git` or a bare repo.
- `internal/git/diff.go`:
  - Builds per-commit diffs using go-git trees and object patches.
  - Counts file-level added/deleted lines and tracks file state (new, deleted, renamed).
- `internal/analysis/analyzer.go`:
  - Orchestrates the overall PR analysis flow: gather commits, compute diffs, detect languages, categorize files, aggregate totals.
  - Derives language stats and cross-system stats.
- `internal/analysis/language.go`:
  - Maps file extensions to languages and handles a few special-case filenames.
- `internal/analysis/categories.go`:
  - Glob-based matching for categorizing files into systems; supports exclude patterns and directory-style globs (e.g., `foo/**`).
- `internal/analysis/aggregate.go`:
  - Utilities for aggregating multiple PR results (trends and distributions).

### 3) Core algorithms

#### A. Commit selection
- Branch range: resolve `base` and `head` revisions via go-git, obtain commit objects, then compute the commits reachable from `head` but not from `base`.
- Merge commit: validate the commit has at least two parents; take first parent (main/base) and second parent (feature branch), then compute commits reachable from second parent but not from first.
- Merge-from-main filter: excludes commits that look like merges from main/master by scanning commit messages for common merge phrases.

Reference: `Repository.getCommitsNotInBase` and `GetCommitsFromMerge`.

#### B. Per-commit diff parsing
- For each commit, compare its tree to the parent tree (or to an empty tree for root commits) to obtain an `object.Patch`.
- For each file patch:
  - Identify path and state (new/deleted/renamed).
  - Iterate patch chunks and count lines starting with `+` (added) and `-` (deleted). Lines without prefixes are context and ignored.
- Aggregate totals per commit and collect a `[]FileDiff`.

Reference: `Repository.GetCommitDiff` in `internal/git/diff.go`.

#### C. Language detection
- Use file extension mapping to classify files (e.g., `.go` -> Go, `.ts` -> TypeScript).
- Handle special names like `Dockerfile`, `Makefile`, `README*`.
- Unknown extensions fall back to `Other`.

Reference: `LanguageDetector.DetectLanguage`.

#### D. Categorization and excludes
- Categories are user-provided or default mappings of category name -> glob patterns.
- A file may belong to multiple categories; if none match, it becomes `uncategorized`.
- Excludes are checked first; excluded files produce no categories.
- Directory-style globs like `frontend/**` match nested paths; simple `filepath.Match` is also used for exact patterns.

Reference: `CategoryMatcher.CategorizeFile`, `matchesPath`, `AddExcludePattern`, `GetDefaultCategories`.

#### E. Aggregation to PR-level stats
- Language stats: for each language, sum `FilesChanged`, `LinesAdded`, `LinesDeleted`, compute `LinesModified` and percentage share over total modified lines, then sort by percentage.
- Cross-system stats:
  - For each commit, build the unique set of systems touched (excluding `uncategorized`).
  - Count commits touching 0-1 systems vs. multiple systems to compute `CrossSystemRate`.
  - Build a co-occurrence matrix and a list of most touched systems.
- PR info totals: `TotalFiles` is unique file count across diffs; `TotalLines` is sum of added+deleted lines; `TotalCommits` is number of commits considered.

Reference: `Analyzer.calculateLanguageStats`, `Analyzer.calculateCrossSystemStats`, and aggregation of totals in `analyzeCommits`.

### 4) Operational details
- Logging: zerolog is integrated; enable with `--log-level` (trace|debug|info|warn|error). Debug logs trace repository opening, commit selection, per-commit diff sizes, and completion summaries.
- Repository path handling:
  - `--repo-path`/`--repo` to specify a path explicitly.
  - If the path does not directly open as a git repo, parent traversal will locate the repo root.
- Output formats: table, json, yaml.
- Categories can be defaulted (`--use-defaults`) or provided via `--categories` string.

### 5) Key files to read
- `cmd/analyze.go` – CLI flow for analysis
- `internal/git/repository.go` – commit graph selection and repo discovery
- `internal/git/diff.go` – per-commit diff computation
- `internal/analysis/analyzer.go` – orchestration and aggregation
- `internal/analysis/language.go`, `internal/analysis/categories.go` – classification logic

### 6) Next steps
- [ ] Add unit tests for repo discovery across nested paths and bare repos.
- [ ] Add optional path-based language overrides (e.g., `**/*.proto` -> Proto).
- [ ] Expose category definitions via config file and merge with CLI inputs.
- [ ] Emit per-category file/line counts to enrich cross-system views.

Save future research in `ttmp/2025-08-22/0X-XXX.md`.
