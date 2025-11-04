## Plan: Split `docmgr` into its own repository, seeded from `wesen-go-template`

### 1) Scope and goals
- Create new repo `go-go-golems/docmgr` on GitHub from template `wesen/wesen-go-template`.
- Extract historical commits for `docmgr` from the `vibes` repo (path: `2025-11-03/doc-manager/docmgr`).
- Rebase the extracted history on top of the template so the template commits form the base.
- Keep the original `vibes` repository intact; no destructive changes.
- Before merging the `docmgr` history into the new repo, analyze the template and write guidelines on how to adapt `docmgr` to the template structure.

References: template `wesen/wesen-go-template` (https://github.com/wesen/wesen-go-template)

### 2) Assumptions
- `vibes` Git repo path: `/home/manuel/workspaces/2025-11-03/documentation-manager/vibes`.
- `docmgr` history lives under: `2025-11-03/doc-manager/docmgr` (relative to `vibes` root).
- New local clone of the template-backed repo will be created at: `/home/manuel/workspaces/2025-11-03/documentation-manager/docmgr`.
- GitHub org/repo: `go-go-golems/docmgr`. Visibility: private (can flip to public later).
- `gh` CLI is installed and authenticated.

### 3) High-level steps
1. Safety: create a backup branch in `vibes`; ensure clean working tree.
2. Split history: create a branch in `vibes` containing only `docmgr` with proper file root using `git subtree split`.
3. Create new repo from template under `go-go-golems` and clone it locally via `gh repo create --template`.
4. Analyze the template and write a guideline doc at `vibes/ttmp/2025-11-04/02-how-to-modify-the-templated-repository-to-include-docmgr.md`.
5. Import the split history into the new repo and rebase the whole `docmgr` history on top of the template `main`.
6. Update module path to `github.com/go-go-golems/docmgr` and fix imports if needed.
7. `go mod tidy` and `go build ./...` to verify.
8. Push `main` to GitHub and validate.
9. Later: remove the old `docmgr` directory from `vibes` in a separate commit/PR.

### 4) Detailed commands

#### 4.1 Safety and prep (non-destructive)
```bash
cd /home/manuel/workspaces/2025-11-03/documentation-manager/vibes && \
  git status && \
  (git switch -c backup/pre-docmgr-split || git switch backup/pre-docmgr-split) && \
  git rev-parse --short HEAD
```

#### 4.2 Extract `docmgr` history from `vibes`
```bash
cd /home/manuel/workspaces/2025-11-03/documentation-manager/vibes && \
  git subtree split --prefix=2025-11-03/doc-manager/docmgr -b docmgr-split
```

This creates a new local branch `docmgr-split` whose root is the `docmgr` directory.

#### 4.3 Create new GitHub repo from template and clone locally
```bash
cd /home/manuel/workspaces/2025-11-03/documentation-manager && \
  gh repo create go-go-golems/docmgr \
    --template wesen/wesen-go-template \
    --private \
    --clone \
    --confirm
```

Local clone expected at: `/home/manuel/workspaces/2025-11-03/documentation-manager/docmgr`.

#### 4.4 Analyze template and write guidelines (before merging code)
- Inspect: `Makefile`, `.golangci.yml`, `.goreleaser.yaml`, `lefthook.yml`, `.github/workflows/`, `cmd/XXX/`, `pkg/`.
- Write: `/home/manuel/workspaces/2025-11-03/documentation-manager/vibes/ttmp/2025-11-04/02-how-to-modify-the-templated-repository-to-include-docmgr.md`.
- Content:
  - Target layout under template conventions (`cmd/docmgr`, `pkg/...`), module path `github.com/go-go-golems/docmgr`.
  - How to integrate CI, linting, hooks, and releases.
  - Merge strategy where template scaffolding overlaps with existing `docmgr` files.
  - Step-by-step checklist to refactor if needed.

#### 4.5 Import split history and rebase on top of template
```bash
cd /home/manuel/workspaces/2025-11-03/documentation-manager/docmgr && \
  git remote add monorepo /home/manuel/workspaces/2025-11-03/documentation-manager/vibes && \
  git fetch monorepo docmgr-split:docmgr-history && \
  git checkout docmgr-history && \
  git rebase --onto main --root && \
  git checkout main && \
  git merge --ff-only docmgr-history
```

If conflicts occur during rebase:
```bash
# Resolve conflicts, favoring docmgr app sources while keeping template scaffolding where additive.
git add -A && git rebase --continue
```

Fallback (if rebase is too conflict-heavy):
```bash
git rebase --abort || true
git checkout -b integrate-docmgr && \
  git merge --no-ff docmgr-history --allow-unrelated-histories
# Resolve conflicts, then
git add -A && git commit
```

#### 4.6 Update module path and imports; tidy and build
```bash
cd /home/manuel/workspaces/2025-11-03/documentation-manager/docmgr && \
  sed -i 's|^module .*|module github.com/go-go-golems/docmgr|' go.mod || true && \
  rg -l 'github.com/docmgr/docmgr' | xargs -r sed -i 's|github.com/docmgr/docmgr|github.com/go-go-golems/docmgr|g' && \
  go mod tidy && \
  go build ./...
```

#### 4.7 Push and validate
```bash
cd /home/manuel/workspaces/2025-11-03/documentation-manager/docmgr && \
  git push -u origin main && \
  git log --oneline --graph --decorate --max-count=30 && \
  go run ./cmd/docmgr --help || true
```

### 5) Validation checklist
- [ ] New repo exists on GitHub under `go-go-golems/docmgr` with template base.
- [ ] Template analysis doc created at `vibes/ttmp/2025-11-04/02-how-to-modify-the-templated-repository-to-include-docmgr.md`.
- [ ] `docmgr` history appears after template base in `git log --graph`.
- [ ] Builds successfully, `go mod tidy` clean, lints pass (optional initially).
- [ ] CLI help runs.

### 6) Rollback strategy
- Local only so far; original `vibes` repo untouched apart from an extra branch (`docmgr-split`).
- If new repo integration fails, reclone from template and retry step 4.5.
- If needed, delete local branch: `git branch -D docmgr-split` in `vibes`.

### 7) Later cleanup in `vibes`
- After validating the new repo, remove `2025-11-03/doc-manager/docmgr` from `vibes` in a separate commit/PR.


