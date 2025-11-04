## How to adapt `docmgr` to the `wesen-go-template` repository

This document explains how to reorganize the `docmgr` codebase to fit the structure and tooling provided by `wesen-go-template` after the historical code is merged.

Template reference: https://github.com/wesen/wesen-go-template

### 1) Template overview (observed files)
- `README.md`: Template header and ASCII art.
- `Makefile`: lint/test/build, goreleaser, tagging, dependency bumps, install target (placeholders named `XXX`).
- `.golangci.yml`: Linter configuration (errcheck, govet, staticcheck, exhaustive, etc.).
- `.goreleaser.yaml`: Release configuration for `XXX`, brew/nfpm publishers, cross-platform builds.
- `lefthook.yml`: Git hooks for pre-commit and pre-push (lint/test and goreleaser).
- `cmd/XXX/main.go`: Entrypoint placeholder.
- `pkg/doc.go`: Package placeholder.

### 2) Target structure for `docmgr`
- Replace `XXX` with `docmgr` across:
  - `cmd/XXX` → `cmd/docmgr`; binary name `docmgr`.
  - Makefile variables/targets referencing `XXX`.
  - `.goreleaser.yaml` `project_name`, `main`, `binary`, `brews.name`, `brews.homepage`.
- Ensure application packages live under `pkg/...` with clear names; prefer high-verbosity code and descriptive names.
- Keep documentation (guides, how-tos) under `doc/` or repository root, depending on audience and frequency.

### 3) Go module path and imports
- Set module to `github.com/go-go-golems/docmgr` in `go.mod`.
- Update import paths from any old module name (e.g., `github.com/docmgr/docmgr/...`) to `github.com/go-go-golems/docmgr/...`.
- Run `go mod tidy` and ensure build success.

### 4) CLI entrypoint migration
- Move/rename existing CLI entrypoint to `cmd/docmgr/main.go`.
- Ensure it wires Cobra commands correctly and aligns with template expectations:
  - Program `Use: "docmgr"`.
  - Makefile `install` target should build `./cmd/docmgr` into `./dist/docmgr` and copy to `$PATH` binary if desired.

### 5) Tooling integration
- Linting: use `.golangci.yml` from template; adjust exclusions if needed.
- Hooks: enable `lefthook` (`lefthook install`), keep lint/test on commit and release/lint/test on push.
- Releases: update `.goreleaser.yaml` from `XXX` → `docmgr` and confirm brew tap/release workflow. If not using nfpm/fury.io initially, you can disable or leave as-is until credentials are available.
- CI: add or adapt GitHub Actions workflows (if not present in template repo) later to run lint/test/build and goreleaser on tags.

### 6) Makefile changes (search for `XXX`)
- Replace:
  - `VERSION` can stay or be managed via tags only.
  - `release` target: update module path from `github.com/go-go-golems/XXX` → `github.com/go-go-golems/docmgr`.
  - `install`: build `./cmd/docmgr` to `./dist/docmgr` and use `DOCMGR_BINARY` env if needed; otherwise mirror the template pattern.

Example diffs to apply conceptually:
```diff
- XXX_BINARY=$(shell which XXX)
- install:
- 	go build -o ./dist/XXX ./cmd/XXX && \
- 		cp ./dist/XXX $(XXX_BINARY)
+ DOCMGR_BINARY=$(shell which docmgr)
+ install:
+ 	go build -o ./dist/docmgr ./cmd/docmgr && \
+ 		cp ./dist/docmgr $(DOCMGR_BINARY)
```

Concrete commands to apply replacements safely:
```bash
# In repo root
rg -n "\\bXXX\\b|cmd/XXX|dist/XXX|go-go-golems/XXX|XXX_BINARY" Makefile
sed -i 's|go-go-golems/XXX|go-go-golems/docmgr|g' Makefile
sed -i 's|XXX_BINARY|DOCMGR_BINARY|g' Makefile
sed -i 's|dist/XXX|dist/docmgr|g' Makefile
sed -i 's|cmd/XXX|cmd/docmgr|g' Makefile
sed -i 's|\bXXX\b|docmgr|g' Makefile
```

### 7) `.goreleaser.yaml` updates
- `project_name: docmgr`
- `builds[].main: ./cmd/docmgr`
- `builds[].binary: docmgr`
- `brews[].name: docmgr`
- `brews[].homepage: https://github.com/go-go-golems/docmgr`
- Optionally keep nfpm publisher; update descriptions and ensure environment variables are set in CI before enabling.

If the file exists, you can apply basic replacements with:
```bash
sed -i 's|project_name: XXX|project_name: docmgr|' .goreleaser.yaml
sed -i 's|main: ./cmd/XXX|main: ./cmd/docmgr|' .goreleaser.yaml
sed -i 's|binary: XXX|binary: docmgr|' .goreleaser.yaml
sed -i 's|name: XXX|name: docmgr|' .goreleaser.yaml
sed -i 's|go-go-golems/XXX|go-go-golems/docmgr|g' .goreleaser.yaml
```

### 8) Documentation and README
- Replace ASCII banner or keep a smaller banner; add a concise description of `docmgr` (purpose, features, quickstart).
- Include install instructions (go install, Homebrew via tap if enabled, download binaries via releases).
- Add `USAGE` with core commands and examples.

### 9) Migration flow after history import
1. After rebasing `docmgr` history onto template `main`, create a feature branch `integrate-docmgr`.
2. Apply all renames (`XXX` → `docmgr`) in `cmd`, Makefile, `.goreleaser.yaml`, and docs.
3. Update `go.mod` and imports.
4. Run `go mod tidy`, `make lint`, `make test`, and `make build`.
5. Open a PR and iterate.

### 10) Checklist
- [ ] Rename `cmd/XXX` → `cmd/docmgr` and set binary name to `docmgr`.
- [ ] Update module path to `github.com/go-go-golems/docmgr`.
- [ ] Fix imports to the new module path.
- [ ] Update Makefile targets and `release` module path.
- [ ] Update `.goreleaser.yaml` fields from `XXX` to `docmgr`.
- [ ] Enable `lefthook` and verify hooks execute.
- [ ] Lint/test/build pass locally.
- [ ] Update `README.md` and docs for `docmgr` specifics.


