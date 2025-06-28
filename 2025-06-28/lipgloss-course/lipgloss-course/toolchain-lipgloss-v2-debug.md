# Debugging Go Modules: Lipgloss v2 Import/Toolchain Issue

## Context
- Project: Terminal UI course using `github.com/charmbracelet/lipgloss/v2` (v2.0.0-beta1)
- Go toolchain: Custom/preview (`toolchain go1.23.10` in go.mod)
- All code imports use the correct `/v2` path.
- go.mod and go.sum do **not** reference any v1-only dependencies.

## Problem
- Running any example (e.g., `go run examples/01-basic-styling.go`) fails with:
  ```
  no required module provides package github.com/charmbracelet/lipgloss/v2; to add it:
      go get github.com/charmbracelet/lipgloss/v2
  ```
- `go.mod` explicitly requires `github.com/charmbracelet/lipgloss/v2 v2.0.0-beta1`.
- All imports in the codebase use the `/v2` path.
- `go mod why github.com/charmbracelet/lipgloss` claims a dependency path via `github.com/go-go-golems/bobatea/cmd/overlay`, but:
  - There is **no reference** to `bobatea` or `go-go-golems` in go.mod, go.sum, or any source file.
  - Grep confirms this.
- `go list -m all` shows only v1 of lipgloss is present, not v2.

## Diagnostics Performed
- Verified all imports use `/v2`.
- Searched for any reference to `bobatea` or v1 in all files, go.mod, go.sum — none found.
- Used `go mod why` and `go mod why -m` — both show a dependency path that does not exist in the current project.
- Cleaned up go.mod (removed any v1 require), ran `go mod tidy`, and re-added v2 — no effect.
- Attempted to run after each step — error persists.

## Hypothesis
- The Go module/build cache is global for a given toolchain and user.
- If the same toolchain was used in another project that did depend on `bobatea` (which in turn depends on lipgloss v1), the cache may be polluted.
- `go mod why` and the build process may be using stale or incorrect cache data, leading to phantom dependencies and resolution errors.
- This is especially likely with custom or preview toolchains, or after switching between projects with different dependency trees.

## Proposed Solution
1. **Clean the Go module and build cache:**
   ```
   go clean -modcache
   go clean -cache
   rm go.sum
   go mod tidy
   ```
   - This will force Go to re-resolve all dependencies from scratch, using only the current go.mod.
2. **Verify again:**
   - Run `go run examples/01-basic-styling.go`.
   - If the error persists, check for Go toolchain bugs or deeper issues with the module proxy/cache.

## Questions for Crosschecking
- Is it possible for the Go module/build cache to cause phantom dependencies to appear in `go mod why` or the build process?
- Can a custom toolchain or switching between projects pollute the cache in this way?
- Is there any other way a dependency like `bobatea` could appear in `go mod why` output if it is not in go.mod, go.sum, or any source file?
- Are there additional steps to fully reset the Go toolchain's view of the module graph?

## Additional Info
- All code and examples use the correct `/v2` import path.
- No direct or indirect reference to `bobatea` or v1 lipgloss in the project.
- The error persists after all standard Go module troubleshooting steps. 