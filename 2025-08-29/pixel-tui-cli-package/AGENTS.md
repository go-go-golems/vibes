# Repository Guidelines

## Project Structure & Module Organization
- `source/`: Go CLI source (`main_cli.go`, `go.mod`, `go.sum`).
- `pixel-tui-cli`: Built binary (do not modify; rebuild locally when needed).
- `documentation/`: Deeper guides and usage examples.
- `examples/`: Helper scripts for demos, batch processing, comparisons.
- `original-images/` and `demos/`: Sample assets for local testing.

## Build, Test, and Development Commands
- Build: `cd source && go build -o ../pixel-tui-cli main_cli.go` — compiles the CLI.
- Run: `./pixel-tui-cli -i original-images/blockbob.gif -w 32 -h 32` — launches the TUI.
- Format: `gofmt -s -w source/*.go` — enforces standard Go formatting.
- Vet: `cd source && go vet ./...` — basic static checks.
- Examples: `cd examples && ./demo_script.sh` — run curated demonstrations.

## Coding Style & Naming Conventions
- Language: Go 1.19+; follow idiomatic Go style.
- Formatting: Must pass `gofmt -s`; no manual style deviations.
- Naming: Exported identifiers use `PascalCase`; unexported use `camelCase`.
- Files: `snake_case.go` (e.g., `main_cli.go`). Keep package names lowercase, short.
- Imports: Group stdlib first, then external. Remove unused code.

## Testing Guidelines
- Framework: Go’s built-in `testing` package.
- Scope: Prefer unit tests for pure helpers (`resizeImage`, `rgbToHex`, `hexToRGB`, color sampling logic).
- Names: `*_test.go` with `TestXxx` functions. Table tests where sensible.
- Run: `cd source && go test ./...`.
- Coverage (target): Aim for >70% on helper logic; UI rendering can be validated via golden outputs if added later.

## Commit & Pull Request Guidelines
- Commits: Use Conventional Commits: `feat:`, `fix:`, `refactor:`, `docs:`, `test:`, `chore:`. Example: `feat(cli): add GIF animation speed control`.
- PRs: Include clear description, rationale, and before/after screenshots or terminal recordings when UI is affected. Link issues. Keep focused and small.
- Binary: Do not commit rebuilt `pixel-tui-cli` unless performing a coordinated release update.

## Security & Configuration Tips
- Input: Only local file paths are accepted; no remote fetching. Validate existence before processing.
- Performance: Prefer small output sizes (32×32/48×36) for large images/GIFs to avoid high memory usage.
