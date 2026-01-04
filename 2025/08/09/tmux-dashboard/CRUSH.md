# CRUSH Guidelines

## Project Commands
- Build project: `go build ./...`
- Run all tests: `go test ./...`
- Run single test: `go test -timeout 30s -run ^<TestName>$ path/to/package`
- Format code: `go fmt ./...`
- Vet code: `go vet ./...`
- Run application: `go run main.go`

## Code Style
- Formatting: run `go fmt` and `goimports` to enforce formatting and grouped imports.
- Import groups:
  1. Standard library
  2. (blank line)
  3. Third-party modules
- Naming:
  - Packages: short, lowercase (e.g., `tmux`, `config`).
  - Exported identifiers: CamelCase; unexported: camelCase.
  - Constants: CamelCase or SCREAMING_SNAKE for enums.
- Types: prefer explicit types; use interfaces for decoupling.
- Error handling: always check and return errors immediately. No ignored errors.
- Logging: avoid package-level globals; inject or pass loggers.

## Repository Layout
```
main.go         Entry point
config.go       Config parsing and validation
tmux.go         Tmux control API
*.go            Tests alongside source (e.g., config_test.go)
examples/       Sample YAML configurations
screenshots/    Reference output captures
``` 

## Cursor / Copilot Rules
- No `.cursor` or Copilot rules in this repo currently.

*Generated with Crush*