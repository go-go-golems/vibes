# Agent Guidelines for arxiv-libgen-cli

## Build Commands
- Build: `go build -o arxiv-libgen-searcher .`
- Run: `./arxiv-libgen-searcher [command] [flags]`
- Debug: Add `--debug` or `-d` flag to any command

## Code Style Guidelines
- **Imports**: Group standard library imports first, third-party imports after
- **Naming**: Use CamelCase for exported vars/funcs, camelCase for unexported
- **Error Handling**: Use zerolog for logging (`log.Fatal()`, `log.Error()`, etc.)
- **Structure**: Use Cobra for CLI commands, define in cmd/ directory
- **Types**: Define structs for API responses, use XML/JSON struct tags
- **Formatting**: Run `go fmt ./...` before committing

## Testing
- Tests should be placed in `*_test.go` files alongside the code they test
- Run tests: `go test ./...`
- Run single test: `go test ./... -run TestName`

## Project Structure
- `cmd/`: Contains Cobra command implementations
- `main.go`: Entry point for CLI application