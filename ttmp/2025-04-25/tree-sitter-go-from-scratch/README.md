# Go File Analyzer (Tree-sitter Demo)

This project demonstrates parsing Go files using Tree-sitter.

## Phase 1: Basic Parsing

- Parses a single Go file.
- Extracts function names and line numbers.
- Provides a simple CLI tool (`cmd/simple-parser`) to run the parser.

### Usage (CLI)

```bash
# Make sure you are in the project root directory
go run ./cmd/simple-parser/main.go ./testdata/simple.go
```

### Testing

```bash
go test ./...
```
