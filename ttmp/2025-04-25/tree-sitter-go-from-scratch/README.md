# Go File Analyzer (Tree-sitter Demo)

This project demonstrates parsing Go files using Tree-sitter.

## Phase 1: Basic Parsing (Completed)

- Parses a single Go file.
- Extracts function names and line numbers.
- Provided a simple CLI tool (`cmd/simple-parser`) to run the parser.

## Phase 2: Directory Analysis (In Progress)

- Recursively analyzes Go files in a directory.
- Extracts package names.
- Aggregates results into a JSON output.
- Excludes specified directories (e.g., `vendor`, `.git`) and test files.
- Provides an enhanced CLI tool (`cmd/analyzer`).

### Usage (CLI - Phase 2)

Build the analyzer:

```bash
go build ./cmd/analyzer/...
```

Run the analyzer on the current directory:

```bash
./analyzer -path .
# or ./analyzer
```

Analyze a specific directory and save to a file:

```bash
./analyzer -path /path/to/your/go/project -output analysis.json
```

Exclude additional directories:

```bash
./analyzer -path . -exclude vendor,.git,tmp
```

### Testing

```bash
go test ./...
```
