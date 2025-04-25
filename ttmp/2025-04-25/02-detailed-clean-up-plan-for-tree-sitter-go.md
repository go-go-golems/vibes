# Detailed Plan for Reorganizing tree-sitter-go

This plan outlines the steps to reorganize the `tree-sitter-go` project into a clean, maintainable structure within the `2025-04-25/tree-sitter-go-cleaned-up` directory.

**Target Structure:**

```
tree-sitter-go-cleaned-up/
├── cmd/
│   └── analyzer/           # Main CLI entry point for codebase analysis
│   └── webserver/          # Web server entry point for visualization
├── internal/
│   ├── analyzer/           # Core codebase analysis logic (recursive, using parser)
│   ├── formatter/          # JSON output formatting logic
│   ├── parser/             # Tree-sitter based Go code parsing logic
│   └── web/                # Web server implementation (API handlers, static files)
│       └── static/         # Static assets (HTML, CSS, JS) for the web UI
├── pkg/                    # Shared libraries (initially empty, potential for models)
│   └── models/             # Shared data structures (e.g., FunctionInfo, FileInfo)
├── testdata/               # Sample Go code for testing
├── go.mod                  # Go module definition
├── go.sum                  # Dependency lock file
└── README.md               # Project documentation
```

**Migration Steps:**

1.  **Setup**

    - [ ] Create the root directory: `mkdir 2025-04-25/tree-sitter-go-cleaned-up` (Command not run, manual step assumed).
    - [ ] Create internal structure: `mkdir -p 2025-04-25/tree-sitter-go-cleaned-up/{cmd/{analyzer,webserver},internal/{analyzer,formatter,parser,web/static},pkg/models,testdata}` (Command not run, manual step assumed).

2.  **Initialize Go Module**

    - [ ] Create `2025-04-25/tree-sitter-go-cleaned-up/go.mod`.
    - [ ] Add initial content:

      ```go
      module github.com/wesen/tree-sitter-go-cleaned-up

      go 1.21
      ```

      _(Note: Using 1.21 as the highest version seen in the original project)_

3.  **Parser Component (`internal/parser`)**

    - [ ] **Identify Core Logic:** The core parsing seems embedded within `recursive_analyzer.go`. We will extract it. The logic in `parser_demo.go` might also be useful for reference or simpler parsing tasks if needed later.
    - [ ] **Copy & Refactor:** Copy `2025-04-25/tree-sitter-go/recursive_analyzer.go` to `2025-04-25/tree-sitter-go-cleaned-up/internal/parser/parser.go`.
    - [ ] **Refactor `internal/parser/parser.go`:**
      - Change `package main` to `package parser`.
      - Remove the `main` function and command-line flag processing.
      - Rename `FileAnalyzer` struct and `NewFileAnalyzer` function if a more specific name like `GoParser` is appropriate.
      - Export necessary structs (`FunctionInfo`, `ImportInfo`, `ParameterInfo`, `ReceiverInfo`, etc.) and functions (`AnalyzeFile`). Consider moving these structs to `pkg/models` if they are needed by multiple internal packages. For now, keep them here and export.
      - Remove analysis logic not directly related to parsing a single file (e.g., `CodebaseAnalyzer`, `AnalyzeCodebase`, `findGoFiles`, `analyzeGoFiles`, `extractDependencies`, `SaveAnalysis`). This logic will go into `internal/analyzer`.
      - Ensure all necessary Tree-sitter queries are present and correctly initialized.
      - Add `import sitter "github.com/smacker/go-tree-sitter"` and `import "github.com/smacker/go-tree-sitter/golang"`.
      - Add `import "github.com/pkg/errors"` for error wrapping. Replace `fmt.Errorf` with `errors.Wrapf` where appropriate.

4.  **Shared Models (`pkg/models`)**

    - [ ] **Identify Shared Structs:** Examine structs like `FunctionInfo`, `FileInfo`, `ImportInfo`, `ParameterInfo`, `ReceiverInfo`, `PackageInfo`, `AnalysisStats`, `CodebaseAnalysis` defined initially in `recursive_analyzer.go` (now moved to `internal/parser/parser.go` temporarily).
    - [ ] **Move to `pkg/models`:** Move structs that will be used by `analyzer`, `formatter`, and potentially `web` into `2025-04-25/tree-sitter-go-cleaned-up/pkg/models/models.go`. Define `package models`.
    - [ ] **Update Imports:** Update `internal/parser/parser.go` to import `github.com/wesen/tree-sitter-go-cleaned-up/pkg/models`.

5.  **Analyzer Component (`internal/analyzer`)**

    - [ ] **Create `internal/analyzer/analyzer.go`:** Create a new file.
    - [ ] **Add `package analyzer`**.
    - [ ] **Implement Analyzer Logic:**
      - Define an `Analyzer` struct.
      - Add a `NewAnalyzer` function that takes a `parser.GoParser` instance (or similar).
      - Move the logic from the original `CodebaseAnalyzer`, `AnalyzeCodebase`, `findGoFiles`, `analyzeGoFiles`, `extractDependencies` (from the original `recursive_analyzer.go`) into methods on the `Analyzer` struct.
      - Update the code to use the `parser.GoParser` to analyze individual files.
      - Update the code to use structs from `pkg/models`.
      - Import `github.com/wesen/tree-sitter-go-cleaned-up/internal/parser` and `github.com/wesen/tree-sitter-go-cleaned-up/pkg/models`.
      - Import `github.com/pkg/errors` for error wrapping.
      - Use `context.Context` in the `AnalyzeCodebase` method signature for cancellation.
      - Use `errgroup` for concurrent file analysis (`analyzeGoFiles` logic).

6.  **Formatter Component (`internal/formatter`)**

    - [ ] **Copy & Refactor:** Copy `2025-04-25/tree-sitter-go/json_formatter.go` to `2025-04-25/tree-sitter-go-cleaned-up/internal/formatter/formatter.go`.
    - [ ] **Refactor `internal/formatter/formatter.go`:**
      - Change `package main` to `package formatter`.
      - Remove the `main` function and `EnhanceAnalysisJSON` function (if it reads from a file).
      - Create a `FormatJSON` function (or similar) that takes the `models.CodebaseAnalysis` struct as input and returns the JSON byte slice or writes to an `io.Writer`.
      - Remove the complex visualization-specific structs and logic for now (`VisualizationData`, `HierarchyNode`, `NetworkVisualization`, etc.) unless the web server component requires them immediately. Focus on formatting the core `CodebaseAnalysis`. Keep the `EnhancedOutput` struct definition but simplify it initially.
      - Update code to use structs from `pkg/models`.
      - Import `github.com/wesen/tree-sitter-go-cleaned-up/pkg/models`.
      - Import `github.com/pkg/errors` for error wrapping.

7.  **Web Server Component (`internal/web`)**

    - [ ] **Copy & Refactor:** Copy `2025-04-25/tree-sitter-go/web_server.go` to `2025-04-25/tree-sitter-go-cleaned-up/internal/web/server.go`.
    - [ ] **Refactor `internal/web/server.go`:**
      - Change `package main` to `package web`.
      - Remove the `main` function.
      - Modify `InitializeServer` and `StartServer` to be exported functions (e.g., `NewServer`, `server.ListenAndServe`).
      - The server should accept instances of the `analyzer.Analyzer` and `formatter.Formatter` (or relevant interfaces) during initialization.
      - Update API handlers (`handleAnalyzeRequest`):
        - Use the injected `analyzer` instance to perform analysis (pass `context.Context`).
        - Use the injected `formatter` instance to format the results.
        - Remove the direct analysis logic (`analyzeCode` function) previously within `web_server.go`.
      - Update code to use structs from `pkg/models`.
      - Import internal packages (`analyzer`, `formatter`, `models`).
      - Import `github.com/pkg/errors`.
      - Review caching logic (`AnalysisCache`); keep it for now.
      - Adjust static file serving path to point to `internal/web/static/`.
    - [ ] **Copy Static Assets:** Copy contents of `2025-04-25/tree-sitter-go/webserver/static/` (if it exists and contains necessary HTML/CSS/JS) to `2025-04-25/tree-sitter-go-cleaned-up/internal/web/static/`. If it doesn't exist, the `generateBasicIndexHTML` logic in `server.go` will create basic files.

8.  **CLI Entry Point - Analyzer (`cmd/analyzer`)**

    - [ ] **Create `cmd/analyzer/main.go`**.
    - [ ] **Implement `main` function:**
      - Add `package main`.
      - Use the `flag` package (or `cobra` if preferred, but stick to `flag` for simplicity based on original code) to parse arguments (`--path`, `--output`, `--exclude`, etc., similar to original `recursive_analyzer.go`).
      - Create a `parser.GoParser` instance.
      - Create an `analyzer.Analyzer` instance, passing the parser.
      - Create a `formatter.Formatter` instance.
      - Call `analyzer.AnalyzeCodebase(ctx, path, ...)` with a background context.
      - Call `formatter.FormatJSON(...)` with the analysis result.
      - Write the resulting JSON to the specified output file.
      - Import necessary internal packages (`analyzer`, `formatter`, `parser`, `models`) using the full module path.
      - Handle errors appropriately.

9.  **CLI Entry Point - Web Server (`cmd/webserver`)**

    - [ ] **Create `cmd/webserver/main.go`**.
    - [ ] **Implement `main` function:**
      - Add `package main`.
      - Use the `flag` package to parse arguments (`--port`, `--static-dir`, `--default-code-dir`, etc., similar to original `web_server.go`).
      - Create instances of `parser.GoParser`, `analyzer.Analyzer`, `formatter.Formatter`.
      - Initialize the `web.Server` using `web.NewServer(...)`, passing the analyzer and formatter.
      - Call `server.ListenAndServe()`.
      - Import necessary internal packages (`analyzer`, `formatter`, `parser`, `web`, `models`).
      - Handle errors.

10. **Test Data (`testdata`)**

    - [ ] **Copy Files:** Copy relevant `.go` files from `2025-04-25/tree-sitter-go/test_parser/`, `2025-04-25/tree-sitter-go/test_simple/`, and `2025-04-25/tree-sitter-go/sample/` into `2025-04-25/tree-sitter-go-cleaned-up/testdata/`. Remove any `main.go` files or build artifacts from these source directories.

11. **Dependencies & Build**

    - [ ] Navigate to `2025-04-25/tree-sitter-go-cleaned-up`.
    - [ ] Run `go mod tidy`. This will download dependencies (like `go-tree-sitter`, `gorilla/mux`, `pkg/errors`) and update `go.sum`.
    - [ ] Attempt to build the CLIs: `go build ./cmd/analyzer/...` and `go build ./cmd/webserver/...`. Fix any compilation errors arising from incorrect refactoring or imports.

12. **Documentation (`README.md`)**

    - [ ] **Create `README.md`** in `2025-04-25/tree-sitter-go-cleaned-up/`.
    - [ ] **Consolidate Content:** Review `recursive_analyzer_readme.md`, `enhanced-json-format-documentation.md`, `go-file-analyzer/README.md`, `go-file-analyzer/DOCUMENTATION.md`, etc.
    - [ ] **Write New README:**
      - Explain the project's purpose.
      - Describe the new directory structure.
      - Provide build instructions (`go build ./cmd/...`).
      - Explain how to run the `analyzer` CLI with flags.
      - Explain how to run the `webserver` CLI with flags.
      - Briefly describe the JSON output format (link to sample if useful).
      - Mention key dependencies.

13. **Final Review & Cleanup**
    - [ ] Review all Go files in `tree-sitter-go-cleaned-up` for TODOs, commented-out code, or unnecessary logic.
    - [ ] Ensure consistent use of `github.com/pkg/errors` for error handling.
    - [ ] Add `var _ Interface = (*Implementation)(nil)` checks if any interfaces were defined and implemented.
    - [ ] Verify `context.Context` usage in `analyzer` and `web` handlers.
    - [ ] Run `go fmt ./...` and `go vet ./...` to check formatting and potential issues.
    - [ ] Manually test the `analyzer` and `webserver` CLIs with sample data from `testdata`.
