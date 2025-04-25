# Tree-Sitter-Go Project Reorganization Analysis

## Overview

The tree-sitter-go project appears to be a Go code analysis tool that uses Tree-sitter for parsing and analyzing Go code. The project is currently disorganized with files in incorrect locations, duplicate implementations, and inconsistent module structure. This document outlines findings from analyzing the project structure and proposes solutions for reorganization.

## Current Project State

### Project Structure Issues

1. **Duplicated Files**: Multiple copies of similar files exist across different directories

   - `main.go` files exist in multiple directories
   - `recursive_analyzer.go` exists both in root and a subdirectory
   - `json_formatter.go` exists both in root and a subdirectory
   - `web_server.go` in root and `server.go` in webserver/ appear to be duplicates

2. **Inconsistent Module Structure**:

   - The webserver directory has its own go.mod file: `github.com/scrapybara/go-file-analyzer/webserver`
   - The go-file-analyzer/recursive_analyzer has its own go.mod: `recursive_analyzer`
   - The go-file-analyzer/json_formatter has its own go.mod
   - The go-file-analyzer/sample has its own go.mod
   - Different Go versions are used (1.18 in webserver, 1.21 in recursive_analyzer)
   - Root directory has no module definition

3. **Scattered Components**:

   - Core functionality is split between the root directory and subdirectories
   - Some files in the root directory should likely be in their respective component directories

4. **Nested Project Structure**:
   - A complete `go-file-analyzer` directory exists inside the tree-sitter-go project
   - This appears to be a more organized version of the same project

### Go Package Analysis

Based on analysis of Go files:

- 26 files using `package main` (scattered entry points)
- 9 files using `package parser` (core parsing functionality)
- 2 files using `package cmd` (command-line tools)
- 1 file each for packages: subpackage, nested, calculator (likely test/demo packages)

### Import Issues

- The main_enhanced.go file uses a relative import: `import "./parser"` which is not supported in Go module mode
- Some files may be referring to packages that don't exist or are in the wrong location (e.g., `github.com/user/go-file-analyzer/parser`)

### Component Identification

Based on file analysis, the project consists of these core components:

1. **Parser**:

   - Functionality for parsing Go files using Tree-sitter
   - Files: `parser_demo.go`, `parser/parser_integration.go`, `parser/enhanced_parser_test.go`

2. **Recursive Analyzer**:

   - Recursively analyzes Go codebases
   - Files: `recursive_analyzer.go`, `recursive_analyzer/main.go`, `simple_recursion.go`
   - Documentation: `recursive_analyzer_readme.md`, `recursive_directory_analysis.md`

3. **JSON Formatter**:

   - Formats the analysis results as enhanced JSON
   - Files: `json_formatter.go`, `json_formatter/main.go`
   - Sample data: `enhanced-output-sample.json`

4. **Web Server**:

   - Web interface for visualization
   - Files: `web_server.go`, `webserver/server.go`, `webserver/static/index.html`

5. **Entry Point**:
   - Main program that ties the components together
   - Files: `main_enhanced.go`

### Dependencies

Key external dependencies include:

- github.com/smacker/go-tree-sitter (Go Tree-sitter binding)
- github.com/smacker/go-tree-sitter/golang (Tree-sitter grammar for Go)
- github.com/gorilla/mux (Web routing)
- github.com/sourcegraph/jsonrpc2 (JSON-RPC)
- github.com/tliron/glsp (Language Server Protocol)

### Test Directories

- `test_parser/`, `test_simple/`, and `sample/` appear to be test directories with sample Go code for testing the analyzer

## Current State Analysis

### Good Parts

1. The project has clear separation of concerns with different components for parsing, analyzing, formatting, and visualization
2. Documentation exists for some components (recursive analyzer, JSON formatter)
3. The `go-file-analyzer` subdirectory appears to be a more organized version of the project

### Problems

1. File duplication leads to maintenance challenges and confusion
2. Inconsistent module structure makes dependency management difficult
3. Relative imports (`import "./parser"`) in main_enhanced.go are problematic and not Go-idiomatic
4. No clear entry point for the application
5. Some files likely represent work-in-progress or intermediate versions
6. Multiple go.mod files with different module paths create conflicts
7. Missing proper dependency management at the root level

## Proposed Reorganization Strategy

### Approach 1: Adopt the go-file-analyzer Structure

The `go-file-analyzer` subdirectory appears to be a more organized version of the project. We could adopt its structure entirely:

```
go-file-analyzer/
├── cmd/                    # Command-line interfaces
├── parser/                 # Parser component
├── recursive_analyzer/     # Recursive analyzer component
├── json_formatter/         # JSON formatter component
├── webserver/              # Web server and interface
├── README.md               # Main documentation
└── screenshots/            # Screenshots for documentation
```

### Approach 2: Clean Reorganization

Reorganize the project with a clean module structure:

```
tree-sitter-go/
├── cmd/
│   └── analyzer/           # Main CLI entry point
├── internal/
│   ├── parser/             # Parser implementation
│   ├── analyzer/           # Code analysis implementation
│   ├── formatter/          # JSON formatting implementation
│   └── visualization/      # Visualization utilities
├── pkg/
│   ├── models/             # Shared data models
│   └── utils/              # Shared utilities
├── web/
│   ├── server/             # Web server implementation
│   └── static/             # Static web assets
├── examples/               # Example usage
├── testdata/               # Test data
├── go.mod                  # Single module file
├── go.sum                  # Dependencies lockfile
└── README.md               # Documentation
```

## Recommended Steps for Reorganization

1. **Create a Single Module**:

   - Set up a single go.mod file at the root with appropriate module name
   - Remove internal go.mod files (like in webserver/ and others)
   - Choose a consistent Go version (probably the most recent: 1.21+)

2. **Consolidate Duplicated Files**:

   - Compare duplicated files to determine the most recent/complete version
   - Keep only the best version and remove duplicates
   - Identify and resolve any function/type name conflicts

3. **Organize Code by Component**:

   - Move parser-related code to a parser package
   - Move analyzer-related code to an analyzer package
   - Move formatter-related code to a formatter package
   - Move web server code to a server package

4. **Create Clean Entry Points**:

   - Create a main CLI application in cmd/analyzer
   - Create a web server entry point in cmd/webserver

5. **Fix Imports**:

   - Update all imports to use the new module path
   - Remove relative imports (`./parser`)
   - Fix any references to non-existent packages

6. **Update Documentation**:
   - Create a comprehensive README.md
   - Document the project structure and usage
   - Consolidate existing documentation (recursive_analyzer_readme.md, etc.)

## Decision Points

Before proceeding with reorganization, consider these questions:

1. Should we keep the `go-file-analyzer` directory and remove the rest, or merge the best parts of both?
2. Is the current functionality complete, or are there unfinished components?
3. Are there specific file versions that are superior to others?
4. What should be the canonical module name for the project?
   - Current options seen: github.com/scrapybara/go-file-analyzer or recursive_analyzer
   - Suggested: github.com/wesen/tree-sitter-go
5. Should we maintain backward compatibility with existing code?

## Next Steps

1. Run the Go tools to verify which files compile successfully:

   - Try compiling each component independently
   - Identify compile errors and dependency issues

2. Compare duplicate files to identify the most up-to-date versions:

   - Use diff tools to compare similar files
   - Look for version markers or timestamps
   - Check for additional features or bug fixes

3. Document the dependencies between components:

   - Create a dependency graph
   - Identify shared types and interfaces

4. Create a detailed migration plan:

   - List each file and its destination in the new structure
   - Document required changes to imports and package declarations

5. Implement the reorganization in a staged approach:
   - Start with the core parser package
   - Then migrate the analyzer and formatter
   - Finally, update the web server and CLI tools
