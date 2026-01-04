# Go Function Parser and Reference Finder

This program parses Go files to extract function and method information, then finds all references (usages and definitions) for each function, outputting the results as JSON.

## Features

- Parses both regular functions and methods
- Extracts detailed function information (name, file path, line numbers, parameters, return types)
- Finds all references (usages and definitions) for each function
- Supports analyzing a single file or an entire directory
- Outputs results in JSON format (regular or pretty-printed)

## Usage

```
./go-treesitter-lsp [options]
```

### Options

- `-file string`: Path to a Go file to analyze
- `-dir string`: Path to a directory containing Go files to analyze
- `-output string`: Path to output JSON file (default: stdout)
- `-pretty`: Pretty print JSON output

### Examples

Analyze a single file:
```
./go-treesitter-lsp -file main.go -pretty
```

Analyze a directory and save output to a file:
```
./go-treesitter-lsp -dir ./src -pretty -output functions.json
```

## Output Format

The program outputs a JSON array where each element contains:

1. Function information:
   - `name`: Function name
   - `file_path`: Path to the file containing the function
   - `start_line`: Starting line number
   - `end_line`: Ending line number
   - `parameters`: Function parameters
   - `return_type`: Function return type (if any)
   - `is_method`: Whether the function is a method
   - `receiver`: Method receiver (if applicable)

2. References:
   - `file_path`: Path to the file containing the reference
   - `line`: Line number
   - `column`: Column number
   - `context`: The line of code containing the reference
   - `type`: Either "definition" or "usage"

## Implementation Details

The program uses Go's standard library for parsing and analyzing Go code:

- `go/ast`: For parsing and traversing the abstract syntax tree
- `go/parser`: For parsing Go source files
- `go/token`: For tracking token positions

## Building from Source

```
go build -o go-treesitter-lsp
```

## Notes

- The program analyzes the code statically and doesn't require the code to be buildable
- For large codebases, the analysis might take some time
