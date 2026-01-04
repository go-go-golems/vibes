# Go Codebase Analyzer

This tool recursively analyzes Go source code in a directory structure, extracting detailed information about functions, methods, packages, and dependencies.

## Features

- **Recursive Directory Analysis**: Automatically traverses the entire directory structure, finding all Go files.
- **Comprehensive Function Analysis**: Extracts detailed information about functions and methods including parameters, return types, and documentation.
- **Method and Receiver Detection**: Properly identifies methods and captures their receiver type information.
- **Package Structure Analysis**: Maps the package structure of the codebase and tracks relationships between packages.
- **Dependency Extraction**: Identifies external dependencies used by the codebase.
- **Parallel Processing**: Analyzes multiple files concurrently for improved performance.
- **Customizable Exclusions**: Allows excluding specific directories and file patterns from analysis.

## Usage

```
go run recursive_analyzer.go [flags]
```

### Flags

- `-path`: Path to the codebase to analyze (default: current directory)
- `-output`: Output JSON file path (default: `codebase_analysis.json`)
- `-exclude`: Comma-separated list of directories to exclude (default: `vendor,node_modules,dist,build`)
- `-exclude-files`: Comma-separated list of file name patterns to exclude (default: `generated,auto_gen`)
- `-concurrency`: Number of files to analyze concurrently (default: 4)
- `-verbose`: Enable verbose logging

## Example

```
go run recursive_analyzer.go -path /path/to/project -output analysis.json -concurrency 8 -verbose
```

## Output Format

The tool produces a JSON file with the following structure:

```json
{
  "basePath": "/absolute/path/to/analyzed/directory",
  "files": [
    {
      "path": "/absolute/path/to/file.go",
      "package": "packagename",
      "imports": [
        {
          "path": "imported/package/path",
          "alias": "optional_alias"
        }
      ],
      "functions": [
        {
          "name": "FunctionName",
          "filepath": "/absolute/path/to/file.go",
          "startLine": 10,
          "endLine": 20,
          "startCol": 1,
          "endCol": 1,
          "isExported": true,
          "isMethod": false,
          "parameters": [
            {
              "name": "paramName",
              "type": "paramType"
            }
          ],
          "returnType": ["returnType"],
          "comments": "Function documentation",
          "package": "packagename"
        }
      ],
      "lastUpdate": "2025-04-24T12:34:56Z"
    }
  ],
  "functions": [ ... ],
  "packages": {
    "packagename": "relative/path/to/package"
  },
  "dependencies": [
    "external/dependency/path"
  ],
  "stats": {
    "totalFiles": 100,
    "totalFunctions": 500,
    "totalMethods": 200,
    "totalPackages": 20,
    "analysisTimeSeconds": 1.25,
    "timestamp": "2025-04-24T12:34:56Z"
  }
}
```

## Implementation Details

### File Analyzer

The `FileAnalyzer` analyzes individual Go files using Tree-sitter queries to extract information about:

- Package declarations
- Import statements
- Function declarations
- Method declarations with receiver information
- Function parameters and return types
- Documentation comments

### Codebase Analyzer

The `CodebaseAnalyzer` orchestrates the analysis of the entire codebase:

1. Recursively finds all Go files in the directory structure
2. Analyzes each file in parallel using the `FileAnalyzer`
3. Aggregates information about functions, methods, and packages
4. Extracts dependency information from import statements
5. Generates statistics about the codebase
6. Outputs the results in JSON format

### Tree-sitter Queries

The tool uses specialized Tree-sitter queries to precisely identify and extract information from Go source code, including:

- Function declarations with parameters and return types
- Method declarations with receiver types
- Parameter declarations with types
- Documentation comments

## Use Cases

- **Code Understanding**: Quickly understand the structure and function of large codebases
- **Documentation Generation**: Use as input for generating documentation about Go code
- **Dependency Analysis**: Analyze internal and external dependencies
- **Code Quality Metrics**: Generate metrics about function complexity, sizes, etc.
- **Refactoring Planning**: Identify areas for refactoring by analyzing function relationships