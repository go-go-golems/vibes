# Go Function Parser with Gopls Integration and Debug Logging

This program parses Go files to extract function and method information, then uses the Go language server (gopls) to find all references (usages and definitions) for each function, outputting the results as JSON. It includes comprehensive debug logging to help diagnose issues with the gopls integration.

## Features

- Parses both regular functions and methods
- Extracts detailed function information (name, file path, line numbers, parameters, return types)
- Uses gopls for finding references with full semantic analysis
- Comprehensive debug logging with zerolog
- Supports analyzing a single file or an entire directory
- Outputs results in JSON format (regular or pretty-printed)
- Option to fall back to AST-based reference finding if needed

## Requirements

- Go 1.23.4 or higher (tested with Go 1.23.8)
- gopls installed (`go install golang.org/x/tools/gopls@latest`)

## Usage

```
./go-function-parser [options]
```

### Options

- `-file string`: Path to a Go file to analyze
- `-dir string`: Path to a directory containing Go files to analyze
- `-output string`: Path to output JSON file (default: stdout)
- `-pretty`: Pretty print JSON output
- `-gopls`: Use gopls for finding references (default: true)
- `-log string`: Log level (debug, info, warn, error) (default: debug)

### Examples

Analyze a single file using gopls with debug logging:
```
./go-function-parser -file main.go -pretty -log debug
```

Analyze a directory and save output to a file:
```
./go-function-parser -dir ./src -pretty -output functions.json -log info
```

Use AST-based reference finding instead of gopls:
```
./go-function-parser -dir ./src -pretty -output functions.json -gopls=false
```

## Output Format

The program outputs a JSON array where each element contains:

1. Function information:
   - `name`: Function name
   - `file_path`: Path to the file containing the function
   - `start_line`: Starting line number
   - `end_line`: Ending line number
   - `start_col`: Starting column number
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

The program uses two approaches for analyzing Go code:

1. **Function Extraction**: Uses Go's standard library (`go/ast`, `go/parser`, `go/token`) to parse Go files and extract function information.

2. **Reference Finding**: 
   - Primary method: Uses gopls via the Language Server Protocol (LSP) over JSON-RPC for accurate reference finding with full semantic analysis.
   - Fallback method: Can use AST-based parsing for simpler reference finding if gopls is not available or disabled.

3. **Logging**:
   - Uses zerolog for structured logging with different log levels
   - Logs include file and line information for easier debugging
   - Captures and logs gopls stderr output
   - Logs detailed information about LSP requests and responses

## Current Limitations and Known Issues

- The gopls integration may have issues with finding references in some cases, resulting in null references in the output
- Gopls requires the files to be part of a valid Go module to work correctly
- The program assumes gopls is installed at a specific path (/home/ubuntu/go/bin/gopls)
- Gopls may report "No active builds contain file" for test files that aren't part of a proper Go module
- Error handling for LSP communication could be improved
- Some edge cases in complex Go code might not be handled correctly

## Debugging Tips

- Use the `-log debug` flag to enable detailed logging
- Check the logs for "failed to unmarshal references result" errors, which indicate issues with the gopls responses
- Look for gopls stderr messages that might provide insights into why reference finding is failing
- Ensure your Go files are part of a valid Go module for best results with gopls
- If gopls integration fails, try the AST-based approach with `-gopls=false`

## Building from Source

```
go mod init github.com/user/go-function-parser
go get -u github.com/rs/zerolog
go mod tidy
go build -o go-function-parser
```

## Notes

- The program analyzes the code statically and doesn't require the code to be buildable
- For large codebases, the analysis might take some time
- The gopls server is launched as a subprocess and communicates via stdin/stdout
- The program includes a 2-second delay after opening files to allow gopls to process them
