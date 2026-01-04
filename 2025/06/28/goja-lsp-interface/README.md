# Goja LSP Interface

A Golang JavaScript interface to the Language Server Protocol (LSP) using the Goja JavaScript engine. This project provides a bridge between JavaScript and LSP servers, allowing you to interact with language servers like `gopls` from JavaScript code.

## Overview

This project implements a complete LSP client in Go and exposes it to JavaScript through the Goja JavaScript engine. It enables JavaScript applications to leverage the power of Language Server Protocol for code intelligence features such as:

- **Hover Information**: Get documentation and type information for symbols
- **Code Completion**: Intelligent code completion suggestions
- **Go to Definition**: Navigate to symbol definitions
- **Find References**: Find all references to a symbol
- **Document Symbols**: Get document structure and symbols
- **Diagnostics**: Real-time error and warning reporting

## Architecture

The project is structured in several layers:

### Core Components

1. **LSP Client (`pkg/lsp`)**: Pure Go implementation of LSP client
   - JSON-RPC communication over stdio
   - Complete LSP protocol support
   - Type-safe message handling
   - Connection management and error handling

2. **JavaScript Bindings (`pkg/jslsp`)**: Goja-based JavaScript interface
   - JavaScript-friendly API wrappers
   - Type conversion between Go and JavaScript
   - Async operation support
   - Error handling and logging

3. **Demo Applications**: Example Go code for testing LSP functionality
   - Sample Go packages with various language features
   - Comprehensive test cases for LSP operations

### Project Structure

```
goja-lsp-interface/
├── pkg/
│   ├── lsp/           # Core LSP client implementation
│   │   ├── types.go   # LSP protocol types and structures
│   │   ├── client.go  # LSP client with JSON-RPC communication
│   │   ├── utils.go   # Utility functions and helpers
│   │   └── utils_test.go # Unit tests
│   └── jslsp/         # JavaScript bindings
│       └── jslsp.go   # Goja JavaScript interface
├── demo/              # Demo Go code for testing
│   ├── pkg/
│   │   ├── models/    # Sample data models
│   │   └── calculator/ # Sample calculator package
│   └── cmd/app/       # Sample application
├── cmd/               # Command-line tools and examples
│   ├── test-lsp/      # LSP client test program
│   ├── js-lsp-test/   # JavaScript LSP test
│   ├── js-example-runner/ # JavaScript example runner
│   └── simple-test/   # Simple communication test
├── examples/          # JavaScript usage examples
│   ├── simple-demo.js # Basic functionality demo
│   ├── lsp-example.js # Complete LSP usage example
│   └── advanced-lsp-example.js # Advanced usage patterns
└── README.md          # This file
```

## Installation

### Prerequisites

- Go 1.23+ (latest version recommended)
- A language server (e.g., `gopls` for Go)

### Setup

1. Clone the repository:
```bash
git clone <repository-url>
cd goja-lsp-interface
```

2. Install dependencies:
```bash
go mod tidy
```

3. Install gopls (for Go language support):
```bash
go install golang.org/x/tools/gopls@latest
```

4. Verify installation:
```bash
gopls version
```

## Usage

### Basic JavaScript Example

```javascript
// Create an LSP client
var client = createLSPClient({
    command: "gopls",
    args: [],
    rootPath: "/path/to/your/go/project",
    debugMode: false
});

// Initialize the client
client.Initialize();

// Open a Go file
var openFile = client.OpenFile("/path/to/file.go");

// Get hover information
var hover = client.GetHover(openFile.uri, 10, 5);
if (hover) {
    console.log("Hover info:", hover.text);
}

// Get code completions
var completions = client.GetCompletion(openFile.uri, 15, 10);
console.log("Completions:", completions.length);

// Find definitions
var definitions = client.GetDefinition(openFile.uri, 20, 8);
console.log("Definitions:", definitions);

// Find references
var references = client.GetReferences(openFile.uri, 20, 8, true);
console.log("References:", references);

// Clean up
client.Close();
```

### Running Examples

1. **Simple Demo**: Basic functionality without LSP server communication
```bash
cd cmd/js-example-runner
go run main.go
```

2. **Full LSP Test**: Complete LSP functionality test
```bash
cd cmd/js-lsp-test
go run main.go
```

3. **Go LSP Client Test**: Direct Go client testing
```bash
cd cmd/test-lsp
go run main.go
```

## API Reference

### JavaScript API

#### `createLSPClient(options)`

Creates a new LSP client instance.

**Parameters:**
- `options` (Object):
  - `command` (string): Language server command (e.g., "gopls")
  - `args` (Array): Command arguments
  - `rootPath` (string): Project root path
  - `debugMode` (boolean): Enable debug logging
  - `workingDir` (string): Working directory

**Returns:** LSP client instance

#### LSP Client Methods

##### `Initialize()`
Initializes the LSP client and establishes communication with the language server.

##### `OpenFile(path)`
Opens a file in the LSP session.

**Parameters:**
- `path` (string): Absolute file path

**Returns:** Object with file information:
```javascript
{
    uri: "file:///path/to/file.go",
    path: "/path/to/file.go", 
    languageId: "go",
    version: 1,
    content: "file content..."
}
```

##### `GetHover(uri, line, character)`
Gets hover information for a position in a file.

**Parameters:**
- `uri` (string): File URI
- `line` (number): Line number (0-based)
- `character` (number): Character position (0-based)

**Returns:** Hover information object or null

##### `GetCompletion(uri, line, character)`
Gets code completion suggestions for a position.

**Parameters:**
- `uri` (string): File URI
- `line` (number): Line number (0-based)
- `character` (number): Character position (0-based)

**Returns:** Array of completion items

##### `GetDefinition(uri, line, character)`
Gets definition locations for a symbol.

**Parameters:**
- `uri` (string): File URI
- `line` (number): Line number (0-based)
- `character` (number): Character position (0-based)

**Returns:** Array of location objects

##### `GetReferences(uri, line, character, includeDeclaration)`
Gets reference locations for a symbol.

**Parameters:**
- `uri` (string): File URI
- `line` (number): Line number (0-based)
- `character` (number): Character position (0-based)
- `includeDeclaration` (boolean): Include declaration in results

**Returns:** Array of location objects

##### `GetCapabilities()`
Gets the language server capabilities.

**Returns:** Object with server capabilities

##### `IsInitialized()`
Checks if the client is initialized.

**Returns:** Boolean

##### `GetOpenFiles()`
Gets list of currently open files.

**Returns:** Array of file information objects

##### `CloseFile(uri)`
Closes a file in the LSP session.

**Parameters:**
- `uri` (string): File URI

##### `Close()`
Closes the LSP client and terminates the language server.

### Utility Functions

#### `fileToURI(path)`
Converts a file path to a URI.

#### `uriToFile(uri)`
Converts a URI to a file path.

#### `getLanguageID(path)`
Gets the language ID from a file extension.

#### `readFile(path)`
Reads file content as a string.

## Testing

### Unit Tests

Run the Go unit tests:

```bash
cd pkg/lsp
go test -v
```

### Integration Tests

Test the complete JavaScript interface:

```bash
cd cmd/js-example-runner
go run main.go
```

### Manual Testing

Test individual components:

```bash
# Test LSP client directly
cd cmd/test-lsp
go run main.go

# Test JavaScript interface
cd cmd/js-lsp-test  
go run main.go
```

## Supported Language Servers

The interface is designed to work with any LSP-compliant language server. Tested with:

- **gopls**: Go language server
- **typescript-language-server**: TypeScript/JavaScript
- **python-lsp-server**: Python
- **rust-analyzer**: Rust

To use with other language servers, simply change the `command` parameter when creating the client.

## Error Handling

The JavaScript interface provides comprehensive error handling:

```javascript
try {
    var client = createLSPClient({
        command: "gopls",
        rootPath: "/invalid/path"
    });
    
    client.Initialize();
    
    // LSP operations...
    
} catch (error) {
    console.error("LSP Error:", error.message);
} finally {
    if (client) {
        client.Close();
    }
}
```

## Performance Considerations

- **Connection Reuse**: Keep the LSP client alive for multiple operations
- **File Management**: Close files when no longer needed
- **Batch Operations**: Group related LSP requests when possible
- **Memory Usage**: Monitor memory usage with large projects

## Troubleshooting

### Common Issues

1. **Language Server Not Found**
   - Ensure the language server is installed and in PATH
   - Check the command name and arguments

2. **Permission Errors**
   - Verify file and directory permissions
   - Check that the language server has access to the project files

3. **Timeout Issues**
   - Increase timeout values for large projects
   - Check network connectivity if using remote servers

4. **Memory Issues**
   - Close unused files with `CloseFile()`
   - Restart the client periodically for long-running applications

### Debug Mode

Enable debug mode for detailed logging:

```javascript
var client = createLSPClient({
    command: "gopls",
    rootPath: "/path/to/project",
    debugMode: true  // Enable detailed logging
});
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

### Development Setup

```bash
# Clone and setup
git clone <repository-url>
cd goja-lsp-interface
go mod tidy

# Run tests
go test ./...

# Run examples
cd cmd/js-example-runner
go run main.go
```

## License

This project is licensed under the MIT License. See LICENSE file for details.

## Acknowledgments

- [Goja](https://github.com/dop251/goja) - JavaScript engine for Go
- [gopls](https://pkg.go.dev/golang.org/x/tools/gopls) - Go language server
- [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) - Protocol specification

## Related Projects

- [LSP Specification](https://microsoft.github.io/language-server-protocol/)
- [Goja JavaScript Engine](https://github.com/dop251/goja)
- [Go Tools](https://pkg.go.dev/golang.org/x/tools)

