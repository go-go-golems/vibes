# Cline Go File Editor

A Go implementation of Cline's sophisticated file editing tools, designed for LLM agent integration and automated code modification.

## Features

- 🔍 **Advanced Diff Processing**: Multiple matching strategies (exact, line-trimmed, block anchor)
- 🔄 **Out-of-Order Replacements**: Handles non-sequential edit operations
- 🛡️ **Robust Error Handling**: Prevents file corruption and provides detailed error messages
- 🚀 **High Performance**: Efficient processing of large files and many operations
- 🤖 **LLM Integration**: Ready-to-use Gemini API client with function calling
- 🧪 **Thoroughly Tested**: Comprehensive test suite with real-world scenarios

## Installation

```bash
go get github.com/cline-go/file-editor
```

## Quick Start

### Basic File Operations

```go
package main

import (
    "fmt"
    "log"
    
    fileeditor "github.com/cline-go/file-editor"
)

func main() {
    // Create file editor for current directory
    editor := fileeditor.NewFileEditor(".")
    
    // Create a new file
    content := `package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}`
    
    err := editor.WriteToFile("hello.go", content)
    if err != nil {
        log.Fatal(err)
    }
    
    // Modify the file using SEARCH/REPLACE
    diff := `------- SEARCH
    fmt.Println("Hello, World!")
=======
    fmt.Println("Hello, Go!")
    fmt.Println("Welcome to file editing!")
+++++++ REPLACE`
    
    err = editor.ReplaceInFile("hello.go", diff)
    if err != nil {
        log.Fatal(err)
    }
    
    // Read the modified content
    result, err := editor.ReadFile("hello.go")
    if err != nil {
        log.Fatal(err)
    }
    
    fmt.Println("Modified file:")
    fmt.Println(result)
}
```

### LLM Integration with Gemini

```go
package main

import (
    "fmt"
    "log"
    
    fileeditor "github.com/cline-go/file-editor"
)

func main() {
    apiKey := "your-gemini-api-key"
    workingDir := "./my-project"
    
    // Create Gemini agent with file editing capabilities
    agent := fileeditor.NewGeminiAgent(apiKey, workingDir)
    
    systemPrompt := `You are a helpful coding assistant with file editing capabilities.
You can create, read, and modify files using the available tools.`
    
    userMessage := "Create a simple HTTP server in Go with a health check endpoint"
    
    response, err := agent.Chat(systemPrompt, userMessage)
    if err != nil {
        log.Fatal(err)
    }
    
    fmt.Println("AI Response:", response)
    
    // List created files
    files, err := agent.ExecuteFunction(fileeditor.FunctionCall{
        Name: "list_files",
        Args: map[string]interface{}{
            "path":      ".",
            "recursive": false,
        },
    })
    if err != nil {
        log.Fatal(err)
    }
    
    fmt.Println("Created files:", files["files"])
}
```

## SEARCH/REPLACE Format

The file editor uses Cline's SEARCH/REPLACE block format:

```
------- SEARCH
[exact content to find]
=======
[new content to replace with]
+++++++ REPLACE
```

### Key Features:

- **Exact matching**: Content must match exactly including whitespace
- **Multiple blocks**: Apply multiple replacements in one operation
- **Out-of-order**: Replacements can be applied in any order
- **Flexible markers**: Supports various marker lengths (minimum 3 characters)

### Example with Multiple Replacements:

```
------- SEARCH
function oldFunction() {
    return "old";
}
=======
function newFunction() {
    return "new";
}
+++++++ REPLACE
------- SEARCH
const oldVariable = "old";
=======
const newVariable = "new";
+++++++ REPLACE
```

## API Reference

### FileEditor

```go
type FileEditor struct {
    // Contains working directory and security settings
}

// Create new file editor
func NewFileEditor(workingDir string) *FileEditor

// File operations
func (fe *FileEditor) WriteToFile(path, content string) error
func (fe *FileEditor) ReadFile(path string) (string, error)
func (fe *FileEditor) ReplaceInFile(path, diff string) error
func (fe *FileEditor) ListFiles(path string, recursive bool) ([]string, error)
func (fe *FileEditor) FileExists(path string) bool
```

### GeminiAgent

```go
type GeminiAgent struct {
    // Contains API client and file editor
}

// Create new Gemini agent
func NewGeminiAgent(apiKey, workingDir string) *GeminiAgent

// Chat with AI using file editing tools
func (ga *GeminiAgent) Chat(systemPrompt, userMessage string) (string, error)

// Execute function calls directly
func (ga *GeminiAgent) ExecuteFunction(call FunctionCall) (map[string]interface{}, error)
```

## Testing

Run the comprehensive test suite:

```bash
# Run all tests
go test -v

# Run specific test categories
go test -v -run TestFileEditor
go test -v -run TestDiffEdgeCases

# Run benchmarks
go test -bench=.

# Run real-world scenario tests
go run cmd/realworld-test/main.go

# Run manual validation tests
go run cmd/manual-test/main.go
```

## Performance

The implementation is optimized for:

- **Large files**: Tested with 1MB+ files
- **Many operations**: 100+ sequential edits on single file
- **Complex projects**: Multi-file projects with various languages
- **Memory efficiency**: Streaming processing for large content

### Benchmarks

```
BenchmarkDiffProcessing-8    	    5000	    250000 ns/op
BenchmarkFileOperations-8    	   10000	    150000 ns/op
```

## Error Handling

The implementation provides robust error handling:

```go
err := editor.ReplaceInFile("file.go", invalidDiff)
if err != nil {
    // File remains unchanged on error
    fmt.Printf("Edit failed: %v\n", err)
}
```

Common error types:
- `SearchContentNotFound`: SEARCH block doesn't match file content
- `InvalidDiffFormat`: Malformed SEARCH/REPLACE blocks
- `FileNotFound`: Target file doesn't exist
- `SecurityViolation`: Path traversal attempt

## Security

- **Path traversal protection**: Prevents access outside working directory
- **Input validation**: Validates all file paths and content
- **Safe operations**: Atomic file operations prevent corruption
- **Error isolation**: Failed operations don't affect other files

## Compatibility

- **100% compatible** with Cline's diff format
- **Drop-in replacement** for Cline's file editing tools
- **Same API surface** for LLM integration
- **Cross-platform**: Works on Windows, macOS, and Linux

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

MIT License - see LICENSE file for details.

## Acknowledgments

- **Cline Project**: Original implementation and algorithms
- **Anthropic**: SEARCH/REPLACE block format inspiration
- **Go Community**: Excellent standard library and tooling

---

For more examples and detailed documentation, see the [examples](examples/) directory and [API documentation](https://pkg.go.dev/github.com/cline-go/file-editor).

