---
Title: Tree-sitter Static Analysis Deep Dive
Ticket: ANALYZE-PR-ANALYZER
Status: active
Topics:
    - analysis
    - go
    - cli
DocType: analysis
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2025-07-29/pr-analyzer-dual/cmd/analyze/function_history_dual.go
      Note: Command tracking function evolution across commits using tree-sitter parsing
    - Path: 2025-07-29/pr-analyzer-dual/cmd/analyze/functions_dual.go
      Note: Command using tree-sitter to extract and analyze functions from PR changes
    - Path: 2025-07-29/pr-analyzer-dual/cmd/get/context_dual.go
      Note: Command using tree-sitter to provide file context and function statistics
    - Path: 2025-07-29/pr-analyzer-dual/go.mod
      Note: Tree-sitter dependency (github.com/smacker/go-tree-sitter)
    - Path: 2025-07-29/pr-analyzer-dual/internal/analysis/function_analysis.go
      Note: Function change detection logic that integrates with tree-sitter parser
    - Path: 2025-07-29/pr-analyzer-dual/internal/treesitter/parser.go
      Note: Core tree-sitter parser implementation with AST traversal and function extraction
ExternalSources: []
Summary: 'Deep analysis of tree-sitter integration for static analysis: AST parsing, function extraction, change detection, and integration patterns'
LastUpdated: 2025-12-03T13:05:45.291377118-05:00
---


# Tree-sitter Static Analysis Deep Dive

## Executive Summary

The pr-analyzer-dual project leverages **tree-sitter** for accurate, AST-based static analysis of Go code. Unlike regex-based approaches, tree-sitter provides a robust, language-aware parser that understands Go syntax structure, enabling precise function extraction, signature parsing, and change detection. This document provides a comprehensive analysis of how tree-sitter is integrated, how it works, and how it enables sophisticated code analysis capabilities.

## Tree-sitter Overview

**Tree-sitter** is an incremental parsing library that builds concrete syntax trees (CST) for source code. Key advantages:

- **Language-aware parsing**: Understands Go syntax rules, not just text patterns
- **Incremental parsing**: Efficient updates when code changes
- **Robust error handling**: Gracefully handles syntax errors
- **Field-based navigation**: Named fields for language constructs (e.g., `name`, `body`, `receiver`)

**Go Bindings**: The project uses `github.com/smacker/go-tree-sitter` with the `golang` language grammar.

## Architecture: Parser Wrapper

### Core Parser Type

```go
type Parser struct {
    parser *sitter.Parser
}
```

**Design Pattern**: The `Parser` struct wraps the underlying `sitter.Parser`, providing:
- **Abstraction**: Hides tree-sitter implementation details
- **Go-specific API**: Methods tailored for function extraction
- **Error handling**: Consistent error wrapping
- **Resource management**: Proper AST tree cleanup

### Initialization

```go
func NewParser() *Parser {
    parser := sitter.NewParser()
    parser.SetLanguage(golang.GetLanguage())
    return &Parser{parser: parser}
}
```

**Key Steps**:
1. Create new tree-sitter parser instance
2. Set language to Go grammar (`golang.GetLanguage()`)
3. Return wrapped parser

**Language Grammar**: The `golang` package provides the Go language grammar definition, which tree-sitter uses to parse Go source code according to the official Go specification.

## Function Data Model

### Function Struct

```go
type Function struct {
    Name       string  // Function name
    StartLine  int     // Starting line (1-based)
    EndLine    int     // Ending line (1-based)
    StartByte  int     // Starting byte offset
    EndByte    int     // Ending byte offset
    Signature  string  // Complete function signature
    Body       string  // Function body code
    Receiver   string  // Method receiver (empty for functions)
    IsExported bool    // Whether function is exported (public)
}
```

**Design Decisions**:
- **Line-based coordinates**: Uses 1-based line numbers (tree-sitter uses 0-based, converted)
- **Byte offsets**: Preserves exact source positions for precise extraction
- **Signature vs Body**: Separates signature (parameters, return types) from implementation
- **Receiver field**: Distinguishes methods from functions
- **Export detection**: Simple uppercase check (Go convention)

## AST Parsing Pipeline

### Step 1: Parse Source Code

```go
func (p *Parser) ParseCode(sourceCode []byte) (*sitter.Tree, error) {
    tree, err := p.parser.ParseCtx(context.Background(), nil, sourceCode)
    if err != nil {
        return nil, fmt.Errorf("failed to parse code: %w", err)
    }
    return tree, nil
}
```

**Process**:
- Takes raw Go source code as `[]byte`
- Calls `ParseCtx()` with context (enables cancellation)
- Returns `*sitter.Tree` (the AST) or error
- **Error handling**: Wraps tree-sitter errors with context

**Tree Lifecycle**: The returned tree must be closed with `tree.Close()` to free resources.

### Step 2: Extract Functions

```go
func (p *Parser) ExtractFunctions(sourceCode []byte) ([]*Function, error) {
    tree, err := p.ParseCode(sourceCode)
    if err != nil {
        return nil, err
    }
    defer tree.Close()  // Ensure cleanup
    
    var functions []*Function
    p.traverseNode(tree.RootNode(), sourceCode, &functions)
    return functions, nil
}
```

**Control Flow**:
1. Parse code to AST
2. Defer tree cleanup (critical for memory management)
3. Traverse AST starting from root node
4. Collect functions in slice
5. Return extracted functions

## AST Traversal Algorithm

### Recursive Traversal

```go
func (p *Parser) traverseNode(node *sitter.Node, sourceCode []byte, functions *[]*Function) {
    nodeType := node.Type()
    
    if nodeType == "function_declaration" {
        fn := p.extractFunctionFromNode(node, sourceCode)
        if fn != nil {
            *functions = append(*functions, fn)
        }
    } else if nodeType == "method_declaration" {
        fn := p.extractMethodFromNode(node, sourceCode)
        if fn != nil {
            *functions = append(*functions, fn)
        }
    }
    
    // Recursively traverse child nodes
    for i := 0; i < int(node.ChildCount()); i++ {
        child := node.Child(i)
        if child != nil {
            p.traverseNode(child, sourceCode, functions)
        }
    }
}
```

**Traversal Strategy**:
- **Depth-first search**: Recursively visits all nodes
- **Node type checking**: Identifies `function_declaration` and `method_declaration` nodes
- **Source code preservation**: Passes `sourceCode` for content extraction
- **Accumulation pattern**: Appends found functions to slice

**Tree-sitter Node Types**:
- `function_declaration`: Standalone functions (`func Foo() {}`)
- `method_declaration`: Methods with receivers (`func (r Receiver) Foo() {}`)

### Why Manual Traversal?

The code comment says "Use a simpler approach - traverse the tree manually" instead of using tree-sitter query API. This suggests:
- **Simplicity**: Direct traversal is easier to understand
- **Control**: Full control over traversal logic
- **Performance**: Potentially faster than query compilation
- **Flexibility**: Easy to extend for other node types

## Function Extraction Details

### Extracting Functions

```go
func (p *Parser) extractFunctionFromNode(node *sitter.Node, sourceCode []byte) *Function {
    // Extract name
    nameNode := node.ChildByFieldName("name")
    name := nameNode.Content(sourceCode)
    
    // Get line numbers (convert from 0-based to 1-based)
    startPoint := node.StartPoint()
    endPoint := node.EndPoint()
    
    // Extract signature and body
    signature := p.extractSignature(node, sourceCode)
    body := p.extractBody(node, sourceCode)
    
    return &Function{
        Name:       name,
        StartLine:  int(startPoint.Row) + 1,
        EndLine:    int(endPoint.Row) + 1,
        StartByte:  int(node.StartByte()),
        EndByte:    int(node.EndByte()),
        Signature:  signature,
        Body:       body,
        IsExported: isExported(name),
    }
}
```

**Key Operations**:
1. **Field-based access**: `ChildByFieldName("name")` - tree-sitter provides named fields
2. **Content extraction**: `node.Content(sourceCode)` - extracts text from source
3. **Coordinate conversion**: `Row + 1` converts 0-based to 1-based line numbers
4. **Byte offsets**: `StartByte()`/`EndByte()` for precise source positions

### Extracting Methods

```go
func (p *Parser) extractMethodFromNode(node *sitter.Node, sourceCode []byte) *Function {
    // Extract name (same as function)
    nameNode := node.ChildByFieldName("name")
    name := nameNode.Content(sourceCode)
    
    // Extract receiver (unique to methods)
    receiverNode := node.ChildByFieldName("receiver")
    receiver := receiverNode.Content(sourceCode)
    
    // ... rest similar to function extraction
    return &Function{
        // ... same fields plus Receiver
        Receiver: receiver,
    }
}
```

**Difference**: Methods have a `receiver` field that functions don't. The extraction logic is nearly identical, with receiver extraction as the key difference.

## Signature and Body Extraction

### Signature Extraction

```go
func (p *Parser) extractSignature(node *sitter.Node, sourceCode []byte) string {
    bodyNode := node.ChildByFieldName("body")
    if bodyNode == nil {
        return node.Content(sourceCode)  // Fallback: entire node
    }
    
    // Extract from start of function to start of body
    startByte := node.StartByte()
    endByte := bodyNode.StartByte()
    
    if endByte > startByte {
        return string(sourceCode[startByte:endByte])
    }
    
    return node.Content(sourceCode)
}
```

**Strategy**: 
- Finds the `body` field node
- Extracts everything from function start to body start
- This includes: `func`, name, parameters, return types
- **Fallback**: If no body node, returns entire node content

**Example**: For `func Foo(x int) string { return "" }`, extracts `func Foo(x int) string `.

### Body Extraction

```go
func (p *Parser) extractBody(node *sitter.Node, sourceCode []byte) string {
    bodyNode := node.ChildByFieldName("body")
    if bodyNode == nil {
        return ""
    }
    return bodyNode.Content(sourceCode)
}
```

**Simple approach**: Directly extracts the body node content, which includes the braces `{ ... }`.

## Change Detection Algorithm

### Finding Changed Functions

```go
func (p *Parser) GetChangedFunctions(sourceCode []byte, changedLines []int) ([]*Function, error) {
    // Extract all functions
    functions, err := p.ExtractFunctions(sourceCode)
    if err != nil {
        return nil, err
    }
    
    var changedFunctions []*Function
    functionMap := make(map[string]*Function)
    
    // Find functions that contain changed lines
    for _, line := range changedLines {
        fn := p.FindFunctionAtLine(functions, line)
        if fn != nil {
            // Use function signature as key to avoid duplicates
            key := fmt.Sprintf("%s:%d", fn.Name, fn.StartLine)
            if _, exists := functionMap[key]; !exists {
                functionMap[key] = fn
                changedFunctions = append(changedFunctions, fn)
            }
        }
    }
    
    return changedFunctions, nil
}
```

**Algorithm**:
1. Extract all functions from source code
2. For each changed line number:
   - Find function containing that line (`FindFunctionAtLine`)
   - Add to changed functions list (deduplicated by name:startLine)
3. Return unique changed functions

**Deduplication**: Uses `name:startLine` as key because:
- Same function name could appear multiple times (different scopes)
- Start line uniquely identifies a function instance
- Prevents counting same function multiple times

### Line-to-Function Mapping

```go
func (p *Parser) FindFunctionAtLine(functions []*Function, line int) *Function {
    for _, fn := range functions {
        if line >= fn.StartLine && line <= fn.EndLine {
            return fn
        }
    }
    return nil
}
```

**Simple range check**: Checks if line falls within function's line range (inclusive). Returns first matching function.

**Edge Cases**:
- Line before any function: Returns `nil`
- Line between functions: Returns function containing that line
- Line after all functions: Returns `nil`

## Integration with Diff Analysis

### Change Detection Flow

The tree-sitter parser integrates with diff analysis to identify which functions changed:

```
1. Parse unified diff → Extract changed line numbers per file
2. For each Go file:
   a. Fetch file content from GitHub
   b. Parse with tree-sitter → Extract all functions
   c. Match changed lines to functions → Mark changed functions
3. Report changed functions with metadata
```

**Integration Point**: `internal/analysis/function_analysis.go`:

```go
func IsFunctionChanged(fn *treesitter.Function, changedLines []int) bool {
    for _, lineNo := range changedLines {
        if lineNo >= fn.StartLine && lineNo <= fn.EndLine {
            return true
        }
    }
    return false
}
```

**Logic**: If any changed line falls within function's line range, function is considered changed.

## Usage Patterns in Commands

### Pattern 1: Extract All Functions

**Used in**: `analyze functions`, `get context`

```go
parser := treesitter.NewParser()
functions, err := parser.ExtractFunctions([]byte(content))
if err != nil {
    // Handle error, continue with other files
    continue
}

for _, fn := range functions {
    isChanged := analysis.IsFunctionChanged(fn, fileChange.ChangedLines)
    // Process function...
}
```

**Pattern**: Extract once, iterate and check change status.

### Pattern 2: Function History Tracking

**Used in**: `analyze function-history`

```go
// For each commit in history:
content, err := client.GetFileContent(ctx, owner, repo, filePath, sha)
functions, err := parser.ExtractFunctions([]byte(content))

// Find specific function by name
var targetFunction *treesitter.Function
for _, fn := range functions {
    if fn.Name == functionName {
        targetFunction = fn
        break
    }
}

// Compare with previous version
if targetFunction != nil {
    // Show function evolution
}
```

**Pattern**: Parse each commit version, find function by name, track changes over time.

### Pattern 3: Change Detection

**Used in**: `analyze functions --only-changed`

```go
parser := treesitter.NewParser()
functions, err := parser.ExtractFunctions([]byte(content))

for _, fn := range functions {
    isChanged := analysis.IsFunctionChanged(fn, fileChange.ChangedLines)
    
    if s.OnlyChanged && !isChanged {
        continue  // Skip unchanged functions
    }
    
    // Process changed function...
}
```

**Pattern**: Extract all functions, filter by change status.

## Error Handling Strategy

### Parsing Errors

**Graceful Degradation**: Parsing errors don't stop the entire analysis:

```go
functions, err := parser.ExtractFunctions([]byte(content))
if err != nil {
    fmt.Printf("## ❌ %s\n*Could not parse Go functions: %v*\n\n", 
                fileChange.FilePath, err)
    continue  // Skip this file, continue with others
}
```

**Rationale**:
- Some files may have syntax errors
- Large files might exceed memory limits
- Invalid Go code shouldn't break entire PR analysis
- User gets partial results rather than complete failure

### Missing Files

**Per-file Error Handling**: Each file is processed independently:

```go
content, err := client.GetFileContent(ctx, owner, repo, filePath, "")
if err != nil {
    fmt.Printf("## ❌ %s\n*Could not retrieve file content: %v*\n\n", 
                filePath, err)
    continue
}
```

**Benefits**: One failed file doesn't prevent analysis of other files.

## Performance Considerations

### Memory Management

**Tree Cleanup**: AST trees must be closed:

```go
tree, err := p.ParseCode(sourceCode)
if err != nil {
    return nil, err
}
defer tree.Close()  // Critical: free tree-sitter resources
```

**Why Important**: Tree-sitter trees allocate native memory. Without cleanup, memory leaks occur.

### Parsing Efficiency

**Incremental Parsing**: Tree-sitter supports incremental parsing, but this codebase doesn't use it. Each file is parsed from scratch.

**Potential Optimization**: Could cache parsed trees and incrementally update them, but current approach is simpler and sufficient for PR analysis (one-time parsing per file).

### Large File Handling

**Limitations**: Tree-sitter has memory limits for very large files. The code handles this gracefully by catching errors and continuing.

## Tree-sitter Node Structure

### Function Declaration Node Structure

Tree-sitter Go grammar provides these fields for `function_declaration`:

- `name`: Function name node
- `parameters`: Parameter list node
- `result`: Return type node (optional)
- `body`: Function body node

**Access Pattern**: `node.ChildByFieldName("name")` accesses named fields directly.

### Method Declaration Node Structure

For `method_declaration`, additional field:

- `receiver`: Method receiver node

**Access Pattern**: Same as function, plus receiver extraction.

## Comparison with Alternative Approaches

### Regex-Based Parsing

**Why tree-sitter is better**:
- **Syntax awareness**: Understands Go grammar, not just patterns
- **Robustness**: Handles edge cases (nested functions, complex signatures)
- **Accuracy**: Correctly identifies function boundaries
- **Maintainability**: Grammar updates handle language changes

**Regex limitations**:
- Can't handle nested structures reliably
- Breaks on complex signatures
- Hard to maintain as language evolves
- False positives/negatives

### go/ast Package

**Why tree-sitter instead of go/ast**:
- **External code**: Can parse code from GitHub without local checkout
- **Language agnostic**: Could extend to other languages
- **Incremental parsing**: Built-in support (though not used here)
- **Error tolerance**: Handles syntax errors gracefully

**go/ast advantages**:
- Native Go tooling
- Type information
- More semantic analysis capabilities

**Trade-off**: Tree-sitter chosen for flexibility and external code parsing.

## Limitations and Edge Cases

### Known Limitations

1. **No type information**: Only extracts signatures, not types
2. **No semantic analysis**: Doesn't understand function calls, dependencies
3. **No import analysis**: Doesn't track imported packages
4. **No comment extraction**: Comments are not extracted
5. **No struct/interface analysis**: Only functions/methods

### Edge Cases Handled

1. **Empty functions**: `func Foo() {}` - body is empty string
2. **Functions without bodies**: Interface methods - handled by fallback
3. **Nested functions**: Not extracted (only top-level)
4. **Function literals**: Not extracted (only declarations)
5. **Multiple functions with same name**: Deduplicated by start line

### Unhandled Edge Cases

1. **Very large files**: May exceed tree-sitter memory limits (handled by error)
2. **Syntax errors**: Parsing fails, file skipped
3. **Generated code**: Parsed like regular code (no special handling)

## Extension Points

### Adding New Node Types

To extract other constructs (e.g., structs, interfaces):

```go
func (p *Parser) traverseNode(node *sitter.Node, sourceCode []byte, functions *[]*Function) {
    nodeType := node.Type()
    
    if nodeType == "function_declaration" {
        // ... existing code
    } else if nodeType == "type_declaration" {
        // NEW: Extract type declarations
        typeDecl := p.extractTypeFromNode(node, sourceCode)
        // ... process type
    }
    
    // ... recursive traversal
}
```

### Enhanced Function Analysis

Could extend `Function` struct with:
- **Parameter names and types**: Parse parameter list
- **Return types**: Extract return type information
- **Documentation**: Extract preceding comments
- **Complexity metrics**: Calculate cyclomatic complexity

### Multi-language Support

Tree-sitter supports many languages. Could extend parser to:
- Support multiple languages
- Language detection
- Language-specific extraction logic

## Testing Considerations

**Current State**: No unit tests found for tree-sitter parser.

**Recommended Tests**:
1. **Function extraction**: Test various function signatures
2. **Method extraction**: Test receivers (value, pointer)
3. **Edge cases**: Empty functions, interface methods
4. **Error handling**: Invalid syntax, large files
5. **Change detection**: Line range matching accuracy

## Summary

The tree-sitter integration provides a robust foundation for static analysis:

**Strengths**:
- ✅ Accurate AST-based parsing
- ✅ Handles Go syntax correctly
- ✅ Graceful error handling
- ✅ Clean abstraction layer
- ✅ Efficient for PR analysis use case

**Areas for Enhancement**:
- 🔄 Add unit tests
- 🔄 Extract more information (types, comments)
- 🔄 Support nested functions
- 🔄 Add semantic analysis capabilities
- 🔄 Optimize for very large codebases

The current implementation strikes a good balance between simplicity and functionality, providing accurate function extraction for PR analysis while remaining maintainable and extensible.

