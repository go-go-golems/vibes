---
Title: Migrating pr-analyzer to Oak Query Templates
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
    - Path: ../../../../../../../oak/cmd/oak/queries/go/definitions.yaml
      Note: Example Oak query file showing function/method extraction with template variables
    - Path: ../../../../../../../oak/pkg/api/query_builder.go
      Note: Oak QueryBuilder API for programmatic query execution
    - Path: ../../../../../../../oak/pkg/cmds/cmd.go
      Note: Oak command structure with query template rendering (RenderQueries)
    - Path: ../../../../../../../oak/pkg/lang.go
      Note: Language name to tree-sitter language mapping utilities
    - Path: ../../../../../../../oak/pkg/tree-sitter/tree-sitter.go
      Note: Oak's SitterQuery struct and QueryResults type definitions
    - Path: 2025-07-29/pr-analyzer-dual/cmd/analyze/functions_dual.go
      Note: Command using current parser that would benefit from Oak migration
    - Path: 2025-07-29/pr-analyzer-dual/internal/analysis/function_analysis.go
      Note: Function change detection that would work with Oak query results
    - Path: 2025-07-29/pr-analyzer-dual/internal/treesitter/parser.go
      Note: Current manual AST traversal implementation that would be replaced by Oak queries
ExternalSources: []
Summary: 'Comprehensive analysis of migrating pr-analyzer from manual AST traversal to Oak''s query template system: architecture comparison, benefits, migration strategy, and implementation plan'
LastUpdated: 2025-12-03T13:23:06.370595375-05:00
---


# Migrating pr-analyzer to Oak Query Templates

## Executive Summary

This document analyzes the feasibility and benefits of migrating pr-analyzer-dual from its current manual AST traversal approach to Oak's query template system. Oak provides a declarative, template-based query system that would significantly simplify function extraction, enable dynamic query composition, and provide better maintainability. The migration would replace ~200 lines of manual traversal code with declarative tree-sitter queries and leverage Oak's proven query execution infrastructure.

## Current Implementation: Manual AST Traversal

### Architecture Overview

**Location**: `vibes/2025-07-29/pr-analyzer-dual/internal/treesitter/parser.go`

**Current Approach**:
- Manual recursive tree traversal (`traverseNode()`)
- Hard-coded node type checking (`function_declaration`, `method_declaration`)
- Manual field extraction using `ChildByFieldName()`
- Custom `Function` struct with manual population
- Line-based change detection using extracted line ranges

**Key Code Pattern**:
```go
func (p *Parser) traverseNode(node *sitter.Node, sourceCode []byte, functions *[]*Function) {
    nodeType := node.Type()
    
    if nodeType == "function_declaration" {
        fn := p.extractFunctionFromNode(node, sourceCode)
        // ... manual extraction
    } else if nodeType == "method_declaration" {
        fn := p.extractMethodFromNode(node, sourceCode)
        // ... manual extraction
    }
    
    // Recursively traverse all children
    for i := 0; i < int(node.ChildCount()); i++ {
        child := node.Child(i)
        if child != nil {
            p.traverseNode(child, sourceCode, functions)
        }
    }
}
```

### Limitations

1. **Hard-coded logic**: Function extraction logic is embedded in Go code
2. **No query reuse**: Cannot easily extract different patterns without code changes
3. **No template support**: Cannot parameterize queries (e.g., filter by name, export status)
4. **Maintenance burden**: Changes to extraction logic require code changes and recompilation
5. **Limited flexibility**: Adding new extraction patterns requires new traversal code
6. **No query composition**: Cannot combine multiple queries easily

## Oak Implementation: Query Template System

### Architecture Overview

**Location**: `oak/pkg/tree-sitter/tree-sitter.go` and `oak/pkg/cmds/cmd.go`

**Oak Approach**:
- **Declarative queries**: Tree-sitter query language in YAML files
- **Template rendering**: Go templates for dynamic query composition
- **Query execution**: `ExecuteQueries()` function handles all execution
- **Structured results**: `QueryResults` map with named query results
- **Capture-based extraction**: Named captures extract specific parts

**Key Components**:

1. **SitterQuery struct**: Named query with template support
2. **ExecuteQueries()**: Executes multiple queries on a tree
3. **RenderQueries()**: Renders query templates with parameters
4. **QueryResults**: Map of query name → Result (with Matches)

### Oak Query Structure

**YAML Format** (`oak/cmd/oak/queries/go/definitions.yaml`):
```yaml
language: go
queries:
  - name: functionDeclarations
    query: |
      ((comment)* @comment .
      (function_declaration
        name: (identifier) @name
        parameters: (parameter_list)? @parameters
        result: (_)? @result
        body: (block) @body)
        {{ if .name }}(#eq? @name "{{.name}}"){{end}}
        {{ if .only_public }}(#match? @name "^[A-Z]"){{end}}
      )
```

**Key Features**:
- **Named captures**: `@name`, `@parameters`, `@body` extract specific parts
- **Template conditionals**: `{{ if .only_public }}` enables dynamic filtering
- **Predicates**: `#eq?`, `#match?` filter matches at query time
- **Comments**: `(comment)* @comment` captures preceding comments

### Oak Query Execution Flow

```
1. Load YAML query file → Parse SitterQuery structs
2. RenderQueries() → Apply Go templates with parameters
3. ExecuteQueries() → Run tree-sitter queries on AST
4. Process QueryResults → Extract captures from matches
5. Template rendering → Format results (optional)
```

## Comparison: Current vs Oak Approach

### Code Complexity

| Aspect | Current (Manual) | Oak (Queries) |
|--------|------------------|---------------|
| **Function extraction** | ~200 lines Go code | ~10 lines query |
| **Method extraction** | Separate function | Same query pattern |
| **Filtering** | Post-processing | Query predicates |
| **Parameterization** | Not supported | Template variables |
| **Maintainability** | Code changes needed | Query file edits |
| **Extensibility** | New traversal code | New query entry |

### Functionality Comparison

| Feature | Current | Oak |
|---------|---------|-----|
| Extract functions | ✅ Manual traversal | ✅ Query with captures |
| Extract methods | ✅ Separate function | ✅ Query with receiver |
| Filter by name | ❌ Post-processing | ✅ Query predicate |
| Filter by export | ❌ Post-processing | ✅ Query predicate |
| Extract comments | ❌ Not supported | ✅ Query captures |
| Extract parameters | ⚠️ Manual extraction | ✅ Named capture |
| Extract return types | ⚠️ Manual extraction | ✅ Named capture |
| Template support | ❌ None | ✅ Go templates |
| Query composition | ❌ Not possible | ✅ Multiple queries |

### Performance Considerations

**Current Approach**:
- Single traversal pass
- O(n) where n = number of nodes
- Efficient but limited

**Oak Approach**:
- Query compilation overhead (one-time per query)
- Query execution uses tree-sitter's optimized query engine
- Multiple queries = multiple passes (but can be optimized)
- **Verdict**: Comparable performance, Oak may be slightly slower for single queries but more efficient for complex patterns

## Benefits of Migration

### 1. Declarative Query Language

**Current**: Logic embedded in Go code
```go
if nodeType == "function_declaration" {
    nameNode := node.ChildByFieldName("name")
    name := nameNode.Content(sourceCode)
    // ... 50+ more lines
}
```

**Oak**: Declarative query
```yaml
(function_declaration
  name: (identifier) @name
  parameters: (parameter_list)? @parameters
  body: (block) @body)
```

**Benefit**: Queries are self-documenting and easier to understand.

### 2. Template-Based Parameterization

**Current**: No parameterization - hard-coded logic
```go
// Cannot filter by name without code changes
```

**Oak**: Template variables enable dynamic queries
```yaml
{{ if .function_name }}(#eq? @name "{{.function_name}}"){{end}}
{{ if .only_public }}(#match? @name "^[A-Z]"){{end}}
```

**Benefit**: Same query file supports multiple use cases.

### 3. Query Composition

**Current**: Single extraction function
```go
ExtractFunctions() // Only functions
// Need separate code for methods, structs, etc.
```

**Oak**: Multiple queries in one file
```yaml
queries:
  - name: functionDeclarations
    query: ...
  - name: methodDeclarations
    query: ...
  - name: structDeclarations
    query: ...
```

**Benefit**: Extract multiple patterns in one pass.

### 4. Better Error Handling

**Current**: Manual error handling per extraction
```go
nameNode := node.ChildByFieldName("name")
if nameNode != nil {
    name = nameNode.Content(sourceCode)
}
```

**Oak**: Tree-sitter handles missing captures gracefully
```yaml
parameters: (parameter_list)? @parameters  # Optional capture
```

**Benefit**: More robust handling of edge cases.

### 5. Easier Testing

**Current**: Test Go code with sample files
- Requires Go test infrastructure
- Hard to test individual extraction logic

**Oak**: Test queries independently
- Query files can be tested with `oak query` command
- Easier to validate query correctness

### 6. Reusability

**Current**: Code is project-specific
- Cannot reuse extraction logic easily
- Tied to `Function` struct

**Oak**: Queries are reusable
- Same query file works across projects
- Results can be mapped to any struct

## Migration Strategy

### Phase 1: Create Oak Query Files

**Step 1.1**: Create query file for function extraction

**File**: `queries/go/functions.yaml`
```yaml
language: go
queries:
  - name: functions
    query: |
      (function_declaration
        name: (identifier) @name
        parameters: (parameter_list)? @parameters
        result: (_)? @result
        body: (block) @body)
        {{ if .only_public }}(#match? @name "^[A-Z]"){{end}}
        {{ if .function_name }}(#eq? @name "{{.function_name}}"){{end}}

  - name: methods
    query: |
      (method_declaration
        receiver: (parameter_list) @receiver
        name: (field_identifier) @name
        parameters: (parameter_list)? @parameters
        result: (_)? @result
        body: (block) @body)
        {{ if .only_public }}(#match? @name "^[A-Z]"){{end}}
        {{ if .function_name }}(#eq? @name "{{.function_name}}"){{end}}
        {{ if .receiver_type }}(#match? @receiver "{{.receiver_type}}"){{end}}
```

**Step 1.2**: Test queries with Oak CLI
```bash
oak query --query-file queries/go/functions.yaml --file test.go
```

### Phase 2: Integrate Oak Query Execution

**Step 2.1**: Add Oak dependency
```go
import (
    "github.com/go-go-golems/oak/pkg/tree-sitter"
    "github.com/go-go-golems/oak/pkg"
)
```

**Step 2.2**: Replace `ExtractFunctions()` with Oak queries

**Current**:
```go
func (p *Parser) ExtractFunctions(sourceCode []byte) ([]*Function, error) {
    tree, err := p.ParseCode(sourceCode)
    // ... manual traversal
}
```

**New**:
```go
func (p *Parser) ExtractFunctions(sourceCode []byte, options *FunctionOptions) ([]*Function, error) {
    tree, err := p.ParseCode(sourceCode)
    if err != nil {
        return nil, err
    }
    defer tree.Close()
    
    // Load queries from YAML
    queries, err := loadFunctionQueries(options)
    if err != nil {
        return nil, err
    }
    
    // Render queries with options
    lang := golang.GetLanguage()
    renderedQueries, err := renderQueries(queries, options)
    if err != nil {
        return nil, err
    }
    
    // Execute queries
    results, err := tree_sitter.ExecuteQueries(lang, tree.RootNode(), renderedQueries, sourceCode)
    if err != nil {
        return nil, err
    }
    
    // Convert results to Function structs
    return convertResultsToFunctions(results, sourceCode)
}
```

### Phase 3: Create Adapter Layer

**Step 3.1**: Convert Oak results to `Function` struct

```go
func convertResultsToFunctions(results tree_sitter.QueryResults, sourceCode []byte) ([]*Function, error) {
    var functions []*Function
    
    // Process function results
    if funcResult, ok := results["functions"]; ok {
        for _, match := range funcResult.Matches {
            fn := matchToFunction(match, sourceCode, false) // false = not a method
            functions = append(functions, fn)
        }
    }
    
    // Process method results
    if methodResult, ok := results["methods"]; ok {
        for _, match := range methodResult.Matches {
            fn := matchToFunction(match, sourceCode, true) // true = is a method
            functions = append(functions, fn)
        }
    }
    
    return functions, nil
}

func matchToFunction(match tree_sitter.Match, sourceCode []byte, isMethod bool) *Function {
    fn := &Function{}
    
    if name, ok := match["name"]; ok {
        fn.Name = name.Text
        fn.IsExported = isExported(name.Text)
    }
    
    if params, ok := match["parameters"]; ok {
        // Extract signature from parameters
    }
    
    if body, ok := match["body"]; ok {
        fn.Body = body.Text
    }
    
    if receiver, ok := match["receiver"]; ok {
        fn.Receiver = receiver.Text
    }
    
    // Extract line numbers from capture points
    if name, ok := match["name"]; ok {
        fn.StartLine = int(name.StartPoint.Row) + 1
    }
    
    // Calculate end line from body or result
    if body, ok := match["body"]; ok {
        fn.EndLine = int(body.EndPoint.Row) + 1
    }
    
    return fn
}
```

### Phase 4: Update Change Detection

**Step 4.1**: Keep `FindFunctionAtLine()` logic (still needed)

**Step 4.2**: Enhance with query-based filtering

```go
func (p *Parser) GetChangedFunctions(
    sourceCode []byte, 
    changedLines []int,
    options *FunctionOptions,
) ([]*Function, error) {
    // Use Oak queries with line-based filtering
    functions, err := p.ExtractFunctions(sourceCode, options)
    if err != nil {
        return nil, err
    }
    
    // Existing change detection logic
    var changedFunctions []*Function
    functionMap := make(map[string]*Function)
    
    for _, line := range changedLines {
        fn := p.FindFunctionAtLine(functions, line)
        if fn != nil {
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

### Phase 5: Update Commands

**Step 5.1**: Update `analyze functions` command

**Current**:
```go
parser := treesitter.NewParser()
functions, err := parser.ExtractFunctions([]byte(content))
```

**New**:
```go
parser := treesitter.NewParser()
options := &treesitter.FunctionOptions{
    OnlyPublic: s.OnlyChanged,
    FunctionName: s.FunctionName, // New parameter
}
functions, err := parser.ExtractFunctions([]byte(content), options)
```

## Implementation Details

### Query File Structure

**Recommended location**: `internal/queries/go/functions.yaml`

**Structure**:
```yaml
language: go
queries:
  - name: functions
    query: |
      ((comment)* @comment .
      (function_declaration
        name: (identifier) @name
        parameters: (parameter_list)? @parameters
        result: (_)? @result
        body: (block) @body)
        {{ if .only_public }}(#match? @name "^[A-Z]"){{end}}
        {{ if .function_name }}(#eq? @name "{{.function_name}}"){{end}}
      )

  - name: methods
    query: |
      ((comment)* @comment .
      (method_declaration
        receiver: (parameter_list) @receiver
        name: (field_identifier) @name
        parameters: (parameter_list)? @parameters
        result: (_)? @result
        body: (block) @body)
        {{ if .only_public }}(#match? @name "^[A-Z]"){{end}}
        {{ if .function_name }}(#eq? @name "{{.function_name}}"){{end}}
```

### Options Struct

```go
type FunctionOptions struct {
    OnlyPublic   bool
    FunctionName string
    ReceiverType string
    WithBody     bool
    WithComments bool
}
```

### Query Loading and Rendering

```go
func loadFunctionQueries(options *FunctionOptions) ([]tree_sitter.SitterQuery, error) {
    // Load from embedded YAML or file
    content, err := os.ReadFile("internal/queries/go/functions.yaml")
    if err != nil {
        return nil, err
    }
    
    var queryFile struct {
        Queries []tree_sitter.SitterQuery `yaml:"queries"`
    }
    
    err = yaml.Unmarshal(content, &queryFile)
    if err != nil {
        return nil, err
    }
    
    return queryFile.Queries, nil
}

func renderQueries(queries []tree_sitter.SitterQuery, options *FunctionOptions) ([]tree_sitter.SitterQuery, error) {
    // Create template data
    data := map[string]interface{}{
        "only_public":   options.OnlyPublic,
        "function_name": options.FunctionName,
        "receiver_type": options.ReceiverType,
    }
    
    // Render each query
    rendered := make([]tree_sitter.SitterQuery, len(queries))
    for i, query := range queries {
        tmpl, err := template.New("query").Parse(query.Query)
        if err != nil {
            return nil, err
        }
        
        var buf bytes.Buffer
        err = tmpl.Execute(&buf, data)
        if err != nil {
            return nil, err
        }
        
        rendered[i] = tree_sitter.SitterQuery{
            Name:  query.Name,
            Query: buf.String(),
        }
    }
    
    return rendered, nil
}
```

## Migration Challenges and Solutions

### Challenge 1: Signature Extraction

**Current**: Manual byte-range extraction
```go
func (p *Parser) extractSignature(node *sitter.Node, sourceCode []byte) string {
    bodyNode := node.ChildByFieldName("body")
    startByte := node.StartByte()
    endByte := bodyNode.StartByte()
    return string(sourceCode[startByte:endByte])
}
```

**Oak Solution**: Extract signature parts separately
```yaml
(function_declaration
  name: (identifier) @name
  parameters: (parameter_list) @parameters
  result: (_)? @result)
```

Then reconstruct:
```go
signature := fmt.Sprintf("func %s%s%s", name.Text, params.Text, result.Text)
```

### Challenge 2: Line Number Calculation

**Current**: Uses `StartPoint` and `EndPoint` from nodes
```go
fn.StartLine = int(startPoint.Row) + 1
fn.EndLine = int(endPoint.Row) + 1
```

**Oak Solution**: Use capture points
```go
if name, ok := match["name"]; ok {
    fn.StartLine = int(name.StartPoint.Row) + 1
}
if body, ok := match["body"]; ok {
    fn.EndLine = int(body.EndPoint.Row) + 1
}
```

### Challenge 3: Backward Compatibility

**Solution**: Keep existing `Function` struct and API
- Internal implementation changes
- External API remains the same
- Gradual migration path

### Challenge 4: Dependency Management

**Challenge**: Adding Oak as dependency

**Solution**: 
- Oak is already in workspace (`oak/pkg`)
- Can use as local module or add to `go.mod`
- Consider Oak's dependencies (glazed, etc.)

## Benefits Realized

### Immediate Benefits

1. **Reduced code**: ~200 lines → ~50 lines + query file
2. **Easier maintenance**: Query changes don't require recompilation
3. **Better testing**: Queries can be tested independently
4. **More flexible**: Easy to add new extraction patterns

### Long-term Benefits

1. **Query reuse**: Same queries work across projects
2. **Community**: Oak queries can be shared
3. **Documentation**: Queries are self-documenting
4. **Extensibility**: Easy to add new query types

## Migration Timeline

### Week 1: Preparation
- [ ] Add Oak dependency
- [ ] Create query files
- [ ] Test queries with Oak CLI
- [ ] Create adapter functions

### Week 2: Core Migration
- [ ] Replace `ExtractFunctions()` with Oak queries
- [ ] Update `ExtractMethods()` (merge into queries)
- [ ] Update change detection
- [ ] Add options struct

### Week 3: Integration
- [ ] Update all commands using parser
- [ ] Add query parameter support
- [ ] Update tests
- [ ] Documentation

### Week 4: Polish
- [ ] Performance testing
- [ ] Error handling improvements
- [ ] Remove old traversal code
- [ ] Final testing

## Risk Assessment

### Low Risk
- ✅ Oak is mature and tested
- ✅ Query syntax is well-documented
- ✅ Can keep old code as fallback

### Medium Risk
- ⚠️ Dependency on Oak (external dependency)
- ⚠️ Learning curve for query syntax
- ⚠️ Performance impact (likely minimal)

### Mitigation Strategies
1. **Gradual migration**: Keep old code, switch via feature flag
2. **Comprehensive testing**: Test all existing functionality
3. **Performance benchmarking**: Compare before/after
4. **Rollback plan**: Keep old implementation available

## Conclusion

Migrating pr-analyzer to Oak's query template system offers significant benefits:

- **Simpler code**: Declarative queries vs manual traversal
- **More flexible**: Template-based parameterization
- **Better maintainability**: Query files vs Go code
- **Easier extension**: Add new patterns via queries

The migration is **feasible** and **recommended**, with manageable risks and clear benefits. The main effort is in:
1. Creating query files
2. Building adapter layer (Oak results → Function struct)
3. Updating command integration

The investment pays off in reduced maintenance burden and increased flexibility for future enhancements.

