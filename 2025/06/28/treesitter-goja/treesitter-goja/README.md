# Tree-sitter Goja JavaScript Golang Module

A comprehensive Tree-sitter implementation that exposes parsing, AST primitives, and query functionality to JavaScript through the Goja runtime. This module provides a powerful interface for analyzing and manipulating JavaScript code using Tree-sitter's parsing capabilities.

## Features

- **JavaScript Parsing**: Parse JavaScript code into Abstract Syntax Trees (AST)
- **Advanced Query System**: Execute complex queries against parsed code
- **AST Utilities**: Comprehensive utilities for tree traversal and manipulation
- **Query Builder**: Fluent API for building complex queries
- **Template Library**: Pre-built query templates for common patterns
- **Performance Optimization**: Caching and query optimization features
- **Validation**: Built-in tree validation and error checking

## Installation

### Prerequisites

- Go 1.24 or later
- Git

### Building from Source

```bash
git clone <repository-url>
cd treesitter-goja
go mod tidy
go build -o treesitter-goja .
```

## Usage

### Basic Usage

```bash
# Parse a JavaScript file
./treesitter-goja path/to/your/file.js

# Use advanced parsing mode
./treesitter-goja path/to/your/file.js --advanced

# Run validation tests
./treesitter-goja --test
```

### JavaScript API

The module exposes a comprehensive JavaScript API through the `treesitter` global object:

```javascript
// Create a parser
const parser = treesitter.createParser("javascript");

// Parse source code
const tree = parser.parse(sourceCode);
const rootNode = tree.rootNode();

// Basic node operations
console.log("Node type:", rootNode.type());
console.log("Children count:", rootNode.childCount());
console.log("Text content:", rootNode.text());

// Create and execute queries
const query = treesitter.createQuery("javascript", 
  "(function_declaration name: (identifier) @func_name)");
const matches = query.execute(tree);

// Use predefined queries
const predefinedQueries = treesitter.getPredefinedQueries();
const functionQuery = treesitter.createQuery("javascript", 
  predefinedQueries.all_functions);
```

## API Reference

### Core API

#### `treesitter.createParser(language)`
Creates a new parser for the specified language.

**Parameters:**
- `language` (string): The language to parse ("javascript" or "js")

**Returns:** Parser object with methods:
- `parse(sourceCode)`: Parse source code and return a Tree object
- `language`: Get the parser's language

#### `treesitter.createQuery(language, queryString)`
Creates a new query for the specified language.

**Parameters:**
- `language` (string): The target language
- `queryString` (string): Tree-sitter query pattern

**Returns:** Query object with methods:
- `execute(tree)`: Execute the query against a tree

### Advanced API

#### `treesitter.createAdvancedQuery(language)`
Creates an advanced query engine with caching and optimization.

**Returns:** AdvancedQuery object with methods:
- `execute(queryString, tree)`: Execute query with caching
- `getStatistics()`: Get performance statistics
- `setCacheEnabled(enabled)`: Enable/disable caching
- `clearCache()`: Clear query cache

#### `treesitter.createASTUtilities()`
Creates AST utilities for advanced tree manipulation.

**Returns:** ASTUtilities object with methods:
- `findNodesByType(root, nodeType)`: Find all nodes of specified type
- `findNodesByProperty(root, property, value)`: Find nodes by property
- `getTreeStatistics(root)`: Get comprehensive tree statistics
- `validateTree(root)`: Validate tree structure
- `traverseTree(root, order, callback)`: Traverse tree with callback

#### `treesitter.createQueryBuilder()`
Creates a fluent query builder.

**Returns:** QueryBuilder object with methods:
- `functionDeclaration(captureName)`: Add function declaration pattern
- `classDeclaration(captureName)`: Add class declaration pattern
- `methodDefinition(captureName)`: Add method definition pattern
- `build()`: Build the final query string

#### `treesitter.createQueryBuilder2()`
Creates an enhanced query builder with more features.

**Returns:** Enhanced QueryBuilder object with additional methods:
- `functionWithName(name, captureName)`: Find function with specific name
- `classWithMethod(className, methodName, classCapture, methodCapture)`: Find class with method
- `variableWithValue(varName, captureName)`: Find variable with specific name
- `callToFunction(funcName, captureName)`: Find calls to specific function
- `analyze(tree)`: Analyze query complexity and patterns

#### `treesitter.createQueryTemplateLibrary()`
Creates a query template library with pre-built patterns.

**Returns:** TemplateLibrary object with methods:
- `listTemplates()`: Get all available template names
- `getTemplate(name)`: Get template by name
- `instantiateTemplate(name, params)`: Create query from template

### Node API

Tree nodes expose the following methods:

- `type()`: Get the node type
- `text()`: Get the text content
- `startByte()`: Get start position in bytes
- `endByte()`: Get end position in bytes
- `childCount()`: Get number of children
- `children()`: Get array of child nodes
- `hasProperty(name)`: Check if node has property
- `getProperty(name)`: Get property value

### Tree API

Tree objects provide:

- `rootNode()`: Get the root node of the tree
- `source`: Access to the original source code

## Predefined Queries

The module includes several predefined query patterns:

- `all_functions`: Find all function declarations
- `all_classes`: Find all class declarations
- `all_variables`: Find all variable declarations
- `all_methods`: Find all method definitions
- `all_calls`: Find all function calls
- `console_logs`: Find console.log statements
- `arrow_functions`: Find arrow function expressions
- `async_functions`: Find async function declarations
- `imports`: Find import statements
- `exports`: Find export statements

## Examples

### Finding All Functions

```javascript
const parser = treesitter.createParser("javascript");
const tree = parser.parse(sourceCode);

const query = treesitter.createQuery("javascript", 
  treesitter.getPredefinedQueries().all_functions);
const matches = query.execute(tree);

matches.forEach(match => {
  const funcName = match.function_name;
  console.log("Found function:", funcName.text());
});
```

### Using AST Utilities

```javascript
const utils = treesitter.createASTUtilities();
const tree = parser.parse(sourceCode);
const root = tree.rootNode();

// Get tree statistics
const stats = utils.getTreeStatistics(root);
console.log("Total nodes:", stats.totalNodes);
console.log("Most common type:", stats.mostCommonType);

// Find specific node types
const functions = utils.findNodesByType(root, "function_declaration");
console.log("Found", functions.length, "functions");

// Validate tree structure
const errors = utils.validateTree(root);
if (errors.length > 0) {
  console.log("Tree validation errors:", errors);
}
```

### Building Complex Queries

```javascript
const builder = treesitter.createQueryBuilder2();

const query = builder
  .functionWithName("myFunction", "func")
  .classWithMethod("MyClass", "myMethod", "class", "method")
  .build();

console.log("Generated query:", query);

const queryObj = treesitter.createQuery("javascript", query);
const matches = queryObj.execute(tree);
```

### Using Query Templates

```javascript
const templates = treesitter.createQueryTemplateLibrary();
const templateNames = templates.listTemplates();

console.log("Available templates:", templateNames);

const query = templates.instantiateTemplate("function_with_params", {
  func_name: "myFunction"
});

const queryObj = treesitter.createQuery("javascript", query);
const matches = queryObj.execute(tree);
```

## Architecture

The module is structured into several key components:

### Core Components

1. **Parser** (`parser.go`): Basic JavaScript parsing functionality
2. **Advanced Parser** (`advanced_parser.go`): Enhanced parsing with additional features
3. **Query Engine** (`query.go`): Basic query execution
4. **Advanced Query Engine** (`advanced_query.go`): Enhanced queries with caching and optimization
5. **AST Utilities** (`ast_utilities.go`): Tree manipulation and analysis utilities
6. **Goja Bindings** (`goja_bindings.go`): JavaScript API bindings
7. **Tree-sitter Interface** (`treesitter.go`): Main library interface

### Data Structures

- **Node**: Represents a single AST node with type, text, position, and children
- **Tree**: Represents a complete parsed tree with root node and source
- **QueryMatch**: Represents the result of a query execution
- **TreeStatistics**: Comprehensive statistics about tree structure
- **QueryAnalysis**: Analysis of query complexity and patterns

## Performance

The module includes several performance optimizations:

- **Query Caching**: Frequently used queries are cached for faster execution
- **Lazy Evaluation**: Tree traversal uses lazy evaluation where possible
- **Memory Optimization**: Efficient memory usage for large ASTs
- **Statistics Tracking**: Performance metrics for query optimization

## Testing

The module includes comprehensive validation tests:

```bash
# Run all validation tests
./treesitter-goja --test
```

Tests cover:
- Basic parsing functionality
- Advanced parsing features
- Query engine operations
- AST utility functions
- Advanced query features
- Query builder functionality
- Template library operations

## Limitations

- Currently supports JavaScript parsing only (extensible to other languages)
- Uses regex-based parsing for demonstration (can be extended with real Tree-sitter)
- Some advanced JavaScript features may not be fully supported in Goja runtime
- Performance optimizations are basic implementations

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Run validation tests: `./treesitter-goja --test`
6. Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Built upon the [go-go-golems/oak](https://github.com/go-go-golems/oak) project
- Uses the [Goja](https://github.com/dop251/goja) JavaScript runtime
- Inspired by [Tree-sitter](https://tree-sitter.github.io/) parsing library

## Version History

### v1.0.0-alpha
- Initial implementation
- Basic JavaScript parsing
- Query system with predefined patterns
- AST utilities and tree manipulation
- Advanced query engine with caching
- Query builder and template library
- Comprehensive validation tests
- Full JavaScript API through Goja runtime

