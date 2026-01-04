# Tree-sitter Goja Module - API Documentation

This document provides comprehensive API documentation for the Tree-sitter Goja JavaScript Golang module.

## Table of Contents

1. [Core API](#core-api)
2. [Advanced API](#advanced-api)
3. [Node API](#node-api)
4. [Tree API](#tree-api)
5. [Query API](#query-api)
6. [Utility API](#utility-api)
7. [Error Handling](#error-handling)
8. [Performance Considerations](#performance-considerations)

## Core API

### treesitter Object

The global `treesitter` object provides access to all module functionality.

#### Properties

- `version()`: Returns the module version string
- `supportedLanguages()`: Returns array of supported languages
- `features()`: Returns array of available features

#### Methods

##### createParser(language)

Creates a new parser instance for the specified language.

**Parameters:**
- `language` (string): Language identifier ("javascript" or "js")

**Returns:** Parser object

**Example:**
```javascript
const parser = treesitter.createParser("javascript");
```

##### createQuery(language, queryString)

Creates a query object for executing Tree-sitter queries.

**Parameters:**
- `language` (string): Target language
- `queryString` (string): Tree-sitter query pattern

**Returns:** Query object

**Example:**
```javascript
const query = treesitter.createQuery("javascript", 
  "(function_declaration name: (identifier) @func_name)");
```

##### getPredefinedQueries()

Returns a map of predefined query patterns.

**Returns:** Object with query name to pattern mappings

**Example:**
```javascript
const queries = treesitter.getPredefinedQueries();
console.log(queries.all_functions);
```

## Advanced API

### Advanced Query Engine

##### createAdvancedQuery(language)

Creates an advanced query engine with caching and performance optimization.

**Parameters:**
- `language` (string): Target language

**Returns:** AdvancedQuery object

**Methods:**
- `execute(queryString, tree)`: Execute query with caching
- `getStatistics()`: Get performance statistics
- `setCacheEnabled(enabled)`: Enable/disable query caching
- `clearCache()`: Clear the query cache

**Example:**
```javascript
const advancedQuery = treesitter.createAdvancedQuery("javascript");
const matches = advancedQuery.execute(queryString, tree);
const stats = advancedQuery.getStatistics();
console.log("Cache hits:", stats.cacheHits);
```

### AST Utilities

##### createASTUtilities()

Creates utilities for advanced AST manipulation and analysis.

**Returns:** ASTUtilities object

**Methods:**

###### findNodesByType(root, nodeType)
Find all nodes of a specific type.

**Parameters:**
- `root` (Node): Root node to search from
- `nodeType` (string): Type of nodes to find

**Returns:** Array of Node objects

###### findNodesByProperty(root, property, value)
Find nodes with specific property values.

**Parameters:**
- `root` (Node): Root node to search from
- `property` (string): Property name
- `value` (string): Property value

**Returns:** Array of Node objects

###### getTreeStatistics(root)
Get comprehensive statistics about the tree.

**Parameters:**
- `root` (Node): Root node to analyze

**Returns:** Statistics object with properties:
- `totalNodes` (number): Total number of nodes
- `leafNodes` (number): Number of leaf nodes
- `maxDepth` (number): Maximum tree depth
- `nodeCounts` (object): Count of each node type
- `mostCommonType` (string): Most frequent node type

###### validateTree(root)
Validate tree structure and return any errors.

**Parameters:**
- `root` (Node): Root node to validate

**Returns:** Array of error strings

###### traverseTree(root, order, callback)
Traverse the tree with a callback function.

**Parameters:**
- `root` (Node): Root node to traverse
- `order` (string): Traversal order ("preorder", "postorder", "breadthfirst")
- `callback` (function): Callback function `(node, depth) => boolean`

**Example:**
```javascript
const utils = treesitter.createASTUtilities();
const functions = utils.findNodesByType(root, "function_declaration");
const stats = utils.getTreeStatistics(root);

utils.traverseTree(root, "preorder", (node, depth) => {
  console.log("  ".repeat(depth) + node.type());
  return true; // continue traversal
});
```

### Query Builder

##### createQueryBuilder()

Creates a basic query builder with fluent API.

**Returns:** QueryBuilder object

**Methods:**
- `functionDeclaration(captureName)`: Add function declaration pattern
- `classDeclaration(captureName)`: Add class declaration pattern
- `variableDeclaration(captureName)`: Add variable declaration pattern
- `methodDefinition(captureName)`: Add method definition pattern
- `arrowFunction(captureName)`: Add arrow function pattern
- `callExpression(captureName)`: Add call expression pattern
- `build()`: Build final query string

##### createQueryBuilder2()

Creates an enhanced query builder with advanced patterns.

**Returns:** Enhanced QueryBuilder object

**Additional Methods:**
- `functionWithName(name, captureName)`: Find function with specific name
- `classWithMethod(className, methodName, classCapture, methodCapture)`: Find class with method
- `variableWithValue(varName, captureName)`: Find variable with specific name
- `callToFunction(funcName, captureName)`: Find calls to specific function
- `analyze(tree)`: Analyze query complexity

**Example:**
```javascript
const builder = treesitter.createQueryBuilder2();
const query = builder
  .functionWithName("myFunction", "func")
  .classWithMethod("MyClass", "myMethod", "class", "method")
  .build();

const analysis = builder.analyze(tree);
console.log("Query complexity:", analysis.complexity);
```

### Template Library

##### createQueryTemplateLibrary()

Creates a library of reusable query templates.

**Returns:** TemplateLibrary object

**Methods:**

###### listTemplates()
Get all available template names.

**Returns:** Array of template names

###### getTemplate(name)
Get template by name.

**Parameters:**
- `name` (string): Template name

**Returns:** Template object with properties:
- `name` (string): Template name
- `description` (string): Template description
- `template` (string): Template pattern
- `parameters` (array): Required parameters

###### instantiateTemplate(name, params)
Create query from template with parameters.

**Parameters:**
- `name` (string): Template name
- `params` (object): Parameter values

**Returns:** Query string

**Example:**
```javascript
const templates = treesitter.createQueryTemplateLibrary();
const templateNames = templates.listTemplates();

const query = templates.instantiateTemplate("function_with_params", {
  func_name: "myFunction"
});
```

## Node API

Node objects represent individual AST nodes and provide the following methods:

### Core Methods

#### type()
Get the node type.

**Returns:** String representing the node type

#### text()
Get the text content of the node.

**Returns:** String containing the node's text

#### startByte()
Get the starting byte position.

**Returns:** Number representing start position

#### endByte()
Get the ending byte position.

**Returns:** Number representing end position

#### childCount()
Get the number of child nodes.

**Returns:** Number of children

#### children()
Get array of child nodes.

**Returns:** Array of Node objects

### Property Methods

#### hasProperty(name)
Check if node has a specific property.

**Parameters:**
- `name` (string): Property name

**Returns:** Boolean

#### getProperty(name)
Get property value.

**Parameters:**
- `name` (string): Property name

**Returns:** String property value or undefined

**Example:**
```javascript
const node = tree.rootNode();
console.log("Node type:", node.type());
console.log("Text content:", node.text());
console.log("Position:", node.startByte(), "-", node.endByte());

if (node.hasProperty("name")) {
  console.log("Name:", node.getProperty("name"));
}

const children = node.children();
children.forEach((child, index) => {
  console.log(`Child ${index}:`, child.type());
});
```

## Tree API

Tree objects represent complete parsed syntax trees.

### Methods

#### rootNode()
Get the root node of the tree.

**Returns:** Root Node object

### Properties

#### source
Access to the original source code.

**Type:** String

**Example:**
```javascript
const tree = parser.parse(sourceCode);
const root = tree.rootNode();
console.log("Root type:", root.type());
console.log("Source length:", tree.source.length);
```

## Query API

Query objects execute Tree-sitter queries against parsed trees.

### Methods

#### execute(tree)
Execute the query against a tree.

**Parameters:**
- `tree` (Tree): Target tree object

**Returns:** Array of match objects

**Match Object Structure:**
Each match object contains captured nodes as properties, where the property name corresponds to the capture name in the query.

**Example:**
```javascript
const query = treesitter.createQuery("javascript", 
  "(function_declaration name: (identifier) @func_name)");

const matches = query.execute(tree);
matches.forEach(match => {
  const funcName = match.func_name;
  console.log("Function:", funcName.text());
  console.log("Position:", funcName.startByte(), "-", funcName.endByte());
});
```

## Utility API

### Parser Configuration

For advanced parsing, you can configure parser behavior:

```javascript
// This is handled internally, but the configuration includes:
// - parseComments: Include comments in AST
// - parseJSX: Support JSX syntax
// - parseTypeScript: Support TypeScript syntax
// - strictMode: Use strict parsing mode
// - ecmaVersion: ECMAScript version (2023)
```

### Statistics Objects

#### QueryStatistics
Performance statistics for query execution:

```javascript
{
  totalQueries: number,    // Total queries executed
  cacheHits: number,       // Number of cache hits
  cacheMisses: number,     // Number of cache misses
  averageTimeMs: number    // Average execution time
}
```

#### TreeStatistics
Comprehensive tree analysis:

```javascript
{
  totalNodes: number,           // Total number of nodes
  leafNodes: number,            // Number of leaf nodes
  maxDepth: number,             // Maximum tree depth
  nodeCounts: object,           // Count by node type
  propertyCount: object,        // Count by property
  mostCommonType: string        // Most frequent node type
}
```

#### QueryAnalysis
Query complexity analysis:

```javascript
{
  queryString: string,     // Original query string
  complexity: number,      // Complexity score
  patterns: array,         // Detected patterns
  suggestions: array       // Optimization suggestions
}
```

## Error Handling

The module uses JavaScript exceptions for error handling:

```javascript
try {
  const parser = treesitter.createParser("javascript");
  const tree = parser.parse(sourceCode);
  const query = treesitter.createQuery("javascript", queryString);
  const matches = query.execute(tree);
} catch (error) {
  console.error("Error:", error.message);
}
```

Common error scenarios:
- Invalid query syntax
- Unsupported language
- Malformed source code
- Invalid node operations

## Performance Considerations

### Query Optimization

1. **Use Caching**: Enable query caching for frequently used patterns
2. **Specific Patterns**: Use specific patterns rather than broad wildcards
3. **Limit Scope**: Limit query scope when possible
4. **Batch Operations**: Group related queries together

### Memory Management

1. **Tree Reuse**: Reuse tree objects when parsing similar code
2. **Query Reuse**: Reuse query objects for multiple executions
3. **Cleanup**: Clear caches periodically for long-running applications

### Best Practices

```javascript
// Good: Reuse parser and queries
const parser = treesitter.createParser("javascript");
const functionQuery = treesitter.createQuery("javascript", 
  treesitter.getPredefinedQueries().all_functions);

// Process multiple files
files.forEach(file => {
  const tree = parser.parse(file.content);
  const matches = functionQuery.execute(tree);
  // Process matches...
});

// Good: Use advanced query engine for repeated queries
const advancedQuery = treesitter.createAdvancedQuery("javascript");
advancedQuery.setCacheEnabled(true);

// Execute same query multiple times (benefits from caching)
const results = trees.map(tree => 
  advancedQuery.execute(queryString, tree));
```

This API documentation provides comprehensive coverage of all available functionality in the Tree-sitter Goja module. For additional examples and use cases, refer to the main README.md file.

