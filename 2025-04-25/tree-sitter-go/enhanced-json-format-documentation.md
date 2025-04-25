# Enhanced JSON Format for Go Code Analysis

## Overview

The enhanced JSON format is designed to provide rich, structured data about Go codebases that is optimized for web visualization and exploration. It extends the basic function information with additional metadata, relationship data, and visualization-specific structures that make it easier to create interactive web interfaces for exploring codebases.

## Format Structure

The enhanced JSON format consists of the following main sections:

```json
{
  "metadata": { ... },        // Basic metadata about the codebase
  "files": [ ... ],           // Detailed information about each file
  "functions": [ ... ],       // Comprehensive function/method information
  "packages": [ ... ],        // Package structure information
  "callGraph": { ... },       // Function call relationships
  "visualizationData": { ... } // Data optimized for web visualizations
}
```

### Metadata

The `metadata` section provides high-level information about the codebase:

```json
"metadata": {
  "name": "project-name",         // Inferred from root directory or module
  "basePath": "/path/to/project", // Absolute path to the analyzed codebase
  "goVersion": "1.18",            // Go version (from go.mod)
  "moduleName": "module/path",    // Module name (from go.mod)
  "numFiles": 100,                // Total number of files analyzed
  "numFunctions": 500,            // Total number of functions found
  "numMethods": 200,              // Total number of methods found
  "numPackages": 20,              // Total number of packages found
  "analysisTimeMs": 1234,         // Analysis time in milliseconds
  "generated": "2025-04-24T12:34:56Z" // When the analysis was generated
}
```

### Files

The `files` array contains detailed information about each Go file:

```json
"files": [
  {
    "path": "relative/path/to/file.go", // Relative to base path
    "absPath": "/absolute/path/to/file.go", // Absolute file path
    "package": "packagename",           // Package name
    "imports": [                         // List of imports
      {
        "path": "imported/package",      // Import path
        "alias": "optional_alias"        // Import alias if provided
      }
    ],
    "functions": [ ... ],                // Functions defined in this file
    "numLines": 123,                     // Number of lines in the file
    "size": 1234                         // File size in bytes
  }
]
```

### Functions

The `functions` array provides comprehensive information about each function and method:

```json
"functions": [
  {
    "id": "packagename.FunctionName",    // Unique identifier
    "name": "FunctionName",              // Function name
    "signature": "func FunctionName(...) ...", // Full function signature
    "filepath": "path/to/file.go",       // File path
    "startLine": 10,                     // Start line in the file
    "endLine": 20,                       // End line in the file
    "startCol": 1,                       // Start column
    "endCol": 1,                         // End column
    "package": "packagename",            // Package name
    "isMethod": false,                   // Whether it's a method
    "isExported": true,                  // Whether it's exported
    "receiver": {                        // Receiver info (for methods)
      "name": "r",                       // Receiver variable name
      "type": "*Receiver",               // Receiver type
      "isPointer": true,                 // Whether it's a pointer receiver
      "typeName": "Receiver"             // Type name without pointer
    },
    "parameters": [                      // Function parameters
      {
        "name": "param1",                // Parameter name
        "type": "string"                 // Parameter type
      }
    ],
    "returnTypes": ["string", "error"],  // Return types
    "comments": "Function documentation",// Function comments
    "calls": ["packagename.OtherFunc"],  // Functions that this function calls
    "calledBy": ["main.main"],           // Functions that call this function
    "complexity": 5                      // Cyclomatic complexity
  }
]
```

### Packages

The `packages` array provides information about the package structure:

```json
"packages": [
  {
    "name": "packagename",              // Package name
    "path": "relative/path",            // Package path
    "files": [                          // Files in this package
      "path/to/file1.go",
      "path/to/file2.go"
    ],
    "functions": [                      // Functions in this package
      "packagename.Func1",
      "packagename.Func2"
    ]
  }
]
```

### Call Graph

The `callGraph` section represents the function call relationships as a graph:

```json
"callGraph": {
  "nodes": [
    {
      "id": "packagename.FunctionName", // Unique identifier
      "label": "FunctionName",          // Display label
      "type": "function",               // Node type: "function", "method", "package"
      "package": "packagename",         // Package name
      "filepath": "path/to/file.go"     // File path
    }
  ],
  "edges": [
    {
      "source": "main.main",           // Source function ID
      "target": "packagename.FunctionName", // Target function ID
      "type": "calls"                   // Edge type: "calls", "belongs_to", etc.
    }
  ]
}
```

### Visualization Data

The `visualizationData` section contains data structures optimized for common web visualizations:

```json
"visualizationData": {
  "packageHierarchy": { ... },  // Hierarchical data for treemap/sunburst visualizations
  "callNetwork": { ... },       // Network data for force-directed graphs
  "moduleDependencies": { ... },// Dependency wheel data
  "functionComplexity": [ ... ] // Complexity data for scatter plots/heatmaps
}
```

#### Package Hierarchy

```json
"packageHierarchy": {
  "name": "root",
  "children": [
    {
      "name": "packagename",
      "path": "path/to/package",
      "value": 10,              // Number of functions
      "children": [ ... ]       // Nested packages or subdirectories
    }
  ]
}
```

#### Call Network

```json
"callNetwork": {
  "nodes": [
    {
      "id": "packagename.FunctionName",
      "label": "FunctionName",
      "package": "packagename",
      "group": "packagename",   // Grouping for coloring
      "value": 10,              // Node size (e.g., lines of code)
      "isMethod": false,
      "type": "function"
    }
  ],
  "links": [
    {
      "source": "main.main",
      "target": "packagename.FunctionName",
      "value": 1,               // Link strength
      "type": "calls"
    }
  ]
}
```

#### Module Dependencies

```json
"moduleDependencies": {
  "names": ["pkg1", "pkg2", "pkg3"],
  "matrix": [                  // Adjacency matrix
    [0, 1, 0],
    [0, 0, 1],
    [0, 0, 0]
  ]
}
```

#### Function Complexity

```json
"functionComplexity": [
  {
    "name": "FunctionName",
    "package": "packagename",
    "complexity": 5,           // Complexity metric
    "lines": 20                // Number of lines
  }
]
```

## Benefits for Web Visualization

The enhanced JSON format offers several benefits for web visualization:

### 1. Structured Data for Multiple Visualization Types

The format is designed to support various visualization types out of the box:

- **Hierarchical Visualizations**: The package hierarchy data is optimized for treemaps, sunburst diagrams, and collapsible trees.
- **Network Visualizations**: The call graph and call network data are formatted for force-directed graphs, arc diagrams, and dependency wheels.
- **Scatter Plots and Heatmaps**: The function complexity data can be directly used for complexity/size scatter plots or heatmaps.

### 2. Unique Identifiers and References

All entities (functions, packages, files) have unique identifiers that can be used to create links between different visualizations, enabling interactive exploration. For example:

- Clicking on a function in a treemap can highlight its connections in a network graph
- Selecting a package can filter the displayed functions in other visualizations
- Functions that call each other can be visually connected across different views

### 3. Hierarchical and Relational Data

The format represents both hierarchical relationships (package structure) and network relationships (function calls), enabling multi-perspective exploration of the codebase.

### 4. Visualization-Specific Data Structures

Rather than requiring the web interface to transform the data for visualization, the format includes pre-processed data structures optimized for common visualization libraries:

- The network data is formatted for libraries like D3.js, Sigma.js, or VisJS
- The hierarchical data is formatted for D3.js hierarchy visualizations
- The dependency matrix is formatted for dependency wheel visualizations

### 5. Rich Metadata

The format includes detailed metadata about functions, files, and packages, enabling rich tooltips, detailed views, and advanced filtering options in the web interface.

## Recommended Visualizations

Based on this format, the following visualizations are recommended for the web interface:

1. **Package Treemap**: Visualize the package structure with each rectangle sized by the number of functions.
2. **Function Call Network**: Display functions as nodes and calls between them as edges, with different colors for different packages.
3. **Complexity/Size Scatter Plot**: Plot functions with complexity on one axis and size (lines of code) on the other, helping identify functions that might need refactoring.
4. **Package Dependency Wheel**: Show dependencies between packages in a circular layout.
5. **Function Timeline**: Display functions along a horizontal timeline based on their position in the file.
6. **Sunburst Diagram**: Show the nested structure of packages and files, with functions as the outer ring.

## Implementation Example

Here's how a D3.js visualization might use this format for a force-directed graph of function calls:

```javascript
// Load the enhanced JSON data
d3.json("analysis-enhanced.json").then(data => {
  // Use the call network data directly
  const { nodes, links } = data.visualizationData.callNetwork;
  
  // Create a force simulation
  const simulation = d3.forceSimulation(nodes)
    .force("link", d3.forceLink(links).id(d => d.id))
    .force("charge", d3.forceManyBody())
    .force("center", d3.forceCenter(width / 2, height / 2));
    
  // Create the links
  const link = svg.append("g")
    .selectAll("line")
    .data(links)
    .enter().append("line")
    .attr("stroke", "#999")
    .attr("stroke-opacity", 0.6);
    
  // Create the nodes
  const node = svg.append("g")
    .selectAll("circle")
    .data(nodes)
    .enter().append("circle")
    .attr("r", d => Math.sqrt(d.value))
    .attr("fill", d => colorScale(d.group))
    .call(drag(simulation));
    
  // Add tooltips
  node.append("title")
    .text(d => `${d.label} (${d.package})\n${getFunction(d.id).signature}`);
    
  // Function to look up full function information
  function getFunction(id) {
    return data.functions.find(f => f.id === id);
  }
  
  // Update positions on each tick
  simulation.on("tick", () => {
    link
      .attr("x1", d => d.source.x)
      .attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x)
      .attr("y2", d => d.target.y);
      
    node
      .attr("cx", d => d.x)
      .attr("cy", d => d.y);
  });
});
```

## Conclusion

The enhanced JSON format transforms the basic function analysis data into a rich, structured format optimized for web visualization and interactive exploration. By including visualization-specific data structures, unique identifiers, and comprehensive metadata, it simplifies the creation of powerful web interfaces for exploring Go codebases.