# Go File Analyzer Usage Guide

This guide provides examples and usage patterns for the Go File Analyzer tool.

## Command-line Usage

### Basic Analysis

To analyze a Go project directory and output the results to a JSON file:

```bash
./recursive_analyzer/recursive_analyzer -path=/path/to/project -output=analysis.json
```

### Enhancing Analysis Results

To enhance the basic analysis output with additional metadata:

```bash
./json_formatter/json_formatter analysis.json enhanced.json
```

### Analyzing Your Own Project

```bash
# Navigate to your project directory
cd /path/to/your/project

# Run the analyzer
/path/to/go-file-analyzer/recursive_analyzer/recursive_analyzer -path=. -output=analysis.json

# Enhance the results
/path/to/go-file-analyzer/json_formatter/json_formatter analysis.json enhanced.json
```

### Analyzing the Go Standard Library

```bash
# Specify the path to the Go standard library
./recursive_analyzer/recursive_analyzer -path=/usr/lib/go/src -output=stdlib.json -verbose

# Enhance the results
./json_formatter/json_formatter stdlib.json stdlib-enhanced.json
```

## Web Interface Usage

### Starting the Web Server

```bash
cd webserver
./webserver -port=8001 -analyzer=../recursive_analyzer/recursive_analyzer -formatter=../json_formatter/json_formatter -static=./static
```

Then open a web browser and navigate to http://localhost:8001

### Analyzing a Project

1. Enter the path to your Go project in the input field
2. Click "Analyze" to start the analysis
3. Wait for the analysis to complete (this may take a few seconds for large projects)
4. The results will be displayed in the web interface

### Navigating the Results

#### Package and File Navigation

- Use the left sidebar to navigate through packages and files
- Click on a package name to expand/collapse the list of files in that package
- Click on a file to view its contents in the main panel

#### Function Navigation

- When a file is selected, a list of functions in that file is displayed above the code view
- Click on a function to highlight it in the code and show its details
- Functions are color-coded:
  - Blue: Regular functions
  - Orange: Methods
  - Bold: Exported functions

#### Code View

- The code view displays the selected file with syntax highlighting
- When a function is selected, it is highlighted in the code view
- Line numbers are displayed on the left side

### Using Visualizations

1. Click on the "Visualizations" tab to access the visualization features
2. Select a visualization type:
   - **Package Dependencies**: Shows dependencies between packages
   - **Function Dependencies**: Shows function calls within a file
   - **Complexity Heatmap**: Visualizes function complexity based on size
   - **Package Tree**: Shows the hierarchical structure of packages

#### Package Dependency Graph

The package dependency graph shows how packages in your project depend on each other.

- Each node represents a package
- Edges represent dependencies between packages
- Hover over a node to see more information about the package
- Drag nodes to rearrange the graph
- Zoom in/out using the mouse wheel

#### Function Dependency Graph

The function dependency graph shows function calls within a file.

- Each node represents a function
- Edges represent function calls
- Hover over a node to see more information about the function
- Click on a node to highlight that function in the code view

#### Complexity Heatmap

The complexity heatmap visualizes function complexity based on size.

- Each cell represents a function
- Color indicates complexity (yellow to red)
- Hover over a cell to see more information about the function
- Click on a cell to navigate to that function

#### Package Tree

The package tree shows the hierarchical structure of packages in your project.

- Each node represents a package
- Parent-child relationships represent package hierarchies
- Hover over a node to see more information about the package

## Example Workflows

### Code Exploration

1. Start the web server
2. Analyze your project
3. Browse through packages and files to understand the project structure
4. Use the function list to find functions of interest
5. View the code with syntax highlighting to understand the implementation

### Code Complexity Analysis

1. Start the web server
2. Analyze your project
3. Switch to the Visualizations tab
4. Select the Complexity Heatmap visualization
5. Identify the most complex functions (red cells)
6. Click on these functions to view their implementation
7. Consider refactoring complex functions to improve maintainability

### Dependency Analysis

1. Start the web server
2. Analyze your project
3. Switch to the Visualizations tab
4. Select the Package Dependency Graph visualization
5. Identify packages with many dependencies (high degree nodes)
6. Consider refactoring to reduce coupling between packages

### Code Review

1. Start the web server
2. Analyze the project you're reviewing
3. Browse through the code using the navigation panel
4. Use the function list to focus on specific functions
5. Use the visualization features to understand the overall structure
6. Identify potential issues like overly complex functions or excessive dependencies

## Tips and Tricks

1. **Performance**: For large projects, the analysis may take some time. Use the `-verbose` flag to see progress.
2. **Memory Usage**: If you're analyzing a very large codebase, the web interface may use a significant amount of memory. Consider closing unused browser tabs.
3. **Exporting Results**: You can save the analysis results as JSON for later use or sharing.
4. **Custom Paths**: You can modify the paths to the analyzer and formatter executables to match your setup.
5. **Refresh Analysis**: To analyze a different project or refresh the analysis, simply enter a new path and click "Analyze" again.