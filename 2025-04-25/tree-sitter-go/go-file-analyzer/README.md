# Go File Analyzer with Tree-sitter

A powerful Go code analysis tool that uses Tree-sitter for precise parsing and provides both a command-line interface and web-based visualization.

## Features

- Parse Go files using Tree-sitter for accurate syntax analysis
- Extract function declarations, methods, and their locations in the source code
- Recursively analyze entire Go projects
- Generate detailed JSON reports of the analysis results
- Interactive web interface for exploring the codebase
- Visualization of package dependencies, function relationships, and code complexity

## Components

The Go File Analyzer consists of several components:

1. **Tree-sitter Parser**: Parses Go files and extracts function information
2. **Recursive Directory Analyzer**: Analyzes entire project directories
3. **JSON Formatter**: Enhances the analysis results with additional metadata
4. **Web Server**: Serves the file analyzer as a web application
5. **Web Interface**: Provides an interactive UI for exploring the analyzed code

## Installation

### Prerequisites

- Go 1.18 or later
- Tree-sitter Go parser

### Installation Steps

```bash
# Clone the repository
git clone https://github.com/yourusername/go-file-analyzer.git
cd go-file-analyzer

# Build the recursive analyzer
cd recursive_analyzer
go build
cd ..

# Build the JSON formatter
cd json_formatter
go build
cd ..

# Build the web server
cd webserver
go build
cd ..
```

## Usage

### Command-line Interface

```bash
# Analyze a Go project
./recursive_analyzer/recursive_analyzer -path=/path/to/project -output=analysis.json

# Enhance the analysis output
./json_formatter/json_formatter analysis.json enhanced.json
```

### Web Interface

```bash
# Start the web server
cd webserver
./webserver -port=8001 -analyzer=../recursive_analyzer/recursive_analyzer -formatter=../json_formatter/json_formatter -static=./static
```

Then open a web browser and navigate to http://localhost:8001

## Web Interface Features

### Code Navigation

The web interface allows you to navigate through the analyzed codebase:

- View the project structure organized by packages
- Browse files within each package
- See functions and methods in each file
- View syntax-highlighted code
- Highlight selected functions in the code view

![File Navigation](screenshots/file_navigation.png)

### Function Details

When you select a function, you can view detailed information about it:

- Name and package
- File path and line numbers
- Whether it's a method or regular function
- Whether it's exported or internal

![Function Details](screenshots/function_details.png)

### Visualizations

The web interface includes several visualizations to help you understand the codebase:

- **Package Dependency Graph**: Shows dependencies between packages
- **Function Dependency Graph**: Shows function calls within a file
- **Complexity Heatmap**: Visualizes function complexity based on size
- **Package Tree**: Shows the hierarchical structure of packages

![Visualization](screenshots/visualization.png)

The complexity heatmap helps identify complex functions that might need refactoring:

![Complexity Heatmap](screenshots/complexity_heatmap.png)

## Development

### Project Structure

```
go-file-analyzer/
├── recursive_analyzer/     # Recursive directory analyzer
├── json_formatter/         # JSON formatter for analysis results
├── webserver/              # Web server and interface
│   ├── server.go           # Go HTTP server
│   └── static/             # Static files for the web interface
│       ├── index.html      # Main HTML page
│       ├── script.js       # Main JavaScript functionality
│       ├── visualization.js # D3.js visualizations
│       └── styles.css      # CSS styles
└── screenshots/            # Screenshots for documentation
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- [Tree-sitter](https://tree-sitter.github.io/tree-sitter/) for the powerful parsing capabilities
- [D3.js](https://d3js.org/) for the visualization features