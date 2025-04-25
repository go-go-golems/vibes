# Go File Analyzer Tutorial

This tutorial will guide you through setting up and using the Go File Analyzer to analyze a Go project.

## Prerequisites

Before starting this tutorial, make sure you have:

- Go 1.18 or later installed
- Basic knowledge of Go programming
- A Go project to analyze (or you can use the example provided)

## Step 1: Download and Build the Project

First, let's download and build the Go File Analyzer:

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

## Step 2: Create a Sample Go Project

For this tutorial, let's create a simple Go project to analyze:

```bash
# Create a directory for our sample project
mkdir -p sample_project/calculator
mkdir -p sample_project/main

# Create a calculator package
cat > sample_project/calculator/calculator.go << 'EOF'
package calculator

// Add adds two numbers and returns the result
func Add(a, b int) int {
    return a + b
}

// Subtract subtracts b from a and returns the result
func Subtract(a, b int) int {
    return a - b
}

// Multiply multiplies two numbers and returns the result
func Multiply(a, b int) int {
    return a * b
}

// Divide divides a by b and returns the result
// If b is 0, it returns 0 to avoid division by zero
func Divide(a, b int) int {
    if b == 0 {
        return 0
    }
    return a / b
}

// Calculator represents a calculator with memory
type Calculator struct {
    memory int
}

// NewCalculator creates a new Calculator with memory set to 0
func NewCalculator() *Calculator {
    return &Calculator{memory: 0}
}

// Add adds the given number to the memory
func (c *Calculator) Add(num int) {
    c.memory += num
}

// Subtract subtracts the given number from the memory
func (c *Calculator) Subtract(num int) {
    c.memory -= num
}

// GetMemory returns the current memory value
func (c *Calculator) GetMemory() int {
    return c.memory
}

// Clear sets the memory to 0
func (c *Calculator) Clear() {
    c.memory = 0
}
EOF

# Create a main package
cat > sample_project/main/main.go << 'EOF'
package main

import (
    "fmt"
    "sample_project/calculator"
)

func main() {
    // Use the calculator package functions
    fmt.Println("5 + 3 =", calculator.Add(5, 3))
    fmt.Println("5 - 3 =", calculator.Subtract(5, 3))
    fmt.Println("5 * 3 =", calculator.Multiply(5, 3))
    fmt.Println("6 / 3 =", calculator.Divide(6, 3))
    
    // Use the Calculator struct
    calc := calculator.NewCalculator()
    calc.Add(10)
    fmt.Println("Memory:", calc.GetMemory())
    calc.Subtract(5)
    fmt.Println("Memory:", calc.GetMemory())
    calc.Clear()
    fmt.Println("Memory after clear:", calc.GetMemory())
    
    // Call helper functions
    printHelp()
    processInput(5)
}

// printHelp prints usage information
func printHelp() {
    fmt.Println("Calculator Usage:")
    fmt.Println("  add [a] [b]     - Add two numbers")
    fmt.Println("  subtract [a] [b] - Subtract b from a")
    fmt.Println("  multiply [a] [b] - Multiply two numbers")
    fmt.Println("  divide [a] [b]   - Divide a by b")
}

// processInput processes a user input
func processInput(choice int) {
    switch choice {
    case 1:
        fmt.Println("Selected: Add")
    case 2:
        fmt.Println("Selected: Subtract")
    case 3:
        fmt.Println("Selected: Multiply")
    case 4:
        fmt.Println("Selected: Divide")
    default:
        fmt.Println("Invalid choice")
    }
}
EOF

# Create a go.mod file
cd sample_project
go mod init sample_project
cd ..
```

## Step 3: Analyze the Project using the Command-Line Interface

Now let's use the recursive analyzer to analyze our sample project:

```bash
# Run the recursive analyzer
./recursive_analyzer/recursive_analyzer -path=./sample_project -output=analysis.json -verbose
```

You should see output similar to:

```
Analyzing ./sample_project...
Analysis complete!
Files analyzed: 2
Functions found: 10
Methods found: 4
Analysis time: 0.05 seconds
Results written to analysis.json
```

Now let's enhance the analysis results using the JSON formatter:

```bash
# Run the JSON formatter
./json_formatter/json_formatter analysis.json enhanced.json
```

You should see:

```
Enhanced JSON output written to enhanced.json
```

## Step 4: Explore the Analysis Results

Let's look at the analysis results:

```bash
# View the enhanced JSON file (you can use any text editor instead of cat)
cat enhanced.json
```

The enhanced JSON file contains information about:
- The files in the project
- The functions and methods in each file
- The packages in the project
- Metadata about the analysis

## Step 5: Start the Web Server

Now let's start the web server to explore the analysis using the web interface:

```bash
# Start the web server
cd webserver
./webserver -port=8001 -analyzer=../recursive_analyzer/recursive_analyzer -formatter=../json_formatter/json_formatter -static=./static
```

You should see:

```
2025/04/24 12:00:00 Starting server on http://localhost:8001
```

## Step 6: Access the Web Interface

Open a web browser and navigate to:

```
http://localhost:8001
```

You should see the Go File Analyzer web interface.

## Step 7: Analyze the Sample Project

1. In the "Analyze Project" input field, enter the path to the sample project:
   ```
   /path/to/go-file-analyzer/sample_project
   ```
   (Make sure to use the full absolute path)

2. Click the "Analyze" button

3. Wait for the analysis to complete

## Step 8: Explore the Project Structure

Now you can explore the project structure:

1. In the left sidebar, you'll see the packages in the project:
   - calculator
   - main

2. Click on the "calculator" package to expand it

3. Click on "calculator.go" to view the file contents

4. Notice that the functions and methods in the file are listed above the code view

## Step 9: Explore Functions and Methods

1. Click on the "Add" function in the function list
   - The function will be highlighted in the code view
   - You can see the function details, including line numbers and export status

2. Click on the "Subtract" method of the Calculator struct
   - The method will be highlighted in the code view
   - You can see that it's marked as a method rather than a function

## Step 10: Explore Visualizations

1. Click on the "Visualizations" tab at the top of the main panel

2. Click on "Package Dependencies" to see a graph of package dependencies
   - In our simple project, there's a dependency from "main" to "calculator"

3. Click on "Function Dependencies" to see function calls within a file
   - Select a file to see the function calls in that file

4. Click on "Complexity Heatmap" to see a visualization of function complexity
   - Functions with more lines of code are shown in darker colors

5. Click on "Package Tree" to see the hierarchical structure of packages
   - Our sample project has a simple structure with just two packages

## Congratulations!

You've successfully:
- Set up the Go File Analyzer
- Created a sample Go project
- Analyzed the project using both the CLI and web interface
- Explored the project structure, functions, and visualizations

## Next Steps

Now that you're familiar with the basic functionality, you can:
- Analyze your own Go projects
- Use the visualizations to understand code structure and complexity
- Integrate the analyzer into your development workflow
- Extend the tool with custom visualizations or analysis features

Check out the [DOCUMENTATION.md](DOCUMENTATION.md) file for more detailed information about the tool's features and capabilities.