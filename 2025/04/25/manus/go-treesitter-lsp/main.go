package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
)

// FunctionInfo represents information about a Go function
type FunctionInfo struct {
	Name       string `json:"name"`
	FilePath   string `json:"file_path"`
	StartLine  int    `json:"start_line"`
	EndLine    int    `json:"end_line"`
	Parameters string `json:"parameters"`
	ReturnType string `json:"return_type,omitempty"`
	IsMethod   bool   `json:"is_method"`
	Receiver   string `json:"receiver,omitempty"`
}

// Reference represents a reference to a function
type Reference struct {
	FilePath  string `json:"file_path"`
	Line      int    `json:"line"`
	Column    int    `json:"column"`
	Context   string `json:"context"`
	Type      string `json:"type"` // "definition" or "usage"
}

// FunctionData represents the combined data for a function
type FunctionData struct {
	Function   FunctionInfo `json:"function"`
	References []Reference  `json:"references"`
}

// extractFunctions extracts function information from a Go file
func extractFunctions(filePath string) ([]FunctionInfo, error) {
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, filePath, nil, parser.ParseComments)
	if err != nil {
		return nil, fmt.Errorf("failed to parse file %s: %v", filePath, err)
	}

	var functions []FunctionInfo

	// Read file content for parameter and return type extraction
	content, err := os.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read file %s: %v", filePath, err)
	}
	lines := strings.Split(string(content), "\n")

	// Extract functions and methods
	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncDecl:
			startPos := fset.Position(x.Pos())
			endPos := fset.Position(x.End())
			
			// Extract parameters
			params := ""
			if x.Type.Params != nil {
				paramPos := fset.Position(x.Type.Params.Pos())
				paramEndPos := fset.Position(x.Type.Params.End())
				if paramPos.Line <= len(lines) && paramEndPos.Line <= len(lines) {
					// Extract from source to preserve original formatting
					params = string(content[x.Type.Params.Pos()-1:x.Type.Params.End()-1])
				}
			}
			
			// Extract return type
			returns := ""
			if x.Type.Results != nil {
				resultPos := fset.Position(x.Type.Results.Pos())
				resultEndPos := fset.Position(x.Type.Results.End())
				if resultPos.Line <= len(lines) && resultEndPos.Line <= len(lines) {
					// Extract from source to preserve original formatting
					returns = string(content[x.Type.Results.Pos()-1:x.Type.Results.End()-1])
				}
			}
			
			isMethod := x.Recv != nil
			receiver := ""
			
			if isMethod && len(x.Recv.List) > 0 {
				// Extract receiver type
				recvPos := fset.Position(x.Recv.Pos())
				recvEndPos := fset.Position(x.Recv.End())
				
				if recvPos.Line <= len(lines) && recvEndPos.Line <= len(lines) {
					receiver = string(content[x.Recv.Pos()-1:x.Recv.End()-1])
					receiver = strings.TrimPrefix(receiver, "(")
					receiver = strings.TrimSuffix(receiver, ")")
				}
			}
			
			function := FunctionInfo{
				Name:       x.Name.Name,
				FilePath:   filePath,
				StartLine:  startPos.Line,
				EndLine:    endPos.Line,
				Parameters: params,
				ReturnType: returns,
				IsMethod:   isMethod,
				Receiver:   receiver,
			}
			
			functions = append(functions, function)
		}
		return true
	})

	return functions, nil
}

// findReferences finds all references (usages and definitions) of a function in a directory
func findReferences(funcName string, isMethod bool, receiver string, dirPath string) ([]Reference, error) {
	var references []Reference
	fset := token.NewFileSet()

	// Walk through all Go files in the directory
	err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && strings.HasSuffix(path, ".go") {
			// Parse the Go file
			node, err := parser.ParseFile(fset, path, nil, parser.AllErrors)
			if err != nil {
				fmt.Printf("Warning: Error processing file %s: %v\n", path, err)
				return nil // Continue with other files
			}

			// Read file content for context extraction
			content, err := os.ReadFile(path)
			if err != nil {
				fmt.Printf("Warning: Error reading file %s: %v\n", path, err)
				return nil
			}
			lines := strings.Split(string(content), "\n")

			// Check for function definitions and usages
			ast.Inspect(node, func(n ast.Node) bool {
				switch x := n.(type) {
				case *ast.FuncDecl:
					// Check if this is the function we're looking for
					if x.Name.Name == funcName {
						// For methods, check receiver type
						if isMethod && x.Recv != nil {
							// Skip if receiver doesn't match
							if !receiverMatches(x.Recv, receiver) {
								return true
							}
						} else if isMethod != (x.Recv != nil) {
							// Skip if method status doesn't match
							return true
						}
						
						pos := fset.Position(x.Pos())
						
						// Extract context (the function signature line)
						lineIndex := pos.Line - 1 // Convert to 0-based index
						context := ""
						if lineIndex >= 0 && lineIndex < len(lines) {
							context = strings.TrimSpace(lines[lineIndex])
						}
						
						references = append(references, Reference{
							FilePath: path,
							Line:     pos.Line,
							Column:   pos.Column,
							Context:  context,
							Type:     "definition",
						})
					}
				case *ast.CallExpr:
					// Check for function calls
					if ident, ok := x.Fun.(*ast.Ident); ok && ident.Name == funcName && !isMethod {
						pos := fset.Position(ident.Pos())
						
						// Extract context (the line containing the function call)
						lineIndex := pos.Line - 1 // Convert to 0-based index
						context := ""
						if lineIndex >= 0 && lineIndex < len(lines) {
							context = strings.TrimSpace(lines[lineIndex])
						}
						
						references = append(references, Reference{
							FilePath: path,
							Line:     pos.Line,
							Column:   pos.Column,
							Context:  context,
							Type:     "usage",
						})
					}
				case *ast.SelectorExpr:
					// Check for method calls
					if isMethod && x.Sel.Name == funcName {
						pos := fset.Position(x.Sel.Pos())
						
						// Extract context
						lineIndex := pos.Line - 1
						context := ""
						if lineIndex >= 0 && lineIndex < len(lines) {
							context = strings.TrimSpace(lines[lineIndex])
						}
						
						references = append(references, Reference{
							FilePath: path,
							Line:     pos.Line,
							Column:   pos.Column,
							Context:  context,
							Type:     "usage",
						})
					}
				}
				return true
			})
		}
		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("failed to walk directory %s: %v", dirPath, err)
	}

	return references, nil
}

// receiverMatches checks if a receiver matches the expected receiver type
func receiverMatches(recv *ast.FieldList, expectedReceiver string) bool {
	if len(recv.List) == 0 {
		return false
	}
	
	// Extract receiver type as string
	var actualRecvType string
	switch rt := recv.List[0].Type.(type) {
	case *ast.StarExpr:
		if ident, ok := rt.X.(*ast.Ident); ok {
			actualRecvType = "*" + ident.Name
		}
	case *ast.Ident:
		actualRecvType = rt.Name
	default:
		return false
	}
	
	// Simple comparison - could be improved for complex receivers
	expectedReceiver = strings.TrimSpace(expectedReceiver)
	parts := strings.Fields(expectedReceiver)
	if len(parts) > 1 {
		// Handle cases like "p *Person"
		expectedType := parts[1]
		return actualRecvType == expectedType
	} else if len(parts) == 1 {
		// Handle cases like "*Person"
		return actualRecvType == parts[0]
	}
	
	return false
}

func main() {
	// Parse command line arguments
	var (
		filePath    string
		dirPath     string
		outputFile  string
		prettyPrint bool
	)

	flag.StringVar(&filePath, "file", "", "Path to a Go file to analyze")
	flag.StringVar(&dirPath, "dir", "", "Path to a directory containing Go files to analyze")
	flag.StringVar(&outputFile, "output", "", "Path to output JSON file (default: stdout)")
	flag.BoolVar(&prettyPrint, "pretty", false, "Pretty print JSON output")
	flag.Parse()

	if filePath == "" && dirPath == "" {
		fmt.Println("Error: Either -file or -dir must be specified")
		flag.Usage()
		os.Exit(1)
	}

	// Process files and collect function data
	var allFunctions []FunctionInfo
	var searchPath string

	if filePath != "" {
		// Process single file
		functions, err := extractFunctions(filePath)
		if err != nil {
			fmt.Printf("Error parsing file: %v\n", err)
			os.Exit(1)
		}
		allFunctions = append(allFunctions, functions...)
		searchPath = filepath.Dir(filePath)
	} else {
		// Process directory
		err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			if !info.IsDir() && strings.HasSuffix(path, ".go") {
				functions, err := extractFunctions(path)
				if err != nil {
					fmt.Printf("Warning: Error processing file %s: %v\n", path, err)
					return nil // Continue with other files
				}
				allFunctions = append(allFunctions, functions...)
			}
			return nil
		})
		if err != nil {
			fmt.Printf("Error walking directory: %v\n", err)
			os.Exit(1)
		}
		searchPath = dirPath
	}

	// Find references for each function
	var result []FunctionData

	for _, function := range allFunctions {
		references, err := findReferences(function.Name, function.IsMethod, function.Receiver, searchPath)
		if err != nil {
			fmt.Printf("Warning: Error finding references for function %s: %v\n", function.Name, err)
		}

		result = append(result, FunctionData{
			Function:   function,
			References: references,
		})
	}

	// Output JSON
	var jsonData []byte
	var err error

	if prettyPrint {
		jsonData, err = json.MarshalIndent(result, "", "  ")
	} else {
		jsonData, err = json.Marshal(result)
	}

	if err != nil {
		fmt.Printf("Error generating JSON: %v\n", err)
		os.Exit(1)
	}

	if outputFile != "" {
		err = os.WriteFile(outputFile, jsonData, 0644)
		if err != nil {
			fmt.Printf("Error writing to output file: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("Output written to %s\n", outputFile)
	} else {
		fmt.Println(string(jsonData))
	}
}
