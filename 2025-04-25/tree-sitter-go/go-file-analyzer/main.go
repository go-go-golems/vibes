package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"sync"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

// FunctionInfo represents information about a function
type FunctionInfo struct {
	Name       string      `json:"name"`
	Filepath   string      `json:"filepath"`
	StartLine  int         `json:"startLine"`
	EndLine    int         `json:"endLine"`
	References []Reference `json:"references,omitempty"`
}

// Reference represents a reference to a function (where it's used)
type Reference struct {
	Filepath   string `json:"filepath"`
	Line       int    `json:"line"`
	Column     int    `json:"column"`
	IsDefining bool   `json:"isDefining"`
}

// Analysis result
type AnalysisResult struct {
	Functions []FunctionInfo `json:"functions"`
}

func main() {
	// Parse command line arguments
	targetPath := flag.String("path", ".", "Path to the file or directory to analyze")
	outputPath := flag.String("output", "analysis.json", "Path to output JSON file")
	skipLSP := flag.Bool("skip-lsp", false, "Skip LSP analysis (faster but won't find references)")
	flag.Parse()

	// Check if the path exists
	_, err := os.Stat(*targetPath)
	if err != nil {
		log.Fatalf("Error accessing path %s: %v", *targetPath, err)
	}

	// Create Tree-sitter parser for Go
	parser := sitter.NewParser()
	parser.SetLanguage(golang.GetLanguage())

	// Analyze files and collect function information
	functions := analyzePath(*targetPath, parser)
	
	fmt.Printf("Found %d functions using Tree-sitter\n", len(functions))

	// Use LSP to find references if not skipped
	if !*skipLSP {
		absPath, err := filepath.Abs(*targetPath)
		if err != nil {
			log.Fatalf("Error getting absolute path: %v", err)
		}
		
		fmt.Println("Analyzing function references using LSP...")
		client, err := NewLSPClient(absPath)
		if err != nil {
			log.Printf("Warning: Failed to create LSP client: %v", err)
			log.Printf("Continuing without LSP analysis (no references will be found)")
		} else {
			functions, err = AnalyzeFunctionReferences(functions, client)
			if err != nil {
				log.Printf("Warning: LSP analysis partial or failed: %v", err)
			}
		}
	}

	// Create analysis result
	result := AnalysisResult{
		Functions: functions,
	}

	// Convert to JSON
	jsonData, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		log.Fatalf("Error marshaling to JSON: %v", err)
	}

	// Write to output file
	err = ioutil.WriteFile(*outputPath, jsonData, 0644)
	if err != nil {
		log.Fatalf("Error writing output file: %v", err)
	}

	fmt.Printf("Analysis complete. Results written to %s\n", *outputPath)
}

// analyzePath analyzes a file or recursively analyzes a directory
func analyzePath(path string, parser *sitter.Parser) []FunctionInfo {
	var functions []FunctionInfo
	var mu sync.Mutex

	// Check if path is a directory or a file
	fileInfo, err := os.Stat(path)
	if err != nil {
		log.Fatalf("Error accessing path %s: %v", path, err)
		return nil
	}

	if fileInfo.IsDir() {
		// Process all Go files in the directory recursively
		err := filepath.Walk(path, func(filePath string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			// Only process .go files
			if !info.IsDir() && filepath.Ext(filePath) == ".go" {
				fileFunctions := analyzeGoFile(filePath, parser)
				mu.Lock()
				functions = append(functions, fileFunctions...)
				mu.Unlock()
			}
			return nil
		})
		if err != nil {
			log.Printf("Error walking directory: %v", err)
		}
	} else if filepath.Ext(path) == ".go" {
		// Process a single Go file
		functions = analyzeGoFile(path, parser)
	} else {
		log.Printf("Skipping non-Go file: %s", path)
	}

	return functions
}

// analyzeGoFile analyzes a single Go file and extracts function information
func analyzeGoFile(filePath string, parser *sitter.Parser) []FunctionInfo {
	// Read file content
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		log.Printf("Error reading file %s: %v", filePath, err)
		return nil
	}

	// Parse file with Tree-sitter
	tree, err := parser.ParseCtx(nil, nil, content)
	if err != nil {
		log.Printf("Error parsing file %s: %v", filePath, err)
		return nil
	}
	defer tree.Close()

	// Query to find function declarations
	query, err := sitter.NewQuery([]byte(`
		(function_declaration name: (identifier) @func_name) @function
		(method_declaration name: (field_identifier) @method_name) @method
	`), golang.GetLanguage())
	if err != nil {
		log.Printf("Error creating query: %v", err)
		return nil
	}

	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, tree.RootNode())

	var functions []FunctionInfo

	// Process query matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}

		var functionNode *sitter.Node
		var nameNode *sitter.Node

		// Extract function name and node
		for _, capture := range match.Captures {
			// Check the capture name
			switch capture.Index {
			case 0: // function or method
				functionNode = capture.Node
			case 1: // func_name or method_name
				nameNode = capture.Node
			}
		}

		if functionNode != nil && nameNode != nil {
			startPoint := functionNode.StartPoint()
			endPoint := functionNode.EndPoint()
			
			functionInfo := FunctionInfo{
				Name:      string(content[nameNode.StartByte():nameNode.EndByte()]),
				Filepath:  filePath,
				StartLine: int(startPoint.Row) + 1, // Convert to 1-based line numbering
				EndLine:   int(endPoint.Row) + 1,   // Convert to 1-based line numbering
				References: []Reference{},          // Will be populated later with LSP
			}
			
			functions = append(functions, functionInfo)
		}
	}

	return functions
}