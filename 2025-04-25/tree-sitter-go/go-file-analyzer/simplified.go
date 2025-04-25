package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"
	"sync"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

// FunctionInfo represents information about a function
type FunctionInfo struct {
	Name      string `json:"name"`
	Filepath  string `json:"filepath"`
	StartLine int    `json:"startLine"`
	EndLine   int    `json:"endLine"`
}

// Analysis result
type AnalysisResult struct {
	Functions []FunctionInfo `json:"functions"`
}

func main() {
	// Parse command line arguments
	targetPath := flag.String("path", ".", "Path to the file or directory to analyze")
	outputPath := flag.String("output", "analysis.json", "Path to output JSON file")
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
	functions := analyzePathSimplified(*targetPath, parser)
	
	fmt.Printf("Found %d functions using Tree-sitter\n", len(functions))

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

// analyzePathSimplified analyzes a file or recursively analyzes a directory
func analyzePathSimplified(path string, parser *sitter.Parser) []FunctionInfo {
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
				// Skip files with test suffix
				if strings.HasSuffix(filePath, "_test.go") {
					return nil
				}
				fileFunctions := analyzeGoFileSimplified(filePath)
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
		functions = analyzeGoFileSimplified(path)
	} else {
		log.Printf("Skipping non-Go file: %s", path)
	}

	return functions
}

// analyzeGoFileSimplified analyzes a single Go file and extracts function information
func analyzeGoFileSimplified(filePath string) []FunctionInfo {
	// Read file content
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		log.Printf("Error reading file %s: %v", filePath, err)
		return nil
	}
	
	// Simple parsing using regular expressions and line scanning
	var functions []FunctionInfo
	lines := strings.Split(string(content), "\n")
	
	for i, line := range lines {
		line = strings.TrimSpace(line)
		
		// Check if line contains a function declaration
		if strings.HasPrefix(line, "func ") {
			// Extract function name
			parts := strings.Fields(line)
			if len(parts) < 2 {
				continue
			}
			
			name := parts[1]
			// If it's a method, extract just the method name
			if strings.Contains(name, "(") {
				name = name[:strings.Index(name, "(")]
			}
			// If it's a function with parameters, extract just the function name
			if strings.Contains(name, "(") {
				name = name[:strings.Index(name, "(")]
			}
			
			// Find the end of the function
			endLine := i
			braceCount := 0
			foundOpenBrace := false
			
			// Scan forward to find the end of the function
			for j := i; j < len(lines); j++ {
				for _, c := range lines[j] {
					if c == '{' {
						braceCount++
						foundOpenBrace = true
					} else if c == '}' {
						braceCount--
					}
				}
				
				if foundOpenBrace && braceCount == 0 {
					endLine = j
					break
				}
			}
			
			// Add the function info
			functionInfo := FunctionInfo{
				Name:      name,
				Filepath:  filePath,
				StartLine: i + 1, // 1-based line numbering
				EndLine:   endLine + 1,
			}
			
			functions = append(functions, functionInfo)
		}
	}
	
	return functions
}