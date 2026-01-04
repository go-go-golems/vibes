package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"
	"time"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

// FunctionInfo represents a function or method
type FunctionInfo struct {
	Name       string `json:"name"`
	Filepath   string `json:"filepath"`
	StartLine  int    `json:"startLine"`
	EndLine    int    `json:"endLine"`
	Package    string `json:"package"`
	IsMethod   bool   `json:"isMethod"`
	IsExported bool   `json:"isExported"`
}

// AnalysisResult represents the result of analyzing a directory
type AnalysisResult struct {
	Functions []FunctionInfo `json:"functions"`
	Stats     struct {
		FileCount     int       `json:"fileCount"`
		FunctionCount int       `json:"functionCount"`
		MethodCount   int       `json:"methodCount"`
		PackageCount  int       `json:"packageCount"`
		AnalysisTime  float64   `json:"analysisTime"`
		Timestamp     time.Time `json:"timestamp"`
	} `json:"stats"`
}

func main() {
	// Path to analyze
	path := "./sample"
	if len(os.Args) > 1 {
		path = os.Args[1]
	}

	// Output file
	outputFile := "analysis.json"
	if len(os.Args) > 2 {
		outputFile = os.Args[2]
	}

	// Start time
	startTime := time.Now()

	// Find all Go files
	var goFiles []string
	err := filepath.Walk(path, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		// Skip directories we don't want to analyze
		if info.IsDir() {
			dir := info.Name()
			if dir == "vendor" || dir == "node_modules" || dir == ".git" {
				return filepath.SkipDir
			}
			return nil
		}
		// Only process .go files
		if filepath.Ext(path) == ".go" && !strings.HasSuffix(path, "_test.go") {
			goFiles = append(goFiles, path)
		}
		return nil
	})
	if err != nil {
		log.Fatalf("Error walking directory: %v", err)
	}

	fmt.Printf("Found %d Go files\n", len(goFiles))

	// Analyze each file
	var allFunctions []FunctionInfo
	packages := make(map[string]bool)
	fileCount := 0

	for _, file := range goFiles {
		functions, err := analyzeFile(file)
		if err != nil {
			log.Printf("Error analyzing %s: %v", file, err)
			continue
		}

		if len(functions) > 0 {
			// Add package to the set
			packages[functions[0].Package] = true
			// Add functions to the list
			allFunctions = append(allFunctions, functions...)
			fileCount++
		}
	}

	// Count methods
	methodCount := 0
	for _, function := range allFunctions {
		if function.IsMethod {
			methodCount++
		}
	}

	// Create result
	result := AnalysisResult{
		Functions: allFunctions,
		Stats: struct {
			FileCount     int       `json:"fileCount"`
			FunctionCount int       `json:"functionCount"`
			MethodCount   int       `json:"methodCount"`
			PackageCount  int       `json:"packageCount"`
			AnalysisTime  float64   `json:"analysisTime"`
			Timestamp     time.Time `json:"timestamp"`
		}{
			FileCount:     fileCount,
			FunctionCount: len(allFunctions),
			MethodCount:   methodCount,
			PackageCount:  len(packages),
			AnalysisTime:  time.Since(startTime).Seconds(),
			Timestamp:     time.Now(),
		},
	}

	// Write to JSON
	jsonData, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		log.Fatalf("Error marshaling to JSON: %v", err)
	}

	err = ioutil.WriteFile(outputFile, jsonData, 0644)
	if err != nil {
		log.Fatalf("Error writing file: %v", err)
	}

	// Print summary
	fmt.Printf("Analysis complete!\n")
	fmt.Printf("Files analyzed: %d\n", result.Stats.FileCount)
	fmt.Printf("Functions found: %d\n", result.Stats.FunctionCount-result.Stats.MethodCount)
	fmt.Printf("Methods found: %d\n", result.Stats.MethodCount)
	fmt.Printf("Packages found: %d\n", result.Stats.PackageCount)
	fmt.Printf("Analysis time: %.2f seconds\n", result.Stats.AnalysisTime)
	fmt.Printf("Results written to %s\n", outputFile)
}

// analyzeFile analyzes a Go file and returns all functions and methods
func analyzeFile(filePath string) ([]FunctionInfo, error) {
	// Read file
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, err
	}

	// Create parser
	parser := sitter.NewParser()
	parser.SetLanguage(golang.GetLanguage())

	// Parse file
	tree, err := parser.ParseCtx(nil, nil, content)
	if err != nil {
		return nil, err
	}
	defer tree.Close()

	// Get package name
	var packageName string
	packageQuery, err := sitter.NewQuery([]byte(`(package_clause (package_identifier) @package_name)`), golang.GetLanguage())
	if err != nil {
		return nil, err
	}

	cursor := sitter.NewQueryCursor()
	cursor.Exec(packageQuery, tree.RootNode())
	if match, ok := cursor.NextMatch(); ok {
		for _, capture := range match.Captures {
			packageName = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			break
		}
	}

	// Get functions and methods
	functionQuery, err := sitter.NewQuery([]byte(`
		(function_declaration name: (identifier) @func_name) @function
		(method_declaration name: (field_identifier) @method_name) @method
	`), golang.GetLanguage())
	if err != nil {
		return nil, err
	}

	cursor = sitter.NewQueryCursor()
	cursor.Exec(functionQuery, tree.RootNode())

	var functions []FunctionInfo

	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}

		var name string
		var node *sitter.Node
		var isMethod bool

		for _, capture := range match.Captures {
			if capture.Index == 0 { // func_name or method_name
				name = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			} else if capture.Index == 1 { // function or method
				node = capture.Node
				isMethod = capture.Node.Type() == "method_declaration"
			}
		}

		if name != "" && node != nil {
			startPoint := node.StartPoint()
			endPoint := node.EndPoint()

			// Check if exported (starts with uppercase)
			isExported := len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z'

			functions = append(functions, FunctionInfo{
				Name:       name,
				Filepath:   filePath,
				StartLine:  int(startPoint.Row) + 1,
				EndLine:    int(endPoint.Row) + 1,
				Package:    packageName,
				IsMethod:   isMethod,
				IsExported: isExported,
			})
		}
	}

	return functions, nil
}