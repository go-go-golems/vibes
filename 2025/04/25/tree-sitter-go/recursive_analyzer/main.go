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
	"time"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

// FileInfo represents a Go file
type FileInfo struct {
	Path      string         `json:"path"`
	Package   string         `json:"package"`
	Functions []FunctionInfo `json:"functions"`
}

// FunctionInfo represents a function
type FunctionInfo struct {
	Name       string   `json:"name"`
	IsMethod   bool     `json:"isMethod"`
	IsExported bool     `json:"isExported"`
	Filepath   string   `json:"filepath"`
	StartLine  int      `json:"startLine"`
	EndLine    int      `json:"endLine"`
	ReturnType []string `json:"returnType,omitempty"`
	Package    string   `json:"package"`
}

// AnalysisResult represents the analysis result
type AnalysisResult struct {
	Files     []FileInfo `json:"files"`
	Functions []FunctionInfo `json:"functions"`
	Stats     struct {
		FileCount     int       `json:"fileCount"`
		FunctionCount int       `json:"functionCount"`
		MethodCount   int       `json:"methodCount"`
		AnalysisTime  float64   `json:"analysisTime"`
		Timestamp     time.Time `json:"timestamp"`
	} `json:"stats"`
}

// Analyzes a Go file and returns functions
func analyzeFile(filePath string) ([]FunctionInfo, string, error) {
	// Read file
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, "", err
	}
	
	// Setup Tree-sitter
	parser := sitter.NewParser()
	parser.SetLanguage(golang.GetLanguage())
	
	// Parse the file
	tree, err := parser.ParseCtx(nil, nil, content)
	if err != nil {
		return nil, "", err
	}
	defer tree.Close()
	
	// Extract package name
	pkgQuery, err := sitter.NewQuery([]byte(`(package_clause (package_identifier) @package_name)`), golang.GetLanguage())
	if err != nil {
		return nil, "", err
	}
	
	pkgCursor := sitter.NewQueryCursor()
	pkgCursor.Exec(pkgQuery, tree.RootNode())
	
	var packageName string
	if match, ok := pkgCursor.NextMatch(); ok {
		for _, capture := range match.Captures {
			packageName = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			break
		}
	}
	
	// Extract functions
	funcQuery, err := sitter.NewQuery(
		[]byte(`
			(function_declaration name: (identifier) @func_name) @function
			(method_declaration name: (field_identifier) @method_name) @method
		`), 
		golang.GetLanguage(),
	)
	if err != nil {
		return nil, packageName, err
	}
	
	cursor := sitter.NewQueryCursor()
	cursor.Exec(funcQuery, tree.RootNode())
	
	var functions []FunctionInfo
	
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		
		var functionNode *sitter.Node
		var nameNode *sitter.Node
		var isMethod bool
		
		for _, capture := range match.Captures {
			switch capture.Index {
			case 0: // func_name or method_name
				nameNode = capture.Node
			case 1: // function or method
				functionNode = capture.Node
				// determine if it's a method based on the query
				isMethod = (capture.Node.Type() == "method_declaration")
			}
		}
		
		if nameNode != nil && functionNode != nil {
			name := string(content[nameNode.StartByte():nameNode.EndByte()])
			isExported := len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z'
			
			startPoint := functionNode.StartPoint()
			endPoint := functionNode.EndPoint()
			
			functionInfo := FunctionInfo{
				Name:       name,
				IsMethod:   isMethod,
				IsExported: isExported,
				Filepath:   filePath,
				StartLine:  int(startPoint.Row) + 1,
				EndLine:    int(endPoint.Row) + 1,
				Package:    packageName,
			}
			
			functions = append(functions, functionInfo)
		}
	}
	
	return functions, packageName, nil
}

// Recursively analyze all Go files in a directory
func analyzeDirectory(dir string) ([]FileInfo, []FunctionInfo, error) {
	var files []FileInfo
	var allFunctions []FunctionInfo
	
	// Walk the directory
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		
		// Skip vendor, node_modules, etc.
		if info.IsDir() {
			name := info.Name()
			if name == "vendor" || name == "node_modules" || name == ".git" || name == "dist" {
				return filepath.SkipDir
			}
			return nil
		}
		
		// Only process .go files
		if filepath.Ext(path) == ".go" && !strings.HasSuffix(path, "_test.go") {
			// Analyze file
			functions, packageName, err := analyzeFile(path)
			if err != nil {
				log.Printf("Error analyzing %s: %v", path, err)
				return nil // Continue with other files
			}
			
			// Create file info
			fileInfo := FileInfo{
				Path:      path,
				Package:   packageName,
				Functions: functions,
			}
			
			files = append(files, fileInfo)
			allFunctions = append(allFunctions, functions...)
		}
		
		return nil
	})
	
	return files, allFunctions, err
}

func main() {
	// Parse flags
	basePath := flag.String("path", ".", "Path to the directory to analyze")
	outputPath := flag.String("output", "analysis.json", "Path to output JSON file")
	verbose := flag.Bool("verbose", false, "Enable verbose output")
	flag.Parse()
	
	if *verbose {
		log.Printf("Analyzing %s...", *basePath)
	}
	
	// Start timer
	startTime := time.Now()
	
	// Analyze directory
	files, functions, err := analyzeDirectory(*basePath)
	if err != nil {
		log.Fatalf("Error analyzing directory: %v", err)
	}
	
	// Count methods
	methodCount := 0
	for _, function := range functions {
		if function.IsMethod {
			methodCount++
		}
	}
	
	// Create result
	result := AnalysisResult{
		Files:     files,
		Functions: functions,
		Stats: struct {
			FileCount     int       `json:"fileCount"`
			FunctionCount int       `json:"functionCount"`
			MethodCount   int       `json:"methodCount"`
			AnalysisTime  float64   `json:"analysisTime"`
			Timestamp     time.Time `json:"timestamp"`
		}{
			FileCount:     len(files),
			FunctionCount: len(functions),
			MethodCount:   methodCount,
			AnalysisTime:  time.Since(startTime).Seconds(),
			Timestamp:     time.Now(),
		},
	}
	
	// Write JSON
	jsonData, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		log.Fatalf("Error creating JSON: %v", err)
	}
	
	err = ioutil.WriteFile(*outputPath, jsonData, 0644)
	if err != nil {
		log.Fatalf("Error writing output file: %v", err)
	}
	
	// Print summary
	fmt.Printf("Analysis complete!\n")
	fmt.Printf("Files analyzed: %d\n", result.Stats.FileCount)
	fmt.Printf("Functions found: %d\n", result.Stats.FunctionCount - result.Stats.MethodCount)
	fmt.Printf("Methods found: %d\n", result.Stats.MethodCount)
	fmt.Printf("Analysis time: %.2f seconds\n", result.Stats.AnalysisTime)
	fmt.Printf("Results written to %s\n", *outputPath)
}