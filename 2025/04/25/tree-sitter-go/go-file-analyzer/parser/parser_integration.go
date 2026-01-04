package parser

import (
	"encoding/json"
	"io/ioutil"
	"log"
	"path/filepath"
	"sync"
)

// AnalysisOutput represents the structure of the final JSON output
type AnalysisOutput struct {
	Files     []FileStructure    `json:"files"`
	Functions []FunctionInfo     `json:"functions"`
	Methods   []EnhancedFunctionInfo `json:"methods,omitempty"`
	Packages  []PackageInfo      `json:"packages"`
}

// AnalyzeCodebase analyzes an entire codebase and returns a structured analysis
func AnalyzeCodebase(path string) (*AnalysisOutput, error) {
	parser := NewGoFileParser()
	enhancedParser := NewEnhancedGoFileParser()
	
	// Analyze files
	files, err := analyzeFiles(path, parser)
	if err != nil {
		return nil, err
	}
	
	// Extract function information
	var functions []FunctionInfo
	for _, file := range files {
		functions = append(functions, file.Functions...)
	}
	
	// Get enhanced function information (including methods)
	enhancedFunctions, err := enhancedParser.ParsePath(path)
	if err != nil {
		log.Printf("Warning: Enhanced parser encountered an error: %v", err)
		// Continue with basic function info
	}
	
	// Extract methods (functions with receivers)
	var methods []EnhancedFunctionInfo
	for _, f := range enhancedFunctions {
		if f.IsMethod {
			methods = append(methods, f)
		}
	}
	
	// Group files by package
	packages := groupFilesByPackage(files)
	
	// Create analysis output
	output := &AnalysisOutput{
		Files:     files,
		Functions: functions,
		Methods:   methods,
		Packages:  packages,
	}
	
	return output, nil
}

// analyzeFiles analyzes all files in a path
func analyzeFiles(path string, parser *GoFileParser) ([]FileStructure, error) {
	var files []FileStructure
	var mu sync.Mutex
	
	// Check if path is a directory or a file
	fileInfo, err := os.Stat(path)
	if err != nil {
		return nil, err
	}
	
	if fileInfo.IsDir() {
		// Process all Go files in the directory recursively
		err := filepath.Walk(path, func(filePath string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			// Only process .go files
			if !info.IsDir() && filepath.Ext(filePath) == ".go" {
				// Skip test files
				if filepath.Base(filePath)[0] == '_' || filepath.Ext(filePath) == "_test.go" {
					return nil
				}
				
				fileStructure, err := parser.AnalyzeFileStructure(filePath)
				if err != nil {
					log.Printf("Error analyzing file %s: %v", filePath, err)
					return nil
				}
				
				mu.Lock()
				files = append(files, *fileStructure)
				mu.Unlock()
			}
			return nil
		})
		
		if err != nil {
			return nil, err
		}
	} else if filepath.Ext(path) == ".go" {
		// Process a single Go file
		fileStructure, err := parser.AnalyzeFileStructure(path)
		if err != nil {
			return nil, err
		}
		files = append(files, *fileStructure)
	}
	
	return files, nil
}

// groupFilesByPackage groups files by their package
func groupFilesByPackage(files []FileStructure) []PackageInfo {
	// Create a map of package names to file paths
	packageMap := make(map[string][]string)
	
	for _, file := range files {
		packageMap[file.Package] = append(packageMap[file.Package], file.Path)
	}
	
	// Convert map to slice
	var packages []PackageInfo
	for packageName, filePaths := range packageMap {
		packages = append(packages, PackageInfo{
			Name:      packageName,
			FilePaths: filePaths,
		})
	}
	
	return packages
}

// ExportAnalysisToJSON exports the analysis to a JSON file
func ExportAnalysisToJSON(analysis *AnalysisOutput, outputPath string) error {
	// Convert to JSON
	jsonData, err := json.MarshalIndent(analysis, "", "  ")
	if err != nil {
		return err
	}
	
	// Write to file
	err = ioutil.WriteFile(outputPath, jsonData, 0644)
	if err != nil {
		return err
	}
	
	return nil
}

// AnalyzeGoFile analyzes a single Go file and returns information about functions, imports, and package
func AnalyzeGoFile(filePath string) (*FileStructure, error) {
	parser := NewGoFileParser()
	return parser.AnalyzeFileStructure(filePath)
}

// AnalyzeGoFileEnhanced analyzes a single Go file and returns enhanced information
func AnalyzeGoFileEnhanced(filePath string) ([]EnhancedFunctionInfo, error) {
	parser := NewEnhancedGoFileParser()
	return parser.ParseFile(filePath)
}