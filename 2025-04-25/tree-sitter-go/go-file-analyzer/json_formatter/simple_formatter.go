package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"sort"
	"time"
)

// SimpleOutput represents a simpler output structure
type SimpleOutput struct {
	Metadata struct {
		Name         string    `json:"name"`
		BasePath     string    `json:"basePath"`
		NumFiles     int       `json:"numFiles"`
		NumFunctions int       `json:"numFunctions"`
		NumMethods   int       `json:"numMethods"`
		Generated    time.Time `json:"generated"`
	} `json:"metadata"`
	Files       []FileInfo     `json:"files"`
	Functions   []FunctionInfo `json:"functions"`
	Packages    []PackageInfo  `json:"packages"`
}

// FileInfo represents a Go source file
type FileInfo struct {
	Path      string         `json:"path"`
	Package   string         `json:"package"`
	Functions []FunctionInfo `json:"functions"`
}

// FunctionInfo represents a function or method
type FunctionInfo struct {
	ID         string `json:"id"`
	Name       string `json:"name"`
	Filepath   string `json:"filepath"`
	StartLine  int    `json:"startLine"`
	EndLine    int    `json:"endLine"`
	Package    string `json:"package"`
	IsMethod   bool   `json:"isMethod"`
	IsExported bool   `json:"isExported"`
}

// PackageInfo represents a Go package
type PackageInfo struct {
	Name      string   `json:"name"`
	Path      string   `json:"path"`
	Files     []string `json:"files"`
	Functions []string `json:"functions"`
}

func main() {
	if len(os.Args) != 3 {
		fmt.Println("Usage: simple_formatter <input.json> <output.json>")
		os.Exit(1)
	}

	inputFile := os.Args[1]
	outputFile := os.Args[2]

	// Read the input JSON
	data, err := ioutil.ReadFile(inputFile)
	if err != nil {
		log.Fatalf("Error reading input file: %v", err)
	}

	// Parse the input JSON
	var input map[string]interface{}
	if err := json.Unmarshal(data, &input); err != nil {
		log.Fatalf("Error parsing JSON: %v", err)
	}

	// Create the enhanced output
	output := SimpleOutput{}
	
	// Set metadata
	output.Metadata.Name = filepath.Base(input["basePath"].(string))
	output.Metadata.BasePath = input["basePath"].(string)
	output.Metadata.NumFiles = len(input["files"].([]interface{}))
	output.Metadata.NumFunctions = len(input["functions"].([]interface{}))
	output.Metadata.Generated = time.Now()
	
	// Process files
	files := input["files"].([]interface{})
	output.Files = make([]FileInfo, 0, len(files))
	
	for _, fileData := range files {
		fileMap := fileData.(map[string]interface{})
		
		fileInfo := FileInfo{
			Path:      fileMap["path"].(string),
			Package:   fileMap["package"].(string),
			Functions: []FunctionInfo{},
		}
		
		// Process functions in this file
		functions := fileMap["functions"].([]interface{})
		fileInfo.Functions = make([]FunctionInfo, 0, len(functions))
		
		for _, funcData := range functions {
			funcMap := funcData.(map[string]interface{})
			
			name := funcMap["name"].(string)
			isMethod := funcMap["isMethod"].(bool)
			isExported := len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z'
			
			functionInfo := FunctionInfo{
				ID:         fmt.Sprintf("%s.%s", fileInfo.Package, name),
				Name:       name,
				Filepath:   fileMap["path"].(string),
				StartLine:  int(funcMap["startLine"].(float64)),
				EndLine:    int(funcMap["endLine"].(float64)),
				Package:    fileInfo.Package,
				IsMethod:   isMethod,
				IsExported: isExported,
			}
			
			fileInfo.Functions = append(fileInfo.Functions, functionInfo)
		}
		
		output.Files = append(output.Files, fileInfo)
	}
	
	// Process all functions
	output.Functions = make([]FunctionInfo, 0)
	for _, fileInfo := range output.Files {
		output.Functions = append(output.Functions, fileInfo.Functions...)
	}
	
	// Count methods
	methodCount := 0
	for _, function := range output.Functions {
		if function.IsMethod {
			methodCount++
		}
	}
	output.Metadata.NumMethods = methodCount
	
	// Process packages
	packages := input["packages"].(map[string]interface{})
	output.Packages = make([]PackageInfo, 0, len(packages))
	
	packageMap := make(map[string]*PackageInfo)
	for packageName, packagePath := range packages {
		packageInfo := PackageInfo{
			Name:      packageName,
			Path:      packagePath.(string),
			Files:     []string{},
			Functions: []string{},
		}
		
		packageMap[packageName] = &packageInfo
	}
	
	// Add files and functions to packages
	for _, fileInfo := range output.Files {
		packageName := fileInfo.Package
		packageInfo := packageMap[packageName]
		
		// Add file to package
		packageInfo.Files = append(packageInfo.Files, fileInfo.Path)
		
		// Add functions to package
		for _, function := range fileInfo.Functions {
			packageInfo.Functions = append(packageInfo.Functions, function.ID)
		}
	}
	
	// Convert package map to slice
	for _, packageInfo := range packageMap {
		output.Packages = append(output.Packages, *packageInfo)
	}
	
	// Sort packages by name
	sort.Slice(output.Packages, func(i, j int) bool {
		return output.Packages[i].Name < output.Packages[j].Name
	})
	
	// Write the output
	outputData, err := json.MarshalIndent(output, "", "  ")
	if err != nil {
		log.Fatalf("Error marshaling JSON: %v", err)
	}
	
	if err := ioutil.WriteFile(outputFile, outputData, 0644); err != nil {
		log.Fatalf("Error writing output file: %v", err)
	}
	
	fmt.Printf("Enhanced JSON output written to %s\n", outputFile)
}
