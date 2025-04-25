package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"sort"
	"time"
)

// SimpleOutput represents a simpler output structure that works with the input data
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

// EnhanceJSON enhances a simple JSON file with more structured information
func EnhanceJSON(inputPath, outputPath string) error {
	// Read the input file
	data, err := ioutil.ReadFile(inputPath)
	if err != nil {
		return fmt.Errorf("error reading input file: %v", err)
	}
	
	// Parse the input JSON
	var input map[string]interface{}
	if err := json.Unmarshal(data, &input); err != nil {
		return fmt.Errorf("error parsing input JSON: %v", err)
	}
	
	// Create the output structure
	output := SimpleOutput{}
	
	// Set metadata
	output.Metadata.Name = filepath.Base(inputPath)
	basePath := "."
	if _, ok := input["Stats"]; ok {
		// Handle path from recursive analyzer
		basePath, _ = input["basePath"].(string)
	} else {
		// Get base path from files
		files := input["files"].([]interface{})
		if len(files) > 0 {
			fileMap := files[0].(map[string]interface{})
			filePath := fileMap["path"].(string)
			basePath = filepath.Dir(filePath)
		}
	}
	output.Metadata.BasePath = basePath
	
	// Get files from input
	var files []interface{}
	if _, ok := input["Stats"]; ok {
		files = input["Files"].([]interface{})
		output.Metadata.NumFiles = len(files)
	} else {
		files = input["files"].([]interface{})
		output.Metadata.NumFiles = len(files)
	}
	
	// Process files
	output.Files = make([]FileInfo, 0, len(files))
	
	for _, fileData := range files {
		fileMap := fileData.(map[string]interface{})
		
		fileInfo := FileInfo{
			Path:      fileMap["Path"].(string),
			Package:   fileMap["Package"].(string),
			Functions: []FunctionInfo{},
		}
		
		// Process functions in this file
		functions := fileMap["Functions"].([]interface{})
		fileInfo.Functions = make([]FunctionInfo, 0, len(functions))
		
		for _, funcData := range functions {
			funcMap := funcData.(map[string]interface{})
			
			name := funcMap["Name"].(string)
			isMethod := funcMap["IsMethod"].(bool)
			isExported := funcMap["IsExported"].(bool)
			
			functionInfo := FunctionInfo{
				ID:         fmt.Sprintf("%s.%s", fileInfo.Package, name),
				Name:       name,
				Filepath:   fileMap["Path"].(string),
				StartLine:  int(funcMap["StartLine"].(float64)),
				EndLine:    int(funcMap["EndLine"].(float64)),
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
	output.Metadata.NumFunctions = len(output.Functions)
	output.Metadata.Generated = time.Now()
	
	// Process packages
	// Group by package
	packageMap := make(map[string]*PackageInfo)
	for _, fileInfo := range output.Files {
		packageName := fileInfo.Package
		
		if _, ok := packageMap[packageName]; !ok {
			packageMap[packageName] = &PackageInfo{
				Name:      packageName,
				Path:      filepath.Dir(fileInfo.Path),
				Files:     []string{},
				Functions: []string{},
			}
		}
		
		packageInfo := packageMap[packageName]
		
		// Add file to package
		packageInfo.Files = append(packageInfo.Files, fileInfo.Path)
		
		// Add functions to package
		for _, function := range fileInfo.Functions {
			packageInfo.Functions = append(packageInfo.Functions, function.ID)
		}
	}
	
	// Convert package map to slice
	output.Packages = make([]PackageInfo, 0, len(packageMap))
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
		return fmt.Errorf("error marshaling output JSON: %v", err)
	}
	
	if err := ioutil.WriteFile(outputPath, outputData, 0644); err != nil {
		return fmt.Errorf("error writing output file: %v", err)
	}
	
	return nil
}

func main() {
	// Parse command line flags
	inputFile := flag.String("input", "", "Input JSON file")
	outputFile := flag.String("output", "", "Output JSON file")
	flag.Parse()
	
	// Check if input and output files are provided as command line arguments
	if *inputFile == "" && *outputFile == "" {
		if len(os.Args) != 3 {
			fmt.Println("Usage: json_formatter <input_json> <output_json>")
			fmt.Println("   or: json_formatter -input=<input_json> -output=<output_json>")
			os.Exit(1)
		}
		*inputFile = os.Args[1]
		*outputFile = os.Args[2]
	}
	
	// Enhance the JSON
	if err := EnhanceJSON(*inputFile, *outputFile); err != nil {
		log.Fatalf("Error enhancing JSON: %v", err)
	}
	
	fmt.Printf("Enhanced JSON output written to %s\n", *outputFile)
}