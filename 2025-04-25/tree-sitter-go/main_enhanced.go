package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"

	"./parser"
)

func main() {
	// Parse command line arguments
	targetPath := flag.String("path", ".", "Path to the file or directory to analyze")
	outputPath := flag.String("output", "enhanced_analysis.json", "Path to output JSON file")
	skipLSP := flag.Bool("skip-lsp", true, "Skip LSP analysis (faster but won't find references)")
	enhancedMode := flag.Bool("enhanced", true, "Use enhanced parser with more details")
	webServer := flag.Bool("web", false, "Start web server for visualization")
	webPort := flag.Int("port", 8001, "Port for web server")
	flag.Parse()

	// Check if the path exists
	_, err := os.Stat(*targetPath)
	if err != nil {
		log.Fatalf("Error accessing path %s: %v", *targetPath, err)
	}

	if *webServer {
		// Start web server
		fmt.Printf("Starting web server on port %d...\n", *webPort)
		fmt.Printf("Analyzing path: %s\n", *targetPath)
		startWebServer(*targetPath, *webPort)
		return
	}

	// Perform analysis based on mode
	if *enhancedMode {
		fmt.Println("Using enhanced parser...")
		enhancedParser := parser.NewEnhancedGoFileParser()
		
		// Analyze path
		fmt.Printf("Analyzing %s...\n", *targetPath)
		functions, err := enhancedParser.ParsePath(*targetPath)
		if err != nil {
			log.Fatalf("Error analyzing path: %v", err)
		}
		
		fmt.Printf("Found %d functions/methods\n", len(functions))
		
		// Count methods vs functions
		methodCount := 0
		for _, f := range functions {
			if f.IsMethod {
				methodCount++
			}
		}
		fmt.Printf("Functions: %d, Methods: %d\n", len(functions)-methodCount, methodCount)
		
		// Create output structure
		type EnhancedAnalysisResult struct {
			Functions []parser.EnhancedFunctionInfo `json:"functions"`
		}
		
		result := EnhancedAnalysisResult{
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
	} else {
		// Use standard parser
		fmt.Println("Using standard parser...")
		
		// Analyze path
		fmt.Printf("Analyzing %s...\n", *targetPath)
		analysisOutput, err := parser.AnalyzeCodebase(*targetPath)
		if err != nil {
			log.Fatalf("Error analyzing codebase: %v", err)
		}
		
		fmt.Printf("Found %d files in %d packages with %d functions\n", 
			len(analysisOutput.Files), 
			len(analysisOutput.Packages), 
			len(analysisOutput.Functions))
		
		// Export to JSON
		err = parser.ExportAnalysisToJSON(analysisOutput, *outputPath)
		if err != nil {
			log.Fatalf("Error exporting analysis to JSON: %v", err)
		}
		
		fmt.Printf("Analysis complete. Results written to %s\n", *outputPath)
	}
}

// startWebServer starts the web server for visualization
func startWebServer(targetPath string, port int) {
	// This will be implemented in a later task
	fmt.Println("Web server functionality will be implemented in a later task.")
}