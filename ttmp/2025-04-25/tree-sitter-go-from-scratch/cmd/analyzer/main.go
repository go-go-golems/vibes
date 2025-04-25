package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/your-org/go-analyzer/internal/analysis"
	"github.com/your-org/go-analyzer/internal/formatter"
	"github.com/your-org/go-analyzer/internal/parser"
)

func main() {
	// Define command-line flags
	pathFlag := flag.String("path", ".", "Directory path to analyze")
	outputFlag := flag.String("output", "", "Output JSON file path (default: stdout)")
	excludeFlag := flag.String("exclude", "vendor,.git", "Comma-separated list of directories to exclude")
	formatFlag := flag.String("format", "basic", "Output format: basic or enhanced")

	flag.Parse()

	// Validate format flag
	if *formatFlag != "basic" && *formatFlag != "enhanced" {
		log.Fatalf("Invalid format specified: %s. Use 'basic' or 'enhanced'.", *formatFlag)
	}

	// Parse exclude directories
	var excludeDirs []string
	if *excludeFlag != "" {
		excludeDirs = strings.Split(*excludeFlag, ",")
		for i := range excludeDirs {
			excludeDirs[i] = strings.TrimSpace(excludeDirs[i])
		}
	}

	// Create a context
	ctx := context.Background()

	// Instantiate the parser
	p, err := parser.NewParser()
	if err != nil {
		log.Fatalf("Error creating parser: %v", err)
	}
	defer p.Close()

	// --- Run Analysis (Phase 2 approach first to get file list) ---
	log.Printf("Analyzing directory: %s (excluding: %v), Format: %s\n", *pathFlag, excludeDirs, *formatFlag)
	// We still run the basic analysis first to get the list of files respecting exclusions.
	basicAnalysisResult, err := analysis.AnalyzeDirectory(ctx, *pathFlag, p, excludeDirs)
	if err != nil {
		log.Fatalf("Error during initial directory scan %s: %v", *pathFlag, err)
	}

	var outputData []byte // Will hold the final JSON bytes

	if *formatFlag == "enhanced" {
		log.Println("Performing enhanced parsing...")
		// --- Temporary Step: Run Enhanced Parsing Separately ---
		// In a future refactor, AnalyzeDirectory should directly use ParseFileEnhanced
		enhancedDataMap := make(map[string]*parser.EnhancedFileInfo)
		for _, fileResult := range basicAnalysisResult.Files {
			absPath := filepath.Join(basicAnalysisResult.BasePath, fileResult.Path)
			enhancedInfo, err := p.ParseFileEnhanced(ctx, absPath)
			if err != nil {
				// Log error but continue, maybe add to an error list in JSON later?
				log.Printf("Warning: Failed to perform enhanced parse on %s: %v", fileResult.Path, err)
				continue
			}
			enhancedDataMap[absPath] = enhancedInfo
		}
		log.Printf("Enhanced parsing complete for %d files.", len(enhancedDataMap))

		// --- Format Enhanced Output ---
		enhancedOutput, err := formatter.FormatEnhancedJSON(basicAnalysisResult, enhancedDataMap)
		if err != nil {
			log.Fatalf("Error formatting enhanced results: %v", err)
		}
		outputData, err = json.MarshalIndent(enhancedOutput, "", "  ")
		if err != nil {
			log.Fatalf("Error marshalling enhanced results to JSON: %v", err)
		}

	} else { // Basic format
		// Marshal the basic result to JSON
		outputData, err = json.MarshalIndent(basicAnalysisResult, "", "  ")
		if err != nil {
			log.Fatalf("Error marshalling basic results to JSON: %v", err)
		}
	}

	// Write the output
	if *outputFlag != "" {
		err = os.WriteFile(*outputFlag, outputData, 0644)
		if err != nil {
			log.Fatalf("Error writing JSON to file %s: %v", *outputFlag, err)
		}
		log.Printf("Analysis results written to %s\n", *outputFlag)
	} else {
		fmt.Println(string(outputData))
	}

	log.Println("Analysis complete.")
}
