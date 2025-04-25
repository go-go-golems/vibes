package main

import (
	"context"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
	"time"

	"github.com/wesen/tree-sitter-go-cleaned-up/internal/analyzer"
	"github.com/wesen/tree-sitter-go-cleaned-up/internal/formatter"
	"github.com/wesen/tree-sitter-go-cleaned-up/internal/parser"
)

func main() {
	// Define command-line flags similar to the original recursive_analyzer
	basePath := flag.String("path", ".", "Path to the codebase directory to analyze")
	outputPath := flag.String("output", "codebase_analysis.json", "Output JSON file path")
	excludeFlag := flag.String("exclude-dirs", "vendor,node_modules,dist,build,.git,.idea,.vscode", "Comma-separated list of directory names to exclude")
	excludeFilesFlag := flag.String("exclude-files", "", "Comma-separated list of file name patterns to exclude (e.g., *_generated.go)")
	concurrency := flag.Int("concurrency", 8, "Number of files to analyze concurrently")
	verbose := flag.Bool("verbose", false, "Enable verbose logging")
	timeout := flag.Duration("timeout", 5*time.Minute, "Maximum analysis duration (e.g., 1m, 5m, 10s)")
	flag.Parse()

	if *verbose {
		log.SetOutput(os.Stderr) // Ensure logs go to stderr
		log.Println("Verbose logging enabled")
	} else {
		log.SetOutput(ioutil.Discard) // Disable logging if not verbose
	}

	log.Printf("Starting analysis for path: %s", *basePath)
	log.Printf("Output file: %s", *outputPath)

	// Parse exclude lists
	var excludeDirs []string
	if *excludeFlag != "" {
		excludeDirs = strings.Split(*excludeFlag, ",")
	}
	var excludeFilePatterns []string
	if *excludeFilesFlag != "" {
		excludeFilePatterns = strings.Split(*excludeFilesFlag, ",")
	}
	log.Printf("Exclude Dirs: %v", excludeDirs)
	log.Printf("Exclude File Patterns: %v", excludeFilePatterns)
	log.Printf("Concurrency: %d", *concurrency)
	log.Printf("Timeout: %v", *timeout)

	// Create context with timeout
	ctx, cancel := context.WithTimeout(context.Background(), *timeout)
	defer cancel()

	// Initialize components
	goParser, err := parser.NewGoParser()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating parser: %v\n", err)
		os.Exit(1)
	}

	analyzerConfig := analyzer.Config{
		Parser:         goParser,
		ExcludeDirs:    excludeDirs,
		ExcludePatterns: excludeFilePatterns,
		Concurrency:    *concurrency,
	}
	analyzerInstance, err := analyzer.NewAnalyzer(analyzerConfig)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating analyzer: %v\n", err)
		os.Exit(1)
	}

	formatterInstance := formatter.NewFormatter()

	// Run analysis
	log.Println("Analyzing codebase...")
	startTime := time.Now()
	analysisResult, err := analyzerInstance.AnalyzeCodebase(ctx, *basePath)
	analysisDuration := time.Since(startTime)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error analyzing codebase: %v\n", err)
		// Exit with a non-zero code, but maybe partial results were generated?
		// If analysisResult is not nil, maybe still try to format and save?
		// For now, exit cleanly on error after timeout/cancellation or major failure.
		if analysisResult == nil { // Only exit if no result could be generated at all
			os.Exit(1)
		}
		log.Printf("Analysis completed with errors in %.2fs. Attempting to save partial results.", analysisDuration.Seconds())
	} else {
		log.Printf("Analysis completed successfully in %.2fs", analysisDuration.Seconds())
	}

	// Format results
	log.Println("Formatting analysis results...")
	jsonData, err := formatterInstance.FormatJSON(analysisResult)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error formatting results: %v\n", err)
		os.Exit(1)
	}

	// Write output file
	log.Printf("Writing results to %s...", *outputPath)
	err = ioutil.WriteFile(*outputPath, jsonData, 0644)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error writing output file %s: %v\n", *outputPath, err)
		os.Exit(1)
	}

	// Print summary to stdout
	fmt.Printf("\n--- Analysis Summary ---\
")
	fmt.Printf("Analyzed Path: %s\n", analysisResult.BasePath)
	fmt.Printf("Files Analyzed: %d\n", analysisResult.Stats.TotalFiles)
	fmt.Printf("Packages Found: %d\n", analysisResult.Stats.TotalPackages)
	fmt.Printf("Functions Found: %d\n", analysisResult.Stats.TotalFunctions)
	fmt.Printf("Methods Found: %d\n", analysisResult.Stats.TotalMethods)
	fmt.Printf("Analysis Time: %.3f s\n", float64(analysisResult.Stats.AnalysisTimeMs)/1000.0)
	fmt.Printf("Results written to: %s\n", *outputPath)

	log.Println("Analyzer finished.")
} 