package cmd

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/user/go-file-analyzer/parser"
)

// AnalyzeOptions contains options for the analyze command
type AnalyzeOptions struct {
	Path       string
	OutputFile string
	Recursive  bool
	Format     string
}

// RunAnalyze runs the analyze command with the given options
func RunAnalyze(options AnalyzeOptions) error {
	// Create a new parser
	goParser := parser.NewGoFileParser()
	
	// Start the timer
	startTime := time.Now()
	
	// Parse the specified path
	var functions []parser.FunctionInfo
	var err error
	
	if options.Recursive {
		// Recursively parse directory
		functions, err = goParser.ParseDirectory(options.Path)
	} else {
		// Parse single file or directory (non-recursive)
		functions, err = goParser.ParsePath(options.Path)
	}
	
	if err != nil {
		return fmt.Errorf("analysis failed: %w", err)
	}
	
	// Calculate elapsed time
	elapsed := time.Since(startTime)
	
	// Create analysis result
	result := struct {
		Functions []parser.FunctionInfo `json:"functions"`
		Metadata  struct {
			AnalyzedPath string        `json:"analyzedPath"`
			FileCount    int           `json:"fileCount"`
			FunctionCount int          `json:"functionCount"`
			ElapsedTime  string        `json:"elapsedTime"`
			Timestamp    string        `json:"timestamp"`
		} `json:"metadata"`
	}{
		Functions: functions,
	}
	
	// Fill metadata
	result.Metadata.AnalyzedPath = options.Path
	result.Metadata.FunctionCount = len(functions)
	
	// Count unique files
	fileMap := make(map[string]bool)
	for _, f := range functions {
		fileMap[f.Filepath] = true
	}
	result.Metadata.FileCount = len(fileMap)
	
	// Set time information
	result.Metadata.ElapsedTime = elapsed.String()
	result.Metadata.Timestamp = time.Now().Format(time.RFC3339)
	
	// Create output based on requested format
	var output []byte
	switch strings.ToLower(options.Format) {
	case "json", "":
		output, err = json.MarshalIndent(result, "", "  ")
	case "jsonl":
		// For JSONL format, each function is a separate line
		jsonLines := make([]string, 0, len(functions))
		for _, f := range functions {
			line, err := json.Marshal(f)
			if err != nil {
				return fmt.Errorf("error marshaling function: %w", err)
			}
			jsonLines = append(jsonLines, string(line))
		}
		output = []byte(strings.Join(jsonLines, "\n"))
	default:
		return fmt.Errorf("unsupported output format: %s", options.Format)
	}
	
	if err != nil {
		return fmt.Errorf("error marshaling to JSON: %w", err)
	}
	
	// Determine output location
	if options.OutputFile == "" || options.OutputFile == "-" {
		// Output to stdout
		fmt.Println(string(output))
		return nil
	}
	
	// Create output directory if it doesn't exist
	outputDir := filepath.Dir(options.OutputFile)
	if outputDir != "." {
		if err := os.MkdirAll(outputDir, 0755); err != nil {
			return fmt.Errorf("error creating output directory: %w", err)
		}
	}
	
	// Write output to file
	if err := ioutil.WriteFile(options.OutputFile, output, 0644); err != nil {
		return fmt.Errorf("error writing output file: %w", err)
	}
	
	// Log success
	fmt.Printf("Analysis complete. Results written to %s\n", options.OutputFile)
	fmt.Printf("Analyzed %d files and found %d functions in %s\n", 
		result.Metadata.FileCount, result.Metadata.FunctionCount, result.Metadata.ElapsedTime)
	
	return nil
}