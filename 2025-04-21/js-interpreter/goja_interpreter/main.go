package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/dop251/goja"
	"github.com/dop251/goja_nodejs/require"
)

// TestResult stores the result of running a JavaScript file
type TestResult struct {
	Filename    string
	Success     bool
	Error       string
	ExecutionMs int64
	Output      string
}

// Custom console implementation to capture output
type captureConsole struct {
	vm     *goja.Runtime
	output strings.Builder
}

func (c *captureConsole) Log(args ...interface{}) {
	s := fmt.Sprintln(args...)
	c.output.WriteString(s)
	fmt.Print(s)
}

func (c *captureConsole) Info(args ...interface{}) {
	s := fmt.Sprintln(args...)
	c.output.WriteString("[INFO] " + s)
	fmt.Print("[INFO] " + s)
}

func (c *captureConsole) Error(args ...interface{}) {
	s := fmt.Sprintln(args...)
	c.output.WriteString("[ERROR] " + s)
	fmt.Print("[ERROR] " + s)
}

func (c *captureConsole) Warn(args ...interface{}) {
	s := fmt.Sprintln(args...)
	c.output.WriteString("[WARN] " + s)
	fmt.Print("[WARN] " + s)
}

func main() {
	// Check if a directory was provided as an argument
	if len(os.Args) < 2 {
		fmt.Println("Usage: goja_interpreter <js_directory>")
		os.Exit(1)
	}

	jsDir := os.Args[1]
	files, err := ioutil.ReadDir(jsDir)
	if err != nil {
		fmt.Printf("Error reading directory %s: %v\n", jsDir, err)
		os.Exit(1)
	}

	// Create results directory if it doesn't exist
	resultsDir := "results"
	if err := os.MkdirAll(resultsDir, 0755); err != nil {
		fmt.Printf("Error creating results directory: %v\n", err)
		os.Exit(1)
	}

	// Create a file to store the test results
	reportFile, err := os.Create(filepath.Join(resultsDir, "test_report.txt"))
	if err != nil {
		fmt.Printf("Error creating report file: %v\n", err)
		os.Exit(1)
	}
	defer reportFile.Close()

	// Write report header
	reportFile.WriteString("JavaScript Test Report\n")
	reportFile.WriteString("=====================\n\n")
	reportFile.WriteString(fmt.Sprintf("Date: %s\n", time.Now().Format(time.RFC1123)))
	reportFile.WriteString(fmt.Sprintf("Directory: %s\n\n", jsDir))
	reportFile.WriteString("Results:\n")
	reportFile.WriteString("--------\n\n")

	// Process each JavaScript file
	var results []TestResult
	for _, file := range files {
		if !file.IsDir() && strings.HasSuffix(file.Name(), ".js") {
			result := runJavaScriptFile(filepath.Join(jsDir, file.Name()))
			results = append(results, result)

			// Write result to report file
			status := "SUCCESS"
			if !result.Success {
				status = "FAILURE"
			}
			reportFile.WriteString(fmt.Sprintf("File: %s\n", result.Filename))
			reportFile.WriteString(fmt.Sprintf("Status: %s\n", status))
			reportFile.WriteString(fmt.Sprintf("Execution Time: %d ms\n", result.ExecutionMs))
			if !result.Success {
				reportFile.WriteString(fmt.Sprintf("Error: %s\n", result.Error))
			}
			reportFile.WriteString("Output:\n")
			reportFile.WriteString("-------\n")
			reportFile.WriteString(result.Output)
			reportFile.WriteString("\n\n")

			// Also write to console
			fmt.Printf("Tested: %s - %s\n", result.Filename, status)
			if !result.Success {
				fmt.Printf("  Error: %s\n", result.Error)
			}
		}
	}

	// Write summary
	successCount := 0
	for _, result := range results {
		if result.Success {
			successCount++
		}
	}
	
	reportFile.WriteString("Summary:\n")
	reportFile.WriteString("--------\n")
	reportFile.WriteString(fmt.Sprintf("Total files: %d\n", len(results)))
	reportFile.WriteString(fmt.Sprintf("Successful: %d\n", successCount))
	reportFile.WriteString(fmt.Sprintf("Failed: %d\n", len(results)-successCount))
	
	fmt.Printf("\nSummary: %d/%d files passed\n", successCount, len(results))
	fmt.Printf("Report written to %s\n", filepath.Join(resultsDir, "test_report.txt"))
}

func runJavaScriptFile(filePath string) TestResult {
	result := TestResult{
		Filename: filepath.Base(filePath),
		Success:  true,
	}

	// Read the JavaScript file
	code, err := ioutil.ReadFile(filePath)
	if err != nil {
		result.Success = false
		result.Error = fmt.Sprintf("Error reading file: %v", err)
		return result
	}

	// Create a new JavaScript runtime
	vm := goja.New()
	
	// Set up the custom console to capture output
	cc := &captureConsole{vm: vm}
	consoleObj := vm.NewObject()
	_ = consoleObj.Set("log", cc.Log)
	_ = consoleObj.Set("info", cc.Info)
	_ = consoleObj.Set("error", cc.Error)
	_ = consoleObj.Set("warn", cc.Warn)
	vm.Set("console", consoleObj)
	
	// Set up the require module
	registry := require.NewRegistry(require.WithGlobalFolders("./node_modules"))
	registry.Enable(vm)

	// Execute the JavaScript code with timing
	startTime := time.Now()
	_, err = vm.RunString(string(code))
	executionTime := time.Since(startTime)
	result.ExecutionMs = executionTime.Milliseconds()
	result.Output = cc.output.String()

	if err != nil {
		result.Success = false
		result.Error = err.Error()
	}

	// Create a detailed output file for this test
	outputDir := filepath.Join("results", "outputs")
	if err := os.MkdirAll(outputDir, 0755); err == nil {
		outputFile, err := os.Create(filepath.Join(outputDir, result.Filename+".txt"))
		if err == nil {
			defer outputFile.Close()
			outputFile.WriteString(fmt.Sprintf("Test results for %s\n", result.Filename))
			outputFile.WriteString(fmt.Sprintf("Execution Time: %d ms\n", result.ExecutionMs))
			outputFile.WriteString(fmt.Sprintf("Status: %v\n", result.Success))
			if !result.Success {
				outputFile.WriteString(fmt.Sprintf("Error: %s\n", result.Error))
			}
			outputFile.WriteString("\nOutput:\n")
			outputFile.WriteString(result.Output)
		}
	}

	return result
}
