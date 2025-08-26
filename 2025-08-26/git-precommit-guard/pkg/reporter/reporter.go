package reporter

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/fatih/color"
	"github.com/user/git-precommit-guard/pkg/config"
	"github.com/user/git-precommit-guard/pkg/detector"
)

// Reporter handles output formatting and reporting
type Reporter struct {
	config  config.ReportingConfig
	verbose bool
}

// NewReporter creates a new reporter instance
func NewReporter(cfg config.ReportingConfig, verbose bool) *Reporter {
	return &Reporter{
		config:  cfg,
		verbose: verbose,
	}
}

// Report generates and outputs the detection results
func (r *Reporter) Report(results []*detector.DetectionResult) error {
	switch r.config.Format {
	case "json":
		return r.reportJSON(results)
	case "console":
		return r.reportConsole(results)
	default:
		return fmt.Errorf("unsupported report format: %s", r.config.Format)
	}
}

// reportJSON outputs results in JSON format
func (r *Reporter) reportJSON(results []*detector.DetectionResult) error {
	report := struct {
		Timestamp string                       `json:"timestamp"`
		Summary   Summary                      `json:"summary"`
		Results   []*detector.DetectionResult `json:"results"`
	}{
		Timestamp: time.Now().Format(time.RFC3339),
		Summary:   r.calculateSummary(results),
		Results:   results,
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	return encoder.Encode(report)
}

// reportConsole outputs results in human-readable console format
func (r *Reporter) reportConsole(results []*detector.DetectionResult) error {
	// Set up colors
	var (
		errorColor   = color.New(color.FgRed, color.Bold)
		warningColor = color.New(color.FgYellow, color.Bold)
		successColor = color.New(color.FgGreen, color.Bold)
		infoColor    = color.New(color.FgCyan)
		dimColor     = color.New(color.Faint)
	)

	// Disable colors if not configured
	if !r.config.Colors {
		color.NoColor = true
	}

	summary := r.calculateSummary(results)
	
	// Print header
	fmt.Println("Git Pre-commit Guard - File Check Results")
	fmt.Println(strings.Repeat("=", 50))

	// Group results by file
	fileResults := r.groupResultsByFile(results)

	for filePath, fileResults := range fileResults {
		fmt.Printf("\n📁 %s\n", infoColor.Sprint(filePath))
		
		for _, result := range fileResults {
			r.printResult(result, errorColor, warningColor, successColor, infoColor, dimColor)
		}
	}

	// Print summary if enabled
	if r.config.Summary {
		fmt.Println("\n" + strings.Repeat("=", 50))
		fmt.Println("Summary:")
		
		if summary.ErrorCount > 0 {
			errorColor.Printf("  ❌ Errors: %d\n", summary.ErrorCount)
		}
		if summary.WarningCount > 0 {
			warningColor.Printf("  ⚠️  Warnings: %d\n", summary.WarningCount)
		}
		if summary.PassedCount > 0 {
			successColor.Printf("  ✅ Passed: %d\n", summary.PassedCount)
		}
		
		fmt.Printf("  📊 Total files checked: %d\n", summary.FilesChecked)
		
		if summary.ErrorCount > 0 {
			errorColor.Println("\n❌ Pre-commit check FAILED")
		} else {
			successColor.Println("\n✅ Pre-commit check PASSED")
		}
	}

	return nil
}

// printResult prints a single detection result
func (r *Reporter) printResult(result *detector.DetectionResult, errorColor, warningColor, successColor, infoColor, dimColor *color.Color) {
	var icon, statusText string
	var colorFunc *color.Color

	if result.Passed {
		if !r.config.ShowPassed && !r.verbose {
			return // Skip passed results unless configured to show them
		}
		icon = "✅"
		statusText = "PASS"
		colorFunc = successColor
	} else {
		switch result.Severity {
		case "error":
			icon = "❌"
			statusText = "FAIL"
			colorFunc = errorColor
		case "warning":
			icon = "⚠️"
			statusText = "WARN"
			colorFunc = warningColor
		default:
			icon = "ℹ️"
			statusText = "INFO"
			colorFunc = infoColor
		}
	}

	// Print main result line
	fmt.Printf("  %s %s [%s] %s\n", 
		icon, 
		colorFunc.Sprint(statusText),
		result.RuleName,
		result.Message)

	// Print details if verbose mode is enabled
	if r.verbose && len(result.Details) > 0 {
		for key, value := range result.Details {
			fmt.Printf("    %s: %s\n", dimColor.Sprint(key), value)
		}
	}
}

// groupResultsByFile groups detection results by file path
func (r *Reporter) groupResultsByFile(results []*detector.DetectionResult) map[string][]*detector.DetectionResult {
	grouped := make(map[string][]*detector.DetectionResult)
	
	for _, result := range results {
		grouped[result.FilePath] = append(grouped[result.FilePath], result)
	}
	
	return grouped
}

// Summary contains summary statistics
type Summary struct {
	FilesChecked int `json:"files_checked"`
	ErrorCount   int `json:"error_count"`
	WarningCount int `json:"warning_count"`
	PassedCount  int `json:"passed_count"`
}

// calculateSummary calculates summary statistics from results
func (r *Reporter) calculateSummary(results []*detector.DetectionResult) Summary {
	summary := Summary{}
	
	filesChecked := make(map[string]bool)
	
	for _, result := range results {
		// Count unique files
		filesChecked[result.FilePath] = true
		
		// Count by status and severity
		if result.Passed {
			summary.PassedCount++
		} else {
			switch result.Severity {
			case "error":
				summary.ErrorCount++
			case "warning":
				summary.WarningCount++
			}
		}
	}
	
	summary.FilesChecked = len(filesChecked)
	
	return summary
}

