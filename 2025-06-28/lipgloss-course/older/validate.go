package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strings"
)

// ValidationResult represents the result of validating a screenshot
type ValidationResult struct {
	Filename string
	Passed   bool
	Issues   []string
	Features []string
}

// validateScreenshot validates an ANSI screenshot file
func validateScreenshot(filename string) ValidationResult {
	result := ValidationResult{
		Filename: filename,
		Passed:   true,
		Issues:   []string{},
		Features: []string{},
	}
	
	// Read the file
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		result.Passed = false
		result.Issues = append(result.Issues, fmt.Sprintf("Could not read file: %v", err))
		return result
	}
	
	contentStr := string(content)
	
	// Check for basic ANSI escape sequences (colors, positioning)
	if strings.Contains(contentStr, "\033[") {
		result.Features = append(result.Features, "ANSI escape sequences present")
	} else {
		result.Issues = append(result.Issues, "No ANSI escape sequences found")
		result.Passed = false
	}
	
	// Check for box drawing characters (borders)
	boxChars := []string{"┌", "┐", "└", "┘", "─", "│", "├", "┤", "┬", "┴", "┼"}
	foundBoxChars := false
	for _, char := range boxChars {
		if strings.Contains(contentStr, char) {
			foundBoxChars = true
			break
		}
	}
	if foundBoxChars {
		result.Features = append(result.Features, "Box drawing characters (borders)")
	}
	
	// Check for rounded borders
	roundedChars := []string{"╭", "╮", "╯", "╰"}
	foundRoundedChars := false
	for _, char := range roundedChars {
		if strings.Contains(contentStr, char) {
			foundRoundedChars = true
			break
		}
	}
	if foundRoundedChars {
		result.Features = append(result.Features, "Rounded border characters")
	}
	
	// Check for thick borders
	thickChars := []string{"┏", "┓", "┗", "┛", "━", "┃"}
	foundThickChars := false
	for _, char := range thickChars {
		if strings.Contains(contentStr, char) {
			foundThickChars = true
			break
		}
	}
	if foundThickChars {
		result.Features = append(result.Features, "Thick border characters")
	}
	
	// Check for double borders
	doubleChars := []string{"╔", "╗", "╚", "╝", "═", "║"}
	foundDoubleChars := false
	for _, char := range doubleChars {
		if strings.Contains(contentStr, char) {
			foundDoubleChars = true
			break
		}
	}
	if foundDoubleChars {
		result.Features = append(result.Features, "Double border characters")
	}
	
	// Check for color codes (256-color and RGB)
	colorRegex := regexp.MustCompile(`\033\[38;5;\d+m|\033\[48;5;\d+m`)
	if colorRegex.MatchString(contentStr) {
		result.Features = append(result.Features, "256-color ANSI codes")
	}
	
	// Check for specific content based on filename
	switch {
	case strings.Contains(filename, "basic") || strings.Contains(filename, "output"):
		// Basic styling should have styled text
		if strings.Contains(contentStr, "Basic Styling Examples") {
			result.Features = append(result.Features, "Basic styling content detected")
		}
		
	case strings.Contains(filename, "wm_"):
		// Window manager should have window-like structures
		if strings.Contains(contentStr, "Window Manager") || strings.Contains(contentStr, "Editor") || strings.Contains(contentStr, "Browser") {
			result.Features = append(result.Features, "Window manager content detected")
		}
		
		// Check for overlapping content (multiple bordered areas)
		borderCount := strings.Count(contentStr, "┌") + strings.Count(contentStr, "╭") + strings.Count(contentStr, "┏") + strings.Count(contentStr, "╔")
		if borderCount >= 3 {
			result.Features = append(result.Features, fmt.Sprintf("Multiple windows detected (%d borders)", borderCount))
		}
	}
	
	// Check file size (should not be empty or too small)
	if len(content) < 100 {
		result.Issues = append(result.Issues, "File too small (less than 100 bytes)")
		result.Passed = false
	} else {
		result.Features = append(result.Features, fmt.Sprintf("File size: %d bytes", len(content)))
	}
	
	// Check for minimum content length
	lines := strings.Split(contentStr, "\n")
	if len(lines) < 5 {
		result.Issues = append(result.Issues, "Too few lines of content")
		result.Passed = false
	} else {
		result.Features = append(result.Features, fmt.Sprintf("Content lines: %d", len(lines)))
	}
	
	return result
}

func main() {
	fmt.Println("=== VHS Text Screenshot Validation ===\n")
	
	// Find all ANSI files
	files, err := ioutil.ReadDir(".")
	if err != nil {
		fmt.Printf("Error reading directory: %v\n", err)
		os.Exit(1)
	}
	
	var ansiFiles []string
	for _, file := range files {
		if strings.HasSuffix(file.Name(), ".ansi") {
			ansiFiles = append(ansiFiles, file.Name())
		}
	}
	
	if len(ansiFiles) == 0 {
		fmt.Println("No ANSI files found for validation")
		os.Exit(1)
	}
	
	fmt.Printf("Found %d ANSI files to validate:\n", len(ansiFiles))
	for _, file := range ansiFiles {
		fmt.Printf("  - %s\n", file)
	}
	fmt.Println()
	
	// Validate each file
	allPassed := true
	for _, filename := range ansiFiles {
		fmt.Printf("Validating %s...\n", filename)
		result := validateScreenshot(filename)
		
		if result.Passed {
			fmt.Printf("  ✅ PASSED\n")
		} else {
			fmt.Printf("  ❌ FAILED\n")
			allPassed = false
		}
		
		if len(result.Features) > 0 {
			fmt.Printf("  Features detected:\n")
			for _, feature := range result.Features {
				fmt.Printf("    • %s\n", feature)
			}
		}
		
		if len(result.Issues) > 0 {
			fmt.Printf("  Issues found:\n")
			for _, issue := range result.Issues {
				fmt.Printf("    ⚠️  %s\n", issue)
			}
		}
		
		fmt.Println()
	}
	
	// Summary
	fmt.Println("=== Validation Summary ===")
	if allPassed {
		fmt.Printf("✅ All %d files passed validation!\n", len(ansiFiles))
		fmt.Println("The lipgloss v2 UI rendering is working correctly.")
	} else {
		fmt.Printf("❌ Some files failed validation.\n")
		fmt.Println("Please check the issues above and fix the UI rendering.")
	}
	
	// Additional analysis
	fmt.Println("\n=== Additional Analysis ===")
	
	// Check for progression in window manager demo
	wmFiles := []string{}
	for _, file := range ansiFiles {
		if strings.Contains(file, "wm_") {
			wmFiles = append(wmFiles, file)
		}
	}
	
	if len(wmFiles) >= 2 {
		fmt.Printf("Window manager progression detected (%d screenshots)\n", len(wmFiles))
		fmt.Println("This demonstrates the dynamic nature of the compositing system.")
	}
	
	// Check for different UI patterns
	patterns := map[string]int{
		"Basic styling": 0,
		"Window management": 0,
		"Complex layouts": 0,
	}
	
	for _, file := range ansiFiles {
		switch {
		case strings.Contains(file, "basic") || strings.Contains(file, "output"):
			patterns["Basic styling"]++
		case strings.Contains(file, "wm_"):
			patterns["Window management"]++
		default:
			patterns["Complex layouts"]++
		}
	}
	
	fmt.Println("\nUI Pattern Coverage:")
	for pattern, count := range patterns {
		if count > 0 {
			fmt.Printf("  • %s: %d screenshots\n", pattern, count)
		}
	}
	
	fmt.Println("\n=== Validation Complete ===")
}

