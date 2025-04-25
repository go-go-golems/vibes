package parser

import (
	"testing"
	"path/filepath"
	"strings"
)

func TestParseFile(t *testing.T) {
	parser := NewGoFileParser()
	
	// Parse the test file
	testFilePath := "../sample/calculator/calculator.go"
	functions, err := parser.ParseFile(testFilePath)
	if err != nil {
		t.Fatalf("Failed to parse file: %v", err)
	}
	
	// Check that we found the expected functions
	expectedFunctions := []string{"Add", "Subtract", "Multiply", "Divide"}
	if len(functions) != len(expectedFunctions) {
		t.Errorf("Expected %d functions, got %d", len(expectedFunctions), len(functions))
	}
	
	// Check that each expected function is found
	foundFunctions := make(map[string]bool)
	for _, function := range functions {
		foundFunctions[function.Name] = true
		
		// Check that file path is correct
		absPath, _ := filepath.Abs(testFilePath)
		if function.Filepath != testFilePath {
			t.Errorf("Expected filepath %s, got %s", absPath, function.Filepath)
		}
		
		// Check that line numbers are reasonable
		if function.StartLine <= 0 || function.EndLine <= 0 {
			t.Errorf("Invalid line numbers for function %s: start=%d, end=%d", 
				function.Name, function.StartLine, function.EndLine)
		}
		
		if function.EndLine <= function.StartLine {
			t.Errorf("End line should be greater than start line for function %s: start=%d, end=%d",
				function.Name, function.StartLine, function.EndLine)
		}
	}
	
	// Verify all expected functions were found
	for _, name := range expectedFunctions {
		if !foundFunctions[name] {
			t.Errorf("Expected to find function %s, but it was not found", name)
		}
	}
}

func TestParseDirectory(t *testing.T) {
	parser := NewGoFileParser()
	
	// Parse the test directory
	testDirPath := "../sample"
	functions, err := parser.ParseDirectory(testDirPath)
	if err != nil {
		t.Fatalf("Failed to parse directory: %v", err)
	}
	
	// Check that we found functions from multiple files
	if len(functions) < 5 {
		t.Errorf("Expected at least 5 functions, got %d", len(functions))
	}
	
	// Check that functions are from different files
	fileCount := make(map[string]int)
	for _, function := range functions {
		fileCount[function.Filepath]++
	}
	
	if len(fileCount) < 2 {
		t.Errorf("Expected functions from at least 2 different files, got %d", len(fileCount))
	}
}

func TestParsePath(t *testing.T) {
	parser := NewGoFileParser()
	
	// Test with a file
	fileFunctions, err := parser.ParsePath("../sample/calculator/calculator.go")
	if err != nil {
		t.Fatalf("Failed to parse file path: %v", err)
	}
	if len(fileFunctions) == 0 {
		t.Errorf("Expected functions from file, got none")
	}
	
	// Test with a directory
	dirFunctions, err := parser.ParsePath("../sample")
	if err != nil {
		t.Fatalf("Failed to parse directory path: %v", err)
	}
	if len(dirFunctions) < len(fileFunctions) {
		t.Errorf("Expected more functions from directory than from single file")
	}
	
	// Test with a non-existent path
	_, err = parser.ParsePath("../nonexistent")
	if err == nil {
		t.Errorf("Expected error for non-existent path, got none")
	}
	
	// Test with a non-Go file
	nonGoFunctions, err := parser.ParsePath("../README.md")
	if err != nil {
		t.Fatalf("Failed to parse non-Go file: %v", err)
	}
	if len(nonGoFunctions) != 0 {
		t.Errorf("Expected no functions from non-Go file, got %d", len(nonGoFunctions))
	}
}