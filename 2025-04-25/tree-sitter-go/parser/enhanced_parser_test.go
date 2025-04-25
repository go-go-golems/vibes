package parser

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestEnhancedParseFile(t *testing.T) {
	parser := NewEnhancedGoFileParser()
	
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
	
	// Check that each expected function is found and has the correct details
	foundFunctions := make(map[string]EnhancedFunctionInfo)
	for _, function := range functions {
		foundFunctions[function.Name] = function
		
		// All functions should be in the calculator package
		if function.Package != "calculator" {
			t.Errorf("Expected package name 'calculator', got '%s'", function.Package)
		}
		
		// All sample functions should be exported
		if !function.IsExported {
			t.Errorf("Expected function %s to be exported", function.Name)
		}
		
		// None of the sample functions are methods
		if function.IsMethod {
			t.Errorf("Expected function %s to not be a method", function.Name)
		}
		
		// All sample functions have 2 parameters
		if len(function.Parameters) != 2 {
			t.Errorf("Expected function %s to have 2 parameters, got %d", 
				function.Name, len(function.Parameters))
		}
		
		// All sample functions return an int
		if len(function.ReturnType) != 1 || function.ReturnType[0] != "int" {
			t.Errorf("Expected function %s to return 'int', got '%v'", 
				function.Name, function.ReturnType)
		}
		
		// Check that function has comments
		if function.Comments == "" {
			t.Errorf("Expected function %s to have comments", function.Name)
		}
	}
	
	// Verify all expected functions were found
	for _, name := range expectedFunctions {
		if _, ok := foundFunctions[name]; !ok {
			t.Errorf("Expected to find function %s, but it was not found", name)
		}
	}
	
	// Check specific functions
	if function, ok := foundFunctions["Divide"]; ok {
		if !strings.Contains(function.Comments, "not used") {
			t.Errorf("Expected Divide function comments to mention 'not used'")
		}
	}
}

func TestEnhancedParseDirectory(t *testing.T) {
	parser := NewEnhancedGoFileParser()
	
	// Parse the test directory
	testDirPath := "../sample"
	functions, err := parser.ParseDirectory(testDirPath)
	if err != nil {
		t.Fatalf("Failed to parse directory: %v", err)
	}
	
	// Check that we found functions from multiple files
	if len(functions) < 7 { // 4 from calculator.go + 3 from main.go
		t.Errorf("Expected at least 7 functions, got %d", len(functions))
	}
	
	// Check that functions are from different packages
	packageCount := make(map[string]int)
	for _, function := range functions {
		packageCount[function.Package]++
	}
	
	if len(packageCount) < 2 {
		t.Errorf("Expected functions from at least 2 different packages, got %d", len(packageCount))
	}
	
	// Check that main package was found
	if count, ok := packageCount["main"]; !ok || count < 3 {
		t.Errorf("Expected at least 3 functions in the 'main' package")
	}
	
	// Check that calculator package was found
	if count, ok := packageCount["calculator"]; !ok || count < 4 {
		t.Errorf("Expected at least 4 functions in the 'calculator' package")
	}
}

func TestMethodParsing(t *testing.T) {
	// Create a temporary file with a method
	tempFilePath := "../sample/temp_method_test.go"
	content := `
package temp

// Person represents a person
type Person struct {
	Name string
	Age  int
}

// GetName returns the person's name
func (p *Person) GetName() string {
	return p.Name
}

// SetName sets the person's name
func (p *Person) SetName(name string) {
	p.Name = name
}

// GetAge returns the person's age
func (p Person) GetAge() int {
	return p.Age
}
`
	// Create the file
	err := ioutil.WriteFile(tempFilePath, []byte(content), 0644)
	if err != nil {
		t.Fatalf("Failed to create temp file: %v", err)
	}
	
	// Clean up after the test
	defer os.Remove(tempFilePath)
	
	// Parse the file
	parser := NewEnhancedGoFileParser()
	functions, err := parser.ParseFile(tempFilePath)
	if err != nil {
		t.Fatalf("Failed to parse file: %v", err)
	}
	
	// Check that we found the methods
	if len(functions) != 3 {
		t.Errorf("Expected 3 methods, got %d", len(functions))
	}
	
	// Check each method
	for _, function := range functions {
		// All should be methods
		if !function.IsMethod {
			t.Errorf("Expected %s to be a method", function.Name)
		}
		
		// All should have a receiver
		if function.Receiver == nil {
			t.Errorf("Expected method %s to have a receiver", function.Name)
		} else {
			// Check receiver type based on method name
			switch function.Name {
			case "GetName", "SetName":
				if function.Receiver.Type != "*Person" {
					t.Errorf("Expected method %s to have receiver type '*Person', got '%s'", 
						function.Name, function.Receiver.Type)
				}
			case "GetAge":
				if function.Receiver.Type != "Person" {
					t.Errorf("Expected method %s to have receiver type 'Person', got '%s'", 
						function.Name, function.Receiver.Type)
				}
			}
		}
		
		// Check return types
		switch function.Name {
		case "GetName":
			if len(function.ReturnType) != 1 || function.ReturnType[0] != "string" {
				t.Errorf("Expected method %s to return 'string', got '%v'", 
					function.Name, function.ReturnType)
			}
		case "SetName":
			if len(function.ReturnType) != 0 {
				t.Errorf("Expected method %s to return nothing, got '%v'", 
					function.Name, function.ReturnType)
			}
			// Check parameters
			if len(function.Parameters) != 1 || function.Parameters[0].Type != "string" {
				t.Errorf("Expected method %s to have one string parameter, got %v", 
					function.Name, function.Parameters)
			}
		case "GetAge":
			if len(function.ReturnType) != 1 || function.ReturnType[0] != "int" {
				t.Errorf("Expected method %s to return 'int', got '%v'", 
					function.Name, function.ReturnType)
			}
		}
	}
}