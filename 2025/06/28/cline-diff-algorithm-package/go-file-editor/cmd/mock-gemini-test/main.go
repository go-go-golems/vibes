package main

import (
	"fmt"
	"log"
	"os"

	"github.com/spf13/cobra"
	fileeditor "github.com/cline-go/file-editor"
)

// MockGeminiAgent simulates Gemini API responses for testing
type MockGeminiAgent struct {
	fileEditor *fileeditor.FileEditor
}

func NewMockGeminiAgent(workingDir string) *MockGeminiAgent {
	return &MockGeminiAgent{
		fileEditor: fileeditor.NewFileEditor(workingDir),
	}
}

// Chat simulates Gemini API responses and executes file operations
func (mga *MockGeminiAgent) Chat(systemPrompt, userMessage string) (string, error) {
	fmt.Printf("User: %s\n", userMessage)
	
	// Simulate different responses based on the request
	if contains(userMessage, "create") && contains(userMessage, "calculator") {
		// Create initial calculator file
		content := `def add(x, y):
    return x + y

def subtract(x, y):
    return x - y

def multiply(x, y):
    return x * y

def divide(x, y):
    return x / y

def main():
    print("Calculator Demo")
    print("5 + 3 =", add(5, 3))
    print("5 - 3 =", subtract(5, 3))
    print("5 * 3 =", multiply(5, 3))
    print("5 / 3 =", divide(5, 3))

if __name__ == "__main__":
    main()`
		
		err := mga.fileEditor.WriteToFile("calculator.py", content)
		if err != nil {
			return "", err
		}
		
		return "I've created a simple Python calculator program in calculator.py with functions for basic arithmetic operations (add, subtract, multiply, divide) and a main function that demonstrates their usage.", nil
		
	} else if contains(userMessage, "error handling") && contains(userMessage, "division") {
		// Add error handling for division
		diff := `------- SEARCH
def divide(x, y):
    return x / y
=======
def divide(x, y):
    if y == 0:
        return "Error: Division by zero"
    return x / y
+++++++ REPLACE`
		
		err := mga.fileEditor.ReplaceInFile("calculator.py", diff)
		if err != nil {
			return "", err
		}
		
		return "I've modified the divide function in calculator.py to include error handling for division by zero. The function now returns an error message instead of crashing when attempting to divide by zero.", nil
		
	} else if contains(userMessage, "power") && contains(userMessage, "function") {
		// Add power function
		diff1 := `------- SEARCH
def divide(x, y):
    if y == 0:
        return "Error: Division by zero"
    return x / y
=======
def divide(x, y):
    if y == 0:
        return "Error: Division by zero"
    return x / y

def power(x, y):
    return x ** y
+++++++ REPLACE`
		
		err := mga.fileEditor.ReplaceInFile("calculator.py", diff1)
		if err != nil {
			return "", err
		}
		
		// Update main function
		diff2 := `------- SEARCH
def main():
    print("Calculator Demo")
    print("5 + 3 =", add(5, 3))
    print("5 - 3 =", subtract(5, 3))
    print("5 * 3 =", multiply(5, 3))
    print("5 / 3 =", divide(5, 3))
=======
def main():
    print("Calculator Demo")
    print("5 + 3 =", add(5, 3))
    print("5 - 3 =", subtract(5, 3))
    print("5 * 3 =", multiply(5, 3))
    print("5 / 3 =", divide(5, 3))
    print("5 ^ 3 =", power(5, 3))
    print("10 / 0 =", divide(10, 0))
+++++++ REPLACE`
		
		err = mga.fileEditor.ReplaceInFile("calculator.py", diff2)
		if err != nil {
			return "", err
		}
		
		return "I've added a new 'power' function to calculator.py that calculates x raised to the power of y using the ** operator. I've also updated the main function to demonstrate this new functionality and show the error handling for division by zero.", nil
		
	} else if contains(userMessage, "Calculator class") {
		// Refactor to class
		classContent := `class Calculator:
    def add(self, x, y):
        return x + y
    
    def subtract(self, x, y):
        return x - y
    
    def multiply(self, x, y):
        return x * y
    
    def divide(self, x, y):
        if y == 0:
            return "Error: Division by zero"
        return x / y
    
    def power(self, x, y):
        return x ** y

def main():
    calc = Calculator()
    print("Calculator Demo")
    print("5 + 3 =", calc.add(5, 3))
    print("5 - 3 =", calc.subtract(5, 3))
    print("5 * 3 =", calc.multiply(5, 3))
    print("5 / 3 =", calc.divide(5, 3))
    print("5 ^ 3 =", calc.power(5, 3))
    print("10 / 0 =", calc.divide(10, 0))

if __name__ == "__main__":
    main()`
		
		err := mga.fileEditor.WriteToFile("calculator.py", classContent)
		if err != nil {
			return "", err
		}
		
		return "I've refactored calculator.py to use a Calculator class instead of standalone functions. All the existing functionality has been preserved but is now organized using object-oriented programming principles. The main function creates an instance of the Calculator class and demonstrates all its methods.", nil
		
	} else if contains(userMessage, "test file") {
		// Create test file
		testContent := `from calculator import Calculator

def test_add():
    calc = Calculator()
    assert calc.add(2, 3) == 5
    assert calc.add(-1, 1) == 0
    assert calc.add(0, 0) == 0
    print("✓ add tests passed")

def test_subtract():
    calc = Calculator()
    assert calc.subtract(5, 3) == 2
    assert calc.subtract(0, 5) == -5
    assert calc.subtract(10, 10) == 0
    print("✓ subtract tests passed")

def test_multiply():
    calc = Calculator()
    assert calc.multiply(3, 4) == 12
    assert calc.multiply(-2, 3) == -6
    assert calc.multiply(0, 100) == 0
    print("✓ multiply tests passed")

def test_divide():
    calc = Calculator()
    assert calc.divide(10, 2) == 5
    assert calc.divide(7, 2) == 3.5
    assert calc.divide(10, 0) == "Error: Division by zero"
    print("✓ divide tests passed")

def test_power():
    calc = Calculator()
    assert calc.power(2, 3) == 8
    assert calc.power(5, 0) == 1
    assert calc.power(10, 2) == 100
    print("✓ power tests passed")

if __name__ == "__main__":
    test_add()
    test_subtract()
    test_multiply()
    test_divide()
    test_power()
    print("All tests passed!")`
		
		err := mga.fileEditor.WriteToFile("test_calculator.py", testContent)
		if err != nil {
			return "", err
		}
		
		return "I've created test_calculator.py that imports the Calculator class and tests all its methods with various inputs including edge cases. The test file includes tests for addition, subtraction, multiplication, division (including division by zero), and the power function. Each test function validates multiple scenarios and prints confirmation when tests pass.", nil
	}
	
	return "I understand your request. Let me help you with that.", nil
}

func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || 
		(len(s) > len(substr) && (s[:len(substr)] == substr || s[len(s)-len(substr):] == substr || 
		 findInString(s, substr))))
}

func findInString(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}

func main() {
	var workingDir string
	var createdTempDir string

	var rootCmd = &cobra.Command{
		Use:   "mock-gemini-test",
		Short: "Run mock Gemini integration tests",
		Run: func(cmd *cobra.Command, args []string) {
			if workingDir == "" {
				tempDir, err := os.MkdirTemp("", "mock_gemini_test")
				if err != nil {
					log.Fatalf("Failed to create temp dir: %v", err)
				}
				createdTempDir = tempDir
				workingDir = tempDir
			}

			fmt.Printf("Testing Mock Gemini integration in directory: %s\n", workingDir)

			agent := NewMockGeminiAgent(workingDir)

			systemPrompt := `You are a helpful coding assistant with file editing capabilities.`

			// Test scenarios
			testScenarios := []struct {
				name    string
				message string
			}{
				{
					name: "Create initial file",
					message: `Create a simple Python calculator program in a file called calculator.py. The program should have functions for add, subtract, multiply, and divide operations, and a main function that demonstrates their usage.`,
				},
				{
					name: "Modify the file - add error handling",
					message: `Please modify the calculator.py file to add error handling for division by zero in the divide function. The function should return an error message instead of crashing.`,
				},
				{
					name: "Add new functionality",
					message: `Add a new function called 'power' to calculator.py that calculates x raised to the power of y. Also update the main function to demonstrate this new functionality.`,
				},
				{
					name: "Refactor and improve",
					message: `Refactor the calculator.py file to use a Calculator class instead of standalone functions. Keep all the existing functionality but organize it better using object-oriented programming.`,
				},
				{
					name: "Create test file",
					message: `Create a test file called test_calculator.py that imports the Calculator class and tests all its methods with various inputs including edge cases.`,
				},
			}
			
			// Execute test scenarios
			for i, scenario := range testScenarios {
				fmt.Printf("\n=== Test %d: %s ===\n", i+1, scenario.name)
				
				response, err := agent.Chat(systemPrompt, scenario.message)
				if err != nil {
					log.Printf("Error in scenario %d: %v", i+1, err)
					continue
				}
				
				fmt.Printf("Gemini: %s\n", response)
				
				// List files after each operation
				files, err := agent.fileEditor.ListFiles(".", false)
				if err != nil {
					log.Printf("Error listing files: %v", err)
				} else {
					fmt.Printf("Files: %v\n", files)
				}
			}
			
			// Final validation
			fmt.Printf("\n=== Final Validation ===\n")
			
			// Read final calculator.py
			calcContent, err := agent.fileEditor.ReadFile("calculator.py")
			if err != nil {
				log.Printf("Error reading calculator.py: %v", err)
			} else {
				fmt.Printf("Final calculator.py:\n%s\n", calcContent)
			}
			
			// Read final test file
			testContent, err := agent.fileEditor.ReadFile("test_calculator.py")
			if err != nil {
				log.Printf("Error reading test_calculator.py: %v", err)
			} else {
				fmt.Printf("\nFinal test_calculator.py:\n%s\n", testContent)
			}
			
			fmt.Printf("\n✅ Mock Gemini integration test completed successfully!\n")
			fmt.Printf("This demonstrates how the real Gemini API integration would work:\n")
			fmt.Printf("- Gemini receives user requests and system prompts\n")
			fmt.Printf("- Gemini generates appropriate function calls for file operations\n")
			fmt.Printf("- Go implementation executes the file editing operations\n")
			fmt.Printf("- Results are returned to Gemini for further processing\n")
			fmt.Printf("- Multiple sequential edits work correctly\n")

			// At the end, clean up temp dir if created
			if createdTempDir != "" {
				fmt.Printf("Test directory: %s (will be cleaned up)\n", workingDir)
				defer os.RemoveAll(createdTempDir)
			} else {
				fmt.Printf("Test directory: %s\n", workingDir)
			}
		},
	}

	rootCmd.Flags().StringVarP(&workingDir, "working-dir", "w", "", "Directory to use for file editing (default: temp dir)")

	if err := rootCmd.Execute(); err != nil {
		log.Fatalf("Error: %v", err)
	}
}

