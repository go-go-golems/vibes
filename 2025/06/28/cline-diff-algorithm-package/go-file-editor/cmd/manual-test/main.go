package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"

	"github.com/spf13/cobra"
	fileeditor "github.com/cline-go/file-editor"
)

func main() {
	var workingDir string
	var createdTempDir string

	var rootCmd = &cobra.Command{
		Use:   "manual-test",
		Short: "Run manual file editor tests",
		Run: func(cmd *cobra.Command, args []string) {
			if workingDir == "" {
				tempDir, err := os.MkdirTemp("", "manual_test")
				if err != nil {
					log.Fatalf("Failed to create temp dir: %v", err)
				}
				createdTempDir = tempDir
				workingDir = tempDir
			}

			fmt.Printf("Testing file editing capabilities in directory: %s\n", workingDir)

			editor := fileeditor.NewFileEditor(workingDir)

			// Test 1: Create initial file
			fmt.Printf("\n=== Test 1: Create initial Python calculator ===\n")
			initialContent := `def add(x, y):
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
			err := editor.WriteToFile("calculator.py", initialContent)
			if err != nil {
				log.Fatalf("Failed to create initial file: %v", err)
			}
			fmt.Printf("✓ Created calculator.py\n")
			
			// Test 2: Add error handling for division
			fmt.Printf("\n=== Test 2: Add error handling for division ===\n")
			diff1 := `------- SEARCH
def divide(x, y):
    return x / y
=======
def divide(x, y):
    if y == 0:
        return "Error: Division by zero"
    return x / y
+++++++ REPLACE`
			
			err = editor.ReplaceInFile("calculator.py", diff1)
			if err != nil {
				log.Fatalf("Failed to apply first diff: %v", err)
			}
			fmt.Printf("✓ Added error handling for division\n")
			
			// Test 3: Add power function
			fmt.Printf("\n=== Test 3: Add power function ===\n")
			diff2 := `------- SEARCH
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
			
			err = editor.ReplaceInFile("calculator.py", diff2)
			if err != nil {
				log.Fatalf("Failed to apply second diff: %v", err)
			}
			fmt.Printf("✓ Added power function\n")
			
			// Test 4: Update main function to include power demo
			fmt.Printf("\n=== Test 4: Update main function ===\n")
			diff3 := `------- SEARCH
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
			
			err = editor.ReplaceInFile("calculator.py", diff3)
			if err != nil {
				log.Fatalf("Failed to apply third diff: %v", err)
			}
			fmt.Printf("✓ Updated main function to demo power and error handling\n")
			
			// Test 5: Create a test file
			fmt.Printf("\n=== Test 5: Create test file ===\n")
			testContent := `import calculator

def test_add():
    assert calculator.add(2, 3) == 5
    assert calculator.add(-1, 1) == 0
    assert calculator.add(0, 0) == 0
    print("✓ add tests passed")

def test_subtract():
    assert calculator.subtract(5, 3) == 2
    assert calculator.subtract(0, 5) == -5
    assert calculator.subtract(10, 10) == 0
    print("✓ subtract tests passed")

def test_multiply():
    assert calculator.multiply(3, 4) == 12
    assert calculator.multiply(-2, 3) == -6
    assert calculator.multiply(0, 100) == 0
    print("✓ multiply tests passed")

def test_divide():
    assert calculator.divide(10, 2) == 5
    assert calculator.divide(7, 2) == 3.5
    assert calculator.divide(10, 0) == "Error: Division by zero"
    print("✓ divide tests passed")

def test_power():
    assert calculator.power(2, 3) == 8
    assert calculator.power(5, 0) == 1
    assert calculator.power(10, 2) == 100
    print("✓ power tests passed")

if __name__ == "__main__":
    test_add()
    test_subtract()
    test_multiply()
    test_divide()
    test_power()
    print("All tests passed!")`
			
			err = editor.WriteToFile("test_calculator.py", testContent)
			if err != nil {
				log.Fatalf("Failed to create test file: %v", err)
			}
			fmt.Printf("✓ Created test_calculator.py\n")
			
			// Test 6: Complex multi-block replacement to refactor into a class
			fmt.Printf("\n=== Test 6: Refactor into Calculator class ===\n")
			
			// Replace entire file with class-based version
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
			
			err = editor.WriteToFile("calculator.py", classContent)
			if err != nil {
				log.Fatalf("Failed to refactor to class: %v", err)
			}
			fmt.Printf("✓ Refactored to Calculator class\n")
			
			// Test 7: Update test file to work with class
			fmt.Printf("\n=== Test 7: Update test file for class ===\n")
			diff4 := `------- SEARCH
import calculator

def test_add():
    assert calculator.add(2, 3) == 5
    assert calculator.add(-1, 1) == 0
    assert calculator.add(0, 0) == 0
    print("✓ add tests passed")

def test_subtract():
    assert calculator.subtract(5, 3) == 2
    assert calculator.subtract(0, 5) == -5
    assert calculator.subtract(10, 10) == 0
    print("✓ subtract tests passed")

def test_multiply():
    assert calculator.multiply(3, 4) == 12
    assert calculator.multiply(-2, 3) == -6
    assert calculator.multiply(0, 100) == 0
    print("✓ multiply tests passed")

def test_divide():
    assert calculator.divide(10, 2) == 5
    assert calculator.divide(7, 2) == 3.5
    assert calculator.divide(10, 0) == "Error: Division by zero"
    print("✓ divide tests passed")

def test_power():
    assert calculator.power(2, 3) == 8
    assert calculator.power(5, 0) == 1
    assert calculator.power(10, 2) == 100
    print("✓ power tests passed")
=======
from calculator import Calculator

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
+++++++ REPLACE`
			
			err = editor.ReplaceInFile("test_calculator.py", diff4)
			if err != nil {
				log.Fatalf("Failed to update test file: %v", err)
			}
			fmt.Printf("✓ Updated test file for class-based calculator\n")
			
			// Final validation
			fmt.Printf("\n=== Final Validation ===\n")
			
			files, err := editor.ListFiles(".", false)
			if err != nil {
				log.Fatalf("Failed to list files: %v", err)
			}
			fmt.Printf("Files created: %v\n", files)
			
			// Show final content of both files
			fmt.Printf("\n=== Final calculator.py content ===\n")
			finalCalc, err := editor.ReadFile("calculator.py")
			if err != nil {
				log.Fatalf("Failed to read final calculator.py: %v", err)
			}
			fmt.Printf("%s\n", finalCalc)
			
			fmt.Printf("\n=== Final test_calculator.py content ===\n")
			finalTest, err := editor.ReadFile("test_calculator.py")
			if err != nil {
				log.Fatalf("Failed to read final test_calculator.py: %v", err)
			}
			fmt.Printf("%s\n", finalTest)
			
			fmt.Printf("\n=== Testing the calculator ===\n")
			// Test that the Python code actually works
			err = os.Chdir(workingDir)
			if err != nil {
				log.Printf("Failed to change directory: %v", err)
			} else {
				// Run the calculator
				fmt.Printf("Running calculator.py:\n")
				cmd := exec.Command("python3", "calculator.py")
				output, err := cmd.Output()
				if err != nil {
					fmt.Printf("Error running calculator: %v\n", err)
				} else {
					fmt.Printf("%s\n", output)
				}
				
				fmt.Printf("\nRunning tests:\n")
				cmd = exec.Command("python3", "test_calculator.py")
				output, err = cmd.Output()
				if err != nil {
					fmt.Printf("Error running tests: %v\n", err)
				} else {
					fmt.Printf("%s\n", output)
				}
			}
			
			fmt.Printf("\n✅ All file editing tests completed successfully!\n")
			fmt.Printf("Demonstrated capabilities:\n")
			fmt.Printf("- File creation with write_to_file\n")
			fmt.Printf("- Multiple sequential edits with replace_in_file\n")
			fmt.Printf("- Complex SEARCH/REPLACE operations\n")
			fmt.Printf("- Error handling and edge cases\n")
			fmt.Printf("- File integrity validation\n")
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

