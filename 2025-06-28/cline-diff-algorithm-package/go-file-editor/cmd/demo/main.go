package main

import (
	"fmt"
	"log"
	"os"

	"github.com/spf13/cobra"
	fileeditor "github.com/cline-go/file-editor"
)

func main() {
	var workingDir string

	var rootCmd = &cobra.Command{
		Use:   "demo",
		Short: "Run file editor demo",
		Run: func(cmd *cobra.Command, args []string) {
			if workingDir == "" {
				workingDir = "."
			}

			fmt.Println("=== Cline Go File Editor Demo ===\n")

			editor := fileeditor.NewFileEditor(workingDir)

			// Example 1: Create a simple Go program
			fmt.Println("1. Creating initial Go program...")
			
			initialContent := `package main

import "fmt"

func main() {
	fmt.Println("Hello, World!")
}`

			err := editor.WriteToFile("demo.go", initialContent)
			if err != nil {
				log.Fatal(err)
			}
			fmt.Println("✓ Created demo.go")

			// Example 2: Add a function using SEARCH/REPLACE
			fmt.Println("\n2. Adding a greeting function...")
			
			diff1 := `------- SEARCH
import "fmt"
=======
import "fmt"

func greet(name string) string {
	return fmt.Sprintf("Hello, %s!", name)
}
+++++++ REPLACE`

			err = editor.ReplaceInFile("demo.go", diff1)
			if err != nil {
				log.Fatal(err)
			}
			fmt.Println("✓ Added greet function")

			// Example 3: Update main function to use the new function
			fmt.Println("\n3. Updating main function...")
			
			diff2 := `------- SEARCH
func main() {
	fmt.Println("Hello, World!")
}
=======
func main() {
	fmt.Println(greet("World"))
	fmt.Println(greet("Go"))
	fmt.Println(greet("Cline"))
}
+++++++ REPLACE`

			err = editor.ReplaceInFile("demo.go", diff2)
			if err != nil {
				log.Fatal(err)
			}
			fmt.Println("✓ Updated main function")

			// Example 4: Add error handling
			fmt.Println("\n4. Adding error handling...")
			
			diff3 := `------- SEARCH
func greet(name string) string {
	return fmt.Sprintf("Hello, %s!", name)
}
=======
func greet(name string) string {
	if name == "" {
		return "Hello, Anonymous!"
	}
	return fmt.Sprintf("Hello, %s!", name)
}
+++++++ REPLACE`

			err = editor.ReplaceInFile("demo.go", diff3)
			if err != nil {
				log.Fatal(err)
			}
			fmt.Println("✓ Added error handling")

			// Example 5: Show final result
			fmt.Println("\n5. Final result:")
			
			finalContent, err := editor.ReadFile("demo.go")
			if err != nil {
				log.Fatal(err)
			}
			
			fmt.Println("--- demo.go ---")
			fmt.Println(finalContent)
			fmt.Println("--- end ---")

			// Example 6: Test the created program
			fmt.Println("\n6. Testing the program...")
			fmt.Println("Run: go run demo.go")
			fmt.Println("Expected output:")
			fmt.Println("Hello, World!")
			fmt.Println("Hello, Go!")
			fmt.Println("Hello, Cline!")

			// Example 7: Demonstrate error handling
			fmt.Println("\n7. Demonstrating error handling...")
			
			invalidDiff := `------- SEARCH
nonexistent content
=======
replacement
+++++++ REPLACE`

			err = editor.ReplaceInFile("demo.go", invalidDiff)
			if err != nil {
				fmt.Printf("✓ Error correctly caught: %v\n", err)
			} else {
				fmt.Println("✗ Expected error but got none")
			}

			// Example 8: List files
			fmt.Println("\n8. Listing files...")
			
			files, err := editor.ListFiles(workingDir, false)
			if err != nil {
				log.Fatal(err)
			}
			
			fmt.Printf("Files in current directory: %v\n", files)

			fmt.Println("\n✅ Demo completed successfully!")
			fmt.Println("\nThe Go implementation successfully replicated Cline's file editing capabilities!")
		},
	}

	rootCmd.Flags().StringVarP(&workingDir, "working-dir", "w", "", "Directory to use for file editing (default: current directory)")

	if err := rootCmd.Execute(); err != nil {
		log.Fatalf("Error: %v", err)
	}
}

