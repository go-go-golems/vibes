package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"

	"github.com/spf13/cobra"
	fileeditor "github.com/cline-go/file-editor"
)

func main() {
	var workingDir string
	var createdTempDir string

	var rootCmd = &cobra.Command{
		Use:   "gemini-test",
		Short: "Run Gemini integration tests",
		Run: func(cmd *cobra.Command, args []string) {
			apiKey := os.Getenv("GEMINI_API_KEY")

			if workingDir == "" {
				// Create a temporary directory for testing
				tempDir, err := os.MkdirTemp("", "gemini_test")
				if err != nil {
					log.Fatalf("Failed to create temp dir: %v", err)
				}
				createdTempDir = tempDir
				workingDir = tempDir
			}

			fmt.Printf("Testing Gemini integration in directory: %s\n", workingDir)

			agent := fileeditor.NewGeminiAgent(apiKey, workingDir)

			systemPrompt := `You are a helpful coding assistant with file editing capabilities. You have access to the following tools:

1. read_file(path) - Read the contents of a file
2. write_to_file(path, content) - Write content to a file (creates file and directories if needed)
3. replace_in_file(path, diff) - Apply SEARCH/REPLACE blocks to modify specific parts of a file
4. list_files(path, recursive) - List files in a directory

When using replace_in_file, use this exact format:
------- SEARCH
[exact content to find]
=======
[new content to replace with]
+++++++ REPLACE

Always be precise with the SEARCH content - it must match exactly including whitespace and indentation.`

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

			for i, scenario := range testScenarios {
				fmt.Printf("\n=== Test %d: %s ===\n", i+1, scenario.name)
				fmt.Printf("Request: %s\n", scenario.message)

				response, err := agent.Chat(systemPrompt, scenario.message)
				if err != nil {
					log.Printf("Error in scenario %d: %v", i+1, err)
					continue
				}

				fmt.Printf("Response: %s\n", response)

				files, err := agent.ExecuteFunction(fileeditor.FunctionCall{
					Name: "list_files",
					Args: map[string]interface{}{
						"path":      ".",
						"recursive": true,
					},
				})
				if err != nil {
					log.Printf("Error listing files: %v", err)
				} else {
					fmt.Printf("Files in directory: %v\n", files["files"])
				}

				if i > 0 {
					content, err := agent.ExecuteFunction(fileeditor.FunctionCall{
						Name: "read_file",
						Args: map[string]interface{}{
							"path": "calculator.py",
						},
					})
					if err != nil {
						log.Printf("Error reading calculator.py: %v", err)
					} else {
						fmt.Printf("\nCurrent calculator.py content:\n%s\n", content["content"])
					}
				}
			}

			fmt.Printf("\n=== Final Validation ===\n")

			files, err := agent.ExecuteFunction(fileeditor.FunctionCall{
				Name: "list_files",
				Args: map[string]interface{}{
					"path":      ".",
					"recursive": true,
				},
			})
			if err != nil {
				log.Printf("Error listing final files: %v", err)
				return
			}

			fmt.Printf("All files created: %v\n", files["files"])

			if fileList, ok := files["files"].([]interface{}); ok {
				for _, file := range fileList {
					if fileName, ok := file.(string); ok && filepath.Ext(fileName) == ".py" {
						content, err := agent.ExecuteFunction(fileeditor.FunctionCall{
							Name: "read_file",
							Args: map[string]interface{}{
								"path": fileName,
							},
						})
						if err != nil {
							log.Printf("Error reading %s: %v", fileName, err)
						} else {
							fmt.Printf("\n=== Content of %s ===\n%s\n", fileName, content["content"])
						}
					}
				}
			}

			fmt.Printf("\nGemini integration test completed successfully!\n")
			fmt.Printf("Test directory: %s", workingDir)
			if createdTempDir != "" {
				fmt.Printf(" (will be cleaned up)\n")
				defer os.RemoveAll(createdTempDir)
			} else {
				fmt.Printf("\n")
			}
		},
	}

	rootCmd.Flags().StringVarP(&workingDir, "working-dir", "w", "", "Directory to use for file editing (default: temp dir)")

	if err := rootCmd.Execute(); err != nil {
		log.Fatalf("Error: %v", err)
	}
}

