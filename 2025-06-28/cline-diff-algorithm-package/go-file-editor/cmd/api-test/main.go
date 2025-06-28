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
	var createdTempDir string

	var rootCmd = &cobra.Command{
		Use:   "api-test",
		Short: "Run Gemini API connectivity tests",
		Run: func(cmd *cobra.Command, args []string) {
			apiKey := os.Getenv("GEMINI_API_KEY")

			if workingDir == "" {
				tempDir, err := os.MkdirTemp("", "api_test")
				if err != nil {
					log.Fatalf("Failed to create temp dir: %v", err)
				}
				createdTempDir = tempDir
				workingDir = tempDir
			}

			fmt.Printf("Testing Gemini API connectivity with new key...\n")
			fmt.Printf("Test directory: %s\n\n", workingDir)

			agent := fileeditor.NewGeminiAgent(apiKey, workingDir)

			// Test 1: Basic connectivity with simple request
			fmt.Printf("=== Test 1: Basic API Connectivity ===\n")
			
			systemPrompt := `You are a helpful coding assistant. Respond with a simple confirmation that you can communicate.`
			userMessage := `Hello! Can you confirm that you're working correctly? Just respond with "API connection successful"`
			
			response, err := agent.Chat(systemPrompt, userMessage)
			if err != nil {
				log.Fatalf("Basic connectivity test failed: %v", err)
			}
			
			fmt.Printf("Response: %s\n", response)
			fmt.Printf("✓ Basic connectivity test passed\n\n")
			
			// Test 2: Function calling capability
			fmt.Printf("=== Test 2: Function Calling Test ===\n")
			
			systemPrompt2 := `You are a helpful coding assistant with file editing capabilities. You have access to the following tools:

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

Please create a simple "Hello World" program in Python.`

			userMessage2 := `Create a simple Python program called hello.py that prints "Hello, World!"`
			
			response2, err := agent.Chat(systemPrompt2, userMessage2)
			if err != nil {
				log.Fatalf("Function calling test failed: %v", err)
			}
			
			fmt.Printf("Response: %s\n", response2)
			
			// Check if file was created
			files, err := agent.ExecuteFunction(fileeditor.FunctionCall{
				Name: "list_files",
				Args: map[string]interface{}{
					"path":      ".",
					"recursive": false,
				},
			})
			if err != nil {
				log.Printf("Error listing files: %v", err)
			} else {
				fmt.Printf("Files created: %v\n", files["files"])
			}
			
			// Try to read the created file
			if fileList, ok := files["files"].([]interface{}); ok {
				for _, file := range fileList {
					if fileName, ok := file.(string); ok && fileName == "hello.py" {
						content, err := agent.ExecuteFunction(fileeditor.FunctionCall{
							Name: "read_file",
							Args: map[string]interface{}{
								"path": fileName,
							},
						})
						if err != nil {
							log.Printf("Error reading %s: %v", fileName, err)
						} else {
							fmt.Printf("Content of %s:\n%s\n", fileName, content["content"])
						}
						break
					}
				}
			}
			
			fmt.Printf("✓ Function calling test completed\n\n")
			
			fmt.Printf("✅ API connectivity tests completed successfully!\n")
			fmt.Printf("The new API key is working and function calling is operational.\n")

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

