// Package main provides a test program for the LSP client utilities.
package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"
	"time"

	"goja-lsp-interface/pkg/lsp"
)

func main() {
	fmt.Println("=== LSP Client Test Program ===")
	
	// Get current working directory
	cwd, err := os.Getwd()
	if err != nil {
		log.Fatalf("Failed to get current working directory: %v", err)
	}
	
	// Go up to project root (we're in cmd/test-lsp)
	projectRoot := filepath.Join(cwd, "../..")
	projectRoot, err = filepath.Abs(projectRoot)
	if err != nil {
		log.Fatalf("Failed to get absolute path: %v", err)
	}
	
	// Set up LSP client options
	opts := lsp.ClientOptions{
		Command:    "gopls",
		Args:       []string{},
		RootURI:    lsp.FileToURI(projectRoot),
		DebugMode:  true,
		WorkingDir: projectRoot,
	}
	
	fmt.Printf("Starting gopls language server...\n")
	fmt.Printf("Root URI: %s\n", opts.RootURI)
	
	// Create LSP client
	client, err := lsp.NewClient(opts)
	if err != nil {
		log.Fatalf("Failed to create LSP client: %v", err)
	}
	defer client.Close()
	
	// Initialize the client
	fmt.Println("\nInitializing LSP client...")
	if err := client.Initialize(); err != nil {
		log.Fatalf("Failed to initialize LSP client: %v", err)
	}
	
	fmt.Println("LSP client initialized successfully!")
	
	// Print server capabilities
	capabilities := client.GetCapabilities()
	fmt.Println("\nServer Capabilities:")
	fmt.Printf("- Hover Provider: %t\n", capabilities.HoverProvider)
	fmt.Printf("- Completion Provider: %t\n", capabilities.CompletionProvider != nil)
	fmt.Printf("- Definition Provider: %t\n", capabilities.DefinitionProvider)
	fmt.Printf("- References Provider: %t\n", capabilities.ReferencesProvider)
	
	// Create LSP manager
	manager := lsp.NewLSPManager(client)
	defer manager.Close()
	
	// Test with demo files
	fmt.Println("\n=== Testing LSP Operations ===")
	
	// Test files to open
	testFiles := []string{
		"demo/pkg/models/user.go",
		"demo/pkg/calculator/calculator.go",
		"demo/cmd/app/main.go",
	}
	
	var openFiles []*lsp.OpenFile
	
	// Open test files
	fmt.Println("\nOpening test files...")
	for _, filePath := range testFiles {
		fullPath := filepath.Join(projectRoot, filePath)
		if _, err := os.Stat(fullPath); os.IsNotExist(err) {
			fmt.Printf("Skipping non-existent file: %s\n", filePath)
			continue
		}
		
		openFile, err := manager.OpenFile(fullPath)
		if err != nil {
			fmt.Printf("Failed to open file %s: %v\n", filePath, err)
			continue
		}
		
		openFiles = append(openFiles, openFile)
		fmt.Printf("Opened: %s (Language: %s)\n", filePath, openFile.LanguageID)
	}
	
	if len(openFiles) == 0 {
		log.Fatalf("No files could be opened for testing")
	}
	
	// Wait a moment for the language server to process the files
	fmt.Println("\nWaiting for language server to process files...")
	time.Sleep(2 * time.Second)
	
	// Test hover functionality
	fmt.Println("\n--- Testing Hover ---")
	testHover(manager, openFiles)
	
	// Test completion functionality
	fmt.Println("\n--- Testing Completion ---")
	testCompletion(manager, openFiles)
	
	// Test definition functionality
	fmt.Println("\n--- Testing Definition ---")
	testDefinition(manager, openFiles)
	
	// Test references functionality
	fmt.Println("\n--- Testing References ---")
	testReferences(manager, openFiles)
	
	fmt.Println("\n=== LSP Test Complete ===")
}

// testHover tests hover functionality on various code elements.
func testHover(manager *lsp.LSPManager, openFiles []*lsp.OpenFile) {
	// Test hover on different types of symbols
	testCases := []struct {
		description string
		line        int
		character   int
	}{
		{"User struct", 10, 5},     // Line with "type User struct"
		{"NewUser function", 45, 5}, // Line with "func NewUser"
		{"String method", 55, 15},   // Line with "func (u *User) String"
		{"fmt.Sprintf call", 56, 10}, // Inside String method
	}
	
	for _, openFile := range openFiles {
		if strings.Contains(openFile.Path, "user.go") {
			fmt.Printf("\nTesting hover in %s:\n", openFile.Path)
			
			for _, tc := range testCases {
				hover, err := manager.GetHover(openFile.URI, tc.line, tc.character)
				if err != nil {
					fmt.Printf("  %s (line %d, col %d): Error - %v\n", tc.description, tc.line+1, tc.character+1, err)
					continue
				}
				
				if hover == nil {
					fmt.Printf("  %s (line %d, col %d): No hover information\n", tc.description, tc.line+1, tc.character+1)
					continue
				}
				
				hoverText := lsp.FormatHover(hover)
				if len(hoverText) > 100 {
					hoverText = hoverText[:100] + "..."
				}
				fmt.Printf("  %s (line %d, col %d): %s\n", tc.description, tc.line+1, tc.character+1, hoverText)
			}
			break
		}
	}
}

// testCompletion tests completion functionality.
func testCompletion(manager *lsp.LSPManager, openFiles []*lsp.OpenFile) {
	// Test completion at various positions
	testCases := []struct {
		description string
		line        int
		character   int
	}{
		{"After 'fmt.'", 56, 12},    // Inside fmt.Sprintf call
		{"After 'u.'", 57, 5},       // Accessing user fields
		{"After 'time.'", 48, 15},   // time package usage
	}
	
	for _, openFile := range openFiles {
		if strings.Contains(openFile.Path, "user.go") {
			fmt.Printf("\nTesting completion in %s:\n", openFile.Path)
			
			for _, tc := range testCases {
				completions, err := manager.GetCompletion(openFile.URI, tc.line, tc.character)
				if err != nil {
					fmt.Printf("  %s (line %d, col %d): Error - %v\n", tc.description, tc.line+1, tc.character+1, err)
					continue
				}
				
				if len(completions) == 0 {
					fmt.Printf("  %s (line %d, col %d): No completions\n", tc.description, tc.line+1, tc.character+1)
					continue
				}
				
				fmt.Printf("  %s (line %d, col %d): %d completions\n", tc.description, tc.line+1, tc.character+1, len(completions))
				
				// Show first few completions
				for i, item := range completions {
					if i >= 3 {
						fmt.Printf("    ... and %d more\n", len(completions)-3)
						break
					}
					fmt.Printf("    - %s\n", lsp.FormatCompletionItem(item))
				}
			}
			break
		}
	}
}

// testDefinition tests go-to-definition functionality.
func testDefinition(manager *lsp.LSPManager, openFiles []*lsp.OpenFile) {
	// Test definition lookup for various symbols
	testCases := []struct {
		description string
		line        int
		character   int
	}{
		{"User type usage", 45, 25},  // In NewUser function return type
		{"fmt package", 56, 10},      // fmt.Sprintf call
		{"time.Now call", 48, 15},    // time.Now() call
	}
	
	for _, openFile := range openFiles {
		if strings.Contains(openFile.Path, "user.go") {
			fmt.Printf("\nTesting definition in %s:\n", openFile.Path)
			
			for _, tc := range testCases {
				definitions, err := manager.GetDefinition(openFile.URI, tc.line, tc.character)
				if err != nil {
					fmt.Printf("  %s (line %d, col %d): Error - %v\n", tc.description, tc.line+1, tc.character+1, err)
					continue
				}
				
				if len(definitions) == 0 {
					fmt.Printf("  %s (line %d, col %d): No definitions found\n", tc.description, tc.line+1, tc.character+1)
					continue
				}
				
				fmt.Printf("  %s (line %d, col %d): %d definitions\n", tc.description, tc.line+1, tc.character+1, len(definitions))
				for _, def := range definitions {
					fmt.Printf("    - %s\n", lsp.FormatLocation(def))
				}
			}
			break
		}
	}
}

// testReferences tests find-references functionality.
func testReferences(manager *lsp.LSPManager, openFiles []*lsp.OpenFile) {
	// Test references for various symbols
	testCases := []struct {
		description string
		line        int
		character   int
	}{
		{"User struct", 10, 5},       // User struct definition
		{"NewUser function", 45, 5},  // NewUser function definition
		{"Name field", 12, 5},        // Name field in User struct
	}
	
	for _, openFile := range openFiles {
		if strings.Contains(openFile.Path, "user.go") {
			fmt.Printf("\nTesting references in %s:\n", openFile.Path)
			
			for _, tc := range testCases {
				references, err := manager.GetReferences(openFile.URI, tc.line, tc.character, true)
				if err != nil {
					fmt.Printf("  %s (line %d, col %d): Error - %v\n", tc.description, tc.line+1, tc.character+1, err)
					continue
				}
				
				if len(references) == 0 {
					fmt.Printf("  %s (line %d, col %d): No references found\n", tc.description, tc.line+1, tc.character+1)
					continue
				}
				
				fmt.Printf("  %s (line %d, col %d): %d references\n", tc.description, tc.line+1, tc.character+1, len(references))
				for i, ref := range references {
					if i >= 5 {
						fmt.Printf("    ... and %d more\n", len(references)-5)
						break
					}
					fmt.Printf("    - %s\n", lsp.FormatLocation(ref))
				}
			}
			break
		}
	}
}

// Additional utility functions for testing

// findSymbolInFile finds a symbol at a specific line in a file.
func findSymbolInFile(openFile *lsp.OpenFile, line int) string {
	lines := strings.Split(openFile.Content, "\n")
	if line >= len(lines) {
		return ""
	}
	
	lineText := strings.TrimSpace(lines[line])
	
	// Extract the first word that looks like a symbol
	words := strings.Fields(lineText)
	for _, word := range words {
		if len(word) > 0 && (word[0] >= 'A' && word[0] <= 'Z' || word[0] >= 'a' && word[0] <= 'z') {
			return word
		}
	}
	
	return ""
}

// printFileInfo prints information about an open file.
func printFileInfo(openFile *lsp.OpenFile) {
	fmt.Printf("File: %s\n", openFile.Path)
	fmt.Printf("URI: %s\n", openFile.URI)
	fmt.Printf("Language: %s\n", openFile.LanguageID)
	fmt.Printf("Version: %d\n", openFile.Version)
	fmt.Printf("Content length: %d bytes\n", len(openFile.Content))
	
	// Count lines
	lines := strings.Split(openFile.Content, "\n")
	fmt.Printf("Lines: %d\n", len(lines))
}

