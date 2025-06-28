// Package main demonstrates the JavaScript LSP interface.
package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"path/filepath"

	"goja-lsp-interface/pkg/jslsp"
)

func main() {
	fmt.Println("=== JavaScript LSP Interface Test ===")
	
	// Get current working directory and go to project root
	cwd, err := os.Getwd()
	if err != nil {
		log.Fatalf("Failed to get current directory: %v", err)
	}
	
	projectRoot := filepath.Join(cwd, "../..")
	projectRoot, err = filepath.Abs(projectRoot)
	if err != nil {
		log.Fatalf("Failed to get absolute path: %v", err)
	}
	
	// Create JavaScript LSP runtime
	ctx := context.Background()
	runtime, err := jslsp.CreateStandaloneJSLSP(ctx)
	if err != nil {
		log.Fatalf("Failed to create JS LSP runtime: %v", err)
	}
	defer runtime.Close()
	
	// Set project root as a global variable
	runtime.SetGlobal("PROJECT_ROOT", projectRoot)
	
	// JavaScript code to test LSP functionality
	jsCode := `
		console.log("Starting JavaScript LSP test...");
		console.log("Project root:", PROJECT_ROOT);
		
		// Create LSP client
		var lspClient = createLSPClient({
			command: "gopls",
			args: [],
			rootPath: PROJECT_ROOT,
			debugMode: false
		});
		
		console.log("LSP client created");
		
		// Initialize the client
		try {
			lspClient.Initialize();
			console.log("LSP client initialized successfully");
		} catch (e) {
			console.error("Failed to initialize LSP client:", e);
			throw e;
		}
		
		// Check if initialized
		if (lspClient.IsInitialized()) {
			console.log("LSP client is initialized");
			
			// Get server capabilities
			var caps = lspClient.GetCapabilities();
			console.log("Server capabilities:");
			console.log("- Hover:", caps.hoverProvider);
			console.log("- Completion:", caps.completionProvider);
			console.log("- Definition:", caps.definitionProvider);
			console.log("- References:", caps.referencesProvider);
		} else {
			console.log("LSP client is not initialized");
		}
		
		// Test file operations
		var testFile = PROJECT_ROOT + "/demo/pkg/models/user.go";
		console.log("Opening test file:", testFile);
		
		try {
			var openFile = lspClient.OpenFile(testFile);
			console.log("File opened successfully:");
			console.log("- URI:", openFile.uri);
			console.log("- Language:", openFile.languageId);
			console.log("- Version:", openFile.version);
			console.log("- Content length:", openFile.content.length);
			
			// Test hover functionality
			console.log("\\nTesting hover at line 10, character 5...");
			try {
				var hover = lspClient.GetHover(openFile.uri, 10, 5);
				if (hover) {
					console.log("Hover result:", hover.text || "No text available");
				} else {
					console.log("No hover information available");
				}
			} catch (e) {
				console.log("Hover test failed:", e.message);
			}
			
			// Test completion functionality
			console.log("\\nTesting completion at line 56, character 10...");
			try {
				var completions = lspClient.GetCompletion(openFile.uri, 56, 10);
				console.log("Found", completions.length, "completion items");
				if (completions.length > 0) {
					console.log("First few completions:");
					for (var i = 0; i < Math.min(3, completions.length); i++) {
						console.log("- " + completions[i].label + (completions[i].detail ? " (" + completions[i].detail + ")" : ""));
					}
				}
			} catch (e) {
				console.log("Completion test failed:", e.message);
			}
			
			// Test definition functionality
			console.log("\\nTesting definition at line 45, character 5...");
			try {
				var definitions = lspClient.GetDefinition(openFile.uri, 45, 5);
				console.log("Found", definitions.length, "definitions");
				for (var i = 0; i < Math.min(3, definitions.length); i++) {
					console.log("- " + definitions[i].text);
				}
			} catch (e) {
				console.log("Definition test failed:", e.message);
			}
			
			// Test references functionality
			console.log("\\nTesting references at line 10, character 5...");
			try {
				var references = lspClient.GetReferences(openFile.uri, 10, 5, true);
				console.log("Found", references.length, "references");
				for (var i = 0; i < Math.min(5, references.length); i++) {
					console.log("- " + references[i].text);
				}
			} catch (e) {
				console.log("References test failed:", e.message);
			}
			
			// List open files
			console.log("\\nOpen files:");
			var openFiles = lspClient.GetOpenFiles();
			for (var i = 0; i < openFiles.length; i++) {
				console.log("- " + openFiles[i].path + " (" + openFiles[i].languageId + ")");
			}
			
		} catch (e) {
			console.error("File operation failed:", e);
		}
		
		console.log("\\nJavaScript LSP test completed!");
		
		// Close the client
		try {
			lspClient.Close();
			console.log("LSP client closed successfully");
		} catch (e) {
			console.error("Error closing LSP client:", e);
		}
	`
	
	// Run the JavaScript code
	fmt.Println("Running JavaScript LSP test...")
	_, err = runtime.RunScript(jsCode)
	if err != nil {
		log.Fatalf("JavaScript execution failed: %v", err)
	}
	
	fmt.Println("\n=== Test Complete ===")
}

