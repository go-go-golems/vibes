// Package main runs JavaScript examples using the LSP interface.
package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"time"

	"goja-lsp-interface/pkg/jslsp"
)

func main() {
	fmt.Println("=== JavaScript LSP Example Runner ===")
	
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
	
	// Run the simple demo
	examplePath := filepath.Join(projectRoot, "examples", "simple-demo.js")
	fmt.Printf("Running JavaScript example: %s\n\n", examplePath)
	
	_, err = runtime.RunFile(examplePath)
	if err != nil {
		log.Fatalf("JavaScript execution failed: %v", err)
	}
	
	// Wait a bit for async operations to complete
	fmt.Println("\nWaiting for async operations...")
	time.Sleep(2 * time.Second)
	
	fmt.Println("\n=== Example Runner Complete ===")
}

