package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"os"

	"github.com/your-org/go-analyzer/internal/parser" // Adjust module path if needed
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: simple-parser <file_path>")
		os.Exit(1)
	}
	filePath := os.Args[1]

	// Create a context
	ctx := context.Background()

	// Instantiate the parser
	p := parser.NewParser()

	// Parse the file
	functions, err := p.ParseFile(ctx, filePath)
	if err != nil {
		log.Fatalf("Error parsing file %s: %v", filePath, err)
	}

	// Print results
	fmt.Printf("Found %d functions in %s:\n", len(functions), filePath)

	// Print as simple text
	// for _, fn := range functions {
	// 	fmt.Printf("  - %s (Lines: %d-%d)\n", fn.Name, fn.StartLine, fn.EndLine)
	// }

	// Print as JSON (more structured)
	jsonData, err := json.MarshalIndent(functions, "", "  ")
	if err != nil {
		log.Fatalf("Error marshalling results to JSON: %v", err)
	}
	fmt.Println(string(jsonData))
}
