package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

func main() {
	// Make sure we have at least one argument (file path)
	if len(os.Args) < 2 {
		fmt.Println("Usage: test_parser <go-file-path>")
		os.Exit(1)
	}
	
	filePath := os.Args[1]
	
	// Read file content
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		log.Fatalf("Error reading file: %v", err)
	}
	
	// Create Tree-sitter parser for Go
	parser := sitter.NewParser()
	parser.SetLanguage(golang.GetLanguage())
	
	// Parse file with Tree-sitter
	tree, err := parser.ParseCtx(nil, nil, content)
	if err != nil {
		log.Fatalf("Error parsing file: %v", err)
	}
	defer tree.Close()
	
	// Query to find function declarations
	query, err := sitter.NewQuery([]byte(`
		(function_declaration name: (identifier) @func_name) @function
		(method_declaration name: (field_identifier) @method_name) @method
	`), golang.GetLanguage())
	if err != nil {
		log.Fatalf("Error creating query: %v", err)
	}
	
	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, tree.RootNode())
	
	functionCount := 0
	
	// Process query matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		
		functionCount++
		
		var functionNode *sitter.Node
		var nameNode *sitter.Node
		
		// Extract function name and node
		for _, capture := range match.Captures {
			switch capture.Index {
			case 0: // function or method name
				nameNode = capture.Node
			case 1: // function or method node
				functionNode = capture.Node
			}
		}
		
		if nameNode != nil {
			name := string(content[nameNode.StartByte():nameNode.EndByte()])
			startLine := nameNode.StartPoint().Row + 1 // 1-based line numbering
			
			fmt.Printf("Found function/method: %s on line %d\n", name, startLine)
			
			// For demonstration, print a few lines around the function/method
			lines := strings.Split(string(content), "\n")
			startLineIdx := int(startLine) - 1 // Convert back to 0-based
			
			// Print a few lines around the function/method
			fmt.Println("--- Context ---")
			contextStart := max(0, startLineIdx-2)
			contextEnd := min(len(lines)-1, startLineIdx+2)
			
			for i := contextStart; i <= contextEnd; i++ {
				marker := "  "
				if i == startLineIdx {
					marker = "> "
				}
				fmt.Printf("%s%d: %s\n", marker, i+1, lines[i])
			}
			fmt.Println("-------------")
		}
	}
	
	fmt.Printf("\nFound %d functions/methods in total\n", functionCount)
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func min(a, b int) int {
	if a > b {
		return b
	}
	return a
}