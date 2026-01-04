package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strings"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

func main() {
	// Sample file path
	filePath := "../sample/calculator/calculator.go"
	
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
	query, err := sitter.NewQuery([]byte(`(function_declaration name: (identifier) @func_name)`), 
		golang.GetLanguage())
	if err != nil {
		log.Fatalf("Error creating query: %v", err)
	}
	
	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, tree.RootNode())
	
	// Process query matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		
		// Get the function name
		if len(match.Captures) > 0 {
			nameNode := match.Captures[0].Node
			name := string(content[nameNode.StartByte():nameNode.EndByte()])
			line := nameNode.StartPoint().Row + 1
			
			fmt.Printf("Found function: %s at line %d\n", name, line)
			
			// Print some context
			lines := strings.Split(string(content), "\n")
			startLine := int(line) - 3
			if startLine < 0 {
				startLine = 0
			}
			endLine := int(line) + 2
			if endLine >= len(lines) {
				endLine = len(lines) - 1
			}
			
			fmt.Println("---")
			for i := startLine; i <= endLine; i++ {
				prefix := "  "
				if i == int(line)-1 {
					prefix = "> "
				}
				fmt.Printf("%s%d: %s\n", prefix, i+1, lines[i])
			}
			fmt.Println("---")
		}
	}
}