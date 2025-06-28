package main

import (
	"fmt"
	"log"

	"github.com/AlexanderGrooff/mermaid-ascii/cmd"
)

func main() {
	// Test mermaid diagram
	mermaidSrc := `graph LR
    A[Start] --> B{Decision}
    B -->|Yes| C[Action 1]
    B -->|No| D[Action 2]
    C --> E[End]
    D --> E`

	fmt.Println("Testing mermaid-ascii integration...")
	
	// Set some options
	cmd.UseAscii = false
	cmd.PaddingBetweenX = 3
	cmd.PaddingBetweenY = 2
	cmd.BoxBorderPadding = 1
	cmd.Coords = false

	// Parse the mermaid
	properties, err := cmd.MermaidFileToMap(mermaidSrc, "test")
	if err != nil {
		log.Fatal("Failed to parse mermaid:", err)
	}

	// Render the diagram
	result := cmd.DrawMap(properties)
	fmt.Println("Rendered diagram:")
	fmt.Println(result)
}

