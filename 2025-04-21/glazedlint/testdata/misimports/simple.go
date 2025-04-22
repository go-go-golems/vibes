package main

import (
	"fmt"
	"github.com/go-go-golems/glazed/pkg/cmds"
)

// This file demonstrates the mis-imports error
// It uses cmds.ParsedLayers and cmds.DefaultSlug which should be from the layers package

func main() {
	// This is just a placeholder to make the file compile
	fmt.Println("Using cmds.ParsedLayers:", cmds.ParsedLayers)
	fmt.Println("Using cmds.DefaultSlug:", cmds.DefaultSlug)
}
