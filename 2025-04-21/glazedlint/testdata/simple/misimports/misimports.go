package misimports

import (
	"fmt"
)

// Define mock types to simulate the glazed package
type MockCmds struct{}

var Cmds = MockCmds{}

// These constants simulate the problematic imports
const (
	ParsedLayers = "ParsedLayers"
	DefaultSlug  = "DefaultSlug"
)

// TestMisimports demonstrates the misimport error
func TestMisimports() {
	// This should trigger our linter for cmds.ParsedLayers
	fmt.Println("Using cmds.ParsedLayers:", ParsedLayers)
	
	// This should trigger our linter for cmds.DefaultSlug
	fmt.Println("Using cmds.DefaultSlug:", DefaultSlug)
}
