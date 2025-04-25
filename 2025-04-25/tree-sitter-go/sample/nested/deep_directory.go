package nested

import (
	"fmt"
	"strings"
)

// NestedStruct is a test struct for deep directory analysis
type NestedStruct struct {
	Name    string
	Value   int
	Enabled bool
}

// NewNestedStruct creates a new NestedStruct
func NewNestedStruct(name string, value int) *NestedStruct {
	return &NestedStruct{
		Name:    name,
		Value:   value,
		Enabled: true,
	}
}

// GetFullName returns the name with a prefix
func (n *NestedStruct) GetFullName() string {
	return fmt.Sprintf("Nested_%s", n.Name)
}

// GetFormattedInfo returns a formatted string with the struct's information
func (n *NestedStruct) GetFormattedInfo() string {
	return fmt.Sprintf("%s: %d (enabled: %t)", 
		n.Name, n.Value, n.Enabled)
}

// Toggle toggles the enabled state
func (n *NestedStruct) Toggle() {
	n.Enabled = !n.Enabled
}

// ProcessData performs some operations on input data
func ProcessData(input string, multiplier int) (string, int) {
	// Convert to uppercase
	processed := strings.ToUpper(input)
	
	// Count words
	words := len(strings.Fields(input))
	
	// Apply multiplier
	count := words * multiplier
	
	return processed, count
}