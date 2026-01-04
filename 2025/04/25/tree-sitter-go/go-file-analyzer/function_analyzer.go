package main

import (
	"fmt"
	"log"
	"path/filepath"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// AnalyzeFunctionReferences finds all references to the functions
func AnalyzeFunctionReferences(functions []FunctionInfo, lspClient *LSPClient) ([]FunctionInfo, error) {
	if lspClient == nil {
		return functions, fmt.Errorf("LSP client is not initialized")
	}

	// Start the LSP client
	if err := lspClient.Start(); err != nil {
		return functions, fmt.Errorf("failed to start LSP client: %v", err)
	}
	defer lspClient.Stop()

	// Process each function
	for i, function := range functions {
		// Find function name position - use the first column of the function declaration line
		// This is a simplification; in a real implementation, we would use the exact position
		column := 0
		
		// Find references to the function
		locations, err := lspClient.FindReferences(function.Filepath, function.StartLine, column)
		if err != nil {
			log.Printf("Warning: Failed to find references for function %s: %v", function.Name, err)
			continue
		}

		// Find the definition location to mark references as definitions
		defLocation, err := lspClient.FindDefinition(function.Filepath, function.StartLine, column)
		if err != nil {
			log.Printf("Warning: Failed to find definition for function %s: %v", function.Name, err)
		}

		// Convert locations to references
		references := locationsToReferences(locations)

		// Mark definition references
		for j := range references {
			// Check if this reference is the definition
			if isMatchingLocation(references[j], defLocation) {
				references[j].IsDefining = true
			}
		}

		// Update the function with the references
		functions[i].References = references
	}

	return functions, nil
}

// isMatchingLocation checks if a reference matches a location
func isMatchingLocation(ref Reference, loc protocol.Location) bool {
	refPath := ref.Filepath
	locPath := string(loc.URI)
	
	// Remove file:// prefix if present
	if strings.HasPrefix(locPath, "file://") {
		locPath = locPath[7:]
	}
	
	// Normalize paths for comparison
	refPath, _ = filepath.Abs(refPath)
	locPath, _ = filepath.Abs(locPath)
	
	return refPath == locPath && 
		   ref.Line == int(loc.Range.Start.Line)+1 && 
		   ref.Column == int(loc.Range.Start.Character)
}

// FindFunctionByPosition finds a function at a given position in the file
func FindFunctionByPosition(functions []FunctionInfo, filePath string, line int) *FunctionInfo {
	absPath, err := filepath.Abs(filePath)
	if err != nil {
		log.Printf("Error getting absolute path: %v", err)
		return nil
	}
	
	for i, function := range functions {
		funcPath, err := filepath.Abs(function.Filepath)
		if err != nil {
			continue
		}
		
		if funcPath == absPath && 
		   line >= function.StartLine && 
		   line <= function.EndLine {
			return &functions[i]
		}
	}
	
	return nil
}