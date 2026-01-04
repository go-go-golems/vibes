package main

import (
	"fmt"

	"github.com/user/goja-repl/internal/engine"
)

// Simple example to test the Goja JavaScript engine integration
func main() {
	// Create a new JavaScript engine
	jsEngine := engine.New()

	// Test basic JavaScript evaluation
	fmt.Println("Testing basic JavaScript evaluation:")
	
	examples := []string{
		"2 + 2",
		"'Hello, ' + 'World!'",
		"const x = 10; const y = 20; x * y",
		"[1, 2, 3].map(n => n * 2).join(', ')",
		"const obj = { name: 'Goja', type: 'JavaScript Engine' }; obj.name + ' is a ' + obj.type",
		"function factorial(n) { return n <= 1 ? 1 : n * factorial(n-1); }; factorial(5)",
	}

	for _, code := range examples {
		result, err := jsEngine.Eval(code)
		if err != nil {
			fmt.Printf("Error evaluating '%s': %v\n", code, err)
			continue
		}
		fmt.Printf("'%s' => %s\n", code, result)
	}

	// Test error handling
	fmt.Println("\nTesting error handling:")
	_, err := jsEngine.Eval("x = y + 10")
	if err != nil {
		fmt.Printf("Expected error caught: %v\n", err)
	}

	// Test slash command parsing
	fmt.Println("\nTesting slash command parsing:")
	testCommands := []string{
		"/help",
		"/clear",
		"/history",
		"/quit",
		"/help history",
		"not a command",
	}

	for _, cmd := range testCommands {
		if engine.IsSlashCommand(cmd) {
			name, args := engine.ParseSlashCommand(cmd)
			fmt.Printf("'%s' => Command: '%s', Args: '%s'\n", cmd, name, args)
		} else {
			fmt.Printf("'%s' => Not a slash command\n", cmd)
		}
	}

	fmt.Println("\nAll tests completed!")
}
