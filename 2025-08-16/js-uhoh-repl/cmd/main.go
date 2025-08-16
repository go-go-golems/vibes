package main

import (
	"context"
	"fmt"
	"log"
	"os"

	tea "github.com/charmbracelet/bubbletea"
	"js-uhoh-repl/pkg/evaluator"
)

func main() {
	// Check if a file was provided as argument
	if len(os.Args) > 1 {
		// Execute file mode
		filePath := os.Args[1]
		fmt.Printf("Executing JavaScript file: %s\n", filePath)
		
		eval, err := evaluator.NewJSUhohEvaluator()
		if err != nil {
			log.Fatal(err)
		}
		
		// Execute the file
		result, err := eval.ExecuteFile(context.Background(), filePath)
		if err != nil {
			log.Fatalf("Error executing file: %v", err)
		}
		
		fmt.Printf("Execution result: %s\n", result)
		return
	}

	// REPL mode
	fmt.Println("Starting JavaScript + Uhoh REPL...")
	
	// Create the JavaScript evaluator with uhoh integration
	eval, err := evaluator.NewJSUhohEvaluator()
	if err != nil {
		log.Fatal(err)
	}

	// Create the REPL model
	model := NewREPLModel(eval)

	// Run the program (no alt screen to help debugging)
	p := tea.NewProgram(model)
	if _, err := p.Run(); err != nil {
		log.Fatal(err)
	}
}

