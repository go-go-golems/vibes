package main

import (
	"fmt"
	"os"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/user/goja-repl/internal/ui"
)

func main() {
	// Create a new model and initialize the program
	model := ui.NewModel()
	
	// Run the program
	p := tea.NewProgram(model)
	if _, err := p.Run(); err != nil {
		fmt.Printf("Error running program: %v\n", err)
		os.Exit(1)
	}
}
