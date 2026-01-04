package main

import (
	"fmt"
	"os"

	tea "github.com/charmbracelet/bubbletea"
	"bubbletea-todo-app/internal/todo"
)

func main() {
	// Create the model
	model := todo.NewModel()

	// Add some sample todos for testing
	model.TodoList.AddItem("Learn Bubbletea")
	model.TodoList.AddItem("Build a todo app")
	model.TodoList.AddItem("Add web interface")
	model.TodoList.SetSelectedIndex(0)

	// Create the program
	p := tea.NewProgram(model, tea.WithAltScreen())

	// Run the program
	if _, err := p.Run(); err != nil {
		fmt.Printf("Error running program: %v\n", err)
		os.Exit(1)
	}
}

