package main

import (
	"fmt"
	"os"

	tea "github.com/charmbracelet/bubbletea"
	"bubbletea-todo-app/internal/todo"
)

func main() {
	// Create the enhanced model
	model := todo.NewEnhancedModel()

	// Add some sample todos with variety
	model.TodoList.AddItem("🚀 Learn Bubbletea framework")
	model.TodoList.AddItem("🎨 Build a colorful todo app")
	model.TodoList.AddItem("🌐 Add enhanced web interface")
	model.TodoList.AddItem("✨ Test ANSI color support")
	model.TodoList.AddItem("🔥 Deploy to production")
	model.TodoList.SetSelectedIndex(0)

	// Create the program with enhanced features
	p := tea.NewProgram(model, tea.WithAltScreen(), tea.WithMouseCellMotion())

	// Run the program
	if _, err := p.Run(); err != nil {
		fmt.Printf("Error running enhanced program: %v\n", err)
		os.Exit(1)
	}
}

