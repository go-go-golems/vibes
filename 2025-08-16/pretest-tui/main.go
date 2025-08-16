package main

import (
	"fmt"
	"os"

	tea "github.com/charmbracelet/bubbletea"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s <pretest-file.yaml>\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "\nExample:\n")
		fmt.Fprintf(os.Stderr, "  %s examples/memory-pretest.yaml\n", os.Args[0])
		os.Exit(1)
	}

	filename := os.Args[1]

	// Load the pretest file
	pretestFile, err := LoadPretestFromFile(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error loading pretest file: %v\n", err)
		os.Exit(1)
	}

	// Create and run the TUI
	model := NewModel(&pretestFile.Pretest)
	
	program := tea.NewProgram(
		model,
		tea.WithAltScreen(),
		tea.WithMouseCellMotion(),
	)

	if _, err := program.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "Error running program: %v\n", err)
		os.Exit(1)
	}
}

