package main

import (
	"fmt"
	"mento-tui/internal/ui"
	"os"

	tea "github.com/charmbracelet/bubbletea"
)

func main() {
	// Set working directory to project root for mock binaries
	if err := os.Chdir("/home/ubuntu/mento-tui"); err != nil {
		fmt.Printf("Error changing directory: %v\n", err)
		os.Exit(1)
	}

	p := tea.NewProgram(
		ui.NewModel(),
		tea.WithAltScreen(),
		tea.WithMouseCellMotion(),
	)

	if _, err := p.Run(); err != nil {
		fmt.Printf("Error running program: %v\n", err)
		os.Exit(1)
	}
}
