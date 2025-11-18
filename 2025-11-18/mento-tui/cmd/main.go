package main

import (
	"flag"
	"fmt"
	"mento-tui/internal/config"
	"mento-tui/internal/ui"
	"os"

	tea "github.com/charmbracelet/bubbletea"
)

func main() {
	configPath := flag.String("config", "./mento-tui.yaml", "path to config file")
	flag.Parse()

	cfg, err := config.Load(*configPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error loading config: %v\n", err)
		os.Exit(1)
	}

	p := tea.NewProgram(
		ui.NewModel(cfg),
		tea.WithAltScreen(),
		tea.WithMouseCellMotion(),
	)

	if _, err := p.Run(); err != nil {
		fmt.Printf("Error running program: %v\n", err)
		os.Exit(1)
	}
}
