package main

import (
	"fmt"
	"mento-tui/internal/cmd"
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/help"
	help_cmd "github.com/go-go-golems/glazed/pkg/help/cmd"
)

func main() {
	// Create and register the mento-tui command
	mentoTuiCmd, err := cmd.NewMentoTuiCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating command: %v\n", err)
		os.Exit(1)
	}

	// Convert to Cobra command - this becomes the root command
	rootCmd, err := cli.BuildCobraCommand(mentoTuiCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building command: %v\n", err)
		os.Exit(1)
	}

	// Setup help system
	helpSystem := help.NewHelpSystem()
	
	// Load documentation from embedded filesystem
	err = cmd.AddDocToHelpSystem(helpSystem)
	if err != nil {
		// Non-fatal: help system will work without docs
		fmt.Fprintf(os.Stderr, "Warning: failed to load help documentation: %v\n", err)
	}

	// Register help system with root command
	help_cmd.SetupCobraRootCommand(helpSystem, rootCmd)

	// Execute the application
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}
