package main

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"github.com/ttmp/ttmp-cli/cmd"
	"github.com/ttmp/ttmp-cli/pkg/doc"
	
	"github.com/go-go-golems/glazed/pkg/help"
	help_cmd "github.com/go-go-golems/glazed/pkg/help/cmd"
)

var version = "0.1.0"

func main() {
	rootCmd := &cobra.Command{
		Use:   "ttmp",
		Short: "Manage structured TTMP documentation for LLM workflows",
		Long: `ttmp is a CLI tool for managing structured documentation in the ttmp/ directory.
It provides commands for creating, organizing, and maintaining ticket-focused
documentation that works well with LLM-assisted development workflows.`,
		Version: version,
	}

	// Initialize help system
	helpSystem := help.NewHelpSystem()
	if err := doc.AddDocToHelpSystem(helpSystem); err != nil {
		fmt.Fprintf(os.Stderr, "Warning: failed to load help documentation: %v\n", err)
	}

	// Register help system with root command
	help_cmd.SetupCobraRootCommand(helpSystem, rootCmd)

	// Add all commands
	if err := cmd.AddCommands(rootCmd); err != nil {
		fmt.Fprintf(os.Stderr, "Error setting up commands: %v\n", err)
		os.Exit(1)
	}

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

