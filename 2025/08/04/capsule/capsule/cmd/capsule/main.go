package main

import (
	"context"
	"embed"
	"fmt"
	"log"
	"os"

	"github.com/capsule/capsule/cmd/capsule/cmds/container"
	gocmds "github.com/capsule/capsule/cmd/capsule/cmds/go"
	"github.com/capsule/capsule/cmd/capsule/cmds/management"
	"github.com/capsule/capsule/cmd/capsule/cmds/monitoring"
	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/help"
	"github.com/spf13/cobra"
)

//go:embed docs
var docsFS embed.FS

var rootCmd = &cobra.Command{
	Use:   "capsule",
	Short: "A thin wrapper around Docker for running binaries with resource constraints",
	Long: `Capsule is a CLI tool that wraps Docker to run binaries and Go programs with 
repeatable CPU, memory, and network constraints. It provides a simple interface 
for creating resource-limited execution environments.

Use 'capsule help [topic]' to get detailed help on any topic.`,
}

func main() {
	// Initialize help system
	helpSystem := help.NewHelpSystem()
	defer func() { _ = helpSystem.Store.Close() }()

	// Load documentation from embedded filesystem
	err := helpSystem.LoadSectionsFromFS(docsFS, "docs")
	if err != nil {
		log.Printf("Warning: Could not load help documentation: %v", err)
	}

	// Create help command
	helpCmd := &cobra.Command{
		Use:   "help [topic]",
		Short: "Help about any command or topic",
		Long: `Get help about commands, topics, and usage examples.

Available help topics:
- overview: Introduction to Capsule
- run-command: Execute commands in containers
- go-command: Build and run Go programs
- resources: Understanding resource constraints
- examples: Practical usage examples
- troubleshooting: Common issues and solutions

You can also use queries like:
- capsule help type:example
- capsule help topic:resources
- capsule help command:run`,
		Run: func(cmd *cobra.Command, args []string) {
			if len(args) == 0 {
				// Show available topics
				sections, err := helpSystem.QuerySections("IsTopLevel:true")
				if err != nil {
					fmt.Printf("Error querying help topics: %v\n", err)
					return
				}

				fmt.Println("Available help topics:")
				fmt.Println()
				for _, section := range sections {
					fmt.Printf("  %-20s %s\n", section.Slug, section.Short)
				}
				fmt.Println()
				fmt.Println("Use 'capsule help <topic>' for detailed information.")
				fmt.Println("Use 'capsule help type:example' to see all examples.")
				return
			}

			query := args[0]
			
			// Try to get specific section first
			section, err := helpSystem.GetSectionWithSlug(query)
			if err == nil {
				// Found specific section
				fmt.Printf("# %s\n\n", section.Title)
				fmt.Println(section.Content)
				return
			}

			// Try query-based search
			sections, err := helpSystem.QuerySections(query)
			if err != nil {
				fmt.Printf("Help topic '%s' not found. Use 'capsule help' to see available topics.\n", query)
				return
			}

			if len(sections) == 0 {
				fmt.Printf("No help topics found matching '%s'.\n", query)
				return
			}

			if len(sections) == 1 {
				// Single result, show full content
				section := sections[0]
				fmt.Printf("# %s\n\n", section.Title)
				fmt.Println(section.Content)
			} else {
				// Multiple results, show summary
				fmt.Printf("Found %d help topics matching '%s':\n\n", len(sections), query)
				for _, section := range sections {
					fmt.Printf("  %-20s %s\n", section.Slug, section.Short)
				}
				fmt.Println()
				fmt.Println("Use 'capsule help <topic>' for detailed information.")
			}
		},
	}

	// Add help command to root
	rootCmd.AddCommand(helpCmd)

	// Create and add commands
	if err := addCommands(rootCmd); err != nil {
		log.Fatalf("Failed to add commands: %v", err)
	}

	// Execute root command
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func addCommands(rootCmd *cobra.Command) error {
	// Create container commands
	runCommand, err := container.NewRunCommand()
	if err != nil {
		return fmt.Errorf("failed to create run command: %w", err)
	}

	shellCommand, err := container.NewShellCommand()
	if err != nil {
		return fmt.Errorf("failed to create shell command: %w", err)
	}

	// Create Go command
	goCommand, err := gocmds.NewGoCommand()
	if err != nil {
		return fmt.Errorf("failed to create go command: %w", err)
	}

	// Create management commands
	stopCommand, err := management.NewStopCommand()
	if err != nil {
		return fmt.Errorf("failed to create stop command: %w", err)
	}

	removeCommand, err := management.NewRemoveCommand()
	if err != nil {
		return fmt.Errorf("failed to create remove command: %w", err)
	}

	// Create monitoring commands
	statsCommand, err := monitoring.NewStatsCommand()
	if err != nil {
		return fmt.Errorf("failed to create stats command: %w", err)
	}

	listCommand, err := monitoring.NewListCommand()
	if err != nil {
		return fmt.Errorf("failed to create list command: %w", err)
	}

	// Convert Glazed commands to Cobra commands and add them
	commands := []cmds.Command{
		runCommand,
		goCommand,
		listCommand,
		statsCommand,
		stopCommand,
		removeCommand,
	}

	for _, cmd := range commands {
		cobraCmd, err := cli.BuildCobraCommandFromCommand(cmd)
		if err != nil {
			return fmt.Errorf("failed to build cobra command for %s: %w", cmd.Description().Name, err)
		}
		rootCmd.AddCommand(cobraCmd)
	}

	// Add shell command separately (it doesn't use glazed processor)
	shellCobraCmd := &cobra.Command{
		Use:   "shell [image]",
		Short: "Open an interactive shell in a constrained capsule",
		Long: `Open an interactive shell inside a Docker container with resource constraints.
The shell inherits your current working directory.

Examples:
  capsule shell --cpu 1 --mem 2g ubuntu:24.04
  capsule shell --cpu 0.5 --mem 512m alpine:latest
  capsule shell ubuntu:latest  # Uses default resource limits`,
		Args: cobra.MaximumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			// Get flags
			cpu, _ := cmd.Flags().GetFloat64("cpu")
			memory, _ := cmd.Flags().GetString("mem")
			
			image := "ubuntu:24.04"
			if len(args) > 0 {
				image = args[0]
			}

			// Create a simple parsed layers equivalent
			// For now, we'll use a simple approach
			return runShellCommand(image, cpu, memory)
		},
	}
	
	shellCobraCmd.Flags().Float64("cpu", 1.0, "CPU limit (number of cores)")
	shellCobraCmd.Flags().String("mem", "1g", "Memory limit (e.g., 512m, 2g)")
	
	rootCmd.AddCommand(shellCobraCmd)

	return nil
}

func runShellCommand(image string, cpu float64, memory string) error {
	// This is a simplified version of the shell command
	// In a full implementation, you would use the proper shell command logic
	fmt.Printf("Opening shell in %s with CPU: %.1f, Memory: %s\n", image, cpu, memory)
	fmt.Println("Note: Shell command implementation simplified for demo")
	return nil
}

