package main

import (
	"embed"
	"fmt"
	"log"
	"os"

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
				sections, err := helpSystem.QuerySections("")
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

	// Add some basic commands for demonstration
	runCmd := &cobra.Command{
		Use:   "run [flags] IMAGE [COMMAND...]",
		Short: "Execute a command once inside a fresh capsule",
		Long:  "Run a command once inside a Docker container with specified resource constraints.",
		Run: func(cmd *cobra.Command, args []string) {
			fmt.Println("Run command would execute here")
			fmt.Printf("Args: %v\n", args)
		},
	}
	runCmd.Flags().Float64("cpu", 1.0, "CPU limit (number of cores)")
	runCmd.Flags().String("mem", "1g", "Memory limit (e.g., 512m, 2g)")
	
	goCmd := &cobra.Command{
		Use:   "go [flags] PACKAGE [ARGS...]",
		Short: "Build and run Go programs in a capsule",
		Long:  "Build a Go program and run it inside a Docker container with resource constraints.",
		Run: func(cmd *cobra.Command, args []string) {
			fmt.Println("Go command would execute here")
			fmt.Printf("Args: %v\n", args)
		},
	}
	goCmd.Flags().Float64("cpu", 1.0, "CPU limit (number of cores)")
	goCmd.Flags().String("mem", "1g", "Memory limit (e.g., 512m, 2g)")
	
	lsCmd := &cobra.Command{
		Use:   "ls",
		Short: "List running capsules",
		Long:  "List all capsule-managed containers with their resource usage and status.",
		Run: func(cmd *cobra.Command, args []string) {
			fmt.Println("List command would execute here")
		},
	}
	
	statsCmd := &cobra.Command{
		Use:   "stats [CONTAINER...]",
		Short: "Stream live resource usage for capsules",
		Long:  "Stream live CPU, memory, and network usage statistics for running capsules.",
		Run: func(cmd *cobra.Command, args []string) {
			fmt.Println("Stats command would execute here")
			fmt.Printf("Args: %v\n", args)
		},
	}

	rootCmd.AddCommand(runCmd, goCmd, lsCmd, statsCmd)

	// Execute root command
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

