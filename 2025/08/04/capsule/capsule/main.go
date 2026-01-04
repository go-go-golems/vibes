package main

import (
	"context"
	"fmt"
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/help"
	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "capsule",
	Short: "A thin wrapper around Docker for running binaries with resource constraints",
	Long: `Capsule is a CLI tool that wraps Docker to run binaries and Go programs 
with repeatable CPU, memory, and network constraints. It provides profiling 
conveniences and makes it easy to test applications under resource limits.`,
}

func main() {
	// Initialize help system
	helpSystem := help.NewHelpSystem()
	helpSystem.SetupCobraRootCommand(rootCmd)

	// Add commands
	addRunCommand(rootCmd)
	addGoCommand(rootCmd)
	addShellCommand(rootCmd)
	addStatsCommand(rootCmd)
	addListCommand(rootCmd)
	addStopCommand(rootCmd)
	addRemoveCommand(rootCmd)

	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

// addRunCommand adds the 'run' command for executing containers with resource constraints
func addRunCommand(rootCmd *cobra.Command) {
	runCmd, err := NewRunCommand()
	if err != nil {
		panic(fmt.Sprintf("Failed to create run command: %v", err))
	}
	
	cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(runCmd)
	if err != nil {
		panic(fmt.Sprintf("Failed to build run command: %v", err))
	}
	
	rootCmd.AddCommand(cobraCmd)
}

// addGoCommand adds the 'go' command for building and running Go programs
func addGoCommand(rootCmd *cobra.Command) {
	goCmd, err := NewGoCommand()
	if err != nil {
		panic(fmt.Sprintf("Failed to create go command: %v", err))
	}
	
	cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(goCmd)
	if err != nil {
		panic(fmt.Sprintf("Failed to build go command: %v", err))
	}
	
	rootCmd.AddCommand(cobraCmd)
}

// addShellCommand adds the 'shell' command for interactive environments
func addShellCommand(rootCmd *cobra.Command) {
	shellCmd, err := NewShellCommand()
	if err != nil {
		panic(fmt.Sprintf("Failed to create shell command: %v", err))
	}
	
	cobraCmd, err := cli.BuildCobraCommandFromBareCommand(shellCmd)
	if err != nil {
		panic(fmt.Sprintf("Failed to build shell command: %v", err))
	}
	
	rootCmd.AddCommand(cobraCmd)
}

// addStatsCommand adds the 'stats' command for monitoring resource usage
func addStatsCommand(rootCmd *cobra.Command) {
	statsCmd, err := NewStatsCommand()
	if err != nil {
		panic(fmt.Sprintf("Failed to create stats command: %v", err))
	}
	
	cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(statsCmd)
	if err != nil {
		panic(fmt.Sprintf("Failed to build stats command: %v", err))
	}
	
	rootCmd.AddCommand(cobraCmd)
}

// addListCommand adds the 'ls' command for listing running capsules
func addListCommand(rootCmd *cobra.Command) {
	listCmd, err := NewListCommand()
	if err != nil {
		panic(fmt.Sprintf("Failed to create list command: %v", err))
	}
	
	cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(listCmd)
	if err != nil {
		panic(fmt.Sprintf("Failed to build list command: %v", err))
	}
	
	rootCmd.AddCommand(cobraCmd)
}

// addStopCommand adds the 'stop' command for stopping capsules
func addStopCommand(rootCmd *cobra.Command) {
	stopCmd, err := NewStopCommand()
	if err != nil {
		panic(fmt.Sprintf("Failed to create stop command: %v", err))
	}
	
	cobraCmd, err := cli.BuildCobraCommandFromBareCommand(stopCmd)
	if err != nil {
		panic(fmt.Sprintf("Failed to build stop command: %v", err))
	}
	
	rootCmd.AddCommand(cobraCmd)
}

// addRemoveCommand adds the 'rm' command for removing capsules
func addRemoveCommand(rootCmd *cobra.Command) {
	rmCmd, err := NewRemoveCommand()
	if err != nil {
		panic(fmt.Sprintf("Failed to create remove command: %v", err))
	}
	
	cobraCmd, err := cli.BuildCobraCommandFromBareCommand(rmCmd)
	if err != nil {
		panic(fmt.Sprintf("Failed to build remove command: %v", err))
	}
	
	rootCmd.AddCommand(cobraCmd)
}

