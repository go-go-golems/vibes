package main

import (
	"log"
	"os"

	"github.com/spf13/cobra"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/procmon/procmon/pkg/cli/commands"
)

func main() {
	rootCmd := &cobra.Command{
		Use:   "procmon",
		Short: "Advanced process monitoring tool with structured data output",
		Long: `Process Monitor (procmon) is a comprehensive CLI tool for monitoring processes, 
threads, system resources, and performance metrics on Linux systems. 

It provides rich structured data output in multiple formats (JSON, CSV, YAML, tables)
and supports advanced monitoring capabilities including memory pressure detection,
thermal monitoring, and performance analysis.`,
	}

	// Add version command
	versionCmd := &cobra.Command{
		Use:   "version",
		Short: "Show version information",
		Run: func(cmd *cobra.Command, args []string) {
			cmd.Println("Process Monitor v1.0.0")
			cmd.Println("Built with Glazed framework for structured data output")
		},
	}
	rootCmd.AddCommand(versionCmd)

	// Create and register list command
	listCmd, err := commands.NewListCommand()
	if err != nil {
		log.Fatalf("Failed to create list command: %v", err)
	}
	
	cobraListCmd, err := cli.BuildCobraCommandDualMode(
		listCmd,
		cli.WithGlazeToggleFlag("structured"),
	)
	if err != nil {
		log.Fatalf("Failed to build list command: %v", err)
	}
	rootCmd.AddCommand(cobraListCmd)

	// Create and register monitor command
	monitorCmd, err := commands.NewMonitorCommand()
	if err != nil {
		log.Fatalf("Failed to create monitor command: %v", err)
	}
	
	cobraMonitorCmd, err := cli.BuildCobraCommandDualMode(
		monitorCmd,
		cli.WithGlazeToggleFlag("structured"),
	)
	if err != nil {
		log.Fatalf("Failed to build monitor command: %v", err)
	}
	rootCmd.AddCommand(cobraMonitorCmd)

	// Create and register system command
	systemCmd, err := commands.NewSystemCommand()
	if err != nil {
		log.Fatalf("Failed to create system command: %v", err)
	}
	
	cobraSystemCmd, err := cli.BuildCobraCommandDualMode(
		systemCmd,
		cli.WithGlazeToggleFlag("structured"),
	)
	if err != nil {
		log.Fatalf("Failed to build system command: %v", err)
	}
	rootCmd.AddCommand(cobraSystemCmd)

	// Create and register analyze command
	analyzeCmd, err := commands.NewAnalyzeCommand()
	if err != nil {
		log.Fatalf("Failed to create analyze command: %v", err)
	}
	
	cobraAnalyzeCmd, err := cli.BuildCobraCommandDualMode(
		analyzeCmd,
		cli.WithGlazeToggleFlag("structured"),
	)
	if err != nil {
		log.Fatalf("Failed to build analyze command: %v", err)
	}
	rootCmd.AddCommand(cobraAnalyzeCmd)

	// Create and register export command
	exportCmd, err := commands.NewExportCommand()
	if err != nil {
		log.Fatalf("Failed to create export command: %v", err)
	}
	
	cobraExportCmd, err := cli.BuildCobraCommandDualMode(
		exportCmd,
		cli.WithGlazeToggleFlag("structured"),
	)
	if err != nil {
		log.Fatalf("Failed to build export command: %v", err)
	}
	rootCmd.AddCommand(cobraExportCmd)

	// Execute the root command
	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

