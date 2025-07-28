package main

import (
	"context"
	"fmt"
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/help"

	procmon_cli "github.com/procmon/procmon/pkg/cli"
)

func main() {
	ctx := context.Background()

	// Create the root command
	rootCmd := &cmds.CommandDescription{
		Name:  "procmon",
		Short: "Advanced process and system monitoring tool",
		Long: `
Process Monitor (procmon) is a comprehensive system monitoring tool that provides
real-time information about processes, threads, memory usage, thermal state,
and power consumption.

Key Features:
- Real-time process and thread monitoring with CPU usage per thread
- Memory pressure and kernel thrashing detection
- Thermal monitoring with temperature sensors
- Battery and power state monitoring
- CPU frequency and governor tracking
- Well-known program analysis (Firefox, Chrome, etc.)
- Interactive terminal UI with multiple tabbed views
- Non-interactive output in JSON, CSV, Table, and YAML formats
- Optional SQLite logging for historical data analysis

Use Cases:
- Debugging high CPU usage by identifying specific threads
- Detecting memory pressure and thrashing conditions
- Monitoring system thermal state and preventing overheating
- Tracking power consumption and battery usage
- Analyzing performance of complex applications
- System administration and performance tuning

Examples:
  # Interactive monitoring of all processes
  procmon monitor

  # Monitor a specific process by PID
  procmon monitor 1234

  # List top CPU consumers in JSON format
  procmon list --output-format json --sort-by cpu --limit 10

  # System health overview
  procmon system --output-format table

  # Monitor with custom settings
  procmon monitor --update-interval 500ms --min-cpu 1.0 --show-kernel

For more information and documentation, visit:
https://github.com/procmon/procmon
`,
	}

	// Create subcommands
	monitorCmd := procmon_cli.NewMonitorCommand()
	listCmd := procmon_cli.NewListCommand()
	systemCmd := procmon_cli.NewSystemCommand()

	// Create command tree
	commands := []cmds.Command{
		monitorCmd,
		listCmd,
		systemCmd,
	}

	// Create help system
	helpSystem := help.NewHelpSystem()
	helpSystem.SetupCobraRootCommand(rootCmd)

	// Create CLI application
	glazedParameterLayer, err := cli.NewGlazedParameterLayers()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating glazed parameter layer: %v\n", err)
		os.Exit(1)
	}

	cobraCommand, err := cli.BuildCobraCommandFromCommand(
		rootCmd,
		commands,
		[]layers.ParameterLayer{glazedParameterLayer},
		cli.WithCobraShort(rootCmd.Short),
		cli.WithCobraLong(rootCmd.Long),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building cobra command: %v\n", err)
		os.Exit(1)
	}

	// Add version command
	versionCmd := &cmds.CommandDescription{
		Name:  "version",
		Short: "Show version information",
		Long:  "Display version, build information, and system details for procmon.",
	}

	versionCobraCmd, err := cli.BuildCobraCommandFromCommand(
		versionCmd,
		[]cmds.Command{&VersionCommand{}},
		[]layers.ParameterLayer{},
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building version command: %v\n", err)
		os.Exit(1)
	}

	cobraCommand.AddCommand(versionCobraCmd)

	// Execute the command
	if err := cobraCommand.ExecuteContext(ctx); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

// VersionCommand implements the version command
type VersionCommand struct {
	*cmds.CommandDescription
}

func (c *VersionCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp cmds.GlazeProcessor,
) error {
	version := map[string]interface{}{
		"version":     "1.0.0",
		"build_date":  "2024-01-15",
		"go_version":  "go1.24.5",
		"platform":    fmt.Sprintf("%s/%s", "linux", "amd64"),
		"features": []string{
			"process_monitoring",
			"thread_analysis",
			"memory_pressure_detection",
			"thermal_monitoring",
			"power_management",
			"sqlite_logging",
			"interactive_ui",
			"multiple_output_formats",
		},
	}

	return gp.AddRow(ctx, version)
}

