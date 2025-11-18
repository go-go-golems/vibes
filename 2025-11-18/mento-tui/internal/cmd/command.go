package cmd

import (
	"context"
	"fmt"
	"mento-tui/internal/config"
	"mento-tui/internal/ui"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	tea "github.com/charmbracelet/bubbletea"
)

// MentoTuiCommand implements the BareCommand interface for the TUI application
type MentoTuiCommand struct {
	*cmds.CommandDescription
}

// MentoTuiSettings holds the command-line parameters
type MentoTuiSettings struct {
	ConfigPath string `glazed.parameter:"config"`
}

// Ensure interface compliance
var _ cmds.BareCommand = &MentoTuiCommand{}

// Run implements the BareCommand interface
func (c *MentoTuiCommand) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	settings := &MentoTuiSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return fmt.Errorf("failed to parse settings: %w", err)
	}

	// Default config path if not provided
	configPath := settings.ConfigPath
	if configPath == "" {
		configPath = "./mento-tui.yaml"
	}

	cfg, err := config.Load(configPath)
	if err != nil {
		return fmt.Errorf("error loading config: %w", err)
	}

	p := tea.NewProgram(
		ui.NewModel(cfg),
		tea.WithAltScreen(),
		tea.WithMouseCellMotion(),
	)

	if _, err := p.Run(); err != nil {
		return fmt.Errorf("error running program: %w", err)
	}

	return nil
}

// NewMentoTuiCommand creates a new MentoTuiCommand instance
func NewMentoTuiCommand() (*MentoTuiCommand, error) {
	// Create command settings layer for debugging features
	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, fmt.Errorf("failed to create command settings layer: %w", err)
	}

	cmdDesc := cmds.NewCommandDescription(
		"mento-tui",
		cmds.WithShort("Terminal User Interface for managing Mento services"),
		cmds.WithLong(`Mento TUI is a comprehensive Terminal User Interface application for managing
multiple services (Identity Server, Frontend/Vite, and Mento Worker) with
real-time monitoring, logging, and configuration management.

Features:
  - Service Management: Start, stop, and restart individual services or all at once
  - Real-time Monitoring: Live CPU and memory usage tracking for each service
  - Process Management: View PIDs, ports, and service status at a glance
  - Log Aggregation: Centralized logging with filtering by service
  - Configuration Viewer: Display environment variables and configuration with secret masking
  - Interactive Navigation: Keyboard-driven interface with intuitive controls

Examples:
  mento-tui                    # Start TUI with default config (./mento-tui.yaml)
  mento-tui --config ./my-config.yaml  # Start TUI with custom config file
`),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"config",
				parameters.ParameterTypeString,
				parameters.WithDefault("./mento-tui.yaml"),
				parameters.WithHelp("Path to the configuration YAML file"),
				parameters.WithShortFlag("c"),
			),
		),
		cmds.WithLayersList(commandSettingsLayer),
	)

	return &MentoTuiCommand{
		CommandDescription: cmdDesc,
	}, nil
}

