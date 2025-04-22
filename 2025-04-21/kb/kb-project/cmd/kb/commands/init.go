// cmd/kb/commands/init.go
package commands

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/kb-project/pkg/config"
	"github.com/kb-project/pkg/store"
)

// InitSettings contains the settings for the init command
type InitSettings struct {
	Path      string `glazed.parameter:"path"`
	Dimension int    `glazed.parameter:"dim"`
}

// InitCmd represents the init command
type InitCmd struct {
	*cmds.CommandDescription
	cfg        *config.Config
	configPath string
}

// NewInitCmd creates a new init command
func NewInitCmd(cfg *config.Config, configPath string) *InitCmd {
	return &InitCmd{
		CommandDescription: cmds.NewCommandDescription(
			"init",
			cmds.WithShort("Initialize the knowledge base"),
			cmds.WithLong("Create the configuration file and initialize the Bleve index"),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"path",
					parameters.ParameterTypeString,
					parameters.WithHelp("Path to the index directory"),
					parameters.WithDefault(cfg.IndexPath),
				),
				parameters.NewParameterDefinition(
					"dim",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Dimension of the embedding vectors"),
					parameters.WithDefault(cfg.Dimension),
				),
			),
		),
		cfg:        cfg,
		configPath: configPath,
	}
}

// Run executes the init command
func (c *InitCmd) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	s := &InitSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	// Update config
	c.cfg.IndexPath = s.Path
	c.cfg.Dimension = s.Dimension

	// Save config
	if err := config.SaveConfig(c.cfg, c.configPath); err != nil {
		return fmt.Errorf("failed to save config: %w", err)
	}

	// Create index directory
	if err := os.MkdirAll(filepath.Dir(s.Path), 0755); err != nil {
		return fmt.Errorf("failed to create index directory: %w", err)
	}

	// Initialize Bleve index
	_, err := store.Open(s.Path, s.Dimension)
	if err != nil {
		return fmt.Errorf("failed to initialize index: %w", err)
	}

	fmt.Printf("Initialized knowledge base at %s with embedding dimension %d\n", s.Path, s.Dimension)
	return nil
}
