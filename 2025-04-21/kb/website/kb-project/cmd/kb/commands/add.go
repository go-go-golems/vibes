// cmd/kb/commands/add.go
package commands

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/kb-project/pkg/config"
)

// AddSettings contains the settings for the add command
type AddSettings struct {
	Path string `glazed.parameter:"path"`
}

// AddCmd represents the add command
type AddCmd struct {
	*cmds.CommandDescription
	cfg *config.Config
}

// NewAddCmd creates a new add command
func NewAddCmd(cfg *config.Config) *AddCmd {
	return &AddCmd{
		CommandDescription: cmds.NewCommandDescription(
			"add",
			cmds.WithShort("Add a repository to the knowledge base"),
			cmds.WithLong("Track a local repository for indexing"),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"path",
					parameters.ParameterTypeString,
					parameters.WithHelp("Path to the repository"),
					parameters.WithRequired(true),
				),
			),
		),
		cfg: cfg,
	}
}

// Run executes the add command
func (c *AddCmd) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	s := &AddSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	// Add the repository to the config
	c.cfg.Repositories = append(c.cfg.Repositories, s.Path)

	// Save the config
	if err := config.SaveConfig(c.cfg, c.cfg.ConfigPath); err != nil {
		return fmt.Errorf("failed to save config: %w", err)
	}

	fmt.Printf("Added repository %s to the knowledge base\n", s.Path)
	return nil
}
