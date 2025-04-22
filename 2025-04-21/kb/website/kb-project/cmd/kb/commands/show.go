// cmd/kb/commands/show.go
package commands

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/kb-project/pkg/config"
	"github.com/kb-project/pkg/show"
	"github.com/kb-project/pkg/store"
)

// ShowSettings contains the settings for the show command
type ShowSettings struct {
	ID        string `glazed.parameter:"id"`
	File      string `glazed.parameter:"file"`
	Line      int    `glazed.parameter:"line"`
}

// ShowCmd represents the show command
type ShowCmd struct {
	*cmds.CommandDescription
	cfg *config.Config
}

// NewShowCmd creates a new show command
func NewShowCmd(cfg *config.Config) *ShowCmd {
	return &ShowCmd{
		CommandDescription: cmds.NewCommandDescription(
			"show",
			cmds.WithShort("Show code chunk details"),
			cmds.WithLong("Display detailed information about a code chunk"),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"id",
					parameters.ParameterTypeString,
					parameters.WithHelp("ID of the chunk to show"),
				),
				parameters.NewParameterDefinition(
					"file",
					parameters.ParameterTypeString,
					parameters.WithHelp("File path to show"),
				),
				parameters.NewParameterDefinition(
					"line",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Line number in the file"),
					parameters.WithDefault(1),
				),
			),
		),
		cfg: cfg,
	}
}

// Run executes the show command
func (c *ShowCmd) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	s := &ShowSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	// Check that either ID or file is specified
	if s.ID == "" && s.File == "" {
		return fmt.Errorf("either --id or --file must be specified")
	}

	// Open the store
	store, err := store.Open(c.cfg.IndexPath, c.cfg.Dimension)
	if err != nil {
		return fmt.Errorf("failed to open store: %w", err)
	}
	defer store.Close()

	// Create shower
	shower := show.NewShower(store)

	// Show the chunk
	var output string
	if s.ID != "" {
		output, err = shower.ShowByID(ctx, s.ID)
		if err != nil {
			return fmt.Errorf("failed to show chunk: %w", err)
		}
	} else {
		output, err = shower.ShowByFileLine(ctx, s.File, s.Line)
		if err != nil {
			return fmt.Errorf("failed to show chunk: %w", err)
		}
	}

	// Print the output
	fmt.Println(output)

	return nil
}
