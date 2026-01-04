package cmd

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/spf13/cobra"
	"github.com/ttmp/ttmp-cli/pkg/vocabulary"
)

type VocabListCommand struct {
	*cmds.CommandDescription
}

type VocabListSettings struct {
	Category string `glazed.parameter:"category"`
	VocabFile string `glazed.parameter:"vocab-file"`
}

func NewVocabListCommand() (*cobra.Command, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"list",
		cmds.WithShort("List vocabulary entries"),
		cmds.WithLong(`List vocabulary entries for a category.

Categories: topics, docTypes, intent

Examples:
  ttmp vocab list topics
  ttmp vocab list docTypes --output json`),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"category",
				parameters.ParameterTypeString,
				parameters.WithHelp("Category to list: topics, docTypes, intent"),
				parameters.WithRequired(true),
			),
			parameters.NewParameterDefinition(
				"vocab-file",
				parameters.ParameterTypeString,
				parameters.WithHelp("Path to vocabulary.yaml"),
				parameters.WithDefault("./doc/vocabulary.yaml"),
			),
		),
		cmds.WithLayersList(glazedLayer, commandSettingsLayer),
	)

	cmd := &VocabListCommand{
		CommandDescription: cmdDesc,
	}

	cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(cmd)
	if err != nil {
		return nil, err
	}

	return cobraCmd, nil
}

func (c *VocabListCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &VocabListSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	// Load vocabulary
	vocab, err := vocabulary.Load(settings.VocabFile)
	if err != nil {
		return fmt.Errorf("failed to load vocabulary: %w", err)
	}

	// Get entries for category
	entries, err := vocabulary.GetEntries(vocab, settings.Category)
	if err != nil {
		return err
	}

	// Output as rows
	for _, entry := range entries {
		row := types.NewRow(
			types.MRP("slug", entry.Slug),
			types.MRP("description", entry.Description),
		)
		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

var _ cmds.GlazeCommand = &VocabListCommand{}

