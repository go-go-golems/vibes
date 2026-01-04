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
)

type SearchCommand struct {
	*cmds.CommandDescription
}

type SearchSettings struct {
	Ticket string `glazed.parameter:"ticket"`
	Topics string `glazed.parameter:"topics"`
	Root   string `glazed.parameter:"root"`
}

func NewSearchCommand() (*cobra.Command, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"search",
		cmds.WithShort("Search for related files"),
		cmds.WithLong(`Search for related files using git history and patterns.

Examples:
  ttmp search --ticket MEN-3475 --topics chat
  ttmp search --ticket MEN-3475 --output json`),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"ticket",
				parameters.ParameterTypeString,
				parameters.WithHelp("Ticket identifier"),
				parameters.WithRequired(true),
			),
			parameters.NewParameterDefinition(
				"topics",
				parameters.ParameterTypeString,
				parameters.WithHelp("Filter by topics"),
				parameters.WithDefault(""),
			),
			parameters.NewParameterDefinition(
				"root",
				parameters.ParameterTypeString,
				parameters.WithHelp("Root directory for ttmp"),
				parameters.WithDefault("./ttmp"),
			),
		),
		cmds.WithLayersList(glazedLayer, commandSettingsLayer),
	)

	cmd := &SearchCommand{
		CommandDescription: cmdDesc,
	}

	cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(cmd)
	if err != nil {
		return nil, err
	}

	return cobraCmd, nil
}

func (c *SearchCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &SearchSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	// Placeholder implementation
	row := types.NewRow(
		types.MRP("file", "example/file.go"),
		types.MRP("relevance", "high"),
		types.MRP("reason", "mentioned in recent commits"),
	)
	
	if err := gp.AddRow(ctx, row); err != nil {
		return err
	}

	return fmt.Errorf("search command not fully implemented")
}

var _ cmds.GlazeCommand = &SearchCommand{}

