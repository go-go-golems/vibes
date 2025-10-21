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
	"github.com/ttmp/ttmp-cli/pkg/ticket"
)

type ListDocsCommand struct {
	*cmds.CommandDescription
}

type ListDocsSettings struct {
	Ticket string `glazed.parameter:"ticket"`
	Root   string `glazed.parameter:"root"`
}

func NewListDocsCommand() (*cobra.Command, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"docs",
		cmds.WithShort("List documents in a ticket"),
		cmds.WithLong(`List all documents in a ticket with metadata.

Examples:
  ttmp list docs --ticket MEN-3475
  ttmp list docs --ticket MEN-3475 --output json`),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"ticket",
				parameters.ParameterTypeString,
				parameters.WithHelp("Ticket identifier"),
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

	cmd := &ListDocsCommand{
		CommandDescription: cmdDesc,
	}

	cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(cmd)
	if err != nil {
		return nil, err
	}

	return cobraCmd, nil
}

func (c *ListDocsCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &ListDocsSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	tickets, err := ticket.FindTickets(settings.Root)
	if err != nil {
		return fmt.Errorf("failed to find tickets: %w", err)
	}

	for _, t := range tickets {
		if settings.Ticket != "" && t.Ticket != settings.Ticket {
			continue
		}

		for _, doc := range t.Documents {
			row := types.NewRow(
				types.MRP("ticket", t.Ticket),
				types.MRP("file", doc.Filename),
				types.MRP("doc_type", doc.Metadata.DocType),
				types.MRP("status", doc.Metadata.Status),
				types.MRP("title", doc.Metadata.Title),
			)

			if len(doc.Metadata.Topics) > 0 {
				row.Set("topics", doc.Metadata.Topics)
			}

			if err := gp.AddRow(ctx, row); err != nil {
				return err
			}
		}
	}

	return nil
}

var _ cmds.GlazeCommand = &ListDocsCommand{}

