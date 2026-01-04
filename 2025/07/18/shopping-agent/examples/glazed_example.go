// Reference implementation from glazed examples
package main

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
)

// ExampleGlazeCommand example - outputs structured data
type ExampleGlazeCommand struct {
	*cmds.CommandDescription
}

func NewExampleGlazeCommand() (*ExampleGlazeCommand, error) {
	glazedParameterLayer, err := settings.NewGlazeParameterLayers()
	if err != nil {
		return nil, err
	}

	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)

	return &ExampleGlazeCommand{
		CommandDescription: cmds.NewCommandDescription(
			"glaze",
			cmds.WithShort("Example glaze command"),
			cmds.WithLong("A glaze command that outputs structured data"),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"rows",
					parameters.ParameterTypeInteger,
					parameters.WithDefault(2),
					parameters.WithHelp("Number of data rows to output"),
				),
			),
			cmds.WithLayers(glazedLayers),
		),
	}, nil
}

func (c *ExampleGlazeCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	s := struct {
		Rows int `glazed.parameter:"rows"`
	}{}

	err := parsedLayers.InitializeStruct(layers.DefaultSlug, &s)
	if err != nil {
		return err
	}

	for i := 0; i < s.Rows; i++ {
		row := types.NewRow(
			types.MRP("id", i+1),
			types.MRP("name", fmt.Sprintf("Item %d", i+1)),
			types.MRP("value", (i+1)*10),
		)
		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

func (c *ExampleGlazeCommand) Description() *cmds.CommandDescription {
	return c.CommandDescription
}

