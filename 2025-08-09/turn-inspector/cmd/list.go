package cmd

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/types"

	"github.com/spf13/cobra"

	"github.com/go-go-golems/glazed/pkg/settings"
	"turn-inspector/ent"
	"turn-inspector/ent/run"
	"turn-inspector/ent/turn"
)

// Keep the parent "list" command as a grouping Cobra command
var listCmd = &cobra.Command{
	Use:   "list",
	Short: "List conversation data",
}

// ListTurnsCommand implements glazed to emit rows: id, run_id, metadata_count, blocks_count
type ListTurnsCommand struct {
	*cmds.CommandDescription
}

func NewListTurnsCommand() (*ListTurnsCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}
	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)

	d := cmds.NewCommandDescription(
		"turns",
		cmds.WithShort("List conversation turns"),
		cmds.WithLong("List conversation turns with optional filtering and pagination."),
		cmds.WithFlags(
			parameters.NewParameterDefinition("limit", parameters.ParameterTypeInteger, parameters.WithDefault(100), parameters.WithHelp("Maximum number of turns to return")),
			parameters.NewParameterDefinition("offset", parameters.ParameterTypeInteger, parameters.WithDefault(0), parameters.WithHelp("Number of turns to skip")),
			parameters.NewParameterDefinition("run-id", parameters.ParameterTypeInteger, parameters.WithDefault(0), parameters.WithHelp("Filter turns by run ID")),
		),
		cmds.WithLayers(glazedLayers),
	)
	return &ListTurnsCommand{CommandDescription: d}, nil
}

func (c *ListTurnsCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	client := GetClient()
	if client == nil {
		return fmt.Errorf("database client not initialized")
	}
	// Read parameters
	s := struct {
		Limit  int `glazed.parameter:"limit"`
		Offset int `glazed.parameter:"offset"`
		RunID  int `glazed.parameter:"run-id"`
	}{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, &s); err != nil { return err }
	limit := s.Limit
	offset := s.Offset
	runID := s.RunID

	q := client.Turn.Query().
		WithMetadata().
		WithBlocks().
		Limit(limit).
		Offset(offset).
		Order(ent.Desc(turn.FieldID))
	if runID != 0 {
		q = q.Where(turn.HasRunWith(run.IDEQ(runID)))
	}
	turns, err := q.All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query turns: %w", err)
	}
	for _, t := range turns {
		rid := 0
		if t.Edges.Run != nil {
			rid = t.Edges.Run.ID
		}
		metadataCount := 0
		if t.Edges.Metadata != nil {
			metadataCount = len(t.Edges.Metadata)
		}
		blocksCount := 0
		if t.Edges.Blocks != nil {
			blocksCount = len(t.Edges.Blocks)
		}
		row := types.NewRow(
			types.MRP("id", t.ID),
			types.MRP("run_id", rid),
			types.MRP("metadata_count", metadataCount),
			types.MRP("blocks_count", blocksCount),
		)
		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}
	return nil
}

func init() {
	rootCmd.AddCommand(listCmd)
	lc, _ := NewListTurnsCommand()
	cobraCmd, _ := cli.BuildCobraCommand(lc)
	listCmd.AddCommand(cobraCmd)
}
