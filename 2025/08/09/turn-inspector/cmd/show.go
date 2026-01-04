package cmd

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/go-go-golems/glazed/pkg/settings"

	"github.com/spf13/cobra"

	"turn-inspector/ent"
	"turn-inspector/ent/block"
	"turn-inspector/ent/turn"
)

var showCmd = &cobra.Command{
	Use:   "show",
	Short: "Show detailed information",
}

// ShowTurnCommand emits a structured object via multi-table rows
// section: turn | turn_metadata | block
// turn row: id, run_id
// metadata rows: section=turn_metadata, source, key, value
// blocks rows: section=block, id, order, kind, role, text or payload (as string)

type ShowTurnCommand struct { *cmds.CommandDescription }

func NewShowTurnCommand() (*ShowTurnCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil { return nil, err }
	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)

	d := cmds.NewCommandDescription(
		"turn",
		cmds.WithShort("Show detailed turn information"),
		cmds.WithFlags(
			parameters.NewParameterDefinition("id", parameters.ParameterTypeInteger, parameters.WithHelp("Turn ID to show")),
		),
		cmds.WithLayers(glazedLayers),
	)
	return &ShowTurnCommand{d}, nil
}

func (c *ShowTurnCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	client := GetClient()
	if client == nil { return fmt.Errorf("database client not initialized") }
	si := struct { ID int `glazed.parameter:"id"` }{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, &si); err != nil { return err }
	id := si.ID
	if id == 0 { return fmt.Errorf("--id is required") }
	t, err := client.Turn.Query().
		Where(turn.IDEQ(id)).
		WithRun().
		WithMetadata().
		WithBlocks(func(bq *ent.BlockQuery) { bq.Order(ent.Asc(block.FieldOrder)).WithMetadata() }).
		Only(ctx)
	if err != nil { return fmt.Errorf("failed to query turn: %w", err) }
	runID := 0
	if t.Edges.Run != nil { runID = t.Edges.Run.ID }
	if err := gp.AddRow(ctx, types.NewRow(
		types.MRP("section", "turn"),
		types.MRP("id", t.ID),
		types.MRP("run_id", runID),
	)); err != nil { return err }
	for _, m := range t.Edges.Metadata {
		if err := gp.AddRow(ctx, types.NewRow(
			types.MRP("section", "turn_metadata"),
			types.MRP("source", m.Source),
			types.MRP("key", m.Key),
			types.MRP("value", m.Value),
		)); err != nil { return err }
	}
	for _, b := range t.Edges.Blocks {
		var text string
		var payload map[string]any
		if b.Payload != nil {
			if tt, ok := b.Payload["text"].(string); ok {
				text = tt
			} else {
				payload = b.Payload
			}
		}
		row := types.NewRow(
			types.MRP("section", "block"),
			types.MRP("id", b.ID),
			types.MRP("order", b.Order),
			types.MRP("kind", string(b.Kind)),
			types.MRP("role", b.Role),
		)
		if text != "" {
			row.Set("text", text)
		} else if payload != nil {
			bs, _ := json.Marshal(payload)
			row.Set("payload", string(bs))
		}
		if err := gp.AddRow(ctx, row); err != nil { return err }
	}
	return nil
}

// ShowBlocksCommand: rows per block: id, order, kind, role, text/payload

type ShowBlocksCommand struct { *cmds.CommandDescription }

func NewShowBlocksCommand() (*ShowBlocksCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil { return nil, err }
	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)
	d := cmds.NewCommandDescription(
		"blocks",
		cmds.WithShort("Show blocks for a turn"),
		cmds.WithFlags(
			parameters.NewParameterDefinition("turn-id", parameters.ParameterTypeInteger, parameters.WithHelp("Turn ID to show blocks for")),
		),
		cmds.WithLayers(glazedLayers),
	)
	return &ShowBlocksCommand{d}, nil
}

func (c *ShowBlocksCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	client := GetClient()
	if client == nil { return fmt.Errorf("database client not initialized") }
	si := struct { TurnID int `glazed.parameter:"turn-id"` }{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, &si); err != nil { return err }
	turnID := si.TurnID
	if turnID == 0 { return fmt.Errorf("--turn-id is required") }
	blocks, err := client.Block.Query().
		Where(block.HasTurnWith(turn.IDEQ(turnID))).
		Order(ent.Asc(block.FieldOrder)).
		All(ctx)
	if err != nil { return fmt.Errorf("failed to query blocks: %w", err) }
	for _, b := range blocks {
		row := types.NewRow(
			types.MRP("id", b.ID),
			types.MRP("order", b.Order),
			types.MRP("kind", string(b.Kind)),
			types.MRP("role", b.Role),
			types.MRP("turn_id", turnID),
		)
		if b.Payload != nil {
			if txt, ok := b.Payload["text"].(string); ok {
				row.Set("text", txt)
			} else {
				bs, _ := json.Marshal(b.Payload)
				row.Set("payload", string(bs))
			}
		}
		if err := gp.AddRow(ctx, row); err != nil { return err }
	}
	return nil
}

func init() {
	rootCmd.AddCommand(showCmd)
	st, _ := NewShowTurnCommand()
	stCmd, _ := cli.BuildCobraCommand(st)
	showCmd.AddCommand(stCmd)
	sb, _ := NewShowBlocksCommand()
	sbCmd, _ := cli.BuildCobraCommand(sb)
	showCmd.AddCommand(sbCmd)
}
