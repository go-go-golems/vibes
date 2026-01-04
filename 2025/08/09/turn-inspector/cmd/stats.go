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
	"github.com/go-go-golems/glazed/pkg/settings"

	
	"turn-inspector/ent/block"
)

// Stats: Glazed command emitting key/value rows for metrics (metric, value)

type StatsCommand struct { *cmds.CommandDescription }

func NewStatsCommand() (*StatsCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil { return nil, err }
	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)
	d := cmds.NewCommandDescription("stats", cmds.WithShort("Show database statistics"),
		cmds.WithFlags(
			parameters.NewParameterDefinition("detailed", parameters.ParameterTypeBool, parameters.WithHelp("Show detailed statistics"), parameters.WithDefault(false)),
		),
		cmds.WithLayers(glazedLayers),
	)
	return &StatsCommand{d}, nil
}

func (c *StatsCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	client := GetClient()
	if client == nil { return fmt.Errorf("database client not initialized") }
	s := struct { Detailed bool `glazed.parameter:"detailed"` }{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, &s); err != nil { return err }
	detailed := s.Detailed

	turnCount, err := client.Turn.Query().Count(ctx)
	if err != nil { return fmt.Errorf("failed to count turns: %w", err) }
	blockCount, err := client.Block.Query().Count(ctx)
	if err != nil { return fmt.Errorf("failed to count blocks: %w", err) }
	turnMetadataCount, err := client.TurnMetadata.Query().Count(ctx)
	if err != nil { return fmt.Errorf("failed to count turn metadata: %w", err) }
	blockMetadataCount, err := client.BlockMetadata.Query().Count(ctx)
	if err != nil { return fmt.Errorf("failed to count block metadata: %w", err) }

	emit := func(metric string, value any) error {
		return gp.AddRow(ctx, types.NewRow(types.MRP("metric", metric), types.MRP("value", value)))
	}
	if err := emit("total_turns", turnCount); err != nil { return err }
	if err := emit("total_blocks", blockCount); err != nil { return err }
	if err := emit("turn_metadata_entries", turnMetadataCount); err != nil { return err }
	if err := emit("block_metadata_entries", blockMetadataCount); err != nil { return err }
	if turnCount > 0 {
		avg := float64(blockCount) / float64(turnCount)
		if err := emit("avg_blocks_per_turn", avg); err != nil { return err }
	}
	if !detailed { return nil }
	// Detailed: block kind distribution
	kinds := []block.Kind{block.KindLlmText, block.KindToolCall, block.KindToolUse, block.KindSystem, block.KindUser, block.KindOther}
	for _, k := range kinds {
		cnt, err := client.Block.Query().Where(block.KindEQ(k)).Count(ctx)
		if err != nil { continue }
		pct := 0.0
		if blockCount > 0 { pct = float64(cnt) / float64(blockCount) * 100 }
		// emit with kind field for breakdowns
		if err := gp.AddRow(ctx, types.NewRow(
			types.MRP("metric", "block_kind_count"),
			types.MRP("kind", string(k)),
			types.MRP("value", cnt),
		)); err != nil { return err }
		if err := gp.AddRow(ctx, types.NewRow(
			types.MRP("metric", "block_kind_pct"),
			types.MRP("kind", string(k)),
			types.MRP("value", fmt.Sprintf("%.1f%%", pct)),
		)); err != nil { return err }
	}
	return nil
}

func init() {
	sc, _ := NewStatsCommand()
	cobraCmd, _ := cli.BuildCobraCommand(sc)
	rootCmd.AddCommand(cobraCmd)
}
