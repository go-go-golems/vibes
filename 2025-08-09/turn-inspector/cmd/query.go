package cmd

import (
	"context"
	"fmt"
	"strings"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/types"

	"github.com/spf13/cobra"

	"github.com/go-go-golems/glazed/pkg/settings"
	"turn-inspector/ent"
	"turn-inspector/ent/block"
	"turn-inspector/ent/run"
	"turn-inspector/ent/turn"
	"turn-inspector/ent/turnmetadata"
)

var queryCmd = &cobra.Command{
	Use:   "query",
	Short: "Query and search conversation data",
}

type QueryTurnsCommand struct {
	*cmds.CommandDescription
}

func NewQueryTurnsCommand() (*QueryTurnsCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil { return nil, err }
	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)
	
	d := cmds.NewCommandDescription(
		"turns",
		cmds.WithShort("Query turns by metadata or content"),
		cmds.WithFlags(
			parameters.NewParameterDefinition("metadata-key", parameters.ParameterTypeString, parameters.WithHelp("Search by metadata key")),
			parameters.NewParameterDefinition("metadata-value", parameters.ParameterTypeString, parameters.WithHelp("Search by metadata value")),
			parameters.NewParameterDefinition("text", parameters.ParameterTypeString, parameters.WithHelp("Search for text in block payloads")),
			parameters.NewParameterDefinition("block-kind", parameters.ParameterTypeString, parameters.WithHelp("Search by block kind")),
			parameters.NewParameterDefinition("run-id", parameters.ParameterTypeInteger, parameters.WithDefault(0), parameters.WithHelp("Filter by run ID")),
		),
		cmds.WithLayers(glazedLayers),
	)
	return &QueryTurnsCommand{CommandDescription: d}, nil
}

func (c *QueryTurnsCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	client := GetClient()
	if client == nil {
		return fmt.Errorf("database client not initialized")
	}
	s := struct {
		MetaKey   string `glazed.parameter:"metadata-key"`
		MetaValue string `glazed.parameter:"metadata-value"`
		Text      string `glazed.parameter:"text"`
		BlockKind string `glazed.parameter:"block-kind"`
		RunID     int    `glazed.parameter:"run-id"`
	}{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, &s); err != nil { return err }
	metaKey := s.MetaKey
	metaVal := s.MetaValue
	textQ := s.Text
	blockKind := s.BlockKind
	runID := s.RunID

	query := client.Turn.Query().WithMetadata().WithBlocks()
	if runID != 0 {
		query = query.Where(turn.HasRunWith(run.IDEQ(runID)))
	}
	if metaKey != "" {
		if metaVal != "" {
			query = query.Where(turn.HasMetadataWith(turnmetadata.And(
				turnmetadata.KeyEQ(metaKey),
				turnmetadata.ValueEQ(metaVal),
			)))
		} else {
			query = query.Where(turn.HasMetadataWith(turnmetadata.KeyEQ(metaKey)))
		}
	} else if metaVal != "" {
		query = query.Where(turn.HasMetadataWith(turnmetadata.ValueEQ(metaVal)))
	}
	if blockKind != "" {
		query = query.Where(turn.HasBlocksWith(block.KindEQ(block.Kind(blockKind))))
	}
	turns, err := query.All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query turns: %w", err)
	}
	// optional post-filter text
	if textQ != "" {
		filtered := make([]*ent.Turn, 0, len(turns))
		for _, t := range turns {
			blocks := t.Edges.Blocks
			for _, b := range blocks {
				if b.Payload != nil {
					if txt, ok := b.Payload["text"].(string); ok {
						if contains(txt, textQ) {
							filtered = append(filtered, t)
							break
						}
					}
				}
			}
		}
		turns = filtered
	}
	criteria := buildCriteriaString(metaKey, metaVal, textQ, blockKind, runID)
	for _, t := range turns {
		rid := 0
		if t.Edges.Run != nil {
			rid = t.Edges.Run.ID
		}
		mc := 0
		if t.Edges.Metadata != nil {
			mc = len(t.Edges.Metadata)
		}
		bc := 0
		if t.Edges.Blocks != nil {
			bc = len(t.Edges.Blocks)
		}
		row := types.NewRow(
			types.MRP("id", t.ID),
			types.MRP("run_id", rid),
			types.MRP("metadata_count", mc),
			types.MRP("blocks_count", bc),
			types.MRP("criteria", criteria),
		)
		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}
	return nil
}

func contains(text, search string) bool {
	// naive case-insensitive contains
	return strings.Contains(strings.ToLower(text), strings.ToLower(search))
}

func buildCriteriaString(metaKey, metaVal, textQ, blockKind string, runID int) string {
	crit := ""
	append := func(k, v string) {
		if v == "" { return }
		if crit != "" { crit += ", " }
		crit += fmt.Sprintf("%s:%s", k, v)
	}
	append("metadata-key", metaKey)
	append("metadata-value", metaVal)
	append("text", textQ)
	append("block-kind", blockKind)
	if runID != 0 {
		if crit != "" { crit += ", " }
		crit += fmt.Sprintf("run-id:%d", runID)
	}
	return crit
}

func init() {
	rootCmd.AddCommand(queryCmd)
	qc, _ := NewQueryTurnsCommand()
	cobraCmd, _ := cli.BuildCobraCommand(qc)
	queryCmd.AddCommand(cobraCmd)
}
