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

	"turn-inspector/ent/block"
)

var createCmd = &cobra.Command{Use: "create", Short: "Create resources"}

// CreateRunCommand -> outputs created run: id, name

type CreateRunCommand struct { *cmds.CommandDescription }

func NewCreateRunCommand() (*CreateRunCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil { return nil, err }
	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)

	d := cmds.NewCommandDescription(
		"run",
		cmds.WithShort("Create a new run"),
		cmds.WithFlags(
			parameters.NewParameterDefinition("name", parameters.ParameterTypeString, parameters.WithHelp("Optional name for the run")),
			parameters.NewParameterDefinition("metadata", parameters.ParameterTypeStringList, parameters.WithHelp("Run metadata as JSON strings")),
		),
		cmds.WithLayers(glazedLayers),
	)
	return &CreateRunCommand{d}, nil
}

func (c *CreateRunCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	client := GetClient()
	if client == nil { return fmt.Errorf("database client not initialized") }
	s := struct {
		Name string   `glazed.parameter:"name"`
		Mds  []string `glazed.parameter:"metadata"`
	}{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, &s); err != nil { return err }
	name := s.Name
	mds := s.Mds
	rc := client.Run.Create()
	if name != "" { rc = rc.SetName(name) }
	r, err := rc.Save(ctx)
	if err != nil { return fmt.Errorf("failed to create run: %w", err) }
	// Parse and attach metadata
	for _, metaStr := range mds {
		var meta map[string]string
		if err := json.Unmarshal([]byte(metaStr), &meta); err != nil { return fmt.Errorf("failed to parse metadata: %w", err) }
		if _, err := client.RunMetadata.Create().
			SetRun(r).
			SetSource(meta["source"]).
			SetKey(meta["key"]).
			SetValue(meta["value"]).
			Save(ctx); err != nil { return fmt.Errorf("failed to create run metadata: %w", err) }
	}
	return gp.AddRow(ctx, types.NewRow(
		types.MRP("id", r.ID),
		types.MRP("name", r.Name),
		types.MRP("metadata_count", len(mds)),
	))
}

// CreateTurnCommand -> outputs created turn: id, run_id, metadata_count, blocks_count

type CreateTurnCommand struct { *cmds.CommandDescription }

func NewCreateTurnCommand() (*CreateTurnCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil { return nil, err }
	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)
	
	d := cmds.NewCommandDescription(
		"turn",
		cmds.WithShort("Create a new conversation turn"),
		cmds.WithFlags(
			parameters.NewParameterDefinition("run-id", parameters.ParameterTypeInteger, parameters.WithHelp("Run ID the turn belongs to")),
			parameters.NewParameterDefinition("metadata", parameters.ParameterTypeStringList, parameters.WithHelp("Turn metadata as JSON strings")),
			parameters.NewParameterDefinition("blocks", parameters.ParameterTypeString, parameters.WithHelp("Blocks as JSON array")),
		),
		cmds.WithLayers(glazedLayers),
	)
	return &CreateTurnCommand{d}, nil
}

func (c *CreateTurnCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	client := GetClient()
	if client == nil { return fmt.Errorf("database client not initialized") }
	si := struct {
		RunID     int      `glazed.parameter:"run-id"`
		Metadata []string `glazed.parameter:"metadata"`
		Blocks   string   `glazed.parameter:"blocks"`
	}{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, &si); err != nil { return err }
	runID := si.RunID
	if runID == 0 { return fmt.Errorf("--run-id is required") }
	mds := si.Metadata
	blocksJSON := si.Blocks
	if blocksJSON == "" { return fmt.Errorf("--blocks is required") }
	// parse metadata
	metadata := make([]map[string]string, 0, len(mds))
	for _, s := range mds {
		var m map[string]string
		if err := json.Unmarshal([]byte(s), &m); err != nil { return fmt.Errorf("failed to parse metadata: %w", err) }
		metadata = append(metadata, m)
	}
	// parse blocks
	var blocksArr []map[string]any
	if err := json.Unmarshal([]byte(blocksJSON), &blocksArr); err != nil { return fmt.Errorf("failed to parse blocks: %w", err) }
	// create turn
	t, err := client.Turn.Create().SetRunID(runID).Save(ctx)
	if err != nil { return fmt.Errorf("failed to create turn: %w", err) }
	createdMeta := 0
	for _, m := range metadata {
		if _, err := client.TurnMetadata.Create().SetTurn(t).SetSource(m["source"]).SetKey(m["key"]).SetValue(m["value"]).Save(ctx); err != nil {
			return fmt.Errorf("failed to create turn metadata: %w", err)
		}
		createdMeta++
	}
	for _, bd := range blocksArr {
		order, _ := bd["order"].(float64)
		kind, _ := bd["kind"].(string)
		role, _ := bd["role"].(string)
		payload, _ := bd["payload"].(map[string]any)
		bc := client.Block.Create().SetTurn(t).SetOrder(int(order)).SetKind(block.Kind(kind))
		if role != "" { bc = bc.SetRole(role) }
		if payload != nil { bc = bc.SetPayload(payload) }
		created, err := bc.Save(ctx)
		if err != nil { return fmt.Errorf("failed to create block: %w", err) }
		if bm, ok := bd["metadata"].([]any); ok {
			for _, mi := range bm {
				if mm, ok := mi.(map[string]any); ok {
					src, _ := mm["source"].(string)
					key, _ := mm["key"].(string)
					val, _ := mm["value"].(string)
					if _, err := client.BlockMetadata.Create().SetBlock(created).SetSource(src).SetKey(key).SetValue(val).Save(ctx); err != nil {
						return fmt.Errorf("failed to create block metadata: %w", err)
					}
				}
			}
		}
	}
	return gp.AddRow(ctx, types.NewRow(
		types.MRP("id", t.ID),
		types.MRP("run_id", runID),
		types.MRP("metadata_count", createdMeta),
		types.MRP("blocks_count", len(blocksArr)),
	))
}

func init() {
	rootCmd.AddCommand(createCmd)
	cr, _ := NewCreateRunCommand()
	crCmd, _ := cli.BuildCobraCommand(cr)
	createCmd.AddCommand(crCmd)
	ct, _ := NewCreateTurnCommand()
	ctCmd, _ := cli.BuildCobraCommand(ct)
	createCmd.AddCommand(ctCmd)
}
