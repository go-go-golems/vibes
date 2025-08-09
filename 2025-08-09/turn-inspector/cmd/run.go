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

	"github.com/spf13/cobra"

	"turn-inspector/ent"
	"turn-inspector/ent/run"
)

var runCmd = &cobra.Command{Use: "run", Short: "Manage runs"}

// RunList: rows: id, name, metadata_count, turns_count

type RunListCommand struct { *cmds.CommandDescription }

func NewRunListCommand() (*RunListCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil { return nil, err }
	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)
	d := cmds.NewCommandDescription("list", cmds.WithShort("List runs"), cmds.WithLayers(glazedLayers))
	return &RunListCommand{d}, nil
}

func (c *RunListCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	client := GetClient()
	if client == nil { return fmt.Errorf("database client not initialized") }
	runs, err := client.Run.Query().WithMetadata().WithTurns().Order(ent.Desc(run.FieldID)).All(ctx)
	if err != nil { return fmt.Errorf("failed to query runs: %w", err) }
	for _, r := range runs {
		row := types.NewRow(
			types.MRP("id", r.ID),
			types.MRP("name", r.Name),
			types.MRP("metadata_count", len(r.Edges.Metadata)),
			types.MRP("turns_count", len(r.Edges.Turns)),
		)
		if err := gp.AddRow(ctx, row); err != nil { return err }
	}
	return nil
}

// RunShow: emits run details and metadata counts

type RunShowCommand struct { *cmds.CommandDescription }

func NewRunShowCommand() (*RunShowCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil { return nil, err }
	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)
	d := cmds.NewCommandDescription("show", cmds.WithShort("Show a run"),
		cmds.WithFlags(
			parameters.NewParameterDefinition("id", parameters.ParameterTypeInteger, parameters.WithHelp("Run ID to show")),
		),
		cmds.WithLayers(glazedLayers),
	)
	return &RunShowCommand{d}, nil
}

func (c *RunShowCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	client := GetClient()
	if client == nil { return fmt.Errorf("database client not initialized") }
	si := struct { ID int `glazed.parameter:"id"` }{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, &si); err != nil { return err }
	runID := si.ID
	if runID == 0 { return fmt.Errorf("--id is required") }
	r, err := client.Run.Query().Where(run.IDEQ(runID)).WithMetadata().WithTurns().Only(ctx)
	if err != nil { return fmt.Errorf("failed to query run: %w", err) }
	// primary row
	if err := gp.AddRow(ctx, types.NewRow(
		types.MRP("id", r.ID),
		types.MRP("name", r.Name),
	)); err != nil { return err }
	// metadata rows
	for _, m := range r.Edges.Metadata {
		if err := gp.AddRow(ctx, types.NewRow(
			types.MRP("section", "run_metadata"),
			types.MRP("source", m.Source),
			types.MRP("key", m.Key),
			types.MRP("value", m.Value),
		)); err != nil { return err }
	}
	return nil
}

// RunDelete: Glazed implementation emitting action, id, status

type RunDeleteCommand struct { *cmds.CommandDescription }

func NewRunDeleteCommand() (*RunDeleteCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil { return nil, err }
	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)
	d := cmds.NewCommandDescription("delete", cmds.WithShort("Delete a run and its turns"),
		cmds.WithFlags(
			parameters.NewParameterDefinition("id", parameters.ParameterTypeInteger, parameters.WithHelp("Run ID to delete")),
			parameters.NewParameterDefinition("confirm", parameters.ParameterTypeBool, parameters.WithHelp("Confirm deletion"), parameters.WithDefault(false)),
		),
		cmds.WithLayers(glazedLayers),
	)
	return &RunDeleteCommand{d}, nil
}

func (c *RunDeleteCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	client := GetClient()
	if client == nil { return fmt.Errorf("database client not initialized") }
	s := struct {
		ID      int  `glazed.parameter:"id"`
		Confirm bool `glazed.parameter:"confirm"`
	}{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, &s); err != nil { return err }
	id := s.ID
	confirm := s.Confirm
	if id == 0 { return fmt.Errorf("--id is required") }
	if !confirm { return fmt.Errorf("--confirm required to delete a run") }
	status := "deleted"
	if err := client.Run.DeleteOneID(id).Exec(ctx); err != nil {
		status = fmt.Sprintf("error: %v", err)
	}
	return gp.AddRow(ctx, types.NewRow(
		types.MRP("action", "delete_run"),
		types.MRP("id", id),
		types.MRP("status", status),
	))
}

func init() {
	rootCmd.AddCommand(runCmd)
	rl, _ := NewRunListCommand()
	rlCmd, _ := cli.BuildCobraCommand(rl)
	runCmd.AddCommand(rlCmd)
	rs, _ := NewRunShowCommand()
	rsCmd, _ := cli.BuildCobraCommand(rs)
	runCmd.AddCommand(rsCmd)
	rd, _ := NewRunDeleteCommand()
	rdCmd, _ := cli.BuildCobraCommand(rd)
	runCmd.AddCommand(rdCmd)
}
