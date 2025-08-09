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
)

var deleteCmd = &cobra.Command{
	Use:   "delete",
	Short: "Delete conversation turns",
	Long:  `Delete conversation turns and all associated data.`,
}

// DeleteTurn glazed command

type DeleteTurnCommand struct { *cmds.CommandDescription }

func NewDeleteTurnCommand() (*DeleteTurnCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil { return nil, err }
	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)
	d := cmds.NewCommandDescription("turn", cmds.WithShort("Delete a specific turn"),
		cmds.WithFlags(
			parameters.NewParameterDefinition("id", parameters.ParameterTypeInteger, parameters.WithHelp("Turn ID to delete")),
			parameters.NewParameterDefinition("confirm", parameters.ParameterTypeBool, parameters.WithHelp("Confirm deletion"), parameters.WithDefault(false)),
		),
		cmds.WithLayers(glazedLayers),
	)
	return &DeleteTurnCommand{d}, nil
}

func (c *DeleteTurnCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
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
	if !confirm { return fmt.Errorf("--confirm required to delete a turn") }
	status := "deleted"
	if err := client.Turn.DeleteOneID(id).Exec(ctx); err != nil {
		status = fmt.Sprintf("error: %v", err)
	}
	return gp.AddRow(ctx, types.NewRow(
		types.MRP("action", "delete_turn"),
		types.MRP("id", id),
		types.MRP("status", status),
	))
}

// DeleteAll glazed command

type DeleteAllCommand struct { *cmds.CommandDescription }

func NewDeleteAllCommand() (*DeleteAllCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil { return nil, err }
	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)
	d := cmds.NewCommandDescription("all", cmds.WithShort("Delete all turns"),
		cmds.WithFlags(
			parameters.NewParameterDefinition("confirm", parameters.ParameterTypeBool, parameters.WithHelp("Confirm deletion of ALL data"), parameters.WithDefault(false)),
		),
		cmds.WithLayers(glazedLayers),
	)
	return &DeleteAllCommand{d}, nil
}

func (c *DeleteAllCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	client := GetClient()
	if client == nil { return fmt.Errorf("database client not initialized") }
	s := struct { Confirm bool `glazed.parameter:"confirm"` }{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, &s); err != nil { return err }
	confirm := s.Confirm
	if !confirm { return fmt.Errorf("--confirm required to delete all data") }
	deleted, err := client.Turn.Delete().Exec(ctx)
	status := "deleted"
	if err != nil {
		status = fmt.Sprintf("error: %v", err)
	}
	return gp.AddRow(ctx, types.NewRow(
		types.MRP("action", "delete_all"),
		types.MRP("deleted_count", deleted),
		types.MRP("status", status),
	))
}

func init() {
	rootCmd.AddCommand(deleteCmd)
	dt, _ := NewDeleteTurnCommand()
	dtCmd, _ := cli.BuildCobraCommand(dt)
	deleteCmd.AddCommand(dtCmd)
	da, _ := NewDeleteAllCommand()
	daCmd, _ := cli.BuildCobraCommand(da)
	deleteCmd.AddCommand(daCmd)
}

