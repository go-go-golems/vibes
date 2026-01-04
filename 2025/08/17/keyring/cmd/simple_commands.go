package cmd

import (
	"context"
	"fmt"

	"keyring/pkg/keyring"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
)

// SimpleListCommand for basic listing functionality
type SimpleListCommand struct {
	*cmds.CommandDescription
	ring *keyring.Ring
}

type SimpleListSettings struct {
	Prefix string `glazed.parameter:"prefix"`
}

func NewSimpleListCommand(ring *keyring.Ring) (*SimpleListCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"list",
		cmds.WithShort("List secrets and paths in the keyring"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"prefix",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("Path prefix to list under"),
				parameters.WithShortFlag("p"),
			),
		),
		cmds.WithLayersList(glazedLayer, commandSettingsLayer),
	)

	return &SimpleListCommand{
		CommandDescription: cmdDesc,
		ring:               ring,
	}, nil
}

func (c *SimpleListCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &SimpleListSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	prefix := keyring.P(settings.Prefix)
	paths, err := c.ring.List(ctx, prefix)
	if err != nil {
		if keyring.IsNotFound(err) {
			return fmt.Errorf("no paths found under prefix: %s", settings.Prefix)
		}
		return fmt.Errorf("failed to list paths: %w", err)
	}

	for _, path := range paths {
		fullPath := prefix.Child(path.String())
		row := types.NewRow(
			types.MRP("path", fullPath.String()),
			types.MRP("type", "path"),
		)

		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

// SimpleDeleteCommand for basic delete functionality
type SimpleDeleteCommand struct {
	*cmds.CommandDescription
	ring *keyring.Ring
}

type SimpleDeleteSettings struct {
	Path string `glazed.parameter:"path"`
}

func NewSimpleDeleteCommand(ring *keyring.Ring) (*SimpleDeleteCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"delete",
		cmds.WithShort("Delete a secret from the keyring"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"path",
				parameters.ParameterTypeString,
				parameters.WithHelp("Hierarchical path to the secret"),
				parameters.WithShortFlag("p"),
				parameters.WithRequired(true),
			),
		),
		cmds.WithLayersList(glazedLayer, commandSettingsLayer),
	)

	return &SimpleDeleteCommand{
		CommandDescription: cmdDesc,
		ring:               ring,
	}, nil
}

func (c *SimpleDeleteCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &SimpleDeleteSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	path := keyring.P(settings.Path)

	if err := c.ring.Delete(ctx, path); err != nil {
		if keyring.IsNotFound(err) {
			return fmt.Errorf("secret not found at path: %s", settings.Path)
		}
		return fmt.Errorf("failed to delete secret: %w", err)
	}

	row := types.NewRow(
		types.MRP("path", settings.Path),
		types.MRP("status", "deleted"),
	)

	return gp.AddRow(ctx, row)
}

