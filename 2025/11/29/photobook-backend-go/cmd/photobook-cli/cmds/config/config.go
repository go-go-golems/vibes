package config

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"photobook-backend-go/internal/config"
)

type ConfigValidateCommand struct {
	*cmds.CommandDescription
}

type ConfigValidateSettings struct {
	ShowSecrets bool `glazed.parameter:"show-secrets"`
}

func (c *ConfigValidateCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &ConfigValidateSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	cfg, err := config.LoadConfig()
	if err != nil {
		return fmt.Errorf("failed to load config: %w", err)
	}

	row := types.NewRow(
		types.MRP("database_url", cfg.DatabaseURL),
		types.MRP("storage_path", cfg.StoragePath),
		types.MRP("port", cfg.Port),
		types.MRP("base_url", cfg.BaseURL),
		types.MRP("valid", true),
	)

	if settings.ShowSecrets {
		row.Set("jwt_secret", cfg.JWTSecret)
	} else {
		row.Set("jwt_secret", "***hidden***")
	}

	return gp.AddRow(ctx, row)
}

func NewConfigValidateCommand() (*ConfigValidateCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"validate",
		cmds.WithShort("Validate configuration"),
		cmds.WithLong("Loads and validates configuration from environment variables"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"show-secrets",
				parameters.ParameterTypeBool,
				parameters.WithDefault(false),
				parameters.WithHelp("Show secret values (JWT_SECRET)"),
			),
		),
		cmds.WithLayersList(glazedLayer),
	)

	return &ConfigValidateCommand{
		CommandDescription: cmdDesc,
	}, nil
}

var _ cmds.GlazeCommand = &ConfigValidateCommand{}

