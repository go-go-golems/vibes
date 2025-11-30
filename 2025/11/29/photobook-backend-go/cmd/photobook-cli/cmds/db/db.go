package db

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"photobook-backend-go/internal/config"
	"photobook-backend-go/internal/db"
)

type DBStatusCommand struct {
	*cmds.CommandDescription
}

type DBStatusSettings struct{}

func (c *DBStatusCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &DBStatusSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	cfg, err := config.LoadConfig()
	if err != nil {
		return fmt.Errorf("failed to load config: %w", err)
	}

	database, err := db.OpenDB(cfg.DatabaseURL)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer database.Close()

	// Check connection
	if err := database.Ping(); err != nil {
		return fmt.Errorf("failed to ping database: %w", err)
	}

	// Get database stats
	var version string
	if err := database.QueryRowContext(ctx, "SELECT sqlite_version()").Scan(&version); err != nil {
		return fmt.Errorf("failed to get version: %w", err)
	}

	// Count tables
	var tableCount int
	if err := database.QueryRowContext(ctx, `
		SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'
	`).Scan(&tableCount); err != nil {
		return fmt.Errorf("failed to count tables: %w", err)
	}

	row := types.NewRow(
		types.MRP("database_url", cfg.DatabaseURL),
		types.MRP("connected", true),
		types.MRP("version", version),
		types.MRP("table_count", tableCount),
	)

	return gp.AddRow(ctx, row)
}

func NewDBStatusCommand() (*DBStatusCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"status",
		cmds.WithShort("Check database status"),
		cmds.WithLong("Opens database connection and reports status"),
		cmds.WithLayersList(glazedLayer),
	)

	return &DBStatusCommand{
		CommandDescription: cmdDesc,
	}, nil
}

var _ cmds.GlazeCommand = &DBStatusCommand{}

