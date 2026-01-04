package main

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	_ "github.com/mattn/go-sqlite3"
)

type AddPersonCommand struct {
	*cmds.CommandDescription
}

type AddPersonSettings struct {
	ID           string `glazed.parameter:"id"`
	Handle       string `glazed.parameter:"handle"`
	DisplayName  string `glazed.parameter:"display-name"`
	DatabasePath string `glazed.parameter:"database"`
}

func (c *AddPersonCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &AddPersonSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	// Open database connection
	db, err := sql.Open("sqlite3", settings.DatabasePath)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer db.Close()

	// Start transaction
	tx, err := db.Begin()
	if err != nil {
		return fmt.Errorf("failed to start transaction: %w", err)
	}
	defer tx.Rollback()

	// Insert into nodes table
	_, err = tx.Exec(`
		INSERT INTO nodes (id, type, title, created_at, updated_at) 
		VALUES (?, ?, ?, ?, ?)`,
		settings.ID, "Person", settings.DisplayName,
		time.Now().Format(time.RFC3339),
		time.Now().Format(time.RFC3339))
	if err != nil {
		return fmt.Errorf("failed to insert node: %w", err)
	}

	// Insert into people table
	_, err = tx.Exec(`
		INSERT INTO people (node_id, handle, display_name) 
		VALUES (?, ?, ?)`,
		settings.ID, settings.Handle, settings.DisplayName)
	if err != nil {
		return fmt.Errorf("failed to insert person: %w", err)
	}

	// Commit transaction
	if err = tx.Commit(); err != nil {
		return fmt.Errorf("failed to commit transaction: %w", err)
	}

	// Output result
	row := types.NewRow(
		types.MRP("id", settings.ID),
		types.MRP("handle", settings.Handle),
		types.MRP("display_name", settings.DisplayName),
		types.MRP("created", "success"),
	)

	return gp.AddRow(ctx, row)
}

// Ensure interface compliance
var _ cmds.GlazeCommand = &AddPersonCommand{}

func NewAddPersonCommand() (*AddPersonCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"add-person",
		cmds.WithShort("Add a new person to the system"),
		cmds.WithLong(`Add a new person to the document management system.

This command creates a new person entry in the database that can be used
as document owners, authors, or in other relationships.

Examples:
  docmgmt add-person --id "person:github:alice" --handle "alice" --display-name "Alice Johnson"
  docmgmt add-person --id "person:github:bob" --handle "bob" --display-name "Bob Smith"
		`),

		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"id",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("Unique identifier for the person (e.g., person:github:username)"),
			),
			parameters.NewParameterDefinition(
				"handle",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("Person's handle (e.g., GitHub username)"),
			),
			parameters.NewParameterDefinition(
				"display-name",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("Person's display name"),
			),
			parameters.NewParameterDefinition(
				"database",
				parameters.ParameterTypeString,
				parameters.WithDefault("/home/ubuntu/cayley/docmgmt.db"),
				parameters.WithHelp("Path to SQLite database"),
			),
		),

		cmds.WithLayersList(glazedLayer, commandSettingsLayer),
	)

	return &AddPersonCommand{
		CommandDescription: cmdDesc,
	}, nil
}

