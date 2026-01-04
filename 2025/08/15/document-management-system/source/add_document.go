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

type AddDocumentCommand struct {
	*cmds.CommandDescription
}

type AddDocumentSettings struct {
	ID               string `glazed.parameter:"id"`
	Title            string `glazed.parameter:"title"`
	Kind             string `glazed.parameter:"kind"`
	Status           string `glazed.parameter:"status"`
	LongLived        bool   `glazed.parameter:"long-lived"`
	ReviewInterval   int    `glazed.parameter:"review-interval"`
	Path             string `glazed.parameter:"path"`
	Summary          string `glazed.parameter:"summary"`
	Owner            string `glazed.parameter:"owner"`
	DatabasePath     string `glazed.parameter:"database"`
}

func (c *AddDocumentCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &AddDocumentSettings{}
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
		settings.ID, "Document", settings.Title,
		time.Now().Format(time.RFC3339),
		time.Now().Format(time.RFC3339))
	if err != nil {
		return fmt.Errorf("failed to insert node: %w", err)
	}

	// Insert into documents table
	longLivedInt := 0
	if settings.LongLived {
		longLivedInt = 1
	}

	var reviewInterval *int
	if settings.ReviewInterval > 0 {
		reviewInterval = &settings.ReviewInterval
	}

	_, err = tx.Exec(`
		INSERT INTO documents (node_id, doc_kind, doc_status, long_lived, review_interval_days, path, summary) 
		VALUES (?, ?, ?, ?, ?, ?, ?)`,
		settings.ID, settings.Kind, settings.Status, longLivedInt, reviewInterval, settings.Path, settings.Summary)
	if err != nil {
		return fmt.Errorf("failed to insert document: %w", err)
	}

	// If owner is specified, link to owner
	if settings.Owner != "" {
		_, err = tx.Exec(`
			INSERT OR IGNORE INTO doc_owners (doc_id, person_id) 
			VALUES (?, ?)`,
			settings.ID, settings.Owner)
		if err != nil {
			return fmt.Errorf("failed to link owner: %w", err)
		}
	}

	// Commit transaction
	if err = tx.Commit(); err != nil {
		return fmt.Errorf("failed to commit transaction: %w", err)
	}

	// Output result
	row := types.NewRow(
		types.MRP("id", settings.ID),
		types.MRP("title", settings.Title),
		types.MRP("kind", settings.Kind),
		types.MRP("status", settings.Status),
		types.MRP("long_lived", settings.LongLived),
		types.MRP("path", settings.Path),
		types.MRP("owner", settings.Owner),
		types.MRP("created", "success"),
	)

	return gp.AddRow(ctx, row)
}

// Ensure interface compliance
var _ cmds.GlazeCommand = &AddDocumentCommand{}

func NewAddDocumentCommand() (*AddDocumentCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"add-document",
		cmds.WithShort("Add a new document to the system"),
		cmds.WithLong(`Add a new document to the document management system.

This command creates a new document entry in the database with the specified
metadata and relationships.

Examples:
  docmgmt add-document --id "doc:workspace:plans/api-redesign.md" --title "API Redesign Plan" --kind "plan" --status "draft"
  docmgmt add-document --id "doc:workspace:howtos/deployment.md" --title "Deployment Guide" --kind "howto" --status "accepted" --long-lived --owner "person:github:alice"
		`),

		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"id",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("Unique identifier for the document (e.g., doc:workspace:path/file.md)"),
			),
			parameters.NewParameterDefinition(
				"title",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("Document title"),
			),
			parameters.NewParameterDefinition(
				"kind",
				parameters.ParameterTypeChoice,
				parameters.WithChoices("working", "plan", "analysis", "report", "howto", "reference", "playbook"),
				parameters.WithRequired(true),
				parameters.WithHelp("Document kind"),
			),
			parameters.NewParameterDefinition(
				"status",
				parameters.ParameterTypeChoice,
				parameters.WithChoices("draft", "provisional", "accepted", "deprecated", "archived"),
				parameters.WithRequired(true),
				parameters.WithHelp("Document status"),
			),
			parameters.NewParameterDefinition(
				"long-lived",
				parameters.ParameterTypeBool,
				parameters.WithDefault(false),
				parameters.WithHelp("Whether this is a long-lived document"),
			),
			parameters.NewParameterDefinition(
				"review-interval",
				parameters.ParameterTypeInteger,
				parameters.WithDefault(0),
				parameters.WithHelp("Review interval in days (0 = no review needed)"),
			),
			parameters.NewParameterDefinition(
				"path",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("Workspace path to the document"),
			),
			parameters.NewParameterDefinition(
				"summary",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("Document summary"),
			),
			parameters.NewParameterDefinition(
				"owner",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("Document owner ID (e.g., person:github:username)"),
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

	return &AddDocumentCommand{
		CommandDescription: cmdDesc,
	}, nil
}

