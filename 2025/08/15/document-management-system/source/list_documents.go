package main

import (
	"context"
	"database/sql"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	_ "github.com/mattn/go-sqlite3"
)

type ListDocumentsCommand struct {
	*cmds.CommandDescription
}

type ListDocumentsSettings struct {
	Kind         string `glazed.parameter:"kind"`
	Status       string `glazed.parameter:"status"`
	LongLived    bool   `glazed.parameter:"long-lived-only"`
	Owner        string `glazed.parameter:"owner"`
	Limit        int    `glazed.parameter:"limit"`
	DatabasePath string `glazed.parameter:"database"`
}

func (c *ListDocumentsCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &ListDocumentsSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	// Open database connection
	db, err := sql.Open("sqlite3", settings.DatabasePath)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer db.Close()

	// Build query
	query := `
		SELECT 
			n.id, n.title, n.created_at, n.updated_at,
			d.doc_kind, d.doc_status, d.long_lived, d.review_interval_days, d.path, d.summary,
			GROUP_CONCAT(DISTINCT p.handle) as owners
		FROM nodes n
		JOIN documents d ON n.id = d.node_id
		LEFT JOIN doc_owners do ON n.id = do.doc_id
		LEFT JOIN people p ON do.person_id = p.node_id
		WHERE n.type = 'Document'`

	args := []interface{}{}
	
	if settings.Kind != "" {
		query += " AND d.doc_kind = ?"
		args = append(args, settings.Kind)
	}
	
	if settings.Status != "" {
		query += " AND d.doc_status = ?"
		args = append(args, settings.Status)
	}
	
	if settings.LongLived {
		query += " AND d.long_lived = 1"
	}
	
	if settings.Owner != "" {
		query += " AND do.person_id = ?"
		args = append(args, settings.Owner)
	}

	query += " GROUP BY n.id ORDER BY n.created_at DESC"
	
	if settings.Limit > 0 {
		query += " LIMIT ?"
		args = append(args, settings.Limit)
	}

	// Execute query
	rows, err := db.Query(query, args...)
	if err != nil {
		return fmt.Errorf("failed to query documents: %w", err)
	}
	defer rows.Close()

	// Process results
	for rows.Next() {
		var id, title, createdAt, updatedAt, docKind, docStatus, path, summary string
		var longLived int
		var reviewInterval sql.NullInt64
		var owners sql.NullString

		err := rows.Scan(&id, &title, &createdAt, &updatedAt, &docKind, &docStatus, 
			&longLived, &reviewInterval, &path, &summary, &owners)
		if err != nil {
			return fmt.Errorf("failed to scan row: %w", err)
		}

		reviewIntervalStr := ""
		if reviewInterval.Valid {
			reviewIntervalStr = fmt.Sprintf("%d days", reviewInterval.Int64)
		}

		ownersStr := ""
		if owners.Valid {
			ownersStr = owners.String
		}

		row := types.NewRow(
			types.MRP("id", id),
			types.MRP("title", title),
			types.MRP("kind", docKind),
			types.MRP("status", docStatus),
			types.MRP("long_lived", longLived == 1),
			types.MRP("review_interval", reviewIntervalStr),
			types.MRP("path", path),
			types.MRP("summary", summary),
			types.MRP("owners", ownersStr),
			types.MRP("created_at", createdAt),
			types.MRP("updated_at", updatedAt),
		)

		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return rows.Err()
}

// Ensure interface compliance
var _ cmds.GlazeCommand = &ListDocumentsCommand{}

func NewListDocumentsCommand() (*ListDocumentsCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"list-documents",
		cmds.WithShort("List documents in the system"),
		cmds.WithLong(`List documents in the document management system with optional filtering.

This command queries the database and returns documents matching the specified criteria.
Results can be filtered by kind, status, owner, and other attributes.

Examples:
  docmgmt list-documents                           # List all documents
  docmgmt list-documents --kind plan               # List only plan documents
  docmgmt list-documents --status draft --limit 5 # List first 5 draft documents
  docmgmt list-documents --long-lived-only         # List only long-lived documents
  docmgmt list-documents --output json             # Output as JSON
		`),

		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"kind",
				parameters.ParameterTypeChoice,
				parameters.WithChoices("working", "plan", "analysis", "report", "howto", "reference", "playbook"),
				parameters.WithHelp("Filter by document kind"),
			),
			parameters.NewParameterDefinition(
				"status",
				parameters.ParameterTypeChoice,
				parameters.WithChoices("draft", "provisional", "accepted", "deprecated", "archived"),
				parameters.WithHelp("Filter by document status"),
			),
			parameters.NewParameterDefinition(
				"long-lived-only",
				parameters.ParameterTypeBool,
				parameters.WithDefault(false),
				parameters.WithHelp("Show only long-lived documents"),
			),
			parameters.NewParameterDefinition(
				"owner",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("Filter by owner ID"),
			),
			parameters.NewParameterDefinition(
				"limit",
				parameters.ParameterTypeInteger,
				parameters.WithDefault(0),
				parameters.WithHelp("Maximum number of documents to show (0 = no limit)"),
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

	return &ListDocumentsCommand{
		CommandDescription: cmdDesc,
	}, nil
}

