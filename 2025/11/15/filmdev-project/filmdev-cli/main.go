package main

import (
	"context"
	"database/sql"
	"fmt"
	"os"
	"path/filepath"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/help"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	_ "github.com/mattn/go-sqlite3"
	"github.com/spf13/cobra"
)

// QueryCommand implements a glazed command to query film development times
type QueryCommand struct {
	*cmds.CommandDescription
}

// QuerySettings holds the command-line parameters
type QuerySettings struct {
	Film      string `glazed.parameter:"film"`
	Developer string `glazed.parameter:"developer"`
	ISO       int    `glazed.parameter:"iso"`
	DBPath    string `glazed.parameter:"db-path"`
}

// Ensure interface compliance
var _ cmds.GlazeCommand = &QueryCommand{}

// RunIntoGlazeProcessor implements the GlazeCommand interface
func (c *QueryCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Parse settings from command line
	settings := &QuerySettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	// Open database
	db, err := sql.Open("sqlite3", settings.DBPath)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer db.Close()

	// Build query based on filters
	query := `
		SELECT 
			f.name as film,
			d.name as developer,
			dt.dilution,
			dt.iso,
			dt.time_35mm,
			dt.time_120,
			dt.time_sheet,
			dt.temp_c,
			dt.notes
		FROM development_times dt
		JOIN films f ON dt.film_id = f.id
		JOIN developers d ON dt.developer_id = d.id
		WHERE 1=1
	`
	args := []interface{}{}

	if settings.Film != "" {
		query += " AND f.name LIKE ?"
		args = append(args, "%"+settings.Film+"%")
	}

	if settings.Developer != "" {
		query += " AND d.name LIKE ?"
		args = append(args, "%"+settings.Developer+"%")
	}

	if settings.ISO > 0 {
		query += " AND dt.iso = ?"
		args = append(args, settings.ISO)
	}

	query += " ORDER BY f.name, d.name, dt.iso, dt.dilution"

	// Execute query
	rows, err := db.Query(query, args...)
	if err != nil {
		return fmt.Errorf("query failed: %w", err)
	}
	defer rows.Close()

	// Process results
	count := 0
	for rows.Next() {
		var film, developer, dilution, time35mm, time120, timeSheet, tempC, notes string
		var iso int

		err := rows.Scan(&film, &developer, &dilution, &iso, &time35mm, &time120, &timeSheet, &tempC, &notes)
		if err != nil {
			return fmt.Errorf("failed to scan row: %w", err)
		}

		row := types.NewRow(
			types.MRP("film", film),
			types.MRP("developer", developer),
			types.MRP("dilution", dilution),
			types.MRP("iso", iso),
			types.MRP("time_35mm", time35mm),
			types.MRP("time_120", time120),
			types.MRP("time_sheet", timeSheet),
			types.MRP("temp_c", tempC),
			types.MRP("notes", notes),
		)

		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
		count++
	}

	if count == 0 {
		fmt.Fprintf(os.Stderr, "No results found\n")
	}

	return rows.Err()
}

// NewQueryCommand creates a new query command
func NewQueryCommand() (*QueryCommand, error) {
	// Create glazed layer for output formatting options
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	// Create command settings layer
	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	// Get default database path
	homeDir, _ := os.UserHomeDir()
	defaultDBPath := filepath.Join(homeDir, "filmdev-project", "filmdev.db")

	// Define command with parameters
	cmdDesc := cmds.NewCommandDescription(
		"query",
		cmds.WithShort("Query film development times"),
		cmds.WithLong(`
Query film development times from the database.

You can filter by film name, developer name, and ISO rating.
All filters support partial matching (case-insensitive).

Examples:
  filmdev query --film "Tri-X"                    # All Tri-X films
  filmdev query --developer "D-76"                # All D-76 developers
  filmdev query --film "HP5" --developer "ID-11"  # HP5 with ID-11
  filmdev query --film "Tri-X" --iso 400          # Tri-X at ISO 400
  filmdev query --output json                     # Output as JSON
  filmdev query --output csv                      # Output as CSV
  filmdev query --fields film,developer,iso,time_35mm  # Select specific fields
		`),

		// Define command flags
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"film",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("Filter by film name (partial match)"),
				parameters.WithShortFlag("f"),
			),
			parameters.NewParameterDefinition(
				"developer",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("Filter by developer name (partial match)"),
				parameters.WithShortFlag("d"),
			),
			parameters.NewParameterDefinition(
				"iso",
				parameters.ParameterTypeInteger,
				parameters.WithDefault(0),
				parameters.WithHelp("Filter by ISO rating (exact match, 0 = all)"),
				parameters.WithShortFlag("i"),
			),
			parameters.NewParameterDefinition(
				"db-path",
				parameters.ParameterTypeString,
				parameters.WithDefault(defaultDBPath),
				parameters.WithHelp("Path to SQLite database file"),
			),
		),

		// Add glazed and command settings layers
		cmds.WithLayersList(glazedLayer, commandSettingsLayer),
	)

	return &QueryCommand{
		CommandDescription: cmdDesc,
	}, nil
}

// ListFilmsCommand lists all films in the database
type ListFilmsCommand struct {
	*cmds.CommandDescription
}

type ListFilmsSettings struct {
	DBPath string `glazed.parameter:"db-path"`
}

var _ cmds.GlazeCommand = &ListFilmsCommand{}

func (c *ListFilmsCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &ListFilmsSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	db, err := sql.Open("sqlite3", settings.DBPath)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer db.Close()

	rows, err := db.Query("SELECT name FROM films ORDER BY name")
	if err != nil {
		return fmt.Errorf("query failed: %w", err)
	}
	defer rows.Close()

	for rows.Next() {
		var name string
		if err := rows.Scan(&name); err != nil {
			return err
		}

		row := types.NewRow(types.MRP("film", name))
		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return rows.Err()
}

func NewListFilmsCommand() (*ListFilmsCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	homeDir, _ := os.UserHomeDir()
	defaultDBPath := filepath.Join(homeDir, "filmdev-project", "filmdev.db")

	cmdDesc := cmds.NewCommandDescription(
		"list-films",
		cmds.WithShort("List all films in the database"),
		cmds.WithLong("List all films available in the database"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"db-path",
				parameters.ParameterTypeString,
				parameters.WithDefault(defaultDBPath),
				parameters.WithHelp("Path to SQLite database file"),
			),
		),
		cmds.WithLayersList(glazedLayer, commandSettingsLayer),
	)

	return &ListFilmsCommand{
		CommandDescription: cmdDesc,
	}, nil
}

// ListDevelopersCommand lists all developers in the database
type ListDevelopersCommand struct {
	*cmds.CommandDescription
}

type ListDevelopersSettings struct {
	DBPath string `glazed.parameter:"db-path"`
}

var _ cmds.GlazeCommand = &ListDevelopersCommand{}

func (c *ListDevelopersCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &ListDevelopersSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	db, err := sql.Open("sqlite3", settings.DBPath)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer db.Close()

	rows, err := db.Query("SELECT name FROM developers ORDER BY name")
	if err != nil {
		return fmt.Errorf("query failed: %w", err)
	}
	defer rows.Close()

	for rows.Next() {
		var name string
		if err := rows.Scan(&name); err != nil {
			return err
		}

		row := types.NewRow(types.MRP("developer", name))
		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return rows.Err()
}

func NewListDevelopersCommand() (*ListDevelopersCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	homeDir, _ := os.UserHomeDir()
	defaultDBPath := filepath.Join(homeDir, "filmdev-project", "filmdev.db")

	cmdDesc := cmds.NewCommandDescription(
		"list-developers",
		cmds.WithShort("List all developers in the database"),
		cmds.WithLong("List all developers available in the database"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"db-path",
				parameters.ParameterTypeString,
				parameters.WithDefault(defaultDBPath),
				parameters.WithHelp("Path to SQLite database file"),
			),
		),
		cmds.WithLayersList(glazedLayer, commandSettingsLayer),
	)

	return &ListDevelopersCommand{
		CommandDescription: cmdDesc,
	}, nil
}

func main() {
	// Create root command
	rootCmd := &cobra.Command{
		Use:   "filmdev",
		Short: "Film development time query tool",
		Long: `Query film development times from the Massive Dev Chart database.

This tool allows you to search for development times by film, developer, and ISO rating.
Data is sourced from digitaltruth.com and stored in a local SQLite database.`,
	}

	// Create help system
	helpSystem := help.NewHelpSystem()
	_ = helpSystem // Help system for future use

	// Create query command
	queryCmd, err := NewQueryCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating query command: %v\n", err)
		os.Exit(1)
	}

	// Create list-films command
	listFilmsCmd, err := NewListFilmsCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating list-films command: %v\n", err)
		os.Exit(1)
	}

	// Create list-developers command
	listDevelopersCmd, err := NewListDevelopersCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating list-developers command: %v\n", err)
		os.Exit(1)
	}

	// Convert to Cobra commands
	queryCobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(queryCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building query command: %v\n", err)
		os.Exit(1)
	}

	listFilmsCobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(listFilmsCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building list-films command: %v\n", err)
		os.Exit(1)
	}

	listDevelopersCobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(listDevelopersCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building list-developers command: %v\n", err)
		os.Exit(1)
	}

	// Add commands to root
	rootCmd.AddCommand(queryCobraCmd)
	rootCmd.AddCommand(listFilmsCobraCmd)
	rootCmd.AddCommand(listDevelopersCobraCmd)

	// Help is built-in to Cobra

	// Execute
	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}
