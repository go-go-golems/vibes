package cmd

import (
	"context"
	"database/sql"
	"fmt"
	"os"

	"github.com/spf13/cobra"
	_ "github.com/mattn/go-sqlite3"
	"entgo.io/ent/dialect"
	entsql "entgo.io/ent/dialect/sql"

	"turn-inspector/ent"
)

var (
	client *ent.Client
	db     *sql.DB
)

// rootCmd represents the base command when called without any subcommands
var rootCmd = &cobra.Command{
	Use:   "turn-inspector",
	Short: "A CLI tool for inspecting and managing conversation turns",
	Long: `Turn Inspector is a comprehensive CLI tool for managing conversation turns
with blocks and metadata. It provides commands to create, query, and display
conversation data with support for various block types and rich metadata.`,
}

// Execute adds all child commands to the root command and sets flags appropriately.
func Execute(ctx context.Context) error {
	return rootCmd.ExecuteContext(ctx)
}

func init() {
	cobra.OnInitialize(initConfig)

	// Initialize database connection, except for help
	rootCmd.PersistentPreRunE = func(cmd *cobra.Command, args []string) error {
		if cmd.Name() == "help" {
			return nil
		}
		return initDatabase()
	}

	rootCmd.PersistentPostRunE = func(cmd *cobra.Command, args []string) error {
		if client != nil {
			return client.Close()
		}
		return nil
	}
}

func initConfig() {
	// Configuration initialization if needed
}

func initDatabase() error {
	var err error
	
	// Get database path from environment or use default
	dbPath := os.Getenv("TURN_INSPECTOR_DB")
	if dbPath == "" {
		dbPath = "turns.db"
	}

	// Open database connection
	db, err = sql.Open("sqlite3", fmt.Sprintf("file:%s?_fk=1", dbPath))
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}

	// Create ent client
	drv := entsql.OpenDB(dialect.SQLite, db)
	client = ent.NewClient(ent.Driver(drv))

	// Create schema
	if err := client.Schema.Create(context.Background()); err != nil {
		return fmt.Errorf("failed to create schema: %w", err)
	}

	return nil
}

// GetClient returns the global ent client
func GetClient() *ent.Client {
	return client
}

