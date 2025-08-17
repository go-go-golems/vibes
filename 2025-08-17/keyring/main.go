package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	"keyring/cmd"
	"keyring/pkg/keyring"
	"keyring/pkg/stores/sqlite"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/spf13/cobra"
)

const (
	defaultDBPath     = "~/.config/keyring/keyring.db"
	defaultConfigPath = "~/.config/keyring/config.yaml"
)

var (
	dbPath   string
	profiles []string
	actor    string
)

func main() {
	rootCmd := &cobra.Command{
		Use:   "keyring",
		Short: "A hierarchical keyring system with profile support",
		Long: `A Go CLI keyring system using the glazed framework with SQLite backend.

Supports hierarchical paths, profiles with fallback, and pluggable backends
with auditing and key state management (active/deprecated/invalidated).

Examples:
  keyring put --path openai/api_key --value sk-xxx
  keyring get --path openai/api_key
  keyring list --prefix openai/
  keyring deprecate --path openai/api_key --message "Use azure_openai instead"
  keyring audit --path openai/api_key --limit 10`,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			// Expand home directory in paths
			dbPath = expandHome(dbPath)
			return nil
		},
	}

	// Global flags
	rootCmd.PersistentFlags().StringVar(&dbPath, "db-path", defaultDBPath, "SQLite database path")
	rootCmd.PersistentFlags().StringSliceVar(&profiles, "profile", []string{"default"}, "Profile search order (comma-separated)")
	rootCmd.PersistentFlags().StringVar(&actor, "actor", "", "Actor name for audit logging")

	// Initialize the keyring system
	initFunc := func() (*keyring.Ring, *sqlite.Store, error) {
		// Ensure database directory exists
		if err := os.MkdirAll(filepath.Dir(dbPath), 0700); err != nil {
			return nil, nil, fmt.Errorf("failed to create database directory: %w", err)
		}

		// Create SQLite store
		store, err := sqlite.NewFromPath(dbPath)
		if err != nil {
			return nil, nil, fmt.Errorf("failed to open database: %w", err)
		}

		// Ensure schema exists
		if err := store.EnsureSchema(context.Background()); err != nil {
			return nil, nil, fmt.Errorf("failed to ensure database schema: %w", err)
		}

		// Create keyring with SQLite backend
		stateStore := sqlite.NewStateStoreAdapter(store)
		ring := keyring.New(
			keyring.WithProfiles(profiles...),
			keyring.WithReaders(store),     // SQLite as reader
			keyring.WithWriter(store),      // SQLite as writer
			keyring.WithStateStore(stateStore), // SQLite as state store
			keyring.WithAuditSink(store),   // SQLite as audit sink
			keyring.WithActor(actor),
			keyring.WithWarningCallback(func(w keyring.Warning) {
				fmt.Fprintf(os.Stderr, "WARNING: %s is %s: %s\n", w.Path.String(), w.Kind, w.Message)
				if w.ReplaceWith != nil {
					fmt.Fprintf(os.Stderr, "  Consider using: %s\n", w.ReplaceWith.String())
				}
			}),
		)

		return ring, store, nil
	}

	// Add commands
	addCommands := func() error {
		ring, _, err := initFunc()
		if err != nil {
			return err
		}

		// Get command
		getCmd, err := cmd.NewGetCommand(ring)
		if err != nil {
			return fmt.Errorf("failed to create get command: %w", err)
		}
		glazedGetCmd, err := cli.BuildCobraCommandFromGlazeCommand(getCmd)
		if err != nil {
			return fmt.Errorf("failed to build get command: %w", err)
		}
		rootCmd.AddCommand(glazedGetCmd)

		// Put command
		putCmd, err := cmd.NewPutCommand(ring)
		if err != nil {
			return fmt.Errorf("failed to create put command: %w", err)
		}
		glazedPutCmd, err := cli.BuildCobraCommandFromGlazeCommand(putCmd)
		if err != nil {
			return fmt.Errorf("failed to build put command: %w", err)
		}
		rootCmd.AddCommand(glazedPutCmd)

		// List command
		listCmd, err := cmd.NewSimpleListCommand(ring)
		if err != nil {
			return fmt.Errorf("failed to create list command: %w", err)
		}
		glazedListCmd, err := cli.BuildCobraCommandFromGlazeCommand(listCmd)
		if err != nil {
			return fmt.Errorf("failed to build list command: %w", err)
		}
		rootCmd.AddCommand(glazedListCmd)

		// Delete command
		deleteCmd, err := cmd.NewSimpleDeleteCommand(ring)
		if err != nil {
			return fmt.Errorf("failed to create delete command: %w", err)
		}
		glazedDeleteCmd, err := cli.BuildCobraCommandFromGlazeCommand(deleteCmd)
		if err != nil {
			return fmt.Errorf("failed to build delete command: %w", err)
		}
		rootCmd.AddCommand(glazedDeleteCmd)

		return nil
	}

	// Add commands to root
	if err := addCommands(); err != nil {
		log.Fatalf("Failed to initialize commands: %v", err)
	}

	// Execute the CLI
	if err := rootCmd.Execute(); err != nil {
		log.Fatalf("Command execution failed: %v", err)
	}
}

// expandHome expands ~ to the user's home directory
func expandHome(path string) string {
	if strings.HasPrefix(path, "~") {
		home, err := os.UserHomeDir()
		if err != nil {
			return path
		}
		return filepath.Join(home, strings.TrimPrefix(path, "~"))
	}
	return path
}

