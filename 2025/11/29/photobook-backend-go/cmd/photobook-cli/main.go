package main

import (
	"fmt"
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/help"
	help_cmd "github.com/go-go-golems/glazed/pkg/help/cmd"
	"github.com/spf13/cobra"
	"photobook-backend-go/cmd/photobook-cli/cmds/config"
	"photobook-backend-go/cmd/photobook-cli/cmds/db"
	"photobook-backend-go/cmd/photobook-cli/cmds/storage"
)

func main() {
	rootCmd := &cobra.Command{
		Use:   "photobook-cli",
		Short: "CLI tools for photobook backend",
		Long:  "Command-line tools to exercise and validate photobook backend functionality",
	}

	// Config commands
	configValidateCmd, err := config.NewConfigValidateCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating config validate command: %v\n", err)
		os.Exit(1)
	}
	cobraConfigValidateCmd, err := cli.BuildCobraCommand(configValidateCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building config validate command: %v\n", err)
		os.Exit(1)
	}

	configCmd := &cobra.Command{
		Use:   "config",
		Short: "Configuration commands",
	}
	configCmd.AddCommand(cobraConfigValidateCmd)
	rootCmd.AddCommand(configCmd)

	// DB commands
	dbStatusCmd, err := db.NewDBStatusCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating db status command: %v\n", err)
		os.Exit(1)
	}
	cobraDBStatusCmd, err := cli.BuildCobraCommand(dbStatusCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building db status command: %v\n", err)
		os.Exit(1)
	}

	dbCmd := &cobra.Command{
		Use:   "db",
		Short: "Database commands",
	}
	dbCmd.AddCommand(cobraDBStatusCmd)
	rootCmd.AddCommand(dbCmd)

	// Storage commands
	storagePutCmd, err := storage.NewStoragePutCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating storage put command: %v\n", err)
		os.Exit(1)
	}
	cobraStoragePutCmd, err := cli.BuildCobraCommand(storagePutCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building storage put command: %v\n", err)
		os.Exit(1)
	}

	storageListCmd, err := storage.NewStorageListCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating storage list command: %v\n", err)
		os.Exit(1)
	}
	cobraStorageListCmd, err := cli.BuildCobraCommand(storageListCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building storage list command: %v\n", err)
		os.Exit(1)
	}

	storageCmd := &cobra.Command{
		Use:   "storage",
		Short: "Storage commands",
	}
	storageCmd.AddCommand(cobraStoragePutCmd)
	storageCmd.AddCommand(cobraStorageListCmd)
	rootCmd.AddCommand(storageCmd)

	// Setup help system
	helpSystem := help.NewHelpSystem()
	help_cmd.SetupCobraRootCommand(helpSystem, rootCmd)

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

