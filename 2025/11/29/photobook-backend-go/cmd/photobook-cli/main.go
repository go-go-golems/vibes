package main

import (
	"fmt"
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/help"
	help_cmd "github.com/go-go-golems/glazed/pkg/help/cmd"
	"github.com/spf13/cobra"
	"photobook-backend-go/cmd/photobook-cli/cmds/auth"
	"photobook-backend-go/cmd/photobook-cli/cmds/config"
	"photobook-backend-go/cmd/photobook-cli/cmds/db"
	"photobook-backend-go/cmd/photobook-cli/cmds/pdfjobs"
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

	// Auth commands
	authRegisterCmd, err := auth.NewAuthRegisterCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating auth register command: %v\n", err)
		os.Exit(1)
	}
	cobraAuthRegisterCmd, err := cli.BuildCobraCommand(authRegisterCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building auth register command: %v\n", err)
		os.Exit(1)
	}

	authLoginCmd, err := auth.NewAuthLoginCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating auth login command: %v\n", err)
		os.Exit(1)
	}
	cobraAuthLoginCmd, err := cli.BuildCobraCommand(authLoginCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building auth login command: %v\n", err)
		os.Exit(1)
	}

	authMeCmd, err := auth.NewAuthMeCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating auth me command: %v\n", err)
		os.Exit(1)
	}
	cobraAuthMeCmd, err := cli.BuildCobraCommand(authMeCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building auth me command: %v\n", err)
		os.Exit(1)
	}

	authCmd := &cobra.Command{
		Use:   "auth",
		Short: "Authentication commands",
	}
	authCmd.AddCommand(cobraAuthRegisterCmd)
	authCmd.AddCommand(cobraAuthLoginCmd)
	authCmd.AddCommand(cobraAuthMeCmd)
	rootCmd.AddCommand(authCmd)

	// PDF jobs commands
	pdfCreateCmd, err := pdfjobs.NewCreateJobCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating PDF create command: %v\n", err)
		os.Exit(1)
	}
	cobraPdfCreateCmd, err := cli.BuildCobraCommand(pdfCreateCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building PDF create command: %v\n", err)
		os.Exit(1)
	}

	pdfListCmd, err := pdfjobs.NewListJobsCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating PDF list command: %v\n", err)
		os.Exit(1)
	}
	cobraPdfListCmd, err := cli.BuildCobraCommand(pdfListCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building PDF list command: %v\n", err)
		os.Exit(1)
	}

	pdfProcessCmd, err := pdfjobs.NewProcessJobsCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating PDF process command: %v\n", err)
		os.Exit(1)
	}
	cobraPdfProcessCmd, err := cli.BuildCobraCommand(pdfProcessCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building PDF process command: %v\n", err)
		os.Exit(1)
	}

	pdfCmd := &cobra.Command{
		Use:   "pdf",
		Short: "PDF job commands",
	}
	pdfCmd.AddCommand(cobraPdfCreateCmd)
	pdfCmd.AddCommand(cobraPdfListCmd)
	pdfCmd.AddCommand(cobraPdfProcessCmd)
	rootCmd.AddCommand(pdfCmd)

	// Setup help system
	helpSystem := help.NewHelpSystem()
	help_cmd.SetupCobraRootCommand(helpSystem, rootCmd)

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

