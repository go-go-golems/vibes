package main

import (
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds/logging"
	"github.com/go-go-golems/glazed/pkg/help"
	help_cmd "github.com/go-go-golems/glazed/pkg/help/cmd"
	"github.com/mdmeta/mdmeta/pkg/commands"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var version = "dev"

var rootCmd = &cobra.Command{
	Use:     "mdmeta",
	Short:   "Markdown metadata management tool",
	Long: `MDMeta is a comprehensive CLI tool for managing markdown files with YAML metadata preambles.

It provides functionality for initializing, validating, querying, and maintaining
document metadata to support modern documentation workflows, CI/CD integration,
and relationship tracking.

Built with the Glazed framework for consistent output formatting and
professional CLI experience.`,
	Version: version,
	PersistentPreRun: func(cmd *cobra.Command, args []string) {
		err := logging.InitLoggerFromViper()
		cobra.CheckErr(err)
	},
}

func main() {
	// Add logging support
	err := logging.AddLoggingLayerToRootCommand(rootCmd, "mdmeta")
	cobra.CheckErr(err)

	// Bind persistent flags
	err = viper.BindPFlags(rootCmd.PersistentFlags())
	cobra.CheckErr(err)

	// Initialize logger
	err = logging.InitLoggerFromViper()
	cobra.CheckErr(err)

	// Set up help system
	helpSystem := help.NewHelpSystem()
	help_cmd.SetupCobraRootCommand(helpSystem, rootCmd)

	// Add commands
	addCommands()

	// Execute
	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

func addCommands() {
	// Init command
	initCmd, err := commands.NewInitCommand()
	cobra.CheckErr(err)
	command, err := cli.BuildCobraCommand(initCmd)
	cobra.CheckErr(err)
	rootCmd.AddCommand(command)

	// List command
	listCmd, err := commands.NewListCommand()
	cobra.CheckErr(err)
	command, err = cli.BuildCobraCommand(listCmd)
	cobra.CheckErr(err)
	rootCmd.AddCommand(command)

	// Validate command
	validateCmd, err := commands.NewValidateCommand()
	cobra.CheckErr(err)
	command, err = cli.BuildCobraCommand(validateCmd)
	cobra.CheckErr(err)
	rootCmd.AddCommand(command)

	// TODO: Add more commands
	// - update: Update metadata fields
	// - search: Full-text and metadata search
	// - query: Structured metadata queries
	// - relate: Manage document relationships
	// - graph: Visualize relationships
	// - check: Repository consistency checks
	// - migrate: Schema migrations
	// - export: Data export
}

