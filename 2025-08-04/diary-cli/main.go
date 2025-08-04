package main

import (
	"context"
	"fmt"
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/help"
	help_cmd "github.com/go-go-golems/glazed/pkg/help/cmd"
	"github.com/spf13/cobra"

	"diary-cli/pkg/commands"
	"diary-cli/pkg/config"
	"diary-cli/pkg/doc"
)

var rootCmd = &cobra.Command{
	Use:   "diary",
	Short: "A CLI tool for managing diary entries in Obsidian markdown files",
	Long: `Diary is a CLI tool that helps you manage diary entries in Obsidian markdown files
with support for the Tasks plugin. It provides interactive forms for adding entries,
querying existing entries, and managing todos.

Features:
- Multiple entry types: TIL, thoughts, activities, links, todos
- Interactive UI using forms or direct command-line input
- Multiple output formats: default markdown, enhanced markdown, or Obsidian Tasks format
- Dual output support: human-readable or structured data (JSON, CSV, etc.)
- Visual editor integration
- Smart date parsing (today, yesterday, specific dates)`,
}

func main() {
	// Initialize configuration
	cfg, err := config.Load()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error loading config: %v\n", err)
		os.Exit(1)
	}

	// Add help system
	helpSystem := help.NewHelpSystem()
	
	// Load documentation
	if err := doc.AddDocToHelpSystem(helpSystem); err != nil {
		fmt.Fprintf(os.Stderr, "Warning: failed to load documentation: %v\n", err)
	}
	
	helpCmd := help_cmd.NewCobraHelpCommand(helpSystem)
	rootCmd.AddCommand(helpCmd)

	// Add commands
	addCmd := commands.NewAddCommand(cfg)
	todoCmd := commands.NewTodoCommand(cfg)
	listCmd := commands.NewListCommand(cfg)
	searchCmd := commands.NewSearchCommand(cfg)
	showCmd := commands.NewShowCommand(cfg)
	appendCmd := commands.NewAppendCommand(cfg)
	configCmd := commands.NewConfigCommand(cfg)
	initCmd := commands.NewInitCommand(cfg)

	// Convert glaze commands to cobra commands with ShortHelpLayer
	listCobraCmd, err := cli.BuildCobraCommand(listCmd,
		cli.WithParserConfig(cli.CobraParserConfig{
			ShortHelpLayers: []string{layers.DefaultSlug},
			MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
		}),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building list command: %v\n", err)
		os.Exit(1)
	}

	searchCobraCmd, err := cli.BuildCobraCommand(searchCmd,
		cli.WithParserConfig(cli.CobraParserConfig{
			ShortHelpLayers: []string{layers.DefaultSlug},
			MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
		}),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building search command: %v\n", err)
		os.Exit(1)
	}

	showCobraCmd, err := cli.BuildCobraCommand(showCmd,
		cli.WithParserConfig(cli.CobraParserConfig{
			ShortHelpLayers: []string{layers.DefaultSlug},
			MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
		}),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building show command: %v\n", err)
		os.Exit(1)
	}

	// Add all commands to root
	rootCmd.AddCommand(addCmd)
	rootCmd.AddCommand(todoCmd)
	rootCmd.AddCommand(listCobraCmd)
	rootCmd.AddCommand(searchCobraCmd)
	rootCmd.AddCommand(showCobraCmd)
	rootCmd.AddCommand(appendCmd)
	rootCmd.AddCommand(configCmd)
	rootCmd.AddCommand(initCmd)

	// Execute
	if err := rootCmd.ExecuteContext(context.Background()); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

