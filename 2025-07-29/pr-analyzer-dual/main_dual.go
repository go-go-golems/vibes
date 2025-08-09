package main

import (
	"context"
	"fmt"
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/help"
	help_cmd "github.com/go-go-golems/glazed/pkg/help/cmd"
	"github.com/pr-analyzer/pr-analyzer/cmd/analyze"
	"github.com/pr-analyzer/pr-analyzer/cmd/get"
	"github.com/pr-analyzer/pr-analyzer/pkg/doc"
	"github.com/spf13/cobra"
)

func main() {
	var rootCmd = &cobra.Command{
		Use:   "pr-analyzer",
		Short: "Analyze GitHub pull requests with tree-sitter and glazed",
		Long: `A comprehensive CLI tool for analyzing GitHub pull requests using tree-sitter for Go code parsing
and glazed for structured output. Supports both human-readable reports and structured data formats.`,
	}

	// Create get command group
	getCmd := &cobra.Command{
		Use:   "get",
		Short: "Get information from pull requests",
		Long:  "Commands to retrieve various types of information from GitHub pull requests",
	}

	// Create analyze command group
	analyzeCmd := &cobra.Command{
		Use:   "analyze",
		Short: "Analyze pull request code changes",
		Long:  "Commands to analyze code changes in pull requests using tree-sitter",
	}

	// Add get subcommands
	commitsCmd, err := get.NewCommitsDualCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating commits command: %v\n", err)
		os.Exit(1)
	}
	commitsCobraCmd, err := cli.BuildCobraCommand(commitsCmd,
		cli.WithDualMode(true),
		cli.WithGlazeToggleFlag("with-glaze-output"),
		cli.WithParserConfig(cli.CobraParserConfig{
			ShortHelpLayers: []string{layers.DefaultSlug},
			MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
		}),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building commits command: %v\n", err)
		os.Exit(1)
	}
	getCmd.AddCommand(commitsCobraCmd)

	contextCmd, err := get.NewContextDualCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating context command: %v\n", err)
		os.Exit(1)
	}
	contextCobraCmd, err := cli.BuildCobraCommand(contextCmd,
		cli.WithDualMode(true),
		cli.WithGlazeToggleFlag("with-glaze-output"),
		cli.WithParserConfig(cli.CobraParserConfig{
			ShortHelpLayers: []string{layers.DefaultSlug},
			MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
		}),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building context command: %v\n", err)
		os.Exit(1)
	}
	getCmd.AddCommand(contextCobraCmd)

	diffCmd, err := get.NewDiffDualCommand()
	if err != nil {
		panic(err)
	}
	diffCobraCmd, err := cli.BuildCobraCommandDualMode(diffCmd)
	if err != nil {
		panic(err)
	}
	getCmd.AddCommand(diffCobraCmd)

	fileHistoryCmd, err := get.NewFileHistoryDualCommand()
	if err != nil {
		panic(err)
	}
	fileHistoryCobraCmd, err := cli.BuildCobraCommandDualMode(fileHistoryCmd)
	if err != nil {
		panic(err)
	}
	getCmd.AddCommand(fileHistoryCobraCmd)

	// Add analyze subcommands
	functionsCmd, err := analyze.NewFunctionsDualCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating functions command: %v\n", err)
		os.Exit(1)
	}
	functionsCobraCmd, err := cli.BuildCobraCommand(functionsCmd,
		cli.WithDualMode(true),
		cli.WithGlazeToggleFlag("with-glaze-output"),
		cli.WithParserConfig(cli.CobraParserConfig{
			ShortHelpLayers: []string{layers.DefaultSlug},
			MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
		}),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building functions command: %v\n", err)
		os.Exit(1)
	}
	analyzeCmd.AddCommand(functionsCobraCmd)

	functionHistoryCmd, err := analyze.NewFunctionHistoryDualCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating function-history command: %v\n", err)
		os.Exit(1)
	}
	functionHistoryCobraCmd, err := cli.BuildCobraCommand(functionHistoryCmd,
		cli.WithDualMode(true),
		cli.WithGlazeToggleFlag("with-glaze-output"),
		cli.WithParserConfig(cli.CobraParserConfig{
			ShortHelpLayers: []string{layers.DefaultSlug},
			MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
		}),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building function-history command: %v\n", err)
		os.Exit(1)
	}
	analyzeCmd.AddCommand(functionHistoryCobraCmd)

	// Add command groups to root
	rootCmd.AddCommand(getCmd)
	rootCmd.AddCommand(analyzeCmd)

	// Setup enhanced help system for the complete application
	helpSystem := help.NewHelpSystem()
	
	// Add documentation to help system
	err = doc.AddDocToHelpSystem(helpSystem)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error loading documentation: %v\n", err)
		// Don't exit, just continue without docs
	}
	
	help_cmd.SetupCobraRootCommand(helpSystem, rootCmd)

	// Execute
	if err := rootCmd.ExecuteContext(context.Background()); err != nil {
		os.Exit(1)
	}
}
