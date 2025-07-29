package main

import (
	"context"
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/pr-analyzer/pr-analyzer/cmd/analyze"
	"github.com/pr-analyzer/pr-analyzer/cmd/get"
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
		panic(err)
	}
	commitsCobraCmd, err := cli.BuildCobraCommandDualMode(commitsCmd)
	if err != nil {
		panic(err)
	}
	getCmd.AddCommand(commitsCobraCmd)

	contextCmd, err := get.NewContextDualCommand()
	if err != nil {
		panic(err)
	}
	contextCobraCmd, err := cli.BuildCobraCommandDualMode(contextCmd)
	if err != nil {
		panic(err)
	}
	getCmd.AddCommand(contextCobraCmd)

	// Add analyze subcommands
	functionsCmd, err := analyze.NewFunctionsDualCommand()
	if err != nil {
		panic(err)
	}
	functionsCobraCmd, err := cli.BuildCobraCommandDualMode(functionsCmd)
	if err != nil {
		panic(err)
	}
	analyzeCmd.AddCommand(functionsCobraCmd)

	// Add command groups to root
	rootCmd.AddCommand(getCmd)
	rootCmd.AddCommand(analyzeCmd)

	// Execute
	if err := rootCmd.ExecuteContext(context.Background()); err != nil {
		os.Exit(1)
	}
}

