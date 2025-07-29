package main

import (
	"log"
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/pr-analyzer/pr-analyzer/cmd/analyze"
	"github.com/pr-analyzer/pr-analyzer/cmd/get"
	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "pr-analyzer",
	Short: "A CLI tool for analyzing GitHub pull requests",
	Long: `pr-analyzer is a comprehensive tool for analyzing GitHub pull requests using tree-sitter for code parsing.
It provides detailed insights into PR diffs, commit history, function changes, and code structure analysis.`,
}

func main() {
	// Add command groups
	getCmd, err := get.NewGetCommand()
	if err != nil {
		log.Fatal(err)
	}
	rootCmd.AddCommand(getCmd)

	analyzeCmd, err := analyze.NewAnalyzeCommand()
	if err != nil {
		log.Fatal(err)
	}
	rootCmd.AddCommand(analyzeCmd)

	// Add glazed global flags
	err = cli.AddGlazedProcessorFlagsToCobraCommand(rootCmd)
	if err != nil {
		log.Fatal(err)
	}

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

