package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

var (
	repoPath     string
	configFile   string
	outputFormat string
)

var rootCmd = &cobra.Command{
	Use:   "pr-analyzer",
	Short: "Analyze GitHub pull requests for language and cross-subsystem statistics",
	Long: `A CLI tool that analyzes GitHub pull requests to compute:
- Percentage of language-related changes (by lines of code)
- Cross-subsystem statistics (commits touching multiple subsystems)
- Custom categorization using glob patterns

Examples:
  pr-analyzer analyze --pr-branch feature/new-api
  pr-analyzer analyze --merge-commit abc123def --output json
  pr-analyzer analyze --categories "frontend:frontend/**,backend:backend/**"`,
}

func Execute() error {
	return rootCmd.Execute()
}

func init() {
	rootCmd.PersistentFlags().StringVar(&repoPath, "repo-path", ".", "Path to git repository")
	rootCmd.PersistentFlags().StringVar(&configFile, "config", "", "Path to config file")
	rootCmd.PersistentFlags().StringVar(&outputFormat, "output", "table", "Output format: table, json, yaml")
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

