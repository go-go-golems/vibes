package analyze

import (
	"github.com/spf13/cobra"
)

// NewAnalyzeCommand creates the 'analyze' command group
func NewAnalyzeCommand() (*cobra.Command, error) {
	analyzeCmd := &cobra.Command{
		Use:   "analyze",
		Short: "Analyze code structures in GitHub pull requests",
		Long:  "Commands to analyze code structures and functions in GitHub pull requests using tree-sitter",
	}

	// Add subcommands
	functionsCmd, err := NewFunctionsCommand()
	if err != nil {
		return nil, err
	}
	analyzeCmd.AddCommand(functionsCmd)

	return analyzeCmd, nil
}
