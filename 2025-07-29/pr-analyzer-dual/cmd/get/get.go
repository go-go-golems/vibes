package get

import (
	"github.com/spf13/cobra"
)

// NewGetCommand creates the 'get' command group
func NewGetCommand() (*cobra.Command, error) {
	getCmd := &cobra.Command{
		Use:   "get",
		Short: "Get information from GitHub pull requests",
		Long:  "Commands to retrieve various types of information from GitHub pull requests",
	}

	// Add subcommands
	diffCmd, err := NewDiffCommand()
	if err != nil {
		return nil, err
	}
	getCmd.AddCommand(diffCmd)

	contextCmd, err := NewContextCommand()
	if err != nil {
		return nil, err
	}
	getCmd.AddCommand(contextCmd)

	commitsCmd, err := NewCommitsCommand()
	if err != nil {
		return nil, err
	}
	getCmd.AddCommand(commitsCmd)

	fileHistoryCmd, err := NewFileHistoryCommand()
	if err != nil {
		return nil, err
	}
	getCmd.AddCommand(fileHistoryCmd)

	return getCmd, nil
}

