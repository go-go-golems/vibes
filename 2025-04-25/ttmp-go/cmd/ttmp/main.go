package main

import (
	"fmt"
	"os"
	"strings"

	"github.com/spf13/cobra"

	"github.com/user/ttmp-go/cmd/ttmp/create"
	"github.com/user/ttmp-go/cmd/ttmp/list"
	"github.com/user/ttmp-go/cmd/ttmp/query"
	"github.com/user/ttmp-go/cmd/ttmp/stats"
	"github.com/user/ttmp-go/cmd/ttmp/validate"
	"github.com/user/ttmp-go/pkg/errors"
)

func main() {
	// Create the root command
	rootCmd := &cobra.Command{
		Use:   "ttmp",
		Short: "TTMP - Tool for managing Thematic Text Metadata with Preamble documents",
		Long: `TTMP is a command-line tool for working with Thematic Text Metadata with Preamble documents.
It allows you to create, validate, query, and list TTMP documents.`,
	}

	// Add subcommands
	rootCmd.AddCommand(
		create.NewCreateCommand(),
		validate.NewValidateCommand(),
		query.NewQueryCommand(),
		list.NewListCommand(),
		stats.NewStatsCommand(),
	)

	// Execute the command
	if err := rootCmd.Execute(); err != nil {
		// Format the error message based on our custom error types
		errorMsg := formatError(err)
		fmt.Fprintln(os.Stderr, errorMsg)
		os.Exit(1)
	}
}

// formatError formats the error message based on our custom error types
func formatError(err error) string {
	if ttmpErr, ok := err.(*errors.TTMPError); ok {
		return ttmpErr.Error()
	}

	if valErrs, ok := err.(*errors.ValidationErrors); ok {
		return valErrs.Error()
	}

	// Handle specific cobra errors
	if strings.Contains(err.Error(), "required flag") {
		return "Error: " + err.Error() + "\nRun 'ttmp help' for usage information."
	}

	// Default error message
	return "Error: " + err.Error()
}