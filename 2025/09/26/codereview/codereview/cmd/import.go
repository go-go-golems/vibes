package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

func newImportCommand() *cobra.Command {
	var format string

	cmd := &cobra.Command{
		Use:   "import <file>",
		Short: "Import a review from YAML or JSON",
		Long:  "Import a code review from YAML or JSON format",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return runImport(args[0], format)
		},
	}

	cmd.Flags().StringVar(&format, "format", "auto", "Import format (yaml, json, auto)")

	return cmd
}

func runImport(file, format string) error {
	// TODO: Implement import functionality
	fmt.Printf("🚧 Import functionality not yet implemented\n")
	fmt.Printf("   File:   %s\n", file)
	fmt.Printf("   Format: %s\n", format)
	fmt.Printf("   This will be implemented later\n")

	return nil
}
