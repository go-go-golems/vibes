package main

import (
	"fmt"
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/help"
	help_cmd "github.com/go-go-golems/glazed/pkg/help/cmd"
	"github.com/spf13/cobra"
)

func main() {
	// Create root command
	rootCmd := &cobra.Command{
		Use:   "docmgmt",
		Short: "Document Management System CLI",
		Long: `A command-line interface for managing documents in a Cayley graph database.
		
This tool provides commands to add, query, and manage documents, people, topics,
and their relationships in a document management system backed by Cayley.`,
	}

	// Create and register commands
	addDocCmd, err := NewAddDocumentCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating add-document command: %v\n", err)
		os.Exit(1)
	}

	listDocsCmd, err := NewListDocumentsCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating list-documents command: %v\n", err)
		os.Exit(1)
	}

	addPersonCmd, err := NewAddPersonCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating add-person command: %v\n", err)
		os.Exit(1)
	}

	queryCmd, err := NewQueryCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating query command: %v\n", err)
		os.Exit(1)
	}

	// Convert to Cobra commands
	cobraAddDocCmd, err := cli.BuildCobraCommand(addDocCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building add-document command: %v\n", err)
		os.Exit(1)
	}

	cobraListDocsCmd, err := cli.BuildCobraCommand(listDocsCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building list-documents command: %v\n", err)
		os.Exit(1)
	}

	cobraAddPersonCmd, err := cli.BuildCobraCommand(addPersonCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building add-person command: %v\n", err)
		os.Exit(1)
	}

	cobraQueryCmd, err := cli.BuildCobraCommand(queryCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building query command: %v\n", err)
		os.Exit(1)
	}

	// Add commands to root
	rootCmd.AddCommand(cobraAddDocCmd)
	rootCmd.AddCommand(cobraListDocsCmd)
	rootCmd.AddCommand(cobraAddPersonCmd)
	rootCmd.AddCommand(cobraQueryCmd)

	// Setup enhanced help system
	helpSystem := help.NewHelpSystem()
	help_cmd.SetupCobraRootCommand(helpSystem, rootCmd)

	// Execute
	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

