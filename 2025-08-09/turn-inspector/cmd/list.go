package cmd

import (
	"context"
	"fmt"
	"os"
	"text/tabwriter"

	"github.com/spf13/cobra"

	"turn-inspector/ent"
	"turn-inspector/ent/turn"
)

var listCmd = &cobra.Command{
	Use:   "list",
	Short: "List conversation turns",
	Long:  `List conversation turns with optional filtering and pagination.`,
}

var listTurnsCmd = &cobra.Command{
	Use:   "turns",
	Short: "List conversation turns",
	Long: `List conversation turns with optional filtering and pagination.
		
This command displays all conversation turns in the database with summary information
including turn ID, creation time, number of blocks, and metadata count.

Examples:
  # List all turns
  turn-inspector list turns
  
  # List turns with limit
  turn-inspector list turns --limit 10`,
	RunE: runListTurns,
}

var (
	limitFlag  int
	offsetFlag int
)

func init() {
	rootCmd.AddCommand(listCmd)
	listCmd.AddCommand(listTurnsCmd)

	listTurnsCmd.Flags().IntVar(&limitFlag, "limit", 100, "Maximum number of turns to return")
	listTurnsCmd.Flags().IntVar(&offsetFlag, "offset", 0, "Number of turns to skip")
}

func runListTurns(cmd *cobra.Command, args []string) error {
	ctx := context.Background()
	client := GetClient()
	if client == nil {
		return fmt.Errorf("database client not initialized")
	}

	// Query turns with metadata and blocks
	turns, err := client.Turn.Query().
		WithMetadata().
		WithBlocks().
		Limit(limitFlag).
		Offset(offsetFlag).
		Order(ent.Desc(turn.FieldID)).
		All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query turns: %w", err)
	}

	// Output results in table format
	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintln(w, "ID\tMetadata Count\tBlocks Count")
	fmt.Fprintln(w, "--\t--------------\t------------")

	for _, t := range turns {
		metadataCount := 0
		if t.Edges.Metadata != nil {
			metadataCount = len(t.Edges.Metadata)
		}

		blocksCount := 0
		if t.Edges.Blocks != nil {
			blocksCount = len(t.Edges.Blocks)
		}

		fmt.Fprintf(w, "%d\t%d\t%d\n", t.ID, metadataCount, blocksCount)
	}

	w.Flush()
	return nil
}

