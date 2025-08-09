package cmd

import (
	"context"
	"fmt"
	"os"
	"text/tabwriter"

	"github.com/spf13/cobra"

	"turn-inspector/ent"
	"turn-inspector/ent/turn"
	"turn-inspector/ent/run"
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
  turn-inspector list turns --limit 10

  # List turns for a specific run
  turn-inspector list turns --run-id 1`,
	RunE: runListTurns,
}

var (
	limitFlag  int
	offsetFlag int
	listRunID  int
)

func init() {
	rootCmd.AddCommand(listCmd)
	listCmd.AddCommand(listTurnsCmd)

	listTurnsCmd.Flags().IntVar(&limitFlag, "limit", 100, "Maximum number of turns to return")
	listTurnsCmd.Flags().IntVar(&offsetFlag, "offset", 0, "Number of turns to skip")
	listTurnsCmd.Flags().IntVar(&listRunID, "run-id", 0, "Filter turns by run ID")
}

func runListTurns(cmd *cobra.Command, args []string) error {
	ctx := context.Background()
	client := GetClient()
	if client == nil {
		return fmt.Errorf("database client not initialized")
	}

	// Query turns with metadata and blocks
	q := client.Turn.Query().
		WithMetadata().
		WithBlocks().
		Limit(limitFlag).
		Offset(offsetFlag).
		Order(ent.Desc(turn.FieldID))
	if listRunID != 0 {
		q = q.Where(turn.HasRunWith(run.IDEQ(listRunID)))
	}
	turns, err := q.All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query turns: %w", err)
	}

	// Output results in table format
	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintln(w, "ID\tRun ID\tMetadata Count\tBlocks Count")
	fmt.Fprintln(w, "--\t------\t--------------\t------------")

	for _, t := range turns {
		runID := 0
		if t.Edges.Run != nil {
			runID = t.Edges.Run.ID
		}
		metadataCount := 0
		if t.Edges.Metadata != nil {
			metadataCount = len(t.Edges.Metadata)
		}

		blocksCount := 0
		if t.Edges.Blocks != nil {
			blocksCount = len(t.Edges.Blocks)
		}

		fmt.Fprintf(w, "%d\t%d\t%d\t%d\n", t.ID, runID, metadataCount, blocksCount)
	}

	w.Flush()
	return nil
}

