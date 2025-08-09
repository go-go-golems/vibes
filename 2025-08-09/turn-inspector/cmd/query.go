package cmd

import (
	"context"
	"fmt"
	"os"
	"text/tabwriter"

	"github.com/spf13/cobra"

	"turn-inspector/ent"
	"turn-inspector/ent/block"
	"turn-inspector/ent/run"
	"turn-inspector/ent/turn"
	"turn-inspector/ent/turnmetadata"
)

var queryCmd = &cobra.Command{
	Use:   "query",
	Short: "Query and search conversation turns",
	Long:  `Query and search conversation turns using various filters and criteria.`,
}

var queryTurnsCmd = &cobra.Command{
	Use:   "turns",
	Short: "Query turns by metadata or content",
	Long: `Query turns by metadata or content with flexible filtering options.
		
This command allows you to search for turns based on:
- Metadata key-value pairs
- Block content and types
- Turn properties

Examples:
  # Find turns with specific metadata
  turn-inspector query turns --metadata-key session --metadata-value abc123
  
  # Find turns containing specific text
  turn-inspector query turns --text "weather"
  
  # Find turns with specific block types
  turn-inspector query turns --block-kind tool_call`,
	RunE: runQueryTurns,
}

var (
	metadataKeyFlag   string
	metadataValueFlag string
	textSearchFlag    string
	blockKindFlag     string
	runFilterID       int
)

func init() {
	rootCmd.AddCommand(queryCmd)
	queryCmd.AddCommand(queryTurnsCmd)

	queryTurnsCmd.Flags().StringVar(&metadataKeyFlag, "metadata-key", "", "Search by metadata key")
	queryTurnsCmd.Flags().StringVar(&metadataValueFlag, "metadata-value", "", "Search by metadata value")
	queryTurnsCmd.Flags().StringVar(&textSearchFlag, "text", "", "Search for text in block payloads")
	queryTurnsCmd.Flags().StringVar(&blockKindFlag, "block-kind", "", "Search by block kind")
	queryTurnsCmd.Flags().IntVar(&runFilterID, "run-id", 0, "Filter by run ID")
}

func runQueryTurns(cmd *cobra.Command, args []string) error {
	ctx := context.Background()
	client := GetClient()
	if client == nil {
		return fmt.Errorf("database client not initialized")
	}

	// Build query based on flags
	query := client.Turn.Query().WithMetadata().WithBlocks()

	if runFilterID != 0 {
		query = query.Where(turn.HasRunWith(run.IDEQ(runFilterID)))
	}

	// Filter by metadata
	if metadataKeyFlag != "" {
		if metadataValueFlag != "" {
			// Both key and value specified
			query = query.Where(turn.HasMetadataWith(
				turnmetadata.And(
					turnmetadata.KeyEQ(metadataKeyFlag),
					turnmetadata.ValueEQ(metadataValueFlag),
				),
			))
		} else {
			// Only key specified
			query = query.Where(turn.HasMetadataWith(
				turnmetadata.KeyEQ(metadataKeyFlag),
			))
		}
	} else if metadataValueFlag != "" {
		// Only value specified
		query = query.Where(turn.HasMetadataWith(
			turnmetadata.ValueEQ(metadataValueFlag),
		))
	}

	// Filter by block kind
	if blockKindFlag != "" {
		query = query.Where(turn.HasBlocksWith(
			block.KindEQ(block.Kind(blockKindFlag)),
		))
	}

	// Execute query
	turns, err := query.All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query turns: %w", err)
	}

	// Filter by text search if specified (post-query filtering)
	if textSearchFlag != "" {
		var filteredTurns []*ent.Turn
		for _, t := range turns {
			// Load blocks if not already loaded
			if t.Edges.Blocks == nil {
				blocks, err := t.QueryBlocks().All(ctx)
				if err != nil {
					continue
				}
				t.Edges.Blocks = blocks
			}

			// Check if any block contains the text
			found := false
			for _, b := range t.Edges.Blocks {
				if b.Payload != nil {
					if text, ok := b.Payload["text"].(string); ok {
						if contains(text, textSearchFlag) {
							found = true
							break
						}
					}
				}
			}
			if found {
				filteredTurns = append(filteredTurns, t)
			}
		}
		turns = filteredTurns
	}

	// Output results
	if len(turns) == 0 {
		fmt.Println("No turns found matching the criteria.")
		return nil
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintln(w, "ID\tRun ID\tMetadata Count\tBlocks Count\tMatching Criteria")
	fmt.Fprintln(w, "--\t------\t--------------\t------------\t-----------------")

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

		criteria := buildCriteriaString()
		fmt.Fprintf(w, "%d\t%d\t%d\t%d\t%s\n", t.ID, runID, metadataCount, blocksCount, criteria)
	}

	w.Flush()
	return nil
}

func contains(text, search string) bool {
	// Simple case-insensitive contains check
	textLower := ""
	searchLower := ""
	for _, r := range text {
		if r >= 'A' && r <= 'Z' {
			textLower += string(r + 32)
		} else {
			textLower += string(r)
		}
	}
	for _, r := range search {
		if r >= 'A' && r <= 'Z' {
			searchLower += string(r + 32)
		} else {
			searchLower += string(r)
		}
	}
	
	// Simple substring search
	if len(searchLower) > len(textLower) {
		return false
	}
	for i := 0; i <= len(textLower)-len(searchLower); i++ {
		if textLower[i:i+len(searchLower)] == searchLower {
			return true
		}
	}
	return false
}

func buildCriteriaString() string {
	criteria := ""
	if metadataKeyFlag != "" {
		criteria += fmt.Sprintf("metadata-key:%s", metadataKeyFlag)
	}
	if metadataValueFlag != "" {
		if criteria != "" {
			criteria += ", "
		}
		criteria += fmt.Sprintf("metadata-value:%s", metadataValueFlag)
	}
	if textSearchFlag != "" {
		if criteria != "" {
			criteria += ", "
		}
		criteria += fmt.Sprintf("text:%s", textSearchFlag)
	}
	if blockKindFlag != "" {
		if criteria != "" {
			criteria += ", "
		}
		criteria += fmt.Sprintf("block-kind:%s", blockKindFlag)
	}
	if runFilterID != 0 {
		if criteria != "" {
			criteria += ", "
		}
		criteria += fmt.Sprintf("run-id:%d", runFilterID)
	}
	return criteria
}

