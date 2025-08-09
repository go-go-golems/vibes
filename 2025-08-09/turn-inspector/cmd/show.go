package cmd

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"text/tabwriter"

	"github.com/spf13/cobra"

	"turn-inspector/ent"
	"turn-inspector/ent/block"
	"turn-inspector/ent/turn"
)

var showCmd = &cobra.Command{
	Use:   "show",
	Short: "Show detailed information about turns",
	Long:  `Show detailed information about conversation turns, blocks, and metadata.`,
}

var showTurnCmd = &cobra.Command{
	Use:   "turn",
	Short: "Show detailed turn information",
	Long: `Show detailed information about a specific conversation turn.
		
This command displays complete information about a turn including:
- Turn metadata
- All blocks in order with their content and metadata
- Timestamps and relationships

Examples:
  # Show turn with ID 1
  turn-inspector show turn --id 1`,
	RunE: runShowTurn,
}

var showBlocksCmd = &cobra.Command{
	Use:   "blocks",
	Short: "Show blocks for a turn",
	Long: `Show all blocks for a specific conversation turn in order.
		
This command displays all blocks belonging to a turn with their content,
metadata, and ordering information.

Examples:
  # Show blocks for turn 1
  turn-inspector show blocks --turn-id 1`,
	RunE: runShowBlocks,
}

var (
	turnIDFlag      int
	showTurnIDFlag  int
	jsonOutputFlag  bool
)

func init() {
	rootCmd.AddCommand(showCmd)
	showCmd.AddCommand(showTurnCmd)
	showCmd.AddCommand(showBlocksCmd)

	showTurnCmd.Flags().IntVar(&showTurnIDFlag, "id", 0, "Turn ID to show")
	showTurnCmd.Flags().BoolVar(&jsonOutputFlag, "json", false, "Output in JSON format")
	showTurnCmd.MarkFlagRequired("id")

	showBlocksCmd.Flags().IntVar(&turnIDFlag, "turn-id", 0, "Turn ID to show blocks for")
	showBlocksCmd.MarkFlagRequired("turn-id")
}

func runShowTurn(cmd *cobra.Command, args []string) error {
	ctx := context.Background()
	client := GetClient()
	if client == nil {
		return fmt.Errorf("database client not initialized")
	}

	// Query turn with all related data
	t, err := client.Turn.Query().
		Where(turn.IDEQ(showTurnIDFlag)).
		WithMetadata().
		WithBlocks(func(bq *ent.BlockQuery) {
			bq.Order(ent.Asc(block.FieldOrder)).WithMetadata()
		}).
		Only(ctx)
	if err != nil {
		return fmt.Errorf("failed to query turn: %w", err)
	}

	if jsonOutputFlag {
		// JSON output
		output := map[string]interface{}{
			"id":         t.ID,
			"metadata":   t.Edges.Metadata,
			"blocks":     t.Edges.Blocks,
		}
		jsonData, err := json.MarshalIndent(output, "", "  ")
		if err != nil {
			return fmt.Errorf("failed to marshal JSON: %w", err)
		}
		fmt.Println(string(jsonData))
	} else {
		// Table output
		fmt.Printf("Turn ID: %d\n\n", t.ID)

		// Turn metadata
		if t.Edges.Metadata != nil && len(t.Edges.Metadata) > 0 {
			fmt.Println("Turn Metadata:")
			w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
			fmt.Fprintln(w, "Source\tKey\tValue")
			fmt.Fprintln(w, "------\t---\t-----")
			for _, meta := range t.Edges.Metadata {
				fmt.Fprintf(w, "%s\t%s\t%s\n", meta.Source, meta.Key, meta.Value)
			}
			w.Flush()
			fmt.Println()
		}

		// Blocks
		if t.Edges.Blocks != nil && len(t.Edges.Blocks) > 0 {
			fmt.Println("Blocks:")
			for _, b := range t.Edges.Blocks {
				fmt.Printf("  Block %d (Order: %d, Kind: %s, Role: %s)\n", 
					b.ID, b.Order, b.Kind, b.Role)
				
				if b.Payload != nil {
					if text, ok := b.Payload["text"].(string); ok {
						fmt.Printf("    Text: %s\n", text)
					} else {
						payloadJSON, _ := json.MarshalIndent(b.Payload, "    ", "  ")
						fmt.Printf("    Payload: %s\n", string(payloadJSON))
					}
				}

				if b.Edges.Metadata != nil && len(b.Edges.Metadata) > 0 {
					fmt.Println("    Metadata:")
					for _, meta := range b.Edges.Metadata {
						fmt.Printf("      %s:%s = %s\n", meta.Source, meta.Key, meta.Value)
					}
				}
				fmt.Println()
			}
		}
	}

	return nil
}

func runShowBlocks(cmd *cobra.Command, args []string) error {
	ctx := context.Background()
	client := GetClient()
	if client == nil {
		return fmt.Errorf("database client not initialized")
	}

	// Query blocks for the turn
	blocks, err := client.Block.Query().
		Where(block.HasTurnWith(turn.IDEQ(turnIDFlag))).
		WithMetadata().
		Order(ent.Asc(block.FieldOrder)).
		All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query blocks: %w", err)
	}

	// Output results
	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintln(w, "ID\tOrder\tKind\tRole\tPayload")
	fmt.Fprintln(w, "--\t-----\t----\t----\t-------")

	for _, b := range blocks {
		payloadStr := ""
		if b.Payload != nil {
			if text, ok := b.Payload["text"].(string); ok {
				payloadStr = text
				if len(payloadStr) > 50 {
					payloadStr = payloadStr[:47] + "..."
				}
			} else {
				payloadStr = fmt.Sprintf("%v", b.Payload)
				if len(payloadStr) > 50 {
					payloadStr = payloadStr[:47] + "..."
				}
			}
		}

		fmt.Fprintf(w, "%d\t%d\t%s\t%s\t%s\n", 
			b.ID, b.Order, b.Kind, b.Role, payloadStr)
	}

	w.Flush()
	return nil
}

