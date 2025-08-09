package cmd

import (
	"context"
	"fmt"
	"os"
	"text/tabwriter"

	"github.com/spf13/cobra"

	"turn-inspector/ent/block"
)

var statsCmd = &cobra.Command{
	Use:   "stats",
	Short: "Show database statistics",
	Long: `Show comprehensive statistics about the conversation turns database.
		
This command displays:
- Total number of turns
- Total number of blocks
- Block type distribution
- Metadata statistics
- Database size information

Examples:
  # Show basic statistics
  turn-inspector stats
  
  # Show detailed statistics
  turn-inspector stats --detailed`,
	RunE: runStats,
}

var detailedFlag bool

func init() {
	rootCmd.AddCommand(statsCmd)
	statsCmd.Flags().BoolVar(&detailedFlag, "detailed", false, "Show detailed statistics")
}

func runStats(cmd *cobra.Command, args []string) error {
	ctx := context.Background()
	client := GetClient()
	if client == nil {
		return fmt.Errorf("database client not initialized")
	}

	// Basic counts
	turnCount, err := client.Turn.Query().Count(ctx)
	if err != nil {
		return fmt.Errorf("failed to count turns: %w", err)
	}

	blockCount, err := client.Block.Query().Count(ctx)
	if err != nil {
		return fmt.Errorf("failed to count blocks: %w", err)
	}

	turnMetadataCount, err := client.TurnMetadata.Query().Count(ctx)
	if err != nil {
		return fmt.Errorf("failed to count turn metadata: %w", err)
	}

	blockMetadataCount, err := client.BlockMetadata.Query().Count(ctx)
	if err != nil {
		return fmt.Errorf("failed to count block metadata: %w", err)
	}

	// Display basic statistics
	fmt.Println("Database Statistics")
	fmt.Println("==================")
	fmt.Printf("Total Turns: %d\n", turnCount)
	fmt.Printf("Total Blocks: %d\n", blockCount)
	fmt.Printf("Turn Metadata Entries: %d\n", turnMetadataCount)
	fmt.Printf("Block Metadata Entries: %d\n", blockMetadataCount)

	if turnCount > 0 {
		avgBlocksPerTurn := float64(blockCount) / float64(turnCount)
		fmt.Printf("Average Blocks per Turn: %.2f\n", avgBlocksPerTurn)
	}

	if !detailedFlag {
		return nil
	}

	fmt.Println("\nDetailed Statistics")
	fmt.Println("==================")

	// Block kind distribution
	blockKinds := []block.Kind{
		block.KindLlmText,
		block.KindToolCall,
		block.KindToolUse,
		block.KindSystem,
		block.KindUser,
		block.KindOther,
	}

	fmt.Println("\nBlock Type Distribution:")
	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintln(w, "Kind\tCount\tPercentage")
	fmt.Fprintln(w, "----\t-----\t----------")

	for _, kind := range blockKinds {
		count, err := client.Block.Query().Where(block.KindEQ(kind)).Count(ctx)
		if err != nil {
			continue
		}
		
		percentage := 0.0
		if blockCount > 0 {
			percentage = float64(count) / float64(blockCount) * 100
		}
		
		fmt.Fprintf(w, "%s\t%d\t%.1f%%\n", kind, count, percentage)
	}
	w.Flush()

	// Metadata source distribution
	fmt.Println("\nTurn Metadata Sources:")
	sources, err := client.TurnMetadata.Query().
		Select("source").
		GroupBy("source").
		Strings(ctx)
	if err == nil && len(sources) > 0 {
		w2 := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
		fmt.Fprintln(w2, "Source\tPresent")
		fmt.Fprintln(w2, "------\t-------")
		for _, source := range sources {
			fmt.Fprintf(w2, "%s\tYes\n", source)
		}
		w2.Flush()
	}

	return nil
}

