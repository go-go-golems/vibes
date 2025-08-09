package cmd

import (
	"context"
	"fmt"

	"github.com/spf13/cobra"

	"turn-inspector/ent/turn"
)

var deleteCmd = &cobra.Command{
	Use:   "delete",
	Short: "Delete conversation turns",
	Long:  `Delete conversation turns and all associated data.`,
}

var deleteTurnCmd = &cobra.Command{
	Use:   "turn",
	Short: "Delete a specific turn",
	Long: `Delete a specific conversation turn and all its associated blocks and metadata.
		
This command will permanently remove:
- The turn record
- All blocks belonging to the turn
- All metadata for the turn and its blocks

Examples:
  # Delete turn with ID 1
  turn-inspector delete turn --id 1
  
  # Delete turn with confirmation
  turn-inspector delete turn --id 1 --confirm`,
	RunE: runDeleteTurn,
}

var deleteAllCmd = &cobra.Command{
	Use:   "all",
	Short: "Delete all turns",
	Long: `Delete all conversation turns and associated data.
		
WARNING: This will permanently remove ALL data from the database.

Examples:
  # Delete all turns (requires confirmation)
  turn-inspector delete all --confirm`,
	RunE: runDeleteAll,
}

var (
	deleteTurnIDFlag int
	confirmFlag      bool
)

func init() {
	rootCmd.AddCommand(deleteCmd)
	deleteCmd.AddCommand(deleteTurnCmd)
	deleteCmd.AddCommand(deleteAllCmd)

	deleteTurnCmd.Flags().IntVar(&deleteTurnIDFlag, "id", 0, "Turn ID to delete")
	deleteTurnCmd.Flags().BoolVar(&confirmFlag, "confirm", false, "Confirm deletion without prompting")
	deleteTurnCmd.MarkFlagRequired("id")

	deleteAllCmd.Flags().BoolVar(&confirmFlag, "confirm", false, "Confirm deletion without prompting")
	deleteAllCmd.MarkFlagRequired("confirm")
}

func runDeleteTurn(cmd *cobra.Command, args []string) error {
	ctx := context.Background()
	client := GetClient()
	if client == nil {
		return fmt.Errorf("database client not initialized")
	}

	// Check if turn exists
	exists, err := client.Turn.Query().Where(turn.IDEQ(deleteTurnIDFlag)).Exist(ctx)
	if err != nil {
		return fmt.Errorf("failed to check if turn exists: %w", err)
	}

	if !exists {
		return fmt.Errorf("turn with ID %d does not exist", deleteTurnIDFlag)
	}

	// Confirm deletion if not already confirmed
	if !confirmFlag {
		fmt.Printf("Are you sure you want to delete turn %d? This action cannot be undone. (y/N): ", deleteTurnIDFlag)
		var response string
		fmt.Scanln(&response)
		if response != "y" && response != "Y" && response != "yes" && response != "Yes" {
			fmt.Println("Deletion cancelled.")
			return nil
		}
	}

	// Delete the turn (cascading delete will handle blocks and metadata)
	err = client.Turn.DeleteOneID(deleteTurnIDFlag).Exec(ctx)
	if err != nil {
		return fmt.Errorf("failed to delete turn: %w", err)
	}

	fmt.Printf("Successfully deleted turn %d\n", deleteTurnIDFlag)
	return nil
}

func runDeleteAll(cmd *cobra.Command, args []string) error {
	ctx := context.Background()
	client := GetClient()
	if client == nil {
		return fmt.Errorf("database client not initialized")
	}

	// Count existing turns
	count, err := client.Turn.Query().Count(ctx)
	if err != nil {
		return fmt.Errorf("failed to count turns: %w", err)
	}

	if count == 0 {
		fmt.Println("No turns to delete.")
		return nil
	}

	fmt.Printf("WARNING: This will delete ALL %d turns and their associated data.\n", count)
	
	if !confirmFlag {
		fmt.Print("Are you absolutely sure? Type 'DELETE ALL' to confirm: ")
		var response string
		fmt.Scanln(&response)
		if response != "DELETE ALL" {
			fmt.Println("Deletion cancelled.")
			return nil
		}
	}

	// Delete all turns
	deleted, err := client.Turn.Delete().Exec(ctx)
	if err != nil {
		return fmt.Errorf("failed to delete turns: %w", err)
	}

	fmt.Printf("Successfully deleted %d turns\n", deleted)
	return nil
}

