package cmd

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/spf13/cobra"

	"turn-inspector/ent/block"
)

var createCmd = &cobra.Command{
	Use:   "create",
	Short: "Create a new conversation turn",
	Long:  `Create a new conversation turn with blocks and metadata.`,
}

var createTurnCmd = &cobra.Command{
	Use:   "turn",
	Short: "Create a new conversation turn",
	Long: `Create a new conversation turn with blocks and metadata.
		
This command allows you to create a complete conversation turn including:
- Turn-level metadata (source, key, value pairs)
- Ordered blocks representing the conversation flow
- Block-level metadata for each block

Examples:
  # Create a simple user-assistant turn
  turn-inspector create turn --blocks '[{"order":0,"kind":"user","role":"user","payload":{"text":"Hello"}},{"order":1,"kind":"llm_text","role":"assistant","payload":{"text":"Hi there!"}}]'
  
  # Create a turn with metadata
  turn-inspector create turn --metadata '[{"source":"session","key":"id","value":"abc123"}]' --blocks '[{"order":0,"kind":"user","role":"user","payload":{"text":"What is the weather?"}}]'`,
	RunE: runCreateTurn,
}

var (
	metadataFlag []string
	blocksFlag   string
)

func init() {
	rootCmd.AddCommand(createCmd)
	createCmd.AddCommand(createTurnCmd)

	createTurnCmd.Flags().StringArrayVar(&metadataFlag, "metadata", []string{}, "Turn metadata as JSON strings")
	createTurnCmd.Flags().StringVar(&blocksFlag, "blocks", "", "Blocks as JSON array")
	createTurnCmd.MarkFlagRequired("blocks")
}

func runCreateTurn(cmd *cobra.Command, args []string) error {
	ctx := context.Background()
	client := GetClient()
	if client == nil {
		return fmt.Errorf("database client not initialized")
	}

	// Parse metadata
	var metadata []map[string]string
	for _, metaStr := range metadataFlag {
		var meta map[string]string
		if err := json.Unmarshal([]byte(metaStr), &meta); err != nil {
			return fmt.Errorf("failed to parse metadata: %w", err)
		}
		metadata = append(metadata, meta)
	}

	// Parse blocks
	var blocks []map[string]interface{}
	if err := json.Unmarshal([]byte(blocksFlag), &blocks); err != nil {
		return fmt.Errorf("failed to parse blocks: %w", err)
	}

	// Create turn
	turnCreate := client.Turn.Create()
	turn, err := turnCreate.Save(ctx)
	if err != nil {
		return fmt.Errorf("failed to create turn: %w", err)
	}

	// Add metadata
	for _, meta := range metadata {
		_, err := client.TurnMetadata.Create().
			SetTurn(turn).
			SetSource(meta["source"]).
			SetKey(meta["key"]).
			SetValue(meta["value"]).
			Save(ctx)
		if err != nil {
			return fmt.Errorf("failed to create turn metadata: %w", err)
		}
	}

	// Add blocks
	for _, blockData := range blocks {
		order, _ := blockData["order"].(float64)
		kind, _ := blockData["kind"].(string)
		role, _ := blockData["role"].(string)
		payload, _ := blockData["payload"].(map[string]interface{})

		blockCreate := client.Block.Create().
			SetTurn(turn).
			SetOrder(int(order)).
			SetKind(block.Kind(kind))

		if role != "" {
			blockCreate = blockCreate.SetRole(role)
		}

		if payload != nil {
			blockCreate = blockCreate.SetPayload(payload)
		}

		createdBlock, err := blockCreate.Save(ctx)
		if err != nil {
			return fmt.Errorf("failed to create block: %w", err)
		}

		// Add block metadata if present
		if blockMeta, ok := blockData["metadata"].([]interface{}); ok {
			for _, metaItem := range blockMeta {
				if meta, ok := metaItem.(map[string]interface{}); ok {
					source, _ := meta["source"].(string)
					key, _ := meta["key"].(string)
					value, _ := meta["value"].(string)

					_, err := client.BlockMetadata.Create().
						SetBlock(createdBlock).
						SetSource(source).
						SetKey(key).
						SetValue(value).
						Save(ctx)
					if err != nil {
						return fmt.Errorf("failed to create block metadata: %w", err)
					}
				}
			}
		}
	}

	// Output result
	fmt.Printf("Created turn %d with %d metadata entries and %d blocks\n", 
		turn.ID, len(metadata), len(blocks))

	return nil
}

