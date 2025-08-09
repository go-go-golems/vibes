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
	Short: "Create resources",
	Long:  `Create runs and conversation turns with blocks and metadata.`,
}

var createRunCmd = &cobra.Command{
	Use:   "run",
	Short: "Create a new run",
	Long: `Create a new run with optional name and metadata.
		
Examples:
  # Create a run with a name
  turn-inspector create run --name "Experiment A"

  # Create a run with metadata
  turn-inspector create run --name "Session 1" \
    --metadata '{"source":"session","key":"id","value":"abc123"}'`,
	RunE: runCreateRun,
}

var createTurnCmd = &cobra.Command{
	Use:   "turn",
	Short: "Create a new conversation turn",
	Long: `Create a new conversation turn within a run, with blocks and metadata.
		
This command allows you to create a complete conversation turn including:
- Turn-level metadata (source, key, value pairs)
- Ordered blocks representing the conversation flow
- Block-level metadata for each block

Examples:
  # Create a simple user-assistant turn
  turn-inspector create turn --run-id 1 --blocks '[{"order":0,"kind":"user","role":"user","payload":{"text":"Hello"}},{"order":1,"kind":"llm_text","role":"assistant","payload":{"text":"Hi there!"}}]'
  
  # Create a turn with metadata
  turn-inspector create turn --run-id 1 --metadata '[{"source":"session","key":"id","value":"abc123"}]' --blocks '[{"order":0,"kind":"user","role":"user","payload":{"text":"What is the weather?"}}]'`,
	RunE: runCreateTurn,
}

var (
	metadataFlag []string
	blocksFlag   string
	runIDFlag    int
	runNameFlag  string
)

func init() {
	rootCmd.AddCommand(createCmd)
	createCmd.AddCommand(createRunCmd)
	createCmd.AddCommand(createTurnCmd)

	createRunCmd.Flags().StringVar(&runNameFlag, "name", "", "Optional name for the run")
	createRunCmd.Flags().StringArrayVar(&metadataFlag, "metadata", []string{}, "Run metadata as JSON strings")

	createTurnCmd.Flags().IntVar(&runIDFlag, "run-id", 0, "Run ID the turn belongs to")
	createTurnCmd.MarkFlagRequired("run-id")
	createTurnCmd.Flags().StringArrayVar(&metadataFlag, "metadata", []string{}, "Turn metadata as JSON strings")
	createTurnCmd.Flags().StringVar(&blocksFlag, "blocks", "", "Blocks as JSON array")
	createTurnCmd.MarkFlagRequired("blocks")
}

func runCreateRun(cmd *cobra.Command, args []string) error {
	ctx := context.Background()
	client := GetClient()
	if client == nil {
		return fmt.Errorf("database client not initialized")
	}

	// Create run
	runCreate := client.Run.Create()
	if runNameFlag != "" {
		runCreate = runCreate.SetName(runNameFlag)
	}
	r, err := runCreate.Save(ctx)
	if err != nil {
		return fmt.Errorf("failed to create run: %w", err)
	}

	// Parse metadata
	var md []map[string]string
	for _, metaStr := range metadataFlag {
		var meta map[string]string
		if err := json.Unmarshal([]byte(metaStr), &meta); err != nil {
			return fmt.Errorf("failed to parse metadata: %w", err)
		}
		md = append(md, meta)
	}
	for _, meta := range md {
		_, err := client.RunMetadata.Create().
			SetRun(r).
			SetSource(meta["source"]).
			SetKey(meta["key"]).
			SetValue(meta["value"]).
			Save(ctx)
		if err != nil {
			return fmt.Errorf("failed to create run metadata: %w", err)
		}
	}

	fmt.Printf("Created run %d\n", r.ID)
	return nil
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

	// Create turn in run
	turnCreate := client.Turn.Create().
		SetRunID(runIDFlag)
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
	fmt.Printf("Created turn %d in run %d with %d metadata entries and %d blocks\n",
		turn.ID, runIDFlag, len(metadata), len(blocks))

	return nil
}

