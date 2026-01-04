package cmd

import (
	"context"
	"fmt"

	"github.com/spf13/cobra"
	"github.com/ttmp/ttmp-cli/pkg/metadata"
)

func NewMetaUpdateCommand() (*cobra.Command, error) {
	cmd := &cobra.Command{
		Use:   "update",
		Short: "Update metadata fields",
		Long: `Update metadata fields in documents.

Examples:
  ttmp meta update --doc index.md --field Status --value active
  ttmp meta update --doc design/01-arch.md --field Intent --value long-term`,
		RunE: func(cmd *cobra.Command, args []string) error {
			doc, _ := cmd.Flags().GetString("doc")
			field, _ := cmd.Flags().GetString("field")
			value, _ := cmd.Flags().GetString("value")

			if doc == "" {
				return fmt.Errorf("--doc is required")
			}
			if field == "" {
				return fmt.Errorf("--field is required")
			}
			if value == "" {
				return fmt.Errorf("--value is required")
			}

			return runMetaUpdate(context.Background(), doc, field, value)
		},
	}

	cmd.Flags().String("doc", "", "Document path (required)")
	cmd.MarkFlagRequired("doc")
	cmd.Flags().String("field", "", "Metadata field name (required)")
	cmd.MarkFlagRequired("field")
	cmd.Flags().String("value", "", "New value for the field (required)")
	cmd.MarkFlagRequired("value")

	return cmd, nil
}

func runMetaUpdate(ctx context.Context, doc, field, value string) error {
	if err := metadata.UpdateField(doc, field, value); err != nil {
		return fmt.Errorf("failed to update metadata: %w", err)
	}

	fmt.Printf("✓ Updated %s = %s in %s\n", field, value, doc)
	return nil
}

