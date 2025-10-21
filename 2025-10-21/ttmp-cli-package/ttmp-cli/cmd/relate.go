package cmd

import (
	"context"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
	"github.com/ttmp/ttmp-cli/pkg/metadata"
	"github.com/ttmp/ttmp-cli/pkg/ticket"
)

func NewRelateCommand() (*cobra.Command, error) {
	cmd := &cobra.Command{
		Use:   "relate",
		Short: "Update RelatedFiles metadata",
		Long: `Update the RelatedFiles field in document metadata.

Examples:
  ttmp relate --ticket MEN-3475 --files go/pkg/chat/handler.go,web/src/api/chat.ts
  ttmp relate --ticket MEN-3475 --doc design/01-architecture.md --files pkg/server.go`,
		RunE: func(cmd *cobra.Command, args []string) error {
			ticketID, _ := cmd.Flags().GetString("ticket")
			filesStr, _ := cmd.Flags().GetString("files")
			doc, _ := cmd.Flags().GetString("doc")
			root, _ := cmd.Flags().GetString("root")

			if ticketID == "" {
				return fmt.Errorf("--ticket is required")
			}
			if filesStr == "" {
				return fmt.Errorf("--files is required")
			}

			files := strings.Split(filesStr, ",")
			for i := range files {
				files[i] = strings.TrimSpace(files[i])
			}

			return runRelate(context.Background(), root, ticketID, doc, files)
		},
	}

	cmd.Flags().String("ticket", "", "Ticket identifier (required)")
	cmd.MarkFlagRequired("ticket")
	cmd.Flags().String("files", "", "Comma-separated list of file paths (required)")
	cmd.MarkFlagRequired("files")
	cmd.Flags().String("doc", "", "Specific document to update (default: index.md)")
	cmd.Flags().String("root", "./ttmp", "Root directory for ttmp")

	return cmd, nil
}

func runRelate(ctx context.Context, root, ticketID, docPath string, files []string) error {
	// Find ticket
	tickets, err := ticket.FindTickets(root)
	if err != nil {
		return err
	}

	var ticketDir string
	for _, t := range tickets {
		if t.Ticket == ticketID {
			ticketDir = t.Path
			break
		}
	}

	if ticketDir == "" {
		return fmt.Errorf("ticket not found: %s", ticketID)
	}

	// Default to index.md
	if docPath == "" {
		docPath = "index.md"
	}

	targetPath := filepath.Join(ticketDir, docPath)

	// Update related files
	if err := metadata.SetRelatedFiles(targetPath, files); err != nil {
		return fmt.Errorf("failed to update related files: %w", err)
	}

	fmt.Printf("✓ Updated RelatedFiles in %s\n", targetPath)
	fmt.Printf("  Added %d files\n", len(files))

	return nil
}

