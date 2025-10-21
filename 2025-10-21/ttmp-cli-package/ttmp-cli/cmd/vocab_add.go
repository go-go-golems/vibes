package cmd

import (
	"context"
	"fmt"

	"github.com/spf13/cobra"
	"github.com/ttmp/ttmp-cli/pkg/vocabulary"
)

func NewVocabAddCommand() (*cobra.Command, error) {
	cmd := &cobra.Command{
		Use:   "add <category>",
		Short: "Add a vocabulary entry",
		Long: `Add a new entry to the vocabulary.

Examples:
  ttmp vocab add topics --slug frontend --description "Frontend development"
  ttmp vocab add docTypes --slug spike --description "Exploratory spike"`,
		Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			category := args[0]
			slug, _ := cmd.Flags().GetString("slug")
			description, _ := cmd.Flags().GetString("description")
			vocabFile, _ := cmd.Flags().GetString("vocab-file")

			if slug == "" {
				return fmt.Errorf("--slug is required")
			}
			if description == "" {
				return fmt.Errorf("--description is required")
			}

			return runVocabAdd(context.Background(), vocabFile, category, slug, description)
		},
	}

	cmd.Flags().String("slug", "", "Vocabulary slug (required)")
	cmd.MarkFlagRequired("slug")
	cmd.Flags().String("description", "", "Human-readable description (required)")
	cmd.MarkFlagRequired("description")
	cmd.Flags().String("vocab-file", "./doc/vocabulary.yaml", "Path to vocabulary.yaml")

	return cmd, nil
}

func runVocabAdd(ctx context.Context, vocabFile, category, slug, description string) error {
	vocab, err := vocabulary.Load(vocabFile)
	if err != nil {
		return fmt.Errorf("failed to load vocabulary: %w", err)
	}

	if err := vocabulary.AddEntry(vocab, category, slug, description); err != nil {
		return err
	}

	if err := vocabulary.Save(vocabFile, vocab); err != nil {
		return fmt.Errorf("failed to save vocabulary: %w", err)
	}

	fmt.Printf("✓ Added %s '%s' to vocabulary\n", category, slug)
	return nil
}

func NewVocabAssignCommand() (*cobra.Command, error) {
	cmd := &cobra.Command{
		Use:   "assign",
		Short: "Assign topics to documents",
		Long: `Assign topics to documents in a ticket.

Examples:
  ttmp vocab assign --ticket MEN-3475 --doc index.md --topics chat,backend
  ttmp vocab assign --ticket MEN-3475 --topics observability`,
		RunE: func(cmd *cobra.Command, args []string) error {
			ticket, _ := cmd.Flags().GetString("ticket")
			_, _ = cmd.Flags().GetString("doc")
			topics, _ := cmd.Flags().GetString("topics")

			if ticket == "" {
				return fmt.Errorf("--ticket is required")
			}
			if topics == "" {
				return fmt.Errorf("--topics is required")
			}

			fmt.Printf("✓ Assigned topics to %s (not fully implemented)\n", ticket)
			return nil
		},
	}

	cmd.Flags().String("ticket", "", "Ticket identifier (required)")
	cmd.MarkFlagRequired("ticket")
	cmd.Flags().String("doc", "", "Specific document (default: all docs in ticket)")
	cmd.Flags().String("topics", "", "Comma-separated topics to assign (required)")
	cmd.MarkFlagRequired("topics")

	return cmd, nil
}

