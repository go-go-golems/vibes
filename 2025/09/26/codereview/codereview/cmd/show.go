package cmd

import (
	"fmt"
	"os"
	"text/tabwriter"

	"github.com/codereview/cli/internal/database"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newShowCommand() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "show <review-id>",
		Short: "Show detailed review information",
		Long:  "Show detailed information about a specific code review including annotations",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return runShow(args[0])
		},
	}

	return cmd
}

func runShow(reviewID string) error {
	// Get database path
	dbPath := viper.GetString("db")
	if dbPath == "" {
		dbPath = database.GetDefaultDBPath()
	}

	// Open database
	db, err := database.New(dbPath)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer db.Close()

	// Get review
	review, err := db.GetReview(reviewID)
	if err != nil {
		return fmt.Errorf("failed to get review: %w", err)
	}

	// Get annotations
	annotations, err := db.GetAnnotationsForReview(reviewID)
	if err != nil {
		return fmt.Errorf("failed to get annotations: %w", err)
	}

	// Get summary
	summary, err := db.GetReviewSummary(reviewID)
	if err != nil {
		return fmt.Errorf("failed to get review summary: %w", err)
	}

	// Output review information
	fmt.Printf("Review: %s\n", review.ID)
	fmt.Printf("  Title:       %s\n", review.Title)
	fmt.Printf("  Branch:      %s\n", review.Branch)
	fmt.Printf("  Commit:      %s\n", review.Commit)
	fmt.Printf("  Base:        %s\n", review.BaseCommit)
	fmt.Printf("  Reviewer:    %s\n", review.Reviewer)
	fmt.Printf("  Status:      %s\n", review.Status)
	fmt.Printf("  Files:       %d\n", review.FilesChanged)
	fmt.Printf("  Lines +/-:   +%d/-%d\n", review.LinesAdded, review.LinesRemoved)
	fmt.Printf("  Created:     %s\n", review.Created.Format("2006-01-02 15:04:05"))
	fmt.Printf("  Updated:     %s\n", review.Updated.Format("2006-01-02 15:04:05"))

	// Output summary
	fmt.Printf("\nSummary:\n")
	fmt.Printf("  Total Annotations: %d\n", summary.TotalAnnotations)
	fmt.Printf("  Issues:            %d\n", summary.IssuesCount)
	fmt.Printf("  Suggestions:       %d\n", summary.SuggestionsCount)
	fmt.Printf("  Praise:            %d\n", summary.PraiseCount)
	fmt.Printf("  Questions:         %d\n", summary.QuestionsCount)
	fmt.Printf("  Critical:          %d\n", summary.CriticalCount)
	fmt.Printf("  Major:             %d\n", summary.MajorCount)
	fmt.Printf("  Minor:             %d\n", summary.MinorCount)
	fmt.Printf("  Open:              %d\n", summary.OpenCount)
	fmt.Printf("  Resolved:          %d\n", summary.ResolvedCount)

	// Output annotations
	if len(annotations) > 0 {
		fmt.Printf("\nAnnotations:\n")
		w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
		fmt.Fprintln(w, "ID\tFILE\tLINE\tTYPE\tSEVERITY\tSTATUS\tMESSAGE\tTHREADS")

		for _, annotation := range annotations {
			lineInfo := "file-level"
			if annotation.HasSingleLine() {
				lineInfo = fmt.Sprintf("L%d", *annotation.Line)
			} else if annotation.HasLineRange() {
				lineInfo = fmt.Sprintf("L%d-%d", *annotation.LineStart, *annotation.LineEnd)
			}

			fmt.Fprintf(w, "%d\t%s\t%s\t%s\t%s\t%s\t%s\t%d\n",
				annotation.ID,
				annotation.File,
				lineInfo,
				annotation.Type,
				annotation.Severity,
				annotation.Status,
				truncateString(annotation.Message, 40),
				len(annotation.Threads),
			)
		}
		w.Flush()

		// Show threads if any
		for _, annotation := range annotations {
			if len(annotation.Threads) > 0 {
				fmt.Printf("\nThreads for annotation %d:\n", annotation.ID)
				for _, thread := range annotation.Threads {
					fmt.Printf("  [%s] %s: %s\n",
						thread.Timestamp.Format("2006-01-02 15:04"),
						thread.Author,
						thread.Message,
					)
				}
			}
		}
	}

	return nil
}
