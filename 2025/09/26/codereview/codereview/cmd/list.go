package cmd

import (
	"fmt"
	"os"
	"text/tabwriter"

	"github.com/codereview/cli/internal/database"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newListCommand() *cobra.Command {
	var status string
	var format string

	cmd := &cobra.Command{
		Use:   "list",
		Short: "List code reviews",
		Long:  "List all code reviews with optional filtering by status",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runList(status, format)
		},
	}

	cmd.Flags().StringVar(&status, "status", "", "Filter by status (pending, approved, changes_requested, draft)")
	cmd.Flags().StringVar(&format, "format", "table", "Output format (table, json)")

	return cmd
}

func runList(status, format string) error {
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

	// List reviews
	reviews, err := db.ListReviews(status)
	if err != nil {
		return fmt.Errorf("failed to list reviews: %w", err)
	}

	if len(reviews) == 0 {
		fmt.Println("No reviews found")
		return nil
	}

	// Output in table format
	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintln(w, "ID\tTITLE\tBRANCH\tCOMMIT\tREVIEWER\tSTATUS\tFILES\tANNOTATIONS\tCREATED")

	for _, review := range reviews {
		// Get summary statistics
		summary, err := db.GetReviewSummary(review.ID)
		if err != nil {
			return fmt.Errorf("failed to get review summary: %w", err)
		}

		commit := review.Commit
		if len(commit) > 8 {
			commit = commit[:8]
		}

		fmt.Fprintf(w, "%s\t%s\t%s\t%s\t%s\t%s\t%d\t%d\t%s\n",
			review.ID,
			truncateString(review.Title, 30),
			review.Branch,
			commit,
			review.Reviewer,
			review.Status,
			review.FilesChanged,
			summary.TotalAnnotations,
			review.Created.Format("2006-01-02 15:04"),
		)
	}

	w.Flush()
	return nil
}

func truncateString(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}
