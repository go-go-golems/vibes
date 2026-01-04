package cmd

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/codereview/cli/internal/database"
	"github.com/codereview/cli/internal/models"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"gopkg.in/yaml.v3"
)

func newExportCommand() *cobra.Command {
	var format string
	var output string

	cmd := &cobra.Command{
		Use:   "export <review-id>",
		Short: "Export a review to YAML or JSON",
		Long:  "Export a code review and its annotations to YAML or JSON format",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return runExport(args[0], format, output)
		},
	}

	cmd.Flags().StringVar(&format, "format", "yaml", "Export format (yaml, json)")
	cmd.Flags().StringVar(&output, "output", "", "Output file (default: stdout)")

	return cmd
}

func runExport(reviewID, format, output string) error {
	// Validate format
	if format != "yaml" && format != "json" {
		return fmt.Errorf("invalid format: %s (must be: yaml, json)", format)
	}

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

	// Create export structure matching the YAML DSL
	exportData := map[string]interface{}{
		"review": map[string]interface{}{
			"id":           review.ID,
			"title":        review.Title,
			"branch":       review.Branch,
			"commit":       review.Commit,
			"base_commit":  review.BaseCommit,
			"reviewer":     review.Reviewer,
			"created":      review.Created.Format("2006-01-02T15:04:05Z"),
			"status":       review.Status,
		},
		"annotations": convertAnnotationsForExport(annotations),
		"summary": map[string]interface{}{
			"files_changed":  review.FilesChanged,
			"lines_added":    review.LinesAdded,
			"lines_removed":  review.LinesRemoved,
			"issues_found":   summary.IssuesCount,
			"suggestions":    summary.SuggestionsCount,
		},
		"tags": review.Tags,
	}

	// Marshal to requested format
	var data []byte
	switch format {
	case "yaml":
		data, err = yaml.Marshal(exportData)
	case "json":
		data, err = json.MarshalIndent(exportData, "", "  ")
	}

	if err != nil {
		return fmt.Errorf("failed to marshal data: %w", err)
	}

	// Write to file or stdout
	if output != "" {
		if err := os.WriteFile(output, data, 0644); err != nil {
			return fmt.Errorf("failed to write output file: %w", err)
		}
		fmt.Printf("✅ Review exported to %s\n", output)
	} else {
		fmt.Print(string(data))
	}

	fmt.Fprintf(os.Stderr, "📊 Exported review %s (%s, %d annotations, %d bytes)\n",
		review.ID, format, len(annotations), len(data))

	return nil
}

func convertAnnotationsForExport(annotations []*models.Annotation) []map[string]interface{} {
	var result []map[string]interface{}

	for _, annotation := range annotations {
		exportAnnotation := map[string]interface{}{
			"file":     annotation.File,
			"type":     annotation.Type,
			"severity": annotation.Severity,
			"message":  annotation.Message,
			"status":   annotation.Status,
		}

		// Add line information
		if annotation.HasSingleLine() {
			exportAnnotation["line"] = *annotation.Line
		} else if annotation.HasLineRange() {
			exportAnnotation["lines"] = []int{*annotation.LineStart, *annotation.LineEnd}
		}

		// Add suggestion if present
		if annotation.Suggestion != "" {
			exportAnnotation["suggestion"] = annotation.Suggestion
		}

		// Add threads if present
		if len(annotation.Threads) > 0 {
			var threads []map[string]interface{}
			for _, thread := range annotation.Threads {
				threads = append(threads, map[string]interface{}{
					"author":    thread.Author,
					"message":   thread.Message,
					"timestamp": thread.Timestamp.Format("2006-01-02T15:04:05Z"),
				})
			}
			exportAnnotation["thread"] = threads
		}

		result = append(result, exportAnnotation)
	}

	return result
}
