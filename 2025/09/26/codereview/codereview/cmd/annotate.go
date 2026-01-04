package cmd

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/codereview/cli/internal/database"
	"github.com/codereview/cli/internal/models"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newAnnotateCommand() *cobra.Command {
	var line string
	var annotationType string
	var severity string
	var message string
	var suggestion string

	cmd := &cobra.Command{
		Use:   "annotate <review-id> <file>",
		Short: "Add an annotation to a review",
		Long:  "Add an annotation (comment) to a specific file and line in a code review",
		Args:  cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			return runAnnotate(args[0], args[1], line, annotationType, severity, message, suggestion)
		},
	}

	cmd.Flags().StringVar(&line, "line", "", "Line number or range (e.g., '42' or '10-15')")
	cmd.Flags().StringVar(&annotationType, "type", "issue", "Annotation type (issue, suggestion, praise, question)")
	cmd.Flags().StringVar(&severity, "severity", "minor", "Severity level (minor, major, critical)")
	cmd.Flags().StringVar(&message, "message", "", "Annotation message (required)")
	cmd.Flags().StringVar(&suggestion, "suggestion", "", "Code suggestion (for suggestion type)")

	cmd.MarkFlagRequired("message")

	return cmd
}

func runAnnotate(reviewID, file, line, annotationType, severity, message, suggestion string) error {
	// Validate annotation type
	if !isValidAnnotationType(annotationType) {
		return fmt.Errorf("invalid annotation type: %s (must be: issue, suggestion, praise, question)", annotationType)
	}

	// Validate severity
	if !isValidSeverity(severity) {
		return fmt.Errorf("invalid severity: %s (must be: minor, major, critical)", severity)
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

	// Verify review exists
	_, err = db.GetReview(reviewID)
	if err != nil {
		return fmt.Errorf("failed to get review: %w", err)
	}

	// Parse line information
	var linePtr *int
	var lineStart, lineEnd *int

	if line != "" {
		if strings.Contains(line, "-") {
			// Line range
			parts := strings.Split(line, "-")
			if len(parts) != 2 {
				return fmt.Errorf("invalid line range format: %s (use format: '10-15')", line)
			}

			start, err := strconv.Atoi(strings.TrimSpace(parts[0]))
			if err != nil {
				return fmt.Errorf("invalid start line number: %s", parts[0])
			}

			end, err := strconv.Atoi(strings.TrimSpace(parts[1]))
			if err != nil {
				return fmt.Errorf("invalid end line number: %s", parts[1])
			}

			if start > end {
				return fmt.Errorf("start line must be less than or equal to end line")
			}

			lineStart = &start
			lineEnd = &end
		} else {
			// Single line
			lineNum, err := strconv.Atoi(line)
			if err != nil {
				return fmt.Errorf("invalid line number: %s", line)
			}
			linePtr = &lineNum
		}
	}

	// Create annotation
	annotation := &models.Annotation{
		ReviewID:   reviewID,
		File:       file,
		Line:       linePtr,
		LineStart:  lineStart,
		LineEnd:    lineEnd,
		Type:       annotationType,
		Severity:   severity,
		Message:    message,
		Suggestion: suggestion,
		Status:     models.AnnotationStatusOpen,
	}

	if err := db.CreateAnnotation(annotation); err != nil {
		return fmt.Errorf("failed to create annotation: %w", err)
	}

	// Format line info for output
	lineInfo := "file-level"
	if annotation.HasSingleLine() {
		lineInfo = fmt.Sprintf("L%d", *annotation.Line)
	} else if annotation.HasLineRange() {
		lineInfo = fmt.Sprintf("L%d-%d", *annotation.LineStart, *annotation.LineEnd)
	}

	// Output success message
	fmt.Printf("✅ Annotation created: %d\n", annotation.ID)
	fmt.Printf("   Review:      %s\n", annotation.ReviewID)
	fmt.Printf("   File:        %s\n", annotation.File)
	fmt.Printf("   Line:        %s\n", lineInfo)
	fmt.Printf("   Type:        %s\n", annotation.Type)
	fmt.Printf("   Severity:    %s\n", annotation.Severity)
	fmt.Printf("   Status:      %s\n", annotation.Status)
	fmt.Printf("   Message:     %s\n", annotation.Message)
	if annotation.Suggestion != "" {
		fmt.Printf("   Suggestion:  %s\n", annotation.Suggestion)
	}
	fmt.Printf("   Created:     %s\n", annotation.Created.Format("2006-01-02 15:04:05"))

	return nil
}

func isValidAnnotationType(annotationType string) bool {
	validTypes := []string{
		models.TypeIssue,
		models.TypeSuggestion,
		models.TypePraise,
		models.TypeQuestion,
	}

	for _, validType := range validTypes {
		if annotationType == validType {
			return true
		}
	}
	return false
}

func isValidSeverity(severity string) bool {
	validSeverities := []string{
		models.SeverityMinor,
		models.SeverityMajor,
		models.SeverityCritical,
	}

	for _, validSeverity := range validSeverities {
		if severity == validSeverity {
			return true
		}
	}
	return false
}
