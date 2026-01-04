package create

import (
	"fmt"
	"path/filepath"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"ttmp-go/pkg/errors"
	"ttmp-go/pkg/model"
	"ttmp-go/pkg/parser"
	"ttmp-go/pkg/util/fileutil"
	"ttmp-go/pkg/validator"
)

// NewCreateCommand creates a new create command
func NewCreateCommand() *cobra.Command {
	var (
		id          string
		docType     string
		title       string
		description string
		tags        []string
		validate    bool
	)

	cmd := &cobra.Command{
		Use:   "create [output-file]",
		Short: "Create a new TTMP document",
		Long:  `Creates a new TTMP document with the specified fields.`,
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			outputFile := args[0]

			// Check if file already exists
			if fileutil.FileExists(outputFile) {
				return errors.NewIOError(fmt.Sprintf("File already exists: %s", outputFile), nil)
			}

			// Generate an ID if not provided
			if id == "" {
				// Use the filename without extension
				base := filepath.Base(outputFile)
				id = strings.TrimSuffix(base, filepath.Ext(base))
				// Convert to lowercase and replace spaces with hyphens
				id = strings.ToLower(id)
				id = strings.ReplaceAll(id, " ", "-")
				// Remove any non-alphanumeric characters (except hyphens and underscores)
				id = cleanID(id)
			}

			// Create a new TTMP document
			now := time.Now()
			doc := &model.TTMPDocument{
				ID:          id,
				Type:        docType,
				Title:       title,
				Description: description,
				Created:     now,
				Tags:        tags,
				Custom:      make(map[string]interface{}),
				Content:     "# " + title + "\n\nYour content here.\n",
				Filename:    outputFile,
			}

			// Validate the document if requested
			if validate {
				v := validator.NewValidator()
				err := v.ValidateDocument(doc)
				if err != nil {
					return errors.NewValidationError("Document validation failed", err.Error())
				}
			}

			// Write the document to a file
			p := parser.NewParser()
			err := p.WriteDocumentToFile(doc, outputFile)
			if err != nil {
				return errors.NewIOError(fmt.Sprintf("Failed to write document to file: %s", outputFile), err)
			}

			fmt.Printf("Created new TTMP document: %s\n", outputFile)
			return nil
		},
	}

	cmd.Flags().StringVar(&id, "id", "", "Document ID (defaults to filename)")
	cmd.Flags().StringVar(&docType, "type", "note", "Document type (e.g., note, concept, reference)")
	cmd.Flags().StringVar(&title, "title", "", "Document title")
	cmd.Flags().StringVar(&description, "description", "", "Document description")
	cmd.Flags().StringSliceVar(&tags, "tags", []string{}, "Document tags (comma-separated)")
	cmd.Flags().BoolVar(&validate, "validate", true, "Validate the document before creating")

	// Make title required
	cmd.MarkFlagRequired("title")

	return cmd
}

// cleanID removes any non-alphanumeric characters from an ID (except hyphens and underscores)
func cleanID(id string) string {
	var result strings.Builder
	for _, char := range id {
		if (char >= 'a' && char <= 'z') || (char >= '0' && char <= '9') || char == '-' || char == '_' {
			result.WriteRune(char)
		}
	}
	return result.String()
}