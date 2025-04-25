package create

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/scrapybara/ttmp/pkg/model"
	"github.com/scrapybara/ttmp/pkg/parser"
)

// NewCreateCommand creates a new create command
func NewCreateCommand(logger *logrus.Logger, parser *parser.TTMPParser) *cobra.Command {
	var (
		id          string
		title       string
		status      string
		tags        string
		category    string
		documentType string
		longevity   string
		owner       string
		sourceFiles string
		content     string
		edit        bool
	)
	
	cmd := &cobra.Command{
		Use:   "create [output_path]",
		Short: "Create a new TTMP document",
		Long:  `Create a new TTMP document with YAML frontmatter.`,
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			outputPath := args[0]
			
			// Check if file already exists
			if _, err := os.Stat(outputPath); err == nil && !edit {
				logger.Fatalf("File already exists: %s. Use --edit to modify.", outputPath)
			}
			
			// Create new document or load existing one
			var doc *model.TTMPDocument
			
			if edit && fileExists(outputPath) {
				var err error
				doc, err = parser.ParseFile(outputPath)
				if err != nil {
					logger.Fatalf("Error parsing existing file: %v", err)
				}
				logger.Infof("Editing existing document: %s", outputPath)
			} else {
				doc = model.NewTTMPDocument()
				now := time.Now()
				doc.Created = &now
				doc.Updated = &now
				
				// If no content is provided, add a default template
				if content == "" {
					content = "# " + title + "\n\n[Your content here]\n"
				}
			}
			
			// Update document with provided values
			if id != "" {
				doc.ID = id
			} else if doc.ID == "" {
				// Generate ID from filename if not provided
				base := filepath.Base(outputPath)
				ext := filepath.Ext(base)
				doc.ID = strings.TrimSuffix(base, ext)
			}
			
			if title != "" {
				doc.Title = title
			}
			
			if status != "" {
				doc.Status = status
			}
			
			if tags != "" {
				doc.Tags = strings.Split(tags, ",")
				for i, tag := range doc.Tags {
					doc.Tags[i] = strings.TrimSpace(tag)
				}
			}
			
			if category != "" {
				doc.Category = category
			}
			
			if documentType != "" {
				doc.DocumentType = documentType
			}
			
			if longevity != "" {
				doc.Longevity = longevity
			}
			
			if owner != "" {
				doc.Owner = owner
			}
			
			if sourceFiles != "" {
				doc.SourceFiles = strings.Split(sourceFiles, ",")
				for i, file := range doc.SourceFiles {
					doc.SourceFiles[i] = strings.TrimSpace(file)
				}
			}
			
			if content != "" {
				doc.Content = content
			}
			
			// Update the updated timestamp
			now := time.Now()
			doc.Updated = &now
			
			// Write document to file
			err := parser.WriteTTMP(doc, outputPath)
			if err != nil {
				logger.Fatalf("Error writing document: %v", err)
			}
			
			fmt.Printf("Created TTMP document: %s\n", outputPath)
		},
	}
	
	// Add flags
	cmd.Flags().StringVar(&id, "id", "", "Document ID")
	cmd.Flags().StringVar(&title, "title", "", "Document title")
	cmd.Flags().StringVar(&status, "status", "draft", "Document status (draft, active, deprecated)")
	cmd.Flags().StringVar(&tags, "tags", "", "Comma-separated tags")
	cmd.Flags().StringVar(&category, "category", "", "Document category")
	cmd.Flags().StringVar(&documentType, "type", "", "Document type (spec, guide, tutorial, etc.)")
	cmd.Flags().StringVar(&longevity, "longevity", "", "Document longevity (short, long)")
	cmd.Flags().StringVar(&owner, "owner", "", "Document owner")
	cmd.Flags().StringVar(&sourceFiles, "source-files", "", "Comma-separated source files")
	cmd.Flags().StringVar(&content, "content", "", "Document content")
	cmd.Flags().BoolVar(&edit, "edit", false, "Edit existing document")
	
	return cmd
}

// fileExists checks if a file exists
func fileExists(path string) bool {
	_, err := os.Stat(path)
	return !os.IsNotExist(err)
}