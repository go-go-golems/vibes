package list

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	"github.com/spf13/cobra"

	"ttmp-go/pkg/errors"
	"ttmp-go/pkg/model"
	"ttmp-go/pkg/parser"
	"ttmp-go/pkg/util/fileutil"
)

// NewListCommand creates a new list command
func NewListCommand() *cobra.Command {
	var (
		recursive bool
		format    string
		output    string
		filterType string
		filterTags []string
	)

	cmd := &cobra.Command{
		Use:   "list [path]",
		Short: "List TTMP documents",
		Long:  `Lists all TTMP documents in a directory.`,
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			path := args[0]

			// Check if path is a directory
			fileInfo, err := fileutil.GetFileInfo(path)
			if err != nil {
				return errors.NewIOError(fmt.Sprintf("Failed to access path: %s", path), err)
			}

			if !fileInfo.IsDir() {
				return errors.NewIOError(fmt.Sprintf("Path is not a directory: %s", path), nil)
			}

			// Find all markdown files
			var files []string
			if recursive {
				files, err = fileutil.FindMarkdownFilesRecursive(path)
			} else {
				files, err = fileutil.FindMarkdownFiles(path)
			}

			if err != nil {
				return errors.NewIOError(fmt.Sprintf("Failed to find markdown files: %s", path), err)
			}

			// Parse all files
			p := parser.NewParser()
			var docs []*model.TTMPDocument
			
			for _, file := range files {
				doc, err := p.ParseFile(file)
				if err != nil {
					fmt.Fprintf(os.Stderr, "Warning: Failed to parse %s: %v\n", file, err)
					continue
				}
				
				// Apply filters
				if filterType != "" && doc.Type != filterType {
					continue
				}
				
				if len(filterTags) > 0 && !hasAnyTag(doc.Tags, filterTags) {
					continue
				}
				
				docs = append(docs, doc)
			}

			// Output the results
			switch strings.ToLower(format) {
			case "json":
				outputJSON(docs, output)
			case "text":
				outputText(docs, output)
			default:
				return errors.NewQueryError(
					fmt.Sprintf("Unsupported output format: %s", format),
					fmt.Errorf("Valid formats are: json, text"),
				)
			}

			return nil
		},
	}

	cmd.Flags().BoolVarP(&recursive, "recursive", "r", false, "Recursively list documents in subdirectories")
	cmd.Flags().StringVarP(&format, "format", "f", "text", "Output format (text, json)")
	cmd.Flags().StringVarP(&output, "output", "o", "", "Output file (default: stdout)")
	cmd.Flags().StringVarP(&filterType, "type", "t", "", "Filter by document type")
	cmd.Flags().StringSliceVar(&filterTags, "tags", []string{}, "Filter by tags (comma-separated)")

	return cmd
}

// hasAnyTag checks if a document has any of the specified tags
func hasAnyTag(docTags []string, filterTags []string) bool {
	for _, docTag := range docTags {
		for _, filterTag := range filterTags {
			if docTag == filterTag {
				return true
			}
		}
	}
	return false
}

// outputJSON outputs the document list in JSON format
func outputJSON(docs []*model.TTMPDocument, outputFile string) {
	// Convert docs to a simple map for JSON output
	type jsonDoc struct {
		ID          string                 `json:"id"`
		Type        string                 `json:"type"`
		Title       string                 `json:"title"`
		Description string                 `json:"description,omitempty"`
		Created     string                 `json:"created"`
		Modified    string                 `json:"modified,omitempty"`
		Tags        []string               `json:"tags,omitempty"`
		Filename    string                 `json:"filename"`
		Custom      map[string]interface{} `json:"custom,omitempty"`
	}

	jsonDocs := make([]jsonDoc, 0, len(docs))
	for _, doc := range docs {
		modified := ""
		if doc.Modified != nil {
			modified = doc.Modified.Format("2006-01-02T15:04:05Z07:00")
		}
		
		jsonDocs = append(jsonDocs, jsonDoc{
			ID:          doc.ID,
			Type:        doc.Type,
			Title:       doc.Title,
			Description: doc.Description,
			Created:     doc.Created.Format("2006-01-02T15:04:05Z07:00"),
			Modified:    modified,
			Tags:        doc.Tags,
			Filename:    doc.Filename,
			Custom:      doc.Custom,
		})
	}

	// Marshal to JSON
	output, err := json.MarshalIndent(jsonDocs, "", "  ")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error marshaling results to JSON: %v\n", err)
		return
	}

	// Write output
	if outputFile == "" {
		fmt.Println(string(output))
	} else {
		err = os.WriteFile(outputFile, output, 0644)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error writing to output file: %v\n", err)
		}
	}
}

// outputText outputs the document list in text format
func outputText(docs []*model.TTMPDocument, outputFile string) {
	// Prepare output
	var output strings.Builder
	
	output.WriteString(fmt.Sprintf("Found %d documents:\n\n", len(docs)))
	
	for i, doc := range docs {
		output.WriteString(fmt.Sprintf("%d. %s\n", i+1, doc.Title))
		output.WriteString(fmt.Sprintf("   ID: %s\n", doc.ID))
		output.WriteString(fmt.Sprintf("   Type: %s\n", doc.Type))
		
		if doc.Description != "" {
			output.WriteString(fmt.Sprintf("   Description: %s\n", doc.Description))
		}
		
		output.WriteString(fmt.Sprintf("   Created: %s\n", doc.Created.Format("2006-01-02")))
		
		if doc.Modified != nil {
			output.WriteString(fmt.Sprintf("   Modified: %s\n", doc.Modified.Format("2006-01-02")))
		}
		
		if len(doc.Tags) > 0 {
			output.WriteString(fmt.Sprintf("   Tags: %s\n", strings.Join(doc.Tags, ", ")))
		}
		
		output.WriteString(fmt.Sprintf("   File: %s\n", doc.Filename))
		output.WriteString("\n")
	}

	// Write output
	if outputFile == "" {
		fmt.Print(output.String())
	} else {
		err := os.WriteFile(outputFile, []byte(output.String()), 0644)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error writing to output file: %v\n", err)
		}
	}
}