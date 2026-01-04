package query

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	"github.com/spf13/cobra"

	"ttmp-go/pkg/errors"
	"ttmp-go/pkg/model"
	"ttmp-go/pkg/parser"
	"ttmp-go/pkg/query"
	"ttmp-go/pkg/util/fileutil"
)

// NewQueryCommand creates a new query command
func NewQueryCommand() *cobra.Command {
	var (
		recursive bool
		format    string
		output    string
	)

	cmd := &cobra.Command{
		Use:   "query [path] [field] [operator] [value]",
		Short: "Query TTMP documents",
		Long:  `Queries TTMP documents based on specified criteria.`,
		Args:  cobra.MinimumNArgs(4),
		RunE: func(cmd *cobra.Command, args []string) error {
			path := args[0]
			field := args[1]
			operator := args[2]
			value := args[3]

			// Validate the operator
			op := query.Operator(operator)
			validOps := map[query.Operator]bool{
				query.Equal:              true,
				query.NotEqual:           true,
				query.Contains:           true,
				query.StartsWith:         true,
				query.EndsWith:           true,
				query.GreaterThan:        true,
				query.LessThan:           true,
				query.GreaterThanOrEqual: true,
				query.LessThanOrEqual:    true,
				query.Matches:            true,
			}
			
			if !validOps[op] {
				return errors.NewQueryError(
					fmt.Sprintf("Invalid operator: %s", operator),
					fmt.Errorf("Valid operators are: eq, ne, contains, startswith, endswith, gt, lt, gte, lte, matches"),
				)
			}

			// Check if path exists
			fileInfo, err := fileutil.GetFileInfo(path)
			if err != nil {
				return errors.NewIOError(fmt.Sprintf("Failed to access path: %s", path), err)
			}

			// Find all markdown files
			var files []string
			if fileInfo.IsDir() {
				if recursive {
					files, err = fileutil.FindMarkdownFilesRecursive(path)
				} else {
					files, err = fileutil.FindMarkdownFiles(path)
				}
				
				if err != nil {
					return errors.NewIOError(fmt.Sprintf("Failed to find markdown files: %s", path), err)
				}
			} else {
				files = []string{path}
			}

			// Parse all files and store documents
			p := parser.NewParser()
			docs := make([]*model.TTMPDocument, 0, len(files))
			
			for _, file := range files {
				doc, err := p.ParseFile(file)
				if err != nil {
					fmt.Fprintf(os.Stderr, "Warning: Failed to parse %s: %v\n", file, err)
					continue
				}
				docs = append(docs, doc)
			}

			// Create a query condition
			condition := query.QueryCondition{
				Field:    field,
				Operator: op,
				Value:    value,
			}

			// Execute the query
			q := query.NewQuerier()
			results, err := q.Query(docs, []query.QueryCondition{condition})
			if err != nil {
				return errors.NewQueryError("Query execution failed", err)
			}

			// Output the results
			switch strings.ToLower(format) {
			case "json":
				outputJSON(results, output)
			case "text":
				outputText(results, output)
			default:
				return errors.NewQueryError(
					fmt.Sprintf("Unsupported output format: %s", format),
					fmt.Errorf("Valid formats are: json, text"),
				)
			}

			return nil
		},
	}

	cmd.Flags().BoolVarP(&recursive, "recursive", "r", false, "Recursively search documents in subdirectories")
	cmd.Flags().StringVarP(&format, "format", "f", "text", "Output format (text, json)")
	cmd.Flags().StringVarP(&output, "output", "o", "", "Output file (default: stdout)")

	return cmd
}

// outputJSON outputs the results in JSON format
func outputJSON(results []*query.QueryResult, outputFile string) {
	// Convert results to a simple map for JSON output
	type jsonResult struct {
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

	jsonResults := make([]jsonResult, 0, len(results))
	for _, r := range results {
		doc := r.Document
		
		modified := ""
		if doc.Modified != nil {
			modified = doc.Modified.Format("2006-01-02T15:04:05Z07:00")
		}
		
		jsonResults = append(jsonResults, jsonResult{
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
	output, err := json.MarshalIndent(jsonResults, "", "  ")
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

// outputText outputs the results in text format
func outputText(results []*query.QueryResult, outputFile string) {
	// Prepare output
	var output strings.Builder
	
	output.WriteString(fmt.Sprintf("Found %d matching documents:\n\n", len(results)))
	
	for i, r := range results {
		doc := r.Document
		
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