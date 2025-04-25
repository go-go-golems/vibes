package list

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/scrapybara/ttmp/pkg/model"
	"github.com/scrapybara/ttmp/pkg/parser"
	"github.com/scrapybara/ttmp/pkg/query"
)

// NewListCommand creates a new list command
func NewListCommand(logger *logrus.Logger, parser *parser.TTMPParser) *cobra.Command {
	var (
		format string
		recursive bool
	)
	
	cmd := &cobra.Command{
		Use:   "list [directory]",
		Short: "List TTMP documents",
		Long:  `List all TTMP documents in a directory.`,
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			directory := args[0]
			
			// Check if directory exists
			info, err := os.Stat(directory)
			if os.IsNotExist(err) {
				logger.Fatalf("Directory does not exist: %s", directory)
			}
			
			if !info.IsDir() {
				logger.Fatalf("Not a directory: %s", directory)
			}
			
			// Find all markdown files
			var files []string
			
			if recursive {
				err = filepath.Walk(directory, func(path string, info os.FileInfo, err error) error {
					if err != nil {
						return err
					}
					
					if !info.IsDir() && strings.HasSuffix(strings.ToLower(info.Name()), ".md") {
						files = append(files, path)
					}
					
					return nil
				})
			} else {
				entries, err := os.ReadDir(directory)
				if err != nil {
					logger.Fatalf("Error reading directory: %v", err)
				}
				
				for _, entry := range entries {
					if !entry.IsDir() && strings.HasSuffix(strings.ToLower(entry.Name()), ".md") {
						files = append(files, filepath.Join(directory, entry.Name()))
					}
				}
			}
			
			if err != nil {
				logger.Fatalf("Error walking directory: %v", err)
			}
			
			// Parse and collect documents
			var docs []*model.TTMPDocument
			
			for _, file := range files {
				doc, err := parser.ParseFile(file)
				if err != nil {
					logger.Warnf("Error parsing file: %s - %v", file, err)
					continue
				}
				
				docs = append(docs, doc)
			}
			
			// Display results
			if len(docs) == 0 {
				fmt.Println("No TTMP documents found.")
				return
			}
			
			switch format {
			case "table":
				fmt.Println(query.FormatDocumentsTable(docs))
			case "list":
				for _, doc := range docs {
					fmt.Printf("%s - %s\n", doc.ID, doc.Title)
				}
			case "detailed":
				for i, doc := range docs {
					if i > 0 {
						fmt.Println(strings.Repeat("-", 80))
					}
					fmt.Println(query.FormatDetailedDocument(doc))
				}
			default:
				logger.Fatalf("Unknown format: %s", format)
			}
			
			fmt.Printf("\nFound %d document(s).\n", len(docs))
		},
	}
	
	// Add flags
	cmd.Flags().StringVar(&format, "format", "table", "Output format (table, list, detailed)")
	cmd.Flags().BoolVarP(&recursive, "recursive", "r", false, "Recursively list all files in subdirectories")
	
	return cmd
}