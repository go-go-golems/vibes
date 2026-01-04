package query

import (
	"fmt"
	"strings"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/scrapybara/ttmp/pkg/parser"
	"github.com/scrapybara/ttmp/pkg/query"
)

// NewQueryCommand creates a new query command
func NewQueryCommand(logger *logrus.Logger, parser *parser.TTMPParser) *cobra.Command {
	var (
		id           string
		tags         string
		category     string
		status       string
		owner        string
		modifiedAfter string
		modifiedBefore string
		sourceFile   string
		textSearch   string
		longevity    string
		documentType string
		format       string
	)
	
	cmd := &cobra.Command{
		Use:   "query [directory]",
		Short: "Query TTMP documents",
		Long:  `Search for TTMP documents based on various criteria.`,
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			directory := args[0]
			
			// Parse dates if provided
			var modifiedAfterTime, modifiedBeforeTime *time.Time
			
			if modifiedAfter != "" {
				t, err := time.Parse("2006-01-02", modifiedAfter)
				if err != nil {
					logger.Fatalf("Invalid date format for modified-after: %v", err)
				}
				modifiedAfterTime = &t
			}
			
			if modifiedBefore != "" {
				t, err := time.Parse("2006-01-02", modifiedBefore)
				if err != nil {
					logger.Fatalf("Invalid date format for modified-before: %v", err)
				}
				modifiedBeforeTime = &t
			}
			
			// Parse tags
			var tagsList []string
			if tags != "" {
				tagsList = strings.Split(tags, ",")
				for i, tag := range tagsList {
					tagsList[i] = strings.TrimSpace(tag)
				}
			}
			
			// Create query options
			options := query.QueryOptions{
				ID:             id,
				Tags:           tagsList,
				Category:       category,
				Status:         status,
				Owner:          owner,
				ModifiedAfter:  modifiedAfterTime,
				ModifiedBefore: modifiedBeforeTime,
				SourceFile:     sourceFile,
				TextSearch:     textSearch,
				Longevity:      longevity,
				DocumentType:   documentType,
			}
			
			// Execute query
			queryEngine := query.NewQueryEngine(logger, parser)
			docs, err := queryEngine.FindDocuments(directory, options)
			if err != nil {
				logger.Fatalf("Error querying documents: %v", err)
			}
			
			// Display results
			if len(docs) == 0 {
				fmt.Println("No documents found matching the query criteria.")
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
			
			fmt.Printf("\nFound %d document(s) matching the query criteria.\n", len(docs))
		},
	}
	
	// Add flags
	cmd.Flags().StringVar(&id, "id", "", "Filter by document ID")
	cmd.Flags().StringVar(&tags, "tags", "", "Filter by tags (comma-separated)")
	cmd.Flags().StringVar(&category, "category", "", "Filter by category")
	cmd.Flags().StringVar(&status, "status", "", "Filter by status")
	cmd.Flags().StringVar(&owner, "owner", "", "Filter by owner")
	cmd.Flags().StringVar(&modifiedAfter, "modified-after", "", "Filter by modification date (YYYY-MM-DD)")
	cmd.Flags().StringVar(&modifiedBefore, "modified-before", "", "Filter by modification date (YYYY-MM-DD)")
	cmd.Flags().StringVar(&sourceFile, "source-file", "", "Filter by source file")
	cmd.Flags().StringVar(&textSearch, "text", "", "Search in document content")
	cmd.Flags().StringVar(&longevity, "longevity", "", "Filter by longevity (short/long)")
	cmd.Flags().StringVar(&documentType, "type", "", "Filter by document type")
	cmd.Flags().StringVar(&format, "format", "table", "Output format (table, list, detailed)")
	
	return cmd
}