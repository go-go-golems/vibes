package cmd

import (
	"fmt"
	"strings"

	"github.com/scrapybara/academic-search-cli/pkg/arxiv"
	"github.com/spf13/cobra"
)

var (
	arxivMaxResults int
	arxivStart      int
	arxivSortBy     string
	arxivSortOrder  string
)

var arxivCmd = &cobra.Command{
	Use:   "arxiv [query]",
	Short: "Search papers on arXiv",
	Long:  `Search for scientific papers on arXiv.org using their API.`,
	Args:  cobra.MinimumNArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		query := strings.Join(args, " ")
		client := arxiv.NewClient()
		
		opts := arxiv.SearchOptions{
			Query:      query,
			MaxResults: arxivMaxResults,
			Start:      arxivStart,
			SortBy:     arxivSortBy,
			SortOrder:  arxivSortOrder,
		}
		
		feed, err := client.Search(opts)
		if err != nil {
			return err
		}
		
		fmt.Printf("Found %d results (showing %d)\n\n", feed.TotalResults, len(feed.Entries))
		
		for i, entry := range feed.Entries {
			fmt.Printf("%d. %s\n", i+1, entry.Title)
			
			// Authors
			authors := []string{}
			for _, author := range entry.Authors {
				authors = append(authors, author.Name)
			}
			fmt.Printf("   Authors: %s\n", strings.Join(authors, ", "))
			
			// Categories
			categories := []string{}
			for _, cat := range entry.Categories {
				categories = append(categories, cat.Term)
			}
			fmt.Printf("   Categories: %s\n", strings.Join(categories, ", "))
			
			// Links
			fmt.Printf("   URL: %s\n", arxiv.GetPaperURL(entry))
			pdfURL := arxiv.GetPDF(entry)
			if pdfURL != "" {
				fmt.Printf("   PDF: %s\n", pdfURL)
			}
			
			// Published date
			fmt.Printf("   Published: %s\n", entry.Published.Format("2006-01-02"))
			
			// Summary (truncated)
			summary := entry.Summary
			if len(summary) > 200 {
				summary = summary[:200] + "..."
			}
			fmt.Printf("   Summary: %s\n\n", strings.ReplaceAll(summary, "\n", " "))
		}
		
		return nil
	},
}

func init() {
	rootCmd.AddCommand(arxivCmd)
	
	arxivCmd.Flags().IntVar(&arxivMaxResults, "max", 5, "Maximum number of results to return")
	arxivCmd.Flags().IntVar(&arxivStart, "start", 0, "Result offset")
	arxivCmd.Flags().StringVar(&arxivSortBy, "sort-by", "relevance", "Sort by (relevance, lastUpdatedDate, submittedDate)")
	arxivCmd.Flags().StringVar(&arxivSortOrder, "sort-order", "descending", "Sort order (ascending, descending)")
}