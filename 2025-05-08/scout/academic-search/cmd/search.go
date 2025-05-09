package cmd

import (
	"fmt"
	"strings"

	"github.com/scrapybara/academic-search-cli/pkg/libgen"
	"github.com/scrapybara/academic-search-cli/pkg/arxiv"
	"github.com/scrapybara/academic-search-cli/pkg/crossref"
	"github.com/spf13/cobra"
)

var (
	searchMaxResults int
	searchSource     string
)

var searchCmd = &cobra.Command{
	Use:   "search [query]",
	Short: "Search papers across all sources",
	Long:  `Search for scientific papers across arXiv, Crossref, and Libgen simultaneously.`,
	Args:  cobra.MinimumNArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		query := strings.Join(args, " ")
		
		// Determine which sources to search
		searchArxiv := searchSource == "all" || searchSource == "arxiv"
		searchCrossref := searchSource == "all" || searchSource == "crossref"
		searchLibgen := searchSource == "all" || searchSource == "libgen"
		
		// ArXiv search
		if searchArxiv {
			client := arxiv.NewClient()
			opts := arxiv.SearchOptions{
				Query:      query,
				MaxResults: searchMaxResults,
				SortBy:     "relevance",
				SortOrder:  "descending",
			}
			
			feed, err := client.Search(opts)
			if err != nil {
				fmt.Printf("Error searching arXiv: %v\n", err)
			} else if feed != nil {
				fmt.Printf("\n=== arXiv Results (%d found) ===\n\n", feed.TotalResults)
				for i, entry := range feed.Entries {
					fmt.Printf("%d. %s\n", i+1, entry.Title)
					
					// Authors
					authors := []string{}
					for _, author := range entry.Authors {
						authors = append(authors, author.Name)
					}
					fmt.Printf("   Authors: %s\n", strings.Join(authors, ", "))
					
					// URL
					fmt.Printf("   URL: %s\n", arxiv.GetPaperURL(entry))
					
					// Published date
					fmt.Printf("   Published: %s\n\n", entry.Published.Format("2006-01-02"))
				}
			}
		}
		
		// Crossref search
		if searchCrossref {
			client := crossref.NewClient("")
			opts := crossref.SearchOptions{
				Query: query,
				Rows:  searchMaxResults,
				Sort:  "score",
				Order: "desc",
			}
			
			resp, err := client.Search(opts)
			if err != nil {
				fmt.Printf("Error searching Crossref: %v\n", err)
			} else if resp != nil {
				fmt.Printf("\n=== Crossref Results (%d found) ===\n\n", resp.Message.TotalResults)
				for i, work := range resp.Message.Items {
					fmt.Printf("%d. %s\n", i+1, crossref.GetTitle(work))
					
					// Authors
					authors := crossref.GetAuthors(work)
					if authors != "" {
						fmt.Printf("   Authors: %s\n", authors)
					}
					
					// DOI and URL
					if work.DOI != "" {
						fmt.Printf("   DOI: %s\n", work.DOI)
					}
					
					if work.URL != "" {
						fmt.Printf("   URL: %s\n", work.URL)
					}
					
					// Year
					year := crossref.GetYear(work)
					if year > 0 {
						fmt.Printf("   Year: %d\n\n", year)
					} else {
						fmt.Println()
					}
				}
			}
		}
		
		// Libgen search
		if searchLibgen {
			client := libgen.NewClient()
			opts := libgen.SearchOptions{
				Query: query,
				Limit: searchMaxResults,
				Page:  1,
			}
			
			books, err := client.Search(opts)
			if err != nil {
				fmt.Printf("Error searching Libgen: %v\n", err)
			} else {
				fmt.Printf("\n=== Libgen Results (%d found) ===\n\n", len(books))
				for i, book := range books {
					fmt.Printf("%d. %s\n", i+1, book.Title)
					
					// Authors
					if book.Author != "" {
						fmt.Printf("   Authors: %s\n", libgen.FormatAuthors(book.Author))
					}
					
					// Publication details
					if book.Year != "" {
						fmt.Printf("   Year: %s\n", book.Year)
					}
					
					if book.Publisher != "" {
						fmt.Printf("   Publisher: %s\n", book.Publisher)
					}
					
					// Download URL
					downloadURL := libgen.GetPrimaryDownloadURL(book)
					fmt.Printf("   Download: %s\n\n", downloadURL)
				}
			}
		}
		
		return nil
	},
}

func init() {
	rootCmd.AddCommand(searchCmd)
	
	searchCmd.Flags().IntVar(&searchMaxResults, "max", 5, "Maximum number of results to return per source")
	searchCmd.Flags().StringVar(&searchSource, "source", "all", "Source to search (all, arxiv, crossref, libgen)")
}