package cmd

import (
	"fmt"
	"strings"
	"sync"

	"github.com/scrapybara/academic-search-cli/pkg/arxiv"
	"github.com/scrapybara/academic-search-cli/pkg/crossref"
	"github.com/scrapybara/academic-search-cli/pkg/libgen"
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
		
		var wg sync.WaitGroup
		var arxivErr, crossrefErr, libgenErr error
		
		// ArXiv search
		var arxivFeed *arxiv.Feed
		if searchArxiv {
			wg.Add(1)
			go func() {
				defer wg.Done()
				client := arxiv.NewClient()
				opts := arxiv.SearchOptions{
					Query:      query,
					MaxResults: searchMaxResults,
					SortBy:     "relevance",
					SortOrder:  "descending",
				}
				arxivFeed, arxivErr = client.Search(opts)
			}()
		}
		
		// Crossref search
		var crossrefResp *crossref.SearchResponse
		if searchCrossref {
			wg.Add(1)
			go func() {
				defer wg.Done()
				client := crossref.NewClient("")
				opts := crossref.SearchOptions{
					Query: query,
					Rows:  searchMaxResults,
					Sort:  "score",
					Order: "desc",
				}
				crossrefResp, crossrefErr = client.Search(opts)
			}()
		}
		
		// Libgen search
		var libgenBooks []libgen.Book
		if searchLibgen {
			wg.Add(1)
			go func() {
				defer wg.Done()
				client := libgen.NewClient()
				opts := libgen.SearchOptions{
					Query: query,
					Limit: searchMaxResults,
					Page:  1,
				}
				libgenBooks, libgenErr = client.Search(opts)
			}()
		}
		
		// Wait for all searches to complete
		wg.Wait()
		
		// Print results
		if searchArxiv {
			if arxivErr != nil {
				fmt.Printf("Error searching arXiv: %v\n", arxivErr)
			} else {
				fmt.Printf("\n=== arXiv Results (%d found) ===\n\n", arxivFeed.TotalResults)
				for i, entry := range arxivFeed.Entries {
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
		
		if searchCrossref {
			if crossrefErr != nil {
				fmt.Printf("Error searching Crossref: %v\n", crossrefErr)
			} else {
				fmt.Printf("\n=== Crossref Results (%d found) ===\n\n", crossrefResp.Message.TotalResults)
				for i, work := range crossrefResp.Message.Items {
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
		
		if searchLibgen {
			if libgenErr != nil {
				fmt.Printf("Error searching Libgen: %v\n", libgenErr)
			} else {
				fmt.Printf("\n=== Libgen Results (%d found) ===\n\n", len(libgenBooks))
				for i, book := range libgenBooks {
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
					downloadURL := libgen.GetDownloadURL(book)
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