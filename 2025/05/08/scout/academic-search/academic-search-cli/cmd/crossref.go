package cmd

import (
	"fmt"
	"strings"

	"github.com/scrapybara/academic-search-cli/pkg/crossref"
	"github.com/spf13/cobra"
)

var (
	crossrefRows   int
	crossrefOffset int
	crossrefSort   string
	crossrefOrder  string
	crossrefFilter string
	userEmail      string
)

var crossrefCmd = &cobra.Command{
	Use:   "crossref [query]",
	Short: "Search papers on Crossref",
	Long:  `Search for scientific papers on Crossref using their API.`,
	Args:  cobra.MinimumNArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		query := strings.Join(args, " ")
		client := crossref.NewClient(userEmail)
		
		opts := crossref.SearchOptions{
			Query:    query,
			Rows:     crossrefRows,
			Offset:   crossrefOffset,
			Sort:     crossrefSort,
			Order:    crossrefOrder,
			Filter:   crossrefFilter,
		}
		
		resp, err := client.Search(opts)
		if err != nil {
			return err
		}
		
		fmt.Printf("Found %d results (showing %d)\n\n", resp.Message.TotalResults, len(resp.Message.Items))
		
		for i, work := range resp.Message.Items {
			fmt.Printf("%d. %s\n", i+1, crossref.GetTitle(work))
			
			// Authors
			authors := crossref.GetAuthors(work)
			if authors != "" {
				fmt.Printf("   Authors: %s\n", authors)
			}
			
			// Publication details
			year := crossref.GetYear(work)
			if year > 0 {
				fmt.Printf("   Year: %d\n", year)
			}
			
			if work.Publisher != "" {
				fmt.Printf("   Publisher: %s\n", work.Publisher)
			}
			
			if work.Type != "" {
				fmt.Printf("   Type: %s\n", work.Type)
			}
			
			// DOI and URL
			if work.DOI != "" {
				fmt.Printf("   DOI: %s\n", work.DOI)
			}
			
			if work.URL != "" {
				fmt.Printf("   URL: %s\n", work.URL)
			}
			
			// Abstract (truncated)
			if work.Abstract != "" {
				abstract := work.Abstract
				if len(abstract) > 200 {
					abstract = abstract[:200] + "..."
				}
				fmt.Printf("   Abstract: %s\n", strings.ReplaceAll(abstract, "\n", " "))
			}
			
			fmt.Println()
		}
		
		return nil
	},
}

func init() {
	rootCmd.AddCommand(crossrefCmd)
	
	crossrefCmd.Flags().IntVar(&crossrefRows, "rows", 5, "Number of results to return")
	crossrefCmd.Flags().IntVar(&crossrefOffset, "offset", 0, "Result offset")
	crossrefCmd.Flags().StringVar(&crossrefSort, "sort", "score", "Sort by (score, relevance, updated, deposited, indexed, published)")
	crossrefCmd.Flags().StringVar(&crossrefOrder, "order", "desc", "Sort order (asc, desc)")
	crossrefCmd.Flags().StringVar(&crossrefFilter, "filter", "", "Filter results (e.g., 'type:journal-article')")
	crossrefCmd.Flags().StringVar(&userEmail, "email", "", "Your email address to provide to Crossref as per their API etiquette")
}