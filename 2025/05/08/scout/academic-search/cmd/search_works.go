package cmd

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/scrapybara/academic-search-cli/pkg/crossref"
	"github.com/spf13/cobra"
)

var (
	searchWorksSource string
	searchWorksLimit  int
	searchWorksFilter string
	searchWorksOutput string
)

// SearchWorksResult holds the lightweight metadata format for unified works
type SearchWorksResult struct {
	Works []WorkResult `json:"works"`
}

// WorkResult represents a simplified work for display or JSON output
type WorkResult struct {
	ID            string   `json:"id"`
	DOI           string   `json:"doi,omitempty"`
	Title         string   `json:"title"`
	Authors       []string `json:"authors,omitempty"`
	Year          int      `json:"year,omitempty"`
	IsOA          bool     `json:"is_oa,omitempty"`
	CitationCount int      `json:"citation_count,omitempty"`
}

var searchWorksCmd = &cobra.Command{
	Use:   "search-works [query]",
	Short: "Search scholarly works across sources",
	Long:  `Search for scholarly works across Crossref with lightweight metadata for ranking.`,
	Args:  cobra.MinimumNArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		query := strings.Join(args, " ")
		var result SearchWorksResult
		
		if searchWorksSource == "openalex" {
			return fmt.Errorf("OpenAlex search not implemented yet")
		} else if searchWorksSource == "crossref" {
			client := crossref.NewClient("")
			
			resp, err := client.Search(crossref.SearchOptions{
				Query: query,
				Rows:  searchWorksLimit,
			})
			if err != nil {
				return err
			}
			
			for _, work := range resp.Message.Items {
				// Extract authors
				var authors []string
				for _, author := range work.Author {
					authors = append(authors, fmt.Sprintf("%s %s", author.Given, author.Family))
				}
				
				// Get year
				year := 0
				if len(work.Published.DateParts) > 0 && len(work.Published.DateParts[0]) > 0 {
					year = work.Published.DateParts[0][0]
				}
				
				// Create work result
				wr := WorkResult{
					ID:      work.DOI,
					DOI:     work.DOI,
					Title:   crossref.GetTitle(work),
					Authors: authors,
					Year:    year,
				}
				
				result.Works = append(result.Works, wr)
			}
		} else {
			return fmt.Errorf("invalid source: %s (must be 'crossref' or 'openalex')", searchWorksSource)
		}
		
		// Output results
		if searchWorksOutput == "json" {
			// Output as JSON
			jsonData, err := json.MarshalIndent(result, "", "  ")
			if err != nil {
				return err
			}
			fmt.Println(string(jsonData))
		} else {
			// Output as text
			fmt.Printf("Found %d results\n\n", len(result.Works))
			
			for i, work := range result.Works {
				fmt.Printf("%d. %s\n", i+1, work.Title)
				
				if len(work.Authors) > 0 {
					fmt.Printf("   Authors: %s\n", strings.Join(work.Authors, ", "))
				}
				
				if work.Year > 0 {
					fmt.Printf("   Year: %d\n", work.Year)
				}
				
				if work.DOI != "" {
					fmt.Printf("   DOI: %s\n", work.DOI)
				}
				
				if work.CitationCount > 0 {
					fmt.Printf("   Citations: %d\n", work.CitationCount)
				}
				
				if work.IsOA {
					fmt.Printf("   Open Access: Yes\n")
				}
				
				fmt.Printf("   ID: %s\n\n", work.ID)
			}
		}
		
		return nil
	},
}

func init() {
	rootCmd.AddCommand(searchWorksCmd)
	
	searchWorksCmd.Flags().StringVar(&searchWorksSource, "source", "crossref", "Source to search (crossref, openalex not yet implemented)")
	searchWorksCmd.Flags().IntVar(&searchWorksLimit, "limit", 20, "Maximum number of results to return")
	searchWorksCmd.Flags().StringVar(&searchWorksFilter, "filter", "", "Optional filter parameters (e.g., has_oa:true)")
	searchWorksCmd.Flags().StringVar(&searchWorksOutput, "output", "text", "Output format (text, json)")
}