package cmd

import (
	"fmt"
	"strings"

	"github.com/scrapybara/academic-search-cli/pkg/openalex"
	"github.com/spf13/cobra"
)

var (
	openalexPerPage  int
	openalexPage     int
	openalexSort     string
	openalexFilter   string
	openalexEmail    string
)

var openalexCmd = &cobra.Command{
	Use:   "openalex [query]",
	Short: "Search papers on OpenAlex",
	Long:  `Search for scientific papers on OpenAlex using their API.`,
	Args:  cobra.MinimumNArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		query := strings.Join(args, " ")
		client := openalex.NewClient(openalexEmail)
		
		// Parse filter string into map
		filterMap := make(map[string]string)
		if openalexFilter != "" {
			filterParts := strings.Split(openalexFilter, ",")
			for _, part := range filterParts {
				keyValue := strings.SplitN(part, ":", 2)
				if len(keyValue) == 2 {
					filterMap[keyValue[0]] = keyValue[1]
				}
			}
		}
		
		opts := openalex.SearchOptions{
			Query:     query,
			PerPage:   openalexPerPage,
			Page:      openalexPage,
			Sort:      openalexSort,
			Filter:    filterMap,
		}
		
		resp, err := client.Search(opts)
		if err != nil {
			return err
		}
		
		fmt.Printf("Found %d results (showing %d)\n\n", resp.Meta.Count, len(resp.Works))
		
		for i, work := range resp.Works {
			fmt.Printf("%d. %s\n", i+1, work.Title)
			
			// Authors
			authors := openalex.GetAuthors(work)
			if authors != "" {
				fmt.Printf("   Authors: %s\n", authors)
			}
			
			// Publication details
			if work.PublicationYear > 0 {
				fmt.Printf("   Year: %d\n", work.PublicationYear)
			}
			
			if work.Type != "" {
				fmt.Printf("   Type: %s\n", work.Type)
			}
			
			// Citation counts
			fmt.Printf("   Citations: %d\n", work.CitationCount)
			
			// Open access status
			if work.OpenAccess.IsOA {
				fmt.Printf("   Open Access: %s\n", work.OpenAccess.OAStatus)
			}
			
			// DOI and URLs
			if work.DOI != "" {
				fmt.Printf("   DOI: %s\n", work.DOI)
			}
			
			workURL := openalex.GetWorkURL(work)
			if workURL != "" {
				fmt.Printf("   URL: %s\n", workURL)
			}
			
			pdfURL := openalex.GetPDFURL(work)
			if pdfURL != "" {
				fmt.Printf("   PDF: %s\n", pdfURL)
			}
			
			// Concepts (if any) - limited to top 3
			if len(work.Concepts) > 0 {
				concepts := []string{}
				count := 0
				for _, concept := range work.Concepts {
					if count < 3 {
						concepts = append(concepts, concept.Name)
						count++
					}
				}
				fmt.Printf("   Concepts: %s\n", strings.Join(concepts, ", "))
			}
			
			fmt.Println()
		}
		
		return nil
	},
}

func init() {
	rootCmd.AddCommand(openalexCmd)
	
	openalexCmd.Flags().IntVar(&openalexPerPage, "per-page", 5, "Number of results per page")
	openalexCmd.Flags().IntVar(&openalexPage, "page", 1, "Page number")
	openalexCmd.Flags().StringVar(&openalexSort, "sort", "cited_by_count:desc", "Sort order (e.g., cited_by_count:desc, publication_date:desc)")
	openalexCmd.Flags().StringVar(&openalexFilter, "filter", "", "Filter results (e.g., is_oa:true,publication_year:2020)")
	openalexCmd.Flags().StringVar(&openalexEmail, "email", "", "Your email address to use OpenAlex's polite pool")
}