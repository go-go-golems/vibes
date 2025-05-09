package cmd

import (
	"fmt"
	"os"
	"strings"

	"arxiv-libgen-cli/pkg/common"
	"arxiv-libgen-cli/pkg/openalex"

	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

var openalexQuery string
var openalexPerPage int
var openalexMailto string
var openalexFilter string
var openalexSort string

var openalexCmd = &cobra.Command{
	Use:   "openalex",
	Short: "Search for scholarly works on OpenAlex",
	Long: `Search for scholarly works (articles, books, datasets, etc.) using the OpenAlex API.

Example:
  arxiv-libgen-cli openalex --query "machine learning applications" --per_page 5 --mailto "your.email@example.com"
  arxiv-libgen-cli openalex -q "bioinformatics" -n 3 -f "publication_year:2022,type:journal-article" -s "cited_by_count:desc" -m "user@example.org"`,
	Run: func(cmd *cobra.Command, args []string) {
		if openalexQuery == "" && openalexFilter == "" {
			fmt.Println("Error: query or filter must be provided.")
			cmd.Help()
			os.Exit(1)
		}
		if openalexMailto == "" {
			fmt.Println("Warning: It is highly recommended to provide an email address using --mailto for the OpenAlex polite pool.")
			log.Warn().Msg("No mailto parameter provided for OpenAlex API polite pool.")
		}

		log.Debug().Str("query", openalexQuery).Int("per_page", openalexPerPage).Str("mailto", openalexMailto).Str("filter", openalexFilter).Str("sort", openalexSort).Msg("OpenAlex search initiated")

		var queryParts []string
		if openalexQuery != "" {
			queryParts = append(queryParts, fmt.Sprintf("query=\"%s\"", openalexQuery))
		}
		if openalexFilter != "" {
			queryParts = append(queryParts, fmt.Sprintf("filter=\"%s\"", openalexFilter))
		}
		searchDesc := strings.Join(queryParts, ", ")
		fmt.Printf("Searching OpenAlex for: %s (per_page: %d, sort: '%s')\n", searchDesc, openalexPerPage, openalexSort)
		
		client := openalex.NewClient(openalexMailto)
		params := common.SearchParams{
			Query:      openalexQuery,
			MaxResults: openalexPerPage,
			Filters: map[string]string{
				"filter": openalexFilter,
				"sort":   openalexSort,
			},
			EmailAddr: openalexMailto,
		}

		results, err := client.Search(params)
		if err != nil {
			log.Error().Err(err).Msg("OpenAlex search failed")
			fmt.Printf("Error: %s\n", err.Error())
			return
		}

		if len(results) == 0 {
			fmt.Println("No results found.")
			log.Info().Msg("No results in OpenAlex response")
			return
		}

		fmt.Printf("\nFound results (showing %d):\n", len(results))
		fmt.Println("--------------------------------------------------")

		for i, result := range results {
			fmt.Printf("Result %d:\n", i+1)
			fmt.Printf("  Title: %s\n", result.Title)
			fmt.Printf("  OpenAlex ID: %s\n", result.SourceURL)
			if result.DOI != "" {
				fmt.Printf("  DOI: %s\n", result.DOI)
			}
			fmt.Printf("  Authors: %s\n", strings.Join(result.Authors, ", "))
			fmt.Printf("  Publication Date: %s\n", result.Published)
			fmt.Printf("  Type: %s\n", result.Type)
			if result.JournalInfo != "" {
				fmt.Printf("  Venue: %s\n", result.JournalInfo)
			}
			fmt.Printf("  Cited By Count: %d\n", result.Citations)
			if result.OAStatus != "" {
				fmt.Printf("  Open Access Status: %s\n", result.OAStatus)
			}
			if result.PDFURL != "" {
				fmt.Printf("  PDF URL: %s\n", result.PDFURL)
			}
			if result.License != "" {
				fmt.Printf("  License: %s\n", result.License)
			}
			if result.Abstract != "" {
				fmt.Printf("  Abstract: %s\n", result.Abstract)
			}
			if relevance, ok := result.Metadata["relevance_score"].(float64); ok && relevance > 0 {
				fmt.Printf("  Relevance Score: %f\n", relevance)
			}
			fmt.Println("--------------------------------------------------")
		}
	},
}

func init() {
	rootCmd.AddCommand(openalexCmd)
	openalexCmd.Flags().StringVarP(&openalexQuery, "query", "q", "", "Search query for OpenAlex (searches title, abstract, fulltext)")
	openalexCmd.Flags().IntVarP(&openalexPerPage, "per_page", "n", 10, "Number of results per page")
	openalexCmd.Flags().StringVarP(&openalexMailto, "mailto", "m", "", "Email address for OpenAlex polite pool (highly recommended)")
	openalexCmd.Flags().StringVarP(&openalexFilter, "filter", "f", "", "Filter parameters for OpenAlex (e.g., publication_year:2022,type:journal-article)")
	openalexCmd.Flags().StringVarP(&openalexSort, "sort", "s", "relevance_score:desc", "Sort order (e.g., cited_by_count:desc, publication_date:asc)")
}