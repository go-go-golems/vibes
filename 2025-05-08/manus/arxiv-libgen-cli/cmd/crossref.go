package cmd

import (
	"fmt"
	"os"
	"strings"

	"arxiv-libgen-cli/pkg/common"
	"arxiv-libgen-cli/pkg/crossref"

	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

var crossrefQuery string
var crossrefRows int
var crossrefMailto string
var crossrefFilter string

// crossrefCmd represents the crossref command
var crossrefCmd = &cobra.Command{
	Use:   "crossref",
	Short: "Search for scholarly works on Crossref",
	Long: `Search for scholarly works (articles, books, datasets, etc.) using the Crossref REST API.

Example:
  arxiv-libgen-cli crossref --query "climate change adaptation" --rows 5 --mailto "your.email@example.com"
  arxiv-libgen-cli crossref -q "consciousness" -n 3 -f "from-pub-date:2022,type:journal-article" -m "user@example.org"`,
	Run: func(cmd *cobra.Command, args []string) {
		if crossrefQuery == "" {
			fmt.Println("Error: query cannot be empty.")
			cmd.Help()
			os.Exit(1)
		}
		if crossrefMailto == "" {
			fmt.Println("Warning: It is recommended to provide an email address using --mailto for the Crossref polite pool.")
			log.Warn().Msg("No mailto parameter provided for Crossref API polite pool.")
		}

		log.Debug().Str("query", crossrefQuery).Int("rows", crossrefRows).Str("mailto", crossrefMailto).Str("filter", crossrefFilter).Msg("Crossref search initiated")
		fmt.Printf("Searching Crossref for: '%s' (rows: %d, filter: '%s')\n", crossrefQuery, crossrefRows, crossrefFilter)

		client := crossref.NewClient(crossrefMailto)
		params := common.SearchParams{
			Query:      crossrefQuery,
			MaxResults: crossrefRows,
			Filters: map[string]string{
				"filter": crossrefFilter,
			},
			EmailAddr: crossrefMailto,
		}

		results, err := client.Search(params)
		if err != nil {
			log.Error().Err(err).Msg("Crossref search failed")
			fmt.Printf("Error: %s\n", err.Error())
			return
		}

		if len(results) == 0 {
			fmt.Println("No results found or error in response message.")
			log.Info().Msg("No results in Crossref response")
			return
		}

		fmt.Printf("\nFound results (showing %d):\n", len(results))
		fmt.Println("--------------------------------------------------")

		for i, result := range results {
			fmt.Printf("Result %d:\n", i+1)
			fmt.Printf("  Title: %s\n", result.Title)
			fmt.Printf("  DOI: %s\n", result.DOI)
			if result.SourceURL != "" {
				fmt.Printf("  URL: %s\n", result.SourceURL)
			}
			if len(result.Authors) > 0 {
				fmt.Printf("  Authors: %s\n", strings.Join(result.Authors, ", "))
			}
			if publisher, ok := result.Metadata["publisher"].(string); ok {
				fmt.Printf("  Publisher: %s\n", publisher)
			}
			fmt.Printf("  Type: %s\n", result.Type)
			if result.Published != "" {
				fmt.Printf("  Published: %s\n", result.Published)
			}
			if result.Abstract != "" {
				fmt.Printf("  Abstract: %s\n", result.Abstract)
			}
			if result.PDFURL != "" {
				fmt.Printf("  PDF URL: %s\n", result.PDFURL)
			}
			fmt.Println("--------------------------------------------------")
		}
	},
}

func init() {
	rootCmd.AddCommand(crossrefCmd)
	crossrefCmd.Flags().StringVarP(&crossrefQuery, "query", "q", "", "Search query for Crossref (required)")
	crossrefCmd.Flags().IntVarP(&crossrefRows, "rows", "n", 10, "Number of results to return")
	crossrefCmd.Flags().StringVarP(&crossrefMailto, "mailto", "m", "", "Email address for Crossref polite pool (recommended)")
	crossrefCmd.Flags().StringVarP(&crossrefFilter, "filter", "f", "", "Filter parameters for Crossref (e.g., from-pub-date:2020,type:journal-article)")
}