package cmd

import (
	"fmt"
	"os"
	"strings"

	"arxiv-libgen-cli/pkg/arxiv"
	"arxiv-libgen-cli/pkg/common"

	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

var arxivQuery string
var arxivMaxResults int

// arxivCmd represents the arxiv command
var arxivCmd = &cobra.Command{
	Use:   "arxiv",
	Short: "Search for scientific papers on Arxiv",
	Long: `Search for scientific papers on Arxiv using its public API.

Example:
  arxiv-libgen-cli arxiv --query "all:electron" --max_results 5
  arxiv-libgen-cli arxiv -q "ti:large language models AND au:Hinton" -n 3`,
	Run: func(cmd *cobra.Command, args []string) {
		if arxivQuery == "" {
			fmt.Println("Error: query cannot be empty.")
			cmd.Help()
			os.Exit(1)
		}
		log.Debug().Str("query", arxivQuery).Int("max_results", arxivMaxResults).Msg("Arxiv search initiated")

		fmt.Printf("Searching Arxiv for: '%s' (max results: %d)\n", arxivQuery, arxivMaxResults)

		client := arxiv.NewClient()
		params := common.SearchParams{
			Query:      arxivQuery,
			MaxResults: arxivMaxResults,
		}

		results, err := client.Search(params)
		if err != nil {
			log.Fatal().Err(err).Msg("Failed to search Arxiv")
			os.Exit(1)
		}

		if len(results) == 0 {
			fmt.Println("No results found.")
			log.Info().Msg("No results found in Arxiv response")
			return
		}

		fmt.Printf("\nFound %d results (showing up to %d):\n", len(results), len(results))
		fmt.Println("--------------------------------------------------")

		for i, result := range results {
			fmt.Printf("Result %d:\n", i+1)
			fmt.Printf("  Title: %s\n", result.Title)
			fmt.Printf("  Authors: %s\n", strings.Join(result.Authors, ", "))
			fmt.Printf("  Published: %s\n", result.Published)
			if updated, ok := result.Metadata["updated"].(string); ok {
				fmt.Printf("  Updated: %s\n", updated)
			}
			fmt.Printf("  ID: %s\n", result.SourceURL)
			if result.PDFURL != "" {
				fmt.Printf("  PDF Link: %s\n", result.PDFURL)
			}
			fmt.Printf("  Abstract: %s\n", result.Abstract)
			fmt.Println("--------------------------------------------------")
		}
		fmt.Println("\nThank you to arXiv for use of its open access interoperability.")
	},
}

func init() {
	rootCmd.AddCommand(arxivCmd)
	arxivCmd.Flags().StringVarP(&arxivQuery, "query", "q", "", "Search query for Arxiv (e.g., 'all:electron', 'ti:\"quantum computing\" AND au:\"John Preskill\"') (required)")
	arxivCmd.Flags().IntVarP(&arxivMaxResults, "max_results", "n", 10, "Maximum number of results to return")
}