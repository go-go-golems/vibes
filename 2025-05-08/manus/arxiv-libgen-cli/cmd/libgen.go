package cmd

import (
	"fmt"
	"os"
	"strings"

	"arxiv-libgen-cli/pkg/common"
	"arxiv-libgen-cli/pkg/libgen"

	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

var libgenQuery string
var libgenMaxResults int
var libgenMirror string

// libgenCmd represents the libgen command
var libgenCmd = &cobra.Command{
	Use:   "libgen",
	Short: "Search for scientific papers on LibGen (via web scraping)",
	Long: `Search for scientific papers on Library Genesis mirrors by scraping search results.

Example:
  arxiv-libgen-cli libgen --query "artificial intelligence" --max_results 5
  arxiv-libgen-cli libgen -q "quantum entanglement" -n 3 --mirror "https://libgen.is"`,
	Run: func(cmd *cobra.Command, args []string) {
		if libgenQuery == "" {
			fmt.Println("Error: query cannot be empty.")
			cmd.Help()
			os.Exit(1)
		}
		log.Debug().Str("query", libgenQuery).Int("max_results", libgenMaxResults).Str("mirror", libgenMirror).Msg("LibGen search initiated")

		fmt.Printf("Searching LibGen mirror at %s for:\n'%s '\n(max results: %d)\n", libgenMirror, libgenQuery, libgenMaxResults)
		fmt.Println("Note: LibGen search is via web scraping and may be slow or break if the site structure changes.")
		
		client := libgen.NewClient(libgenMirror)
		params := common.SearchParams{
			Query:      libgenQuery,
			MaxResults: libgenMaxResults,
		}

		results, err := client.Search(params)
		if err != nil {
			log.Error().Err(err).Msg("LibGen search failed")
			fmt.Printf("Error: %s\n", err.Error())
			return
		}

		if len(results) == 0 {
			fmt.Println("No results found, or failed to parse results from the mirror.")
			fmt.Println("The structure of LibGen mirrors changes frequently. This scraper might need an update.")
			log.Info().Msg("No entries parsed from LibGen HTML")
			return
		}
		log.Debug().Int("num_entries_parsed", len(results)).Msg("Finished parsing LibGen results")

		fmt.Printf("\nFound %d results (showing up to %d):\n", len(results), libgenMaxResults)
		fmt.Println("--------------------------------------------------")

		maxShow := min(len(results), libgenMaxResults)
		for i := 0; i < maxShow; i++ {
			result := results[i]
			fmt.Printf("Result %d:\n", i+1)
			fmt.Printf("  Title: %s\n", result.Title)
			fmt.Printf("  Authors: %s\n", strings.Join(result.Authors, ", "))
			if result.JournalInfo != "" { fmt.Printf("  Journal Info: %s\n", result.JournalInfo) }
			if result.DOI != "" { fmt.Printf("  DOI: %s\n", result.DOI) }
			if result.FileSize != "" { fmt.Printf("  File Size: %s\n", result.FileSize) }
			if result.SourceURL != "" { fmt.Printf("  Article/Details Link: %s\n", result.SourceURL) }
			if editLink, ok := result.Metadata["edit_link"].(string); ok && editLink != "" {
				fmt.Printf("  Edit Metadata Link: %s\n", editLink)
			}
			if mirrorLinks, ok := result.Metadata["mirror_links"].([]string); ok && len(mirrorLinks) > 0 {
				fmt.Printf("  Mirror Links: %s\n", strings.Join(mirrorLinks, ", "))
			}
			fmt.Println("--------------------------------------------------")
		}
	},
}

func init() {
	rootCmd.AddCommand(libgenCmd)
	libgenCmd.Flags().StringVarP(&libgenQuery, "query", "q", "", "Search query for LibGen (e.g., \"artificial intelligence\", \"ISBN:9783319994912\") (required)")
	libgenCmd.Flags().IntVarP(&libgenMaxResults, "max_results", "n", 10, "Maximum number of results to display")
	libgenCmd.Flags().StringVarP(&libgenMirror, "mirror", "m", "https://libgen.is", "LibGen mirror URL (e.g., https://libgen.is, http://libgen.st)")
}

// min returns the smaller of a and b
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}