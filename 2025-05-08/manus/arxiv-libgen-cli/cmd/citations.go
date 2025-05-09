package cmd

import (
	"arxiv-libgen-cli/pkg/scholarly"
	"encoding/json"
	"fmt"
	"os"

	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

var (
	citationsWorkID    string
	citationsDirection string
	citationsLimit     int
	citationsJsonOutput bool
)

// citationsCmd represents the citations command
var citationsCmd = &cobra.Command{
	Use:   "citations",
	Short: "Get citations for a scholarly work",
	Long: `Retrieve one hop of the citation graph - either works this paper cites
(outgoing references) or works that cite it (incoming citations).

Example:
  arxiv-libgen-cli citations --id "10.1038/nphys1170" --direction refs
  arxiv-libgen-cli citations --id "W2741809809" --direction cited_by --limit 20`,
	Run: func(cmd *cobra.Command, args []string) {
		if citationsWorkID == "" {
			fmt.Println("Error: work_id cannot be empty")
			cmd.Help()
			os.Exit(1)
		}

		if citationsDirection != "refs" && citationsDirection != "cited_by" {
			fmt.Printf("Error: direction must be either 'refs' or 'cited_by', got '%s'\n", citationsDirection)
			cmd.Help()
			os.Exit(1)
		}

		log.Debug().Str("work_id", citationsWorkID).Str("direction", citationsDirection).Int("limit", citationsLimit).Msg("Getting citations")

		req := scholarly.GetCitationsRequest{
			WorkID:    citationsWorkID,
			Direction: citationsDirection,
			Limit:     citationsLimit,
		}

		response, err := scholarly.GetCitations(req)
		if err != nil {
			log.Error().Err(err).Msg("Failed to get citations")
			fmt.Printf("Error: %s\n", err.Error())
			os.Exit(1)
		}

		if len(response.Citations) == 0 {
			fmt.Printf("No %s found for the work.\n", getDirectionDisplayName(citationsDirection))
			return
		}

		if citationsJsonOutput {
			printCitationsAsJSON(response)
		} else {
			printCitationsNice(response, citationsDirection)
		}
	},
}

func init() {
	rootCmd.AddCommand(citationsCmd)

	citationsCmd.Flags().StringVarP(&citationsWorkID, "id", "i", "", "Work ID (DOI or OpenAlex ID) (required)")
	citationsCmd.Flags().StringVarP(&citationsDirection, "direction", "r", "cited_by", "Citation direction: 'refs' (outgoing) or 'cited_by' (incoming)")
	citationsCmd.Flags().IntVarP(&citationsLimit, "limit", "l", 20, "Maximum number of citations to return")
	citationsCmd.Flags().BoolVarP(&citationsJsonOutput, "json", "j", false, "Output as JSON")

	citationsCmd.MarkFlagRequired("id")
}

// getDirectionDisplayName returns a user-friendly name for the citation direction
func getDirectionDisplayName(direction string) string {
	if direction == "refs" {
		return "references"
	}
	return "citations"
}

// printCitationsAsJSON prints the citations as JSON
func printCitationsAsJSON(response *scholarly.GetCitationsResponse) {
	jsonData, err := json.MarshalIndent(response, "", "  ")
	if err != nil {
		log.Error().Err(err).Msg("Failed to marshal citations to JSON")
		fmt.Println("Error formatting citations as JSON")
		return
	}

	fmt.Println(string(jsonData))
}

// printCitationsNice prints the citations in a nice format
func printCitationsNice(response *scholarly.GetCitationsResponse, direction string) {
	directionName := getDirectionDisplayName(direction)
	fmt.Printf("Found %d %s:\n", len(response.Citations), directionName)
	fmt.Println("--------------------------------------------------")

	for i, citation := range response.Citations {
		fmt.Printf("%d. %s\n", i+1, citation.Title)
		if citation.DOI != "" {
			fmt.Printf("   DOI: %s\n", citation.DOI)
		}
		if citation.Year > 0 {
			fmt.Printf("   Year: %d\n", citation.Year)
		}
		if citation.ID != "" {
			fmt.Printf("   ID: %s\n", citation.ID)
		}
		fmt.Println("--------------------------------------------------")
	}

	if response.NextCursor != "" {
		fmt.Printf("\nMore results available. Use cursor token for pagination.\n")
	}
}