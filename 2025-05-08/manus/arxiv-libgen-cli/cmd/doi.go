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
	doiString string
	doiJsonOutput bool
)

// doiCmd represents the doi command
var doiCmd = &cobra.Command{
	Use:   "doi",
	Short: "Resolve a DOI to get full metadata",
	Long: `Resolve a DOI to get complete metadata from both Crossref and OpenAlex.

This command fetches metadata from multiple sources and merges them into a single
rich record with information like authors, citations, concepts, and more.

Example:
  arxiv-libgen-cli doi --doi "10.1038/nphys1170"
  arxiv-libgen-cli doi --doi "10.1103/PhysRevLett.116.061102" --json`,
	Run: func(cmd *cobra.Command, args []string) {
		if doiString == "" {
			fmt.Println("Error: DOI cannot be empty")
			cmd.Help()
			os.Exit(1)
		}

		log.Debug().Str("doi", doiString).Msg("Resolving DOI")

		req := scholarly.ResolveDOIRequest{
			DOI: doiString,
		}

		work, err := scholarly.ResolveDOI(req)
		if err != nil {
			log.Error().Err(err).Msg("Failed to resolve DOI")
			fmt.Printf("Error: %s\n", err.Error())
			os.Exit(1)
		}

		if doiJsonOutput {
			printWorkAsJSON(work)
		} else {
			printWorkDetails(work)
		}
	},
}

func init() {
	rootCmd.AddCommand(doiCmd)

	doiCmd.Flags().StringVarP(&doiString, "doi", "i", "", "DOI to resolve (required)")
	doiCmd.Flags().BoolVarP(&doiJsonOutput, "json", "j", false, "Output as JSON")

	doiCmd.MarkFlagRequired("doi")
}

// printWorkAsJSON prints the work as JSON
func printWorkAsJSON(work *scholarly.Work) {
	jsonData, err := json.MarshalIndent(work, "", "  ")
	if err != nil {
		log.Error().Err(err).Msg("Failed to marshal work to JSON")
		fmt.Println("Error formatting work as JSON")
		return
	}

	fmt.Println(string(jsonData))
}

// printWorkDetails prints the work details in a human-readable format
func printWorkDetails(work *scholarly.Work) {
	fmt.Println("--------------------------------------------------")
	fmt.Printf("Title: %s\n", work.Title)
	
	if len(work.Authors) > 0 {
		fmt.Printf("Authors: %s\n", formatAuthors(work.Authors))
	}
	
	if work.Year > 0 {
		fmt.Printf("Year: %d\n", work.Year)
	}
	
	if work.DOI != "" {
		fmt.Printf("DOI: %s\n", work.DOI)
	}
	
	if work.ID != "" {
		fmt.Printf("ID: %s\n", work.ID)
	}
	
	if work.CitationCount > 0 {
		fmt.Printf("Citations: %d\n", work.CitationCount)
	}
	
	if work.IsOA {
		fmt.Printf("Open Access: Yes\n")
	}
	
	if work.PDFURL != "" {
		fmt.Printf("PDF URL: %s\n", work.PDFURL)
	}
	
	if work.Abstract != "" {
		fmt.Printf("\nAbstract:\n%s\n", work.Abstract)
	}
	
	fmt.Printf("\nSource: %s\n", work.SourceName)
	fmt.Println("--------------------------------------------------")
}

// formatAuthors formats the authors list
func formatAuthors(authors []string) string {
	if len(authors) <= 3 {
		return fmt.Sprintf("%s", authors)
	}
	return fmt.Sprintf("%s [+ %d more]", authors[:3], len(authors)-3)
}