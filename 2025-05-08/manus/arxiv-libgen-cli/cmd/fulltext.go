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
	fullTextDOI      string
	fullTextTitle    string
	fullTextVersion  string
	fullTextJsonOutput bool
)

// fullTextCmd represents the fulltext command
var fullTextCmd = &cobra.Command{
	Use:   "fulltext",
	Short: "Find full text URL for a scholarly work",
	Long: `Find the best PDF or HTML URL for a scholarly work, checking
open access sources first and falling back to LibGen if necessary.

Provide either a DOI or a title to search for. By default, the
published version is preferred over accepted or submitted versions.

Example:
  arxiv-libgen-cli fulltext --doi "10.1038/nphys1170"
  arxiv-libgen-cli fulltext --title "The rise of quantum biology" --version accepted`,
	Run: func(cmd *cobra.Command, args []string) {
		if fullTextDOI == "" && fullTextTitle == "" {
			fmt.Println("Error: either DOI or title must be provided")
			cmd.Help()
			os.Exit(1)
		}

		log.Debug().Str("doi", fullTextDOI).Str("title", fullTextTitle).Str("version", fullTextVersion).Msg("Finding full text")

		req := scholarly.FindFullTextRequest{
			DOI:          fullTextDOI,
			Title:        fullTextTitle,
			PreferVersion: fullTextVersion,
		}

		response, err := scholarly.FindFullText(req)
		if err != nil {
			log.Error().Err(err).Msg("Failed to find full text")
			fmt.Printf("Error: %s\n", err.Error())
			os.Exit(1)
		}

		if fullTextJsonOutput {
			printFullTextAsJSON(response)
		} else {
			printFullTextNice(response)
		}
	},
}

func init() {
	rootCmd.AddCommand(fullTextCmd)

	fullTextCmd.Flags().StringVarP(&fullTextDOI, "doi", "i", "", "DOI of the work")
	fullTextCmd.Flags().StringVarP(&fullTextTitle, "title", "t", "", "Title of the work")
	fullTextCmd.Flags().StringVarP(&fullTextVersion, "version", "v", "published", "Preferred version (published, accepted, submitted)")
	fullTextCmd.Flags().BoolVarP(&fullTextJsonOutput, "json", "j", false, "Output as JSON")
}

// printFullTextAsJSON prints the full text info as JSON
func printFullTextAsJSON(response *scholarly.FindFullTextResponse) {
	jsonData, err := json.MarshalIndent(response, "", "  ")
	if err != nil {
		log.Error().Err(err).Msg("Failed to marshal full text info to JSON")
		fmt.Println("Error formatting full text info as JSON")
		return
	}

	fmt.Println(string(jsonData))
}

// printFullTextNice prints the full text info in a nice format
func printFullTextNice(response *scholarly.FindFullTextResponse) {
	fmt.Println("--------------------------------------------------")
	fmt.Println("Full Text URL Found!")
	
	// Show URL type (PDF or HTML)
	fileType := "PDF"
	if !response.IsPDF {
		fileType = "HTML"
	}
	
	fmt.Printf("%s URL: %s\n", fileType, response.PDFURL)
	fmt.Printf("Source: %s\n", response.Source)
	
	if response.OAStatus != "" {
		fmt.Printf("Open Access Status: %s\n", response.OAStatus)
	}
	
	if response.License != "" {
		fmt.Printf("License: %s\n", response.License)
	}
	
	if response.MD5 != "" {
		fmt.Printf("MD5: %s\n", response.MD5)
	}
	
	fmt.Println("--------------------------------------------------")

	// Add a note if the source is LibGen
	if response.Source == "libgen" {
		fmt.Println("\nNote: This URL was obtained from LibGen. Please respect copyright laws\nand the terms of use for the content you access.")
	}
}