package cmd

import (
	"encoding/xml"
	"fmt"
	"io/ioutil"
	// "log" // Replaced by zerolog
	"net/http"
	"net/url"
	"os"
	"strings"

	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

// AtomFeed represents the top-level structure of the Arxiv API response.
// Based on Atom 1.0 and Arxiv API specifics.
type AtomFeed struct {
	XMLName xml.Name `xml:"feed"`
	Title   string   `xml:"title"`
	ID      string   `xml:"id"`
	Updated string   `xml:"updated"`
	Entries []Entry  `xml:"entry"`
	TotalResults int `xml:"totalResults"` // OpenSearch extension
	StartIndex int `xml:"startIndex"` // OpenSearch extension
	ItemsPerPage int `xml:"itemsPerPage"` // OpenSearch extension
}

// Entry represents a single paper in the Arxiv API response.
// Based on Atom 1.0 and Arxiv API specifics.
type Entry struct {
	ID        string    `xml:"id"`        // Usually the Arxiv URL for the paper
	Updated   string    `xml:"updated"`
	Published string    `xml:"published"`
	Title     string    `xml:"title"`
	Summary   string    `xml:"summary"` // Abstract
	Authors   []Author  `xml:"author"`
	DOI       string    `xml:"doi"`       // Arxiv extension
	Comment   string    `xml:"comment"`   // Arxiv extension
	JournalRef string   `xml:"journal_ref"` // Arxiv extension
	Link      []Link    `xml:"link"`
	PrimaryCategory Category `xml:"primary_category"` // Arxiv extension
	Categories []Category `xml:"category"`
}

// Author represents an author of a paper.
// Based on Atom 1.0.
type Author struct {
	Name string `xml:"name"`
}

// Link represents a link related to the paper (e.g., PDF, abstract page).
// Based on Atom 1.0.
type Link struct {
	Href  string `xml:"href,attr"`
	Rel   string `xml:"rel,attr,omitempty"`
	Type  string `xml:"type,attr,omitempty"`
	Title string `xml:"title,attr,omitempty"`
}

// Category represents a subject category of the paper.
// Based on Atom 1.0 and Arxiv extension.
type Category struct {
	Term   string `xml:"term,attr"`
	Scheme string `xml:"scheme,attr,omitempty"`
}

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

		baseURL := "http://export.arxiv.org/api/query"
		params := url.Values{}
		params.Add("search_query", arxivQuery)
		params.Add("max_results", fmt.Sprintf("%d", arxivMaxResults))
		params.Add("sortBy", "relevance") // Default sort order

		apiURL := baseURL + "?" + params.Encode()
		log.Debug().Str("url", apiURL).Msg("Requesting Arxiv API URL")
		fmt.Println("Requesting URL:", apiURL)

		// Arxiv API rate limiting: wait 3 seconds between requests.
		// Since this is a single command execution, we don't need to manage state for multiple requests here,
		// but if we were to make repeated calls, this would be important.
		// For now, we'll just proceed. If this tool were to be used in a loop, a time.Sleep(3 * time.Second) would be needed.

		client := &http.Client{}
		req, err := http.NewRequest("GET", apiURL, nil)
		if err != nil {
			log.Fatal().Err(err).Msg("Error creating Arxiv API request")
		}
		// Set a User-Agent
		req.Header.Set("User-Agent", "arxiv-libgen-cli/0.1 (https://github.com/user/repo - please update with actual repo if public)")

		resp, err := client.Do(req)
		if err != nil {
			log.Fatal().Err(err).Msg("Error making GET request to Arxiv API")
		}
		defer resp.Body.Close()
		log.Debug().Str("status", resp.Status).Int("statusCode", resp.StatusCode).Msg("Arxiv API response received")

		if resp.StatusCode != http.StatusOK {
			bodyBytes, _ := ioutil.ReadAll(resp.Body)
			log.Error().Str("status", resp.Status).Bytes("body", bodyBytes).Msg("Arxiv API request failed")
			fmt.Printf("Error: Arxiv API request failed with status %s\n", resp.Status)
			return
		}

		body, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			log.Fatal().Err(err).Msg("Error reading Arxiv API response body")
		}
		log.Debug().Int("body_size", len(body)).Msg("Arxiv API response body read")

		var feed AtomFeed
		if err := xml.Unmarshal(body, &feed); err != nil {
			log.Error().Err(err).Str("body_prefix", string(body[:500])).Msg("Error unmarshalling Arxiv XML response") // Log prefix of body
			fmt.Println("Error parsing Arxiv API response.")
			return
		}

		if len(feed.Entries) == 0 {
			fmt.Println("No results found.")
			log.Info().Msg("No results found in Arxiv response")
			return
		}

		log.Debug().Int("total_results_api", feed.TotalResults).Int("entries_in_page", len(feed.Entries)).Msg("Arxiv results parsed")
		fmt.Printf("\nFound %d results (showing up to %d):\n", feed.TotalResults, len(feed.Entries))
		fmt.Println("--------------------------------------------------")

		for i, entry := range feed.Entries {
			fmt.Printf("Result %d:\n", i+1)
			fmt.Printf("  Title: %s\n", strings.Join(strings.Fields(entry.Title), " ")) // Clean up whitespace
			var authors []string
			for _, author := range entry.Authors {
				authors = append(authors, author.Name)
			}
			fmt.Printf("  Authors: %s\n", strings.Join(authors, ", "))
			fmt.Printf("  Published: %s\n", entry.Published)
			fmt.Printf("  Updated: %s\n", entry.Updated)
			fmt.Printf("  ID: %s\n", entry.ID)
			pdfLink := ""
			for _, link := range entry.Link {
				if link.Title == "pdf" {
					pdfLink = link.Href
					break
				}
			}
			if pdfLink != "" {
				fmt.Printf("  PDF Link: %s\n", pdfLink)
			}
			fmt.Printf("  Abstract: %s\n", strings.Join(strings.Fields(entry.Summary), " ")) // Clean up whitespace
			fmt.Println("--------------------------------------------------")
			// Adhere to rate limits if making multiple calls in a loop, not strictly necessary for one-off CLI command.
			// time.Sleep(3 * time.Second) // If this were part of a larger loop of requests
		}
		fmt.Println("\nThank you to arXiv for use of its open access interoperability.")
	},
}

func init() {
	rootCmd.AddCommand(arxivCmd)
	arxivCmd.Flags().StringVarP(&arxivQuery, "query", "q", "", "Search query for Arxiv (e.g., 'all:electron', 'ti:\"quantum computing\" AND au:\"John Preskill\"') (required)")
	arxivCmd.Flags().IntVarP(&arxivMaxResults, "max_results", "n", 10, "Maximum number of results to return")
	// Mark query as required, though Cobra doesn't enforce it programmatically without extra checks in RunE
	// arxivCmd.MarkFlagRequired("query") // This would require RunE to return an error
}

