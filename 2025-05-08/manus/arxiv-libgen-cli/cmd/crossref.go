package cmd

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"strings"

	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

var crossrefQuery string
var crossrefRows int
var crossrefMailto string
var crossrefFilter string

// CrossrefResponse represents the top-level structure of the Crossref API response.
type CrossrefResponse struct {
	Status  string         `json:"status"`
	MessageType string `json:"message-type"`
	MessageVersion string `json:"message-version"`
	Message CrossrefMessage `json:"message"`
}

// CrossrefMessage contains the actual search results.
type CrossrefMessage struct {
	TotalResults int             `json:"total-results"`
	Items        []CrossrefItem  `json:"items"`
	Query        CrossrefQuery   `json:"query"`
	ItemsPerPage int `json:"items-per-page"`
}

// CrossrefQuery details the query performed.
type CrossrefQuery struct {
	SearchTerms string `json:"search-terms"`
	StartIndex int `json:"start-index"`
}

// CrossrefItem represents a single work item from Crossref.
type CrossrefItem struct {
	DOI       string   `json:"DOI"`
	Title     []string `json:"title"`
	Author    []CrossrefAuthor `json:"author,omitempty"`
	Publisher string   `json:"publisher"`
	Type      string   `json:"type"`
	Created   CrossrefDate   `json:"created,omitempty"`
	Issued    CrossrefDateParts `json:"issued,omitempty"` // Contains date-parts
	URL       string   `json:"URL"`
	Abstract  string   `json:"abstract,omitempty"` // May not always be present or may be truncated
	ISSN      []string `json:"ISSN,omitempty"`
	ISBN      []string `json:"ISBN,omitempty"`
	Subject   []string `json:"subject,omitempty"`
	Link      []CrossrefLink `json:"link,omitempty"`
}

// CrossrefAuthor represents an author.
type CrossrefAuthor struct {
	Given   string `json:"given,omitempty"`
	Family  string `json:"family,omitempty"`
	Sequence string `json:"sequence,omitempty"`
	Affiliation []CrossrefAffiliation `json:"affiliation,omitempty"`
}

// CrossrefAffiliation represents an author's affiliation.
type CrossrefAffiliation struct {
	Name string `json:"name,omitempty"`
}

// CrossrefDate represents a date object in Crossref responses.
type CrossrefDate struct {
	DateParts [][]int `json:"date-parts,omitempty"`
	DateTime  string  `json:"date-time,omitempty"`
	Timestamp int64   `json:"timestamp,omitempty"`
}

// CrossrefDateParts is specifically for the "issued" field which often has only date-parts.
type CrossrefDateParts struct {
	DateParts [][]int `json:"date-parts,omitempty"`
}

// CrossrefLink represents a link associated with a work.
type CrossrefLink struct {
	URL string `json:"URL"`
	ContentType string `json:"content-type,omitempty"`
	ContentVersion string `json:"content-version,omitempty"`
	IntendedApplication string `json:"intended-application,omitempty"`
}


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

		baseURL := "https://api.crossref.org/works"
		params := url.Values{}
		params.Add("query", crossrefQuery)
		params.Add("rows", fmt.Sprintf("%d", crossrefRows))
		if crossrefMailto != "" {
			params.Add("mailto", crossrefMailto)
		}
		if crossrefFilter != "" {
			params.Add("filter", crossrefFilter)
		}
		// Add select for common fields to keep response manageable, user can override with more specific tools if needed
		params.Add("select", "DOI,title,author,publisher,type,created,issued,URL,abstract,ISSN,ISBN,subject,link")

		apiURL := baseURL + "?" + params.Encode()
		log.Debug().Str("url", apiURL).Msg("Requesting Crossref API URL")
		fmt.Printf("Searching Crossref for: '%s' (rows: %d, filter: '%s')\n", crossrefQuery, crossrefRows, crossrefFilter)
		fmt.Println("Requesting URL:", apiURL)

		client := &http.Client{}
		req, err := http.NewRequest("GET", apiURL, nil)
		if err != nil {
			log.Fatal().Err(err).Msg("Error creating Crossref API request")
		}
		if crossrefMailto != "" { // As per docs, User-Agent with mailto is good practice
			req.Header.Set("User-Agent", fmt.Sprintf("arxiv-libgen-cli/0.1 (mailto:%s)", crossrefMailto))
		} else {
			req.Header.Set("User-Agent", "arxiv-libgen-cli/0.1 (https://github.com/user/repo - please update with actual repo if public)")
		}

		resp, err := client.Do(req)
		if err != nil {
			log.Fatal().Err(err).Msg("Error making GET request to Crossref API")
		}
		defer resp.Body.Close()

		log.Debug().Str("status", resp.Status).Int("statusCode", resp.StatusCode).Msg("Crossref API response received")

		if resp.StatusCode != http.StatusOK {
			bodyBytes, _ := ioutil.ReadAll(resp.Body)
			log.Error().Str("status", resp.Status).Bytes("body", bodyBytes).Msg("Crossref API request failed")
			fmt.Printf("Error: Crossref API request failed with status %s\n", resp.Status)
			return
		}

		body, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			log.Fatal().Err(err).Msg("Error reading Crossref API response body")
		}

		var crossrefResp CrossrefResponse
		if err := json.Unmarshal(body, &crossrefResp); err != nil {
			log.Error().Err(err).Bytes("body", body).Msg("Error unmarshalling Crossref JSON response")
			fmt.Println("Error parsing Crossref API response.")
			return
		}

		if crossrefResp.Status != "ok" || len(crossrefResp.Message.Items) == 0 {
			fmt.Println("No results found or error in response message.")
			log.Info().Interface("response", crossrefResp).Msg("No results or non-ok status in Crossref response")
			return
		}

		fmt.Printf("\nFound %d total results (showing up to %d):\n", crossrefResp.Message.TotalResults, len(crossrefResp.Message.Items))
		fmt.Println("--------------------------------------------------")

		for i, item := range crossrefResp.Message.Items {
			fmt.Printf("Result %d:\n", i+1)
			if len(item.Title) > 0 {
				fmt.Printf("  Title: %s\n", strings.Join(item.Title, "; "))
			}
			fmt.Printf("  DOI: %s\n", item.DOI)
			if item.URL != "" {
				fmt.Printf("  URL: %s\n", item.URL)
			}
			var authors []string
			for _, author := range item.Author {
				authors = append(authors, fmt.Sprintf("%s %s", author.Given, author.Family))
			}
			if len(authors) > 0 {
				fmt.Printf("  Authors: %s\n", strings.Join(authors, ", "))
			}
			fmt.Printf("  Publisher: %s\n", item.Publisher)
			fmt.Printf("  Type: %s\n", item.Type)
			if item.Issued.DateParts != nil && len(item.Issued.DateParts) > 0 && len(item.Issued.DateParts[0]) > 0 {
				dateStr := fmt.Sprintf("%d", item.Issued.DateParts[0][0])
				if len(item.Issued.DateParts[0]) > 1 {
					dateStr += fmt.Sprintf("-%02d", item.Issued.DateParts[0][1])
				}
				if len(item.Issued.DateParts[0]) > 2 {
					dateStr += fmt.Sprintf("-%02d", item.Issued.DateParts[0][2])
				}
				fmt.Printf("  Issued Date: %s\n", dateStr)
			} else if item.Created.DateTime != "" {
				fmt.Printf("  Created Date: %s\n", item.Created.DateTime)
			}
			if item.Abstract != "" {
				// Clean up abstract, remove potential HTML tags (simple replace)
				cleanAbstract := strings.ReplaceAll(item.Abstract, "<jats:p>", "")
				cleanAbstract = strings.ReplaceAll(cleanAbstract, "</jats:p>", "")
				cleanAbstract = strings.ReplaceAll(cleanAbstract, "<title>", "")
				cleanAbstract = strings.ReplaceAll(cleanAbstract, "</title>", "")
				fmt.Printf("  Abstract: %s\n", strings.Join(strings.Fields(cleanAbstract), " "))
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
	// crossrefCmd.MarkFlagRequired("query") // Cobra doesn't enforce this without RunE returning error
}

