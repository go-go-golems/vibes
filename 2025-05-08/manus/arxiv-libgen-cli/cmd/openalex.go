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

var openalexQuery string
var openalexPerPage int
var openalexMailto string
var openalexFilter string
var openalexSort string

// OpenAlexResponse represents the top-level structure of the OpenAlex API response for works.
type OpenAlexResponse struct {
	Meta    OpenAlexMeta     `json:"meta"`
	Results []OpenAlexWork   `json:"results"`
	GroupBy []interface{}    `json:"group_by"`
}

// OpenAlexMeta contains metadata about the response.
type OpenAlexMeta struct {
	Count            int    `json:"count"`
	DBResponseTimeMs int    `json:"db_response_time_ms"`
	Page             int    `json:"page"`
	PerPage          int    `json:"per_page"`
	NextCursor       string `json:"next_cursor,omitempty"`
}

// OpenAlexAuthorship links authors to works.
type OpenAlexAuthorship struct {
	AuthorPosition       string                `json:"author_position"`
	Author               OpenAlexAuthor        `json:"author"`
	Institutions         []OpenAlexInstitution `json:"institutions,omitempty"`
	RawAffiliationString string                `json:"raw_affiliation_string,omitempty"`
}

// OpenAlexAuthor represents an author.
type OpenAlexAuthor struct {
	ID          string `json:"id"`
	DisplayName string `json:"display_name"`
	Orcid       string `json:"orcid,omitempty"`
}

// OpenAlexInstitution represents an institution.
type OpenAlexInstitution struct {
	ID          string `json:"id"`
	DisplayName string `json:"display_name"`
	Ror         string `json:"ror,omitempty"`
	CountryCode string `json:"country_code,omitempty"`
	Type        string `json:"type,omitempty"`
}

// OpenAlexOpenAccess contains OA details.
type OpenAlexOpenAccess struct {
	IsOA                     bool   `json:"is_oa"`
	OAStatus                 string `json:"oa_status"`
	OAURL                    string `json:"oa_url,omitempty"`
	AnyRepositoryHasFulltext bool   `json:"any_repository_has_fulltext,omitempty"`
}

// OpenAlexSourceInfo is for the "source" object nested within primary_location.
type OpenAlexSourceInfo struct {
	ID                   string   `json:"id,omitempty"`
	DisplayName          string   `json:"display_name,omitempty"`
	IssnL                string   `json:"issn_l,omitempty"`
	Issn                 []string `json:"issn,omitempty"`
	IsOA                 bool     `json:"is_oa,omitempty"`
	IsInDoaj             bool     `json:"is_in_doaj,omitempty"`
	HostOrganizationName string   `json:"host_organization_name,omitempty"`
	Type                 string   `json:"type,omitempty"`
	Publisher            string   `json:"publisher,omitempty"`
}

// OpenAlexPrimaryLocation represents the primary_location object.
type OpenAlexPrimaryLocation struct {
	IsOA           bool                `json:"is_oa"`
	LandingPageURL string              `json:"landing_page_url,omitempty"`
	PdfURL         string              `json:"pdf_url,omitempty"`
	Source         *OpenAlexSourceInfo `json:"source,omitempty"`
	License        string              `json:"license,omitempty"`
	Version        string              `json:"version,omitempty"`
}

// OpenAlexConcept represents a concept associated with a work.
type OpenAlexConcept struct {
	ID          string  `json:"id"`
	DisplayName string  `json:"display_name"`
	Level       int     `json:"level"`
	Score       float32 `json:"score"`
}

// OpenAlexWork represents a single scholarly work.
type OpenAlexWork struct {
	ID                    string                  `json:"id"`
	DOI                   string                  `json:"doi"`
	Title                 string                  `json:"title"` 
	DisplayName           string                  `json:"display_name"`
	PublicationYear       int                     `json:"publication_year"`
	PublicationDate       string                  `json:"publication_date"`
	CitedByCount          int                     `json:"cited_by_count"`
	Authorships           []OpenAlexAuthorship    `json:"authorships"`
	PrimaryLocation       *OpenAlexPrimaryLocation  `json:"primary_location,omitempty"`
	OpenAccess            *OpenAlexOpenAccess     `json:"open_access,omitempty"`
	Type                  string                  `json:"type"`
	AbstractInvertedIndex map[string][]int        `json:"abstract_inverted_index,omitempty"`
	Abstract              string                  // This is reconstructed, not directly from JSON
	Concepts              []OpenAlexConcept       `json:"concepts,omitempty"`
	ReferencedWorks       []string                `json:"referenced_works,omitempty"`
	RelatedWorks          []string                `json:"related_works,omitempty"`
	RelevanceScore        float64                 `json:"relevance_score,omitempty"`
}

func reconstructAbstract(invertedIndex map[string][]int) string {
	if invertedIndex == nil || len(invertedIndex) == 0 {
		return "(Abstract not available or not reconstructible from provided data)"
	}
	maxLength := 0
	for _, positions := range invertedIndex {
		for _, pos := range positions {
			if pos+1 > maxLength {
				maxLength = pos + 1
			}
		}
	}
	if maxLength == 0 {
		return "(Abstract data present but could not be ordered)"
	}
	orderedWords := make([]string, maxLength)
	for word, positions := range invertedIndex {
		for _, pos := range positions {
			if pos < maxLength {
				orderedWords[pos] = word
			}
		}
	}
	finalWords := []string{}
	for _, w := range orderedWords {
		if w != "" {
			finalWords = append(finalWords, w)
		}
	}
	return strings.Join(finalWords, " ")
}

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

		baseURL := "https://api.openalex.org/works"
		params := url.Values{}
		if openalexQuery != "" {
			params.Add("search", openalexQuery)
		}
		params.Add("per_page", fmt.Sprintf("%d", openalexPerPage))
		if openalexMailto != "" {
			params.Add("mailto", openalexMailto)
		}
		if openalexFilter != "" {
			params.Add("filter", openalexFilter)
		}
		if openalexSort != "" {
			params.Add("sort", openalexSort)
		}
		// Ensure select fields are valid according to OpenAlex documentation
		params.Add("select", "id,doi,title,display_name,publication_year,publication_date,cited_by_count,authorships,primary_location,open_access,type,concepts,abstract_inverted_index,relevance_score,referenced_works,related_works")

		apiURL := baseURL + "?" + params.Encode()
		log.Debug().Str("url", apiURL).Msg("Requesting OpenAlex API URL")

		var queryParts []string
		if openalexQuery != "" {
			queryParts = append(queryParts, fmt.Sprintf("query=\"%s\"", openalexQuery))
		}
		if openalexFilter != "" {
			queryParts = append(queryParts, fmt.Sprintf("filter=\"%s\"", openalexFilter))
		}
		searchDesc := strings.Join(queryParts, ", ")
		// Corrected fmt.Printf line to avoid literal newlines in the format string
		fmt.Printf("Searching OpenAlex for: %s (per_page: %d, sort: '%s')\n", searchDesc, openalexPerPage, openalexSort)
		fmt.Println("Requesting URL:", apiURL)
		client := &http.Client{}
		req, err := http.NewRequest("GET", apiURL, nil)
		if err != nil {
			log.Fatal().Err(err).Msg("Error creating OpenAlex API request")
		}
		if openalexMailto != "" {
			req.Header.Set("User-Agent", fmt.Sprintf("arxiv-libgen-cli/0.1 (mailto:%s)", openalexMailto))
		} else {
			req.Header.Set("User-Agent", "arxiv-libgen-cli/0.1 (https://github.com/user/repo - please update with actual repo if public)")
		}

		resp, err := client.Do(req)
		if err != nil {
			log.Fatal().Err(err).Msg("Error making GET request to OpenAlex API")
		}
		defer resp.Body.Close()

		log.Debug().Str("status", resp.Status).Int("statusCode", resp.StatusCode).Msg("OpenAlex API response received")

		if resp.StatusCode != http.StatusOK {
			bodyBytes, _ := ioutil.ReadAll(resp.Body)
			log.Error().Str("status", resp.Status).Bytes("body", bodyBytes).Msg("OpenAlex API request failed")
			fmt.Printf("Error: OpenAlex API request failed with status %s\n", resp.Status)
			return
		}

		body, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			log.Fatal().Err(err).Msg("Error reading OpenAlex API response body")
		}

		var oaResp OpenAlexResponse
		if err := json.Unmarshal(body, &oaResp); err != nil {
			log.Error().Err(err).Str("body_snippet", string(body[:500])).Msg("Error unmarshalling OpenAlex JSON response") 
			fmt.Println("Error parsing OpenAlex API response.")
			return
		}

		if len(oaResp.Results) == 0 {
			fmt.Println("No results found.")
			log.Info().Interface("response_meta", oaResp.Meta).Msg("No results in OpenAlex response")
			return
		}

		fmt.Printf("\nFound %d total results (showing %d in this page):\n", oaResp.Meta.Count, len(oaResp.Results))
		fmt.Println("--------------------------------------------------")

		for i, work := range oaResp.Results {
			fmt.Printf("Result %d:\n", i+1)
			fmt.Printf("  Title: %s\n", work.DisplayName) 
			fmt.Printf("  OpenAlex ID: %s\n", work.ID)
			if work.DOI != "" {
				fmt.Printf("  DOI: %s\n", work.DOI)
			}
			var authors []string
			for _, authorship := range work.Authorships {
				authors = append(authors, authorship.Author.DisplayName)
			}
			if len(authors) > 0 {
				fmt.Printf("  Authors: %s\n", strings.Join(authors, ", "))
			}
			fmt.Printf("  Publication Year: %d\n", work.PublicationYear)
			fmt.Printf("  Type: %s\n", work.Type)
			if work.PrimaryLocation != nil && work.PrimaryLocation.Source != nil && work.PrimaryLocation.Source.DisplayName != "" {
				fmt.Printf("  Venue: %s\n", work.PrimaryLocation.Source.DisplayName)
			} else {
				fmt.Println("  Venue: (Not available)")
			}
			fmt.Printf("  Cited By Count: %d\n", work.CitedByCount)
			if work.OpenAccess != nil {
				fmt.Printf("  Open Access URL: %s (Status: %s, Repository Fulltext: %t)\n", work.OpenAccess.OAURL, work.OpenAccess.OAStatus, work.OpenAccess.AnyRepositoryHasFulltext)
			} else {
				fmt.Println("  Open Access: (Details not available)")
			}
			
			work.Abstract = reconstructAbstract(work.AbstractInvertedIndex)
			fmt.Printf("  Abstract: %s\n", work.Abstract)
			if work.RelevanceScore > 0 {
				fmt.Printf("  Relevance Score: %f\n", work.RelevanceScore)
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

