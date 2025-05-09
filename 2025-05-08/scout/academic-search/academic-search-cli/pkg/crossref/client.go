package crossref

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"time"
)

const (
	baseURL = "https://api.crossref.org/works"
)

// Client represents a Crossref API client
type Client struct {
	httpClient *http.Client
	userAgent  string
}

// NewClient creates a new Crossref API client
func NewClient(email string) *Client {
	userAgent := "AcademicSearchCLI"
	if email != "" {
		userAgent += " (" + email + ")"
	}
	
	return &Client{
		httpClient: &http.Client{
			Timeout: 30 * time.Second,
		},
		userAgent: userAgent,
	}
}

// SearchResponse represents the top-level response from Crossref
type SearchResponse struct {
	Status          string      `json:"status"`
	MessageType     string      `json:"message-type"`
	MessageVersion  string      `json:"message-version"`
	Message         Message     `json:"message"`
}

// Message contains the search results
type Message struct {
	Facets         interface{}  `json:"facets"`
	TotalResults   int          `json:"total-results"`
	Items          []Work       `json:"items"`
	NextCursor     string       `json:"next-cursor"`
	ItemsPerPage   int          `json:"items-per-page"`
	Query          QueryInfo    `json:"query"`
}

// QueryInfo contains information about the search query
type QueryInfo struct {
	StartIndex     int          `json:"start-index"`
	SearchTerms    string       `json:"search-terms"`
}

// Work represents a scholarly work in the Crossref response
type Work struct {
	DOI            string       `json:"DOI"`
	URL            string       `json:"URL"`
	Title          []string     `json:"title"`
	Author         []Author     `json:"author"`
	Published      Published    `json:"published"`
	Publisher      string       `json:"publisher"`
	Type           string       `json:"type"`
	Link           []Link       `json:"link"`
	Abstract       string       `json:"abstract"`
}

// Author represents an author of a scholarly work
type Author struct {
	Given          string       `json:"given"`
	Family         string       `json:"family"`
	Sequence       string       `json:"sequence"`
	ORCID          string       `json:"ORCID"`
}

// Published contains publication date information
type Published struct {
	DateParts      [][]int      `json:"date-parts"`
}

// Link represents a link to the work
type Link struct {
	URL            string       `json:"URL"`
	ContentType    string       `json:"content-type"`
}

// SearchOptions represents the search parameters for Crossref
type SearchOptions struct {
	Query         string
	Rows          int
	Offset        int
	Sort          string
	Order         string
	Filter        string
}

// Search searches for works on Crossref using the given options
func (c *Client) Search(opts SearchOptions) (*SearchResponse, error) {
	if opts.Rows <= 0 {
		opts.Rows = 20
	}

	query := url.Values{}
	query.Set("query", opts.Query)
	query.Set("rows", fmt.Sprintf("%d", opts.Rows))
	query.Set("offset", fmt.Sprintf("%d", opts.Offset))
	
	if opts.Sort != "" {
		query.Set("sort", opts.Sort)
		if opts.Order != "" {
			query.Set("order", opts.Order)
		}
	}
	
	if opts.Filter != "" {
		query.Set("filter", opts.Filter)
	}

	req, err := http.NewRequest("GET", baseURL+"?"+query.Encode(), nil)
	if err != nil {
		return nil, err
	}
	
	// Set user agent as per Crossref etiquette
	req.Header.Set("User-Agent", c.userAgent)

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("crossref API error: %s, status code: %d", string(body), resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	var searchResp SearchResponse
	err = json.Unmarshal(body, &searchResp)
	if err != nil {
		return nil, err
	}

	return &searchResp, nil
}

// GetTitle returns the title of the work
func GetTitle(work Work) string {
	if len(work.Title) > 0 {
		return work.Title[0]
	}
	return "[No Title]"
}

// GetAuthors returns a formatted string of all authors
func GetAuthors(work Work) string {
	result := ""
	for i, author := range work.Author {
		if i > 0 {
			result += ", "
		}
		result += fmt.Sprintf("%s %s", author.Given, author.Family)
	}
	return result
}

// GetYear returns the publication year
func GetYear(work Work) int {
	if len(work.Published.DateParts) > 0 && len(work.Published.DateParts[0]) > 0 {
		return work.Published.DateParts[0][0]
	}
	return 0
}