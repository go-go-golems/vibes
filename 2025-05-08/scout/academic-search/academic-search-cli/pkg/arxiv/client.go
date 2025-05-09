package arxiv

import (
	"encoding/xml"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"time"
)

const (
	baseURL = "http://export.arxiv.org/api/query"
)

// Client represents an arXiv API client
type Client struct {
	httpClient *http.Client
}

// NewClient creates a new arXiv API client
func NewClient() *Client {
	return &Client{
		httpClient: &http.Client{
			Timeout: 30 * time.Second,
		},
	}
}

// Feed represents the top-level response from arXiv
type Feed struct {
	XMLName     xml.Name `xml:"feed"`
	TotalResults int      `xml:"totalResults"`
	StartIndex   int      `xml:"startIndex"`
	ItemsPerPage int      `xml:"itemsPerPage"`
	Entries     []Entry  `xml:"entry"`
}

// Entry represents a paper in the arXiv response
type Entry struct {
	ID        string    `xml:"id"`
	Updated   time.Time `xml:"updated"`
	Published time.Time `xml:"published"`
	Title     string    `xml:"title"`
	Summary   string    `xml:"summary"`
	Authors   []Author  `xml:"author"`
	Links     []Link    `xml:"link"`
	Categories []Category `xml:"category"`
	DOI       string    `xml:"doi"`
}

// Author represents an author of a paper
type Author struct {
	Name string `xml:"name"`
}

// Link represents a link to the paper
type Link struct {
	Href  string `xml:"href,attr"`
	Type  string `xml:"type,attr"`
	Rel   string `xml:"rel,attr"`
	Title string `xml:"title,attr"`
}

// Category represents a category of a paper
type Category struct {
	Term string `xml:"term,attr"`
}

// SearchOptions represents the search parameters for arXiv
type SearchOptions struct {
	Query      string
	Start      int
	MaxResults int
	SortBy     string
	SortOrder  string
}

// Search searches for papers on arXiv using the given options
func (c *Client) Search(opts SearchOptions) (*Feed, error) {
	if opts.MaxResults <= 0 {
		opts.MaxResults = 10
	}

	baseQuery := url.Values{}
	baseQuery.Set("search_query", opts.Query)
	baseQuery.Set("start", fmt.Sprintf("%d", opts.Start))
	baseQuery.Set("max_results", fmt.Sprintf("%d", opts.MaxResults))
	
	if opts.SortBy != "" {
		if opts.SortOrder == "" {
			opts.SortOrder = "descending" 
		}
		baseQuery.Set("sortBy", opts.SortBy)
		baseQuery.Set("sortOrder", opts.SortOrder)
	}

	req, err := http.NewRequest("GET", baseURL+"?"+baseQuery.Encode(), nil)
	if err != nil {
		return nil, err
	}

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("arxiv API error: %s, status code: %d", string(body), resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	var feed Feed
	err = xml.Unmarshal(body, &feed)
	if err != nil {
		return nil, err
	}

	return &feed, nil
}

// GetPaperURL returns the URL for the paper
func GetPaperURL(entry Entry) string {
	for _, link := range entry.Links {
		if link.Type == "text/html" {
			return link.Href
		}
	}
	return strings.Replace(entry.ID, "http://arxiv.org/abs/", "https://arxiv.org/abs/", 1)
}

// GetPDF returns the URL for the PDF of the paper
func GetPDF(entry Entry) string {
	for _, link := range entry.Links {
		if link.Title == "pdf" || strings.Contains(link.Href, "pdf") {
			return link.Href
		}
	}
	return ""
}