package archive

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"time"
)

const (
	baseURL = "https://archive.org/advancedsearch.php"
)

// Client represents an Internet Archive API client
type Client struct {
	httpClient *http.Client
}

// NewClient creates a new Internet Archive API client
func NewClient() *Client {
	return &Client{
		httpClient: &http.Client{
			Timeout: 30 * time.Second,
		},
	}
}

// SearchResponse represents the top-level response from Internet Archive
type SearchResponse struct {
	ResponseHeader ResponseHeader `json:"responseHeader"`
	Response       Response       `json:"response"`
}

// ResponseHeader contains metadata about the response
type ResponseHeader struct {
	Status int               `json:"status"`
	QTime  int               `json:"QTime"`
	Params map[string]string `json:"params"`
}

// Response contains the search results
type Response struct {
	NumFound int      `json:"numFound"`
	Start    int      `json:"start"`
	Docs     []Document `json:"docs"`
}

// Document represents a document in the Internet Archive
type Document struct {
	Identifier   string   `json:"identifier"`
	Title        string   `json:"title"`
	Creator      []string `json:"creator"`
	Description  string   `json:"description"`
	Subject      []string `json:"subject"`
	PublicDate   string   `json:"publicdate"`
	DownloadURL  string   `json:"download_url"`
	Format       []string `json:"format"`
	Collection   []string `json:"collection"`
	Language     []string `json:"language"`
	Publisher    []string `json:"publisher"`
	Year         string   `json:"year"`
	MediaType    string   `json:"mediatype"`
	NumDownloads int      `json:"downloads"`
}

// SearchOptions represents the search parameters for Internet Archive
type SearchOptions struct {
	Query     string
	Sort      string
	Fields    []string
	Rows      int
	Page      int
	MediaType string
}

// Search searches for documents on Internet Archive using the given options
func (c *Client) Search(opts SearchOptions) (*SearchResponse, error) {
	if opts.Rows <= 0 {
		opts.Rows = 50
	}

	queryParams := url.Values{}
	queryParams.Set("q", opts.Query)
	queryParams.Set("output", "json")
	
	// If specific fields are requested
	if len(opts.Fields) > 0 {
		queryParams.Set("fl", strings.Join(opts.Fields, ","))
	} else {
		// Default fields
		queryParams.Set("fl", "identifier,title,creator,description,subject,publicdate,downloads,mediatype,collection,format")
	}
	
	// Add sort parameter if provided
	if opts.Sort != "" {
		queryParams.Set("sort", opts.Sort)
	} else {
		// Default sort
		queryParams.Set("sort", "downloads desc")
	}
	
	// Set row count and page
	queryParams.Set("rows", fmt.Sprintf("%d", opts.Rows))
	
	// Calculate start from page
	start := (opts.Page - 1) * opts.Rows
	if start < 0 {
		start = 0
	}
	queryParams.Set("start", fmt.Sprintf("%d", start))
	
	// Filter by media type if specified
	if opts.MediaType != "" {
		queryParams.Set("q", fmt.Sprintf("%s AND mediatype:%s", opts.Query, opts.MediaType))
	}

	// Construct the URL
	reqURL := fmt.Sprintf("%s?%s", baseURL, queryParams.Encode())
	
	req, err := http.NewRequest("GET", reqURL, nil)
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
		return nil, fmt.Errorf("Internet Archive API error: %s, status code: %d", string(body), resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	var result SearchResponse
	err = json.Unmarshal(body, &result)
	if err != nil {
		return nil, err
	}

	return &result, nil
}

// GetMetadata gets metadata for a single document by ID
func (c *Client) GetMetadata(id string) (*Document, error) {
	reqURL := fmt.Sprintf("https://archive.org/metadata/%s", id)
	
	req, err := http.NewRequest("GET", reqURL, nil)
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
		return nil, fmt.Errorf("Internet Archive API error: %s, status code: %d", string(body), resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	// The metadata endpoint returns a different structure
	var metadataResp struct {
		Result Document `json:"metadata"`
	}
	
	err = json.Unmarshal(body, &metadataResp)
	if err != nil {
		return nil, err
	}

	return &metadataResp.Result, nil
}

// GetCreators formats creators for display
func GetCreators(doc Document) string {
	if len(doc.Creator) > 0 {
		return strings.Join(doc.Creator, ", ")
	}
	return "[Unknown]"
}

// GetDownloadURL constructs the download URL for a document
func GetDownloadURL(doc Document) string {
	return fmt.Sprintf("https://archive.org/download/%s", doc.Identifier)
}

// GetViewURL constructs the view URL for a document
func GetViewURL(doc Document) string {
	return fmt.Sprintf("https://archive.org/details/%s", doc.Identifier)
}