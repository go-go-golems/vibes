package openalex

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
	baseURL = "https://api.openalex.org"
)

// Client represents an OpenAlex API client
type Client struct {
	httpClient *http.Client
	email      string
}

// NewClient creates a new OpenAlex API client
func NewClient(email string) *Client {
	return &Client{
		httpClient: &http.Client{
			Timeout: 30 * time.Second,
		},
		email: email,
	}
}

// WorksResponse represents the top-level response from OpenAlex works endpoint
type WorksResponse struct {
	Meta   Meta    `json:"meta"`
	Works  []Work  `json:"results"`
}

// Meta contains metadata about the response
type Meta struct {
	Count       int    `json:"count"`
	PerPage     int    `json:"per_page"`
	Page        int    `json:"page"`
	NextCursor  string `json:"next_cursor"`
}

// Work represents a scholarly work in the OpenAlex response
type Work struct {
	ID                string            `json:"id"`
	DOI               string            `json:"doi"`
	Title             string            `json:"display_name"`
	Type              string            `json:"type"`
	Authorships       []Authorship      `json:"authorships"`
	PublicationYear   int               `json:"publication_year"`
	PublicationDate   string            `json:"publication_date"`
	PrimaryLocation   Location          `json:"primary_location"`
	OpenAccess        OpenAccess        `json:"open_access"`
	CitationCount     int               `json:"cited_by_count"`
	ReferencedWorks   []string          `json:"referenced_works"`
	RelatedWorks      []string          `json:"related_works"`
	Abstract          string            `json:"abstract_inverted_index"`
	Concepts          []Concept         `json:"concepts"`
	Locations         []Location        `json:"locations"`
}

// Authorship represents an author of a work
type Authorship struct {
	Author  Author    `json:"author"`
	Position string   `json:"position"`
}

// Author represents an author entity
type Author struct {
	ID    string      `json:"id"`
	Name  string      `json:"display_name"`
	ORCID string      `json:"orcid"`
}

// Location represents where a work is located
type Location struct {
	Source         Source      `json:"source"`
	Version        string      `json:"version"`
	IsOA           bool        `json:"is_oa"`
	PDF            string      `json:"pdf_url"`
	HTML           string      `json:"landing_page_url"`
}

// Source represents the source of a work
type Source struct {
	ID    string      `json:"id"`
	Name  string      `json:"display_name"`
	Type  string      `json:"type"`
}

// OpenAccess represents open access status of a work
type OpenAccess struct {
	IsOA          bool        `json:"is_oa"`
	OAStatus      string      `json:"oa_status"`
}

// Concept represents a subject concept
type Concept struct {
	ID               string     `json:"id"`
	Name             string     `json:"display_name"`
	Wikidata         string     `json:"wikidata"`
	Level            int        `json:"level"`
	Score            float64    `json:"score"`
}

// ConceptsResponse represents the response from text-to-concepts endpoint
type ConceptsResponse struct {
	Concepts []Concept `json:"concepts"`
}

// SearchOptions represents the search parameters for OpenAlex
type SearchOptions struct {
	Query     string
	Filter    map[string]string
	PerPage   int
	Page      int
	Sort      string
	Select    []string
	Cursor    string
}

// Search searches for works on OpenAlex using the given options
func (c *Client) Search(opts SearchOptions) (*WorksResponse, error) {
	if opts.PerPage <= 0 {
		opts.PerPage = 25
	}

	queryParams := url.Values{}
	
	if opts.Query != "" {
		queryParams.Set("search", opts.Query)
	}
	
	queryParams.Set("per_page", fmt.Sprintf("%d", opts.PerPage))
	
	if opts.Page > 0 {
		queryParams.Set("page", fmt.Sprintf("%d", opts.Page))
	}
	
	if opts.Sort != "" {
		queryParams.Set("sort", opts.Sort)
	}
	
	if len(opts.Select) > 0 {
		queryParams.Set("select", strings.Join(opts.Select, ","))
	}
	
	if opts.Cursor != "" {
		queryParams.Set("cursor", opts.Cursor)
	}
	
	// Add filters
	for key, value := range opts.Filter {
		queryParams.Add("filter", fmt.Sprintf("%s:%s", key, value))
	}

	// Construct the URL
	reqURL := fmt.Sprintf("%s/works?%s", baseURL, queryParams.Encode())
	
	req, err := http.NewRequest("GET", reqURL, nil)
	if err != nil {
		return nil, err
	}
	
	// Polite pool if email provided
	if c.email != "" {
		req.Header.Set("User-Agent", "academic-search-cli "+c.email)
	}

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("OpenAlex API error: %s, status code: %d", string(body), resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	var result WorksResponse
	err = json.Unmarshal(body, &result)
	if err != nil {
		return nil, err
	}

	return &result, nil
}

// GetWork gets a single work by ID or DOI
func (c *Client) GetWork(id string) (*Work, error) {
	// Determine if it's a DOI or OpenAlex ID
	var reqURL string
	if strings.HasPrefix(id, "10.") {
		reqURL = fmt.Sprintf("%s/works/https://doi.org/%s", baseURL, id)
	} else {
		reqURL = fmt.Sprintf("%s/works/%s", baseURL, id)
	}
	
	req, err := http.NewRequest("GET", reqURL, nil)
	if err != nil {
		return nil, err
	}
	
	// Polite pool if email provided
	if c.email != "" {
		req.Header.Set("User-Agent", "academic-search-cli "+c.email)
	}

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("OpenAlex API error: %s, status code: %d", string(body), resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	var work Work
	err = json.Unmarshal(body, &work)
	if err != nil {
		return nil, err
	}

	return &work, nil
}

// GetCitations gets works that cite the given work ID
func (c *Client) GetCitations(workID string, perPage int, cursor string) (*WorksResponse, error) {
	if perPage <= 0 {
		perPage = 100
	}

	queryParams := url.Values{}
	queryParams.Set("filter", fmt.Sprintf("cites:%s", workID))
	queryParams.Set("per_page", fmt.Sprintf("%d", perPage))
	
	if cursor != "" {
		queryParams.Set("cursor", cursor)
	}

	reqURL := fmt.Sprintf("%s/works?%s", baseURL, queryParams.Encode())
	
	req, err := http.NewRequest("GET", reqURL, nil)
	if err != nil {
		return nil, err
	}
	
	if c.email != "" {
		req.Header.Set("User-Agent", "academic-search-cli "+c.email)
	}

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("OpenAlex API error: %s, status code: %d", string(body), resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	var result WorksResponse
	err = json.Unmarshal(body, &result)
	if err != nil {
		return nil, err
	}

	return &result, nil
}

// SuggestKeywords suggests concepts based on text
func (c *Client) SuggestKeywords(text string, maxKeywords int) ([]Concept, error) {
	// OpenAlex's text endpoint for concept inference
	reqURL := fmt.Sprintf("%s/text", baseURL)
	
	// Create request body
	requestBody := strings.NewReader(fmt.Sprintf(`{"text": "%s"}`, text))
	
	req, err := http.NewRequest("POST", reqURL, requestBody)
	if err != nil {
		return nil, err
	}
	
	req.Header.Set("Content-Type", "application/json")
	if c.email != "" {
		req.Header.Set("User-Agent", "academic-search-cli "+c.email)
	}

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("OpenAlex API error: %s, status code: %d", string(body), resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	var result ConceptsResponse
	err = json.Unmarshal(body, &result)
	if err != nil {
		return nil, err
	}

	// Limit the number of concepts returned
	if maxKeywords > 0 && maxKeywords < len(result.Concepts) {
		result.Concepts = result.Concepts[:maxKeywords]
	}

	return result.Concepts, nil
}

// GetAuthors formats authors for display
func GetAuthors(work Work) string {
	authors := []string{}
	for _, authorship := range work.Authorships {
		authors = append(authors, authorship.Author.Name)
	}
	return strings.Join(authors, ", ")
}

// GetPDFURL gets the URL of the PDF version of the work
func GetPDFURL(work Work) string {
	// First check primary location
	if work.PrimaryLocation.PDF != "" {
		return work.PrimaryLocation.PDF
	}
	
	// Then check other locations, prioritizing open access versions
	for _, location := range work.Locations {
		if location.IsOA && location.PDF != "" {
			return location.PDF
		}
	}
	
	// If no open access PDF, return any PDF
	for _, location := range work.Locations {
		if location.PDF != "" {
			return location.PDF
		}
	}
	
	return ""
}

// GetWorkURL gets the URL of the HTML version of the work
func GetWorkURL(work Work) string {
	// First check primary location
	if work.PrimaryLocation.HTML != "" {
		return work.PrimaryLocation.HTML
	}
	
	// Then check other locations
	for _, location := range work.Locations {
		if location.HTML != "" {
			return location.HTML
		}
	}
	
	// If DOI is available, use that
	if work.DOI != "" {
		return fmt.Sprintf("https://doi.org/%s", work.DOI)
	}
	
	return ""
}