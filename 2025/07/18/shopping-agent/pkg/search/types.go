package search

import "time"

// SearchParams represents the parameters for a product search
type SearchParams struct {
	Query      string  `json:"query"`
	Site       string  `json:"site"`
	MaxResults int     `json:"max_results"`
	MinPrice   float64 `json:"min_price"`
	MaxPrice   float64 `json:"max_price"`
}

// ProductResult represents a single product search result
type ProductResult struct {
	Title        string    `json:"title"`
	Price        float64   `json:"price"`
	Currency     string    `json:"currency"`
	URL          string    `json:"url"`
	Site         string    `json:"site"`
	Availability string    `json:"availability"`
	Rating       float64   `json:"rating"`
	Reviews      int       `json:"reviews"`
	ImageURL     string    `json:"image_url"`
	Description  string    `json:"description"`
	SearchedAt   time.Time `json:"searched_at"`
}

// SearchResults represents a collection of product search results
type SearchResults []ProductResult

// SiteConfig represents configuration for a specific e-commerce site
type SiteConfig struct {
	Name        string            `json:"name"`
	BaseURL     string            `json:"base_url"`
	SearchURL   string            `json:"search_url"`
	Selectors   map[string]string `json:"selectors"`
	RateLimit   time.Duration     `json:"rate_limit"`
	UserAgent   string            `json:"user_agent"`
	Headers     map[string]string `json:"headers"`
	Enabled     bool              `json:"enabled"`
}

// SearchEngine interface defines the contract for different search implementations
type SearchEngine interface {
	Search(params SearchParams) (SearchResults, error)
	GetSupportedSites() []string
	IsSupported(site string) bool
}

// Searcher interface defines the main search functionality
type Searcher interface {
	Search(params SearchParams) (SearchResults, error)
	AddEngine(engine SearchEngine) error
	GetAvailableSites() []string
}

