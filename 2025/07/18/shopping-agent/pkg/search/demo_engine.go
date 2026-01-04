package search

import (
	"math/rand"
	"strings"
	"time"

	"github.com/rs/zerolog/log"
)

// DemoEngine implements SearchEngine interface for testing purposes
type DemoEngine struct {
	products []ProductResult
}

// NewDemoEngine creates a new demo search engine with sample data
func NewDemoEngine() *DemoEngine {
	engine := &DemoEngine{}
	engine.initializeSampleData()
	return engine
}

// initializeSampleData creates sample product data for demonstration
func (d *DemoEngine) initializeSampleData() {
	d.products = []ProductResult{
		{
			Title:        "Apple iPhone 15 Pro 128GB",
			Price:        999.99,
			Currency:     "USD",
			URL:          "https://demo-store.com/iphone-15-pro",
			Site:         "demo",
			Availability: "In Stock",
			Rating:       4.8,
			Reviews:      1250,
			ImageURL:     "https://demo-store.com/images/iphone-15-pro.jpg",
			Description:  "Latest iPhone with A17 Pro chip, titanium design, and advanced camera system",
		},
		{
			Title:        "Samsung Galaxy S24 Ultra 256GB",
			Price:        1199.99,
			Currency:     "USD",
			URL:          "https://demo-store.com/galaxy-s24-ultra",
			Site:         "demo",
			Availability: "In Stock",
			Rating:       4.7,
			Reviews:      890,
			ImageURL:     "https://demo-store.com/images/galaxy-s24-ultra.jpg",
			Description:  "Premium Android phone with S Pen, 200MP camera, and AI features",
		},
		{
			Title:        "MacBook Air M3 13-inch 256GB",
			Price:        1099.99,
			Currency:     "USD",
			URL:          "https://demo-store.com/macbook-air-m3",
			Site:         "demo",
			Availability: "In Stock",
			Rating:       4.9,
			Reviews:      567,
			ImageURL:     "https://demo-store.com/images/macbook-air-m3.jpg",
			Description:  "Ultra-thin laptop with M3 chip, all-day battery life, and Liquid Retina display",
		},
		{
			Title:        "Sony WH-1000XM5 Wireless Headphones",
			Price:        399.99,
			Currency:     "USD",
			URL:          "https://demo-store.com/sony-wh1000xm5",
			Site:         "demo",
			Availability: "In Stock",
			Rating:       4.6,
			Reviews:      2340,
			ImageURL:     "https://demo-store.com/images/sony-wh1000xm5.jpg",
			Description:  "Industry-leading noise canceling headphones with 30-hour battery life",
		},
		{
			Title:        "Nintendo Switch OLED Console",
			Price:        349.99,
			Currency:     "USD",
			URL:          "https://demo-store.com/nintendo-switch-oled",
			Site:         "demo",
			Availability: "Limited Stock",
			Rating:       4.8,
			Reviews:      1890,
			ImageURL:     "https://demo-store.com/images/nintendo-switch-oled.jpg",
			Description:  "Gaming console with vibrant OLED screen and enhanced audio",
		},
		{
			Title:        "iPad Pro 12.9-inch M2 128GB",
			Price:        1099.99,
			Currency:     "USD",
			URL:          "https://demo-store.com/ipad-pro-m2",
			Site:         "demo",
			Availability: "In Stock",
			Rating:       4.7,
			Reviews:      445,
			ImageURL:     "https://demo-store.com/images/ipad-pro-m2.jpg",
			Description:  "Professional tablet with M2 chip, Liquid Retina XDR display, and Apple Pencil support",
		},
		{
			Title:        "Dell XPS 13 Plus Laptop",
			Price:        1299.99,
			Currency:     "USD",
			URL:          "https://demo-store.com/dell-xps-13-plus",
			Site:         "demo",
			Availability: "In Stock",
			Rating:       4.5,
			Reviews:      678,
			ImageURL:     "https://demo-store.com/images/dell-xps-13-plus.jpg",
			Description:  "Premium Windows laptop with 13th Gen Intel Core processor and InfinityEdge display",
		},
		{
			Title:        "AirPods Pro 2nd Generation",
			Price:        249.99,
			Currency:     "USD",
			URL:          "https://demo-store.com/airpods-pro-2",
			Site:         "demo",
			Availability: "In Stock",
			Rating:       4.8,
			Reviews:      3456,
			ImageURL:     "https://demo-store.com/images/airpods-pro-2.jpg",
			Description:  "Wireless earbuds with active noise cancellation and spatial audio",
		},
	}

	log.Debug().Int("products", len(d.products)).Msg("Demo engine initialized with sample data")
}

// Search performs a search using the demo data
func (d *DemoEngine) Search(params SearchParams) (SearchResults, error) {
	log.Debug().
		Str("query", params.Query).
		Str("site", params.Site).
		Msg("Demo engine performing search")

	var results SearchResults
	query := strings.ToLower(params.Query)

	// Filter products based on query
	for _, product := range d.products {
		if d.matchesQuery(product, query) {
			// Add some randomization to simulate real search results
			result := product
			result.Price = d.addPriceVariation(product.Price)
			result.SearchedAt = time.Now()
			
			results = append(results, result)
		}
	}

	// Limit results
	if params.MaxResults > 0 && len(results) > params.MaxResults {
		results = results[:params.MaxResults]
	}

	log.Debug().
		Str("query", params.Query).
		Int("results", len(results)).
		Msg("Demo search completed")

	return results, nil
}

// matchesQuery checks if a product matches the search query
func (d *DemoEngine) matchesQuery(product ProductResult, query string) bool {
	searchText := strings.ToLower(product.Title + " " + product.Description)
	
	// Split query into words and check if any match
	queryWords := strings.Fields(query)
	for _, word := range queryWords {
		if strings.Contains(searchText, word) {
			return true
		}
	}
	
	return false
}

// addPriceVariation adds small random variation to prices to simulate market changes
func (d *DemoEngine) addPriceVariation(basePrice float64) float64 {
	// Add random variation of ±5%
	variation := (rand.Float64() - 0.5) * 0.1 // -0.05 to +0.05
	newPrice := basePrice * (1 + variation)
	
	// Round to 2 decimal places
	return float64(int(newPrice*100)) / 100
}

// GetSupportedSites returns the sites supported by this engine
func (d *DemoEngine) GetSupportedSites() []string {
	return []string{"demo"}
}

// IsSupported checks if a site is supported by this engine
func (d *DemoEngine) IsSupported(site string) bool {
	return site == "demo" || site == "all"
}

