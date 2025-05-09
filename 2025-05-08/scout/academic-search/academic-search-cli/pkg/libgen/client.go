package libgen

import (
	"fmt"
	"net/http"
	"strings"
	"time"
)

const (
	// Use the public mirror API that doesn't require complex authentication
	baseURL = "https://libgen.is/search.php"
)

// Client represents a Libgen API client
type Client struct {
	httpClient *http.Client
}

// NewClient creates a new Libgen API client
func NewClient() *Client {
	return &Client{
		httpClient: &http.Client{
			Timeout: 30 * time.Second,
		},
	}
}

// Book represents a book in the Libgen response
type Book struct {
	ID          string `json:"id"`
	Title       string `json:"title"`
	Author      string `json:"author"`
	Year        string `json:"year"`
	Edition     string `json:"edition"`
	Publisher   string `json:"publisher"`
	Pages       string `json:"pages"`
	Language    string `json:"language"`
	Size        string `json:"size"`
	Extension   string `json:"extension"`
	MD5         string `json:"md5"`
	DOI         string `json:"doi"`
}

// SearchOptions represents the search parameters for Libgen
type SearchOptions struct {
	Query    string
	Fields   string
	Limit    int
	Page     int
}

// Search searches for books on Libgen
// Note: Libgen API is unofficial and may change, this is a simplified version
func (c *Client) Search(opts SearchOptions) ([]Book, error) {
	if opts.Limit <= 0 {
		opts.Limit = 25
	}
	
	// For demo purposes, return simulated results
	// In a real application, you would parse the HTML response from Libgen
	// as they don't provide a stable JSON API
	
	// Simulate delay for a more realistic experience
	time.Sleep(1 * time.Second)
	
	lowerQuery := strings.ToLower(opts.Query)
	
	var books []Book
	
	// Create mock results based on query
	if strings.Contains(lowerQuery, "artificial intelligence") || strings.Contains(lowerQuery, "ai") {
		books = append(books, Book{
			ID: "12345",
			Title: "Artificial Intelligence: A Modern Approach",
			Author: "Stuart Russell, Peter Norvig",
			Year: "2020",
			Edition: "4th Edition",
			Publisher: "Pearson",
			Pages: "1136",
			Language: "English",
			Size: "23 MB",
			Extension: "pdf",
			MD5: "a1b2c3d4e5f6g7h8i9j0",
			DOI: "10.1234/5678",
		})
		
		books = append(books, Book{
			ID: "23456",
			Title: "Deep Learning",
			Author: "Ian Goodfellow, Yoshua Bengio, Aaron Courville",
			Year: "2016",
			Publisher: "MIT Press",
			Pages: "800",
			Language: "English",
			Size: "14 MB",
			Extension: "pdf",
			MD5: "j9i8h7g6f5e4d3c2b1a0",
		})
	}
	
	if strings.Contains(lowerQuery, "reinforcement learning") || strings.Contains(lowerQuery, "rl") {
		books = append(books, Book{
			ID: "34567",
			Title: "Reinforcement Learning: An Introduction",
			Author: "Richard S. Sutton, Andrew G. Barto",
			Year: "2018",
			Edition: "2nd Edition",
			Publisher: "MIT Press",
			Pages: "552",
			Language: "English",
			Size: "12 MB",
			Extension: "pdf",
			MD5: "z1x2c3v4b5n6m7k8j9h0",
		})
	}
	
	if strings.Contains(lowerQuery, "machine learning") || strings.Contains(lowerQuery, "ml") {
		books = append(books, Book{
			ID: "45678",
			Title: "Pattern Recognition and Machine Learning",
			Author: "Christopher M. Bishop",
			Year: "2006",
			Publisher: "Springer",
			Pages: "738",
			Language: "English",
			Size: "18 MB",
			Extension: "pdf",
			MD5: "q1w2e3r4t5y6u7i8o9p0",
		})
	}
	
	if strings.Contains(lowerQuery, "neural networks") || strings.Contains(lowerQuery, "nn") {
		books = append(books, Book{
			ID: "56789",
			Title: "Neural Networks and Deep Learning",
			Author: "Michael Nielsen",
			Year: "2015",
			Publisher: "Determination Press",
			Pages: "224",
			Language: "English",
			Size: "5 MB",
			Extension: "pdf",
			MD5: "a1s2d3f4g5h6j7k8l9z0",
		})
	}
	
	// If no specific books found, return generic AI books
	if len(books) == 0 {
		books = append(books, Book{
			ID: "67890",
			Title: "The Hundred-Page Machine Learning Book",
			Author: "Andriy Burkov",
			Year: "2019",
			Publisher: "Andriy Burkov",
			Pages: "160",
			Language: "English",
			Size: "3 MB",
			Extension: "pdf",
			MD5: "p0o9i8u7y6t5r4e3w2q1",
		})
	}
	
	// Limit results based on options
	if len(books) > opts.Limit {
		books = books[:opts.Limit]
	}
	
	return books, nil
}

// GetDownloadURL generates a download URL for a book
func GetDownloadURL(book Book) string {
	// Note: Libgen download URLs may change over time, this is a simulated example
	return fmt.Sprintf("https://libgen.is/get.php?md5=%s", book.MD5)
}

// FormatAuthors formats the authors string for better display
func FormatAuthors(authors string) string {
	return strings.ReplaceAll(authors, ",", ", ")
}