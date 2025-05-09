package cmd

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/scrapybara/academic-search-cli/pkg/libgen"
	"github.com/spf13/cobra"
)

var (
	libgenMD5      string
	libgenTitle    string
	libgenOutDir   string
	libgenMirror   int
	libgenFileName string
)

var libgenDownloadCmd = &cobra.Command{
	Use:   "libgen-download",
	Short: "Download a book from Library Genesis",
	Long:  `Download a book from Library Genesis using its MD5 hash or by searching for its title.`,
	RunE: func(cmd *cobra.Command, args []string) error {
		// Validate input
		if libgenMD5 == "" && libgenTitle == "" {
			return fmt.Errorf("either --md5 or --title must be provided")
		}
		
		// Create output directory if it doesn't exist
		if libgenOutDir != "" {
			err := os.MkdirAll(libgenOutDir, 0755)
			if err != nil {
				return fmt.Errorf("failed to create output directory: %v", err)
			}
		}
		
		client := libgen.NewClient()
		var book libgen.Book
		var downloadURL string
		
		// If MD5 is provided, directly get download links
		if libgenMD5 != "" {
			// Create a minimal book structure with the MD5
			book = libgen.Book{
				MD5: libgenMD5,
			}
			
			// Get download links
			links, err := client.GetDownloadLinks(libgenMD5)
			if err != nil {
				return fmt.Errorf("failed to get download links: %v", err)
			}
			
			if len(links) == 0 {
				return fmt.Errorf("no download links found for MD5: %s", libgenMD5)
			}
			
			book.DownloadLinks = links
			
			// Select download URL based on mirror preference
			if libgenMirror < len(links) {
				downloadURL = links[libgenMirror]
			} else {
				downloadURL = links[0]
			}
		} else {
			// Search for the book by title
			fmt.Printf("Searching for book: %s\n", libgenTitle)
			
			books, err := client.Search(libgen.SearchOptions{
				Query:  libgenTitle,
				Limit:  1,
				Fields: "title",
			})
			
			if err != nil {
				return fmt.Errorf("search failed: %v", err)
			}
			
			if len(books) == 0 {
				return fmt.Errorf("no books found with title: %s", libgenTitle)
			}
			
			// Use the first result
			book = books[0]
			
			// Get download URL
			if len(book.DownloadLinks) == 0 {
				return fmt.Errorf("no download links found for book: %s", book.Title)
			}
			
			// Select download URL based on mirror preference
			if libgenMirror < len(book.DownloadLinks) {
				downloadURL = book.DownloadLinks[libgenMirror]
			} else {
				downloadURL = book.DownloadLinks[0]
			}
			
			fmt.Printf("Found book: %s by %s (%s)\n", book.Title, book.Author, book.Year)
		}
		
		// Determine filename
		filename := libgenFileName
		if filename == "" {
			// Create a filename based on book metadata or MD5
			if book.Title != "" {
				sanitizedTitle := sanitizeFilename(book.Title)
				if book.Author != "" {
					sanitizedAuthor := sanitizeFilename(book.Author)
					filename = fmt.Sprintf("%s - %s", sanitizedAuthor, sanitizedTitle)
				} else {
					filename = sanitizedTitle
				}
				
				if book.Extension != "" {
					filename = fmt.Sprintf("%s.%s", filename, strings.ToLower(book.Extension))
				} else {
					// Try to get extension from URL
					ext := filepath.Ext(downloadURL)
					if ext != "" {
						filename = filename + ext
					} else {
						filename = filename + ".pdf" // Default to PDF
					}
				}
			} else {
				// If we only have MD5, use it as filename
				filename = fmt.Sprintf("%s.pdf", book.MD5)
			}
		}
		
		// Add directory if specified
		if libgenOutDir != "" {
			filename = filepath.Join(libgenOutDir, filename)
		}
		
		// Download the file
		fmt.Printf("Downloading from: %s\n", downloadURL)
		fmt.Printf("Saving to: %s\n", filename)
		
		err := downloadFile(downloadURL, filename)
		if err != nil {
			return fmt.Errorf("download failed: %v", err)
		}
		
		fmt.Printf("Successfully downloaded to: %s\n", filename)
		return nil
	},
}

// sanitizeFilename removes invalid characters from a filename
func sanitizeFilename(input string) string {
	// Replace problematic characters
	replacer := strings.NewReplacer(
		"/", "-",
		"\\", "-",
		":", "-",
		"*", "",
		"?", "",
		"\"", "",
		"<", "",
		">", "",
		"|", "",
	)
	sanitized := replacer.Replace(input)
	
	// Trim spaces and truncate if too long
	sanitized = strings.TrimSpace(sanitized)
	if len(sanitized) > 100 {
		sanitized = sanitized[:100]
	}
	
	return sanitized
}

// downloadFile downloads a file from a URL
func downloadFile(url string, filepath string) error {
	// Create HTTP client with timeout
	client := &http.Client{
		Timeout: 30 * time.Minute, // Longer timeout for downloads
	}
	
	// Set up the request with headers that mimic a browser
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return err
	}
	
	req.Header.Set("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")
	req.Header.Set("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
	req.Header.Set("Accept-Language", "en-US,en;q=0.5")
	req.Header.Set("Referer", "https://libgen.is/")
	
	// Get the data
	resp, err := client.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	
	// Check server response
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("bad status: %s", resp.Status)
	}
	
	// Create the file
	out, err := os.Create(filepath)
	if err != nil {
		return err
	}
	defer out.Close()
	
	// Write the body to file
	_, err = io.Copy(out, resp.Body)
	return err
}

func init() {
	rootCmd.AddCommand(libgenDownloadCmd)
	
	libgenDownloadCmd.Flags().StringVar(&libgenMD5, "md5", "", "MD5 hash of the book to download")
	libgenDownloadCmd.Flags().StringVar(&libgenTitle, "title", "", "Title of the book to search for and download")
	libgenDownloadCmd.Flags().StringVar(&libgenOutDir, "out-dir", "", "Output directory for downloaded files")
	libgenDownloadCmd.Flags().IntVar(&libgenMirror, "mirror", 0, "Mirror to use (0 for first mirror, 1 for second, etc.)")
	libgenDownloadCmd.Flags().StringVar(&libgenFileName, "filename", "", "Filename to save as (default: derived from book metadata)")
}