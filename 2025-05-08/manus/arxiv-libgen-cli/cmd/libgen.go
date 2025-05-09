package cmd

import (
	"fmt"
	"io/ioutil" // Added for ioutil.WriteFile
	"net/http"
	"net/url"
	"os"
	"regexp" // Added for DOI regex
	"strings"
	"time"

	"github.com/PuerkitoBio/goquery"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

var libgenQuery string
var libgenMaxResults int
var libgenMirror string

// LibgenEntry represents a single search result from LibGen (scraped)
// Fields are based on common data found on LibGen search result pages for scientific articles.
type LibgenEntry struct {
	Authors     string
	Title       string
	JournalInfo string // Combined Journal, Volume, Issue, Year
	DOI         string
	FileSize    string
	DownloadURL string   // Primary link from title, if any
	MirrorLinks []string // Links to download pages on various mirrors
	EditLink    string   // Link to edit metadata
}

// libgenCmd represents the libgen command
var libgenCmd = &cobra.Command{
	Use:   "libgen",
	Short: "Search for scientific papers on LibGen (via web scraping)",
	Long: `Search for scientific papers on Library Genesis mirrors by scraping search results.

Example:
  arxiv-libgen-cli libgen --query "artificial intelligence" --max_results 5
  arxiv-libgen-cli libgen -q "quantum entanglement" -n 3 --mirror "https://libgen.is"`,
	Run: func(cmd *cobra.Command, args []string) {
		if libgenQuery == "" {
			fmt.Println("Error: query cannot be empty.")
			cmd.Help()
			os.Exit(1)
		}
		log.Debug().Str("query", libgenQuery).Int("max_results", libgenMaxResults).Str("mirror", libgenMirror).Msg("LibGen search initiated")

		// --- Start of URL construction and base URL for resolving ---
		effectiveMirror := libgenMirror
		if !strings.HasPrefix(effectiveMirror, "http://") && !strings.HasPrefix(effectiveMirror, "https://") {
			effectiveMirror = "https://" + effectiveMirror
		}

		// baseDomainURL is the parsed version of the user-provided mirror (e.g., https://libgen.is)
		baseDomainURL, err := url.Parse(effectiveMirror)
		if err != nil {
			log.Fatal().Err(err).Str("mirror", effectiveMirror).Msg("Invalid mirror URL parsing for base domain")
		}

		// Construct the actual API URL we are going to hit, e.g., https://libgen.is/scimag/?q=query
		// This will also serve as the base for resolving relative links on the fetched page.
		apiURLObject := &url.URL{
			Scheme: baseDomainURL.Scheme,
			Host:   baseDomainURL.Host,
			Path:   "/scimag/", // Path for scientific articles on mirrors like libgen.is
		}
		
		qParams := apiURLObject.Query() // Get a new Values map
		qParams.Set("q", libgenQuery)
		apiURLObject.RawQuery = qParams.Encode()
		
		apiURL := apiURLObject.String() // The final URL to fetch
		// --- End of URL construction ---

		log.Debug().Str("url", apiURL).Msg("Requesting LibGen URL")
		fmt.Printf("Searching LibGen mirror at %s for: 
'%s
' (max results: %d)\n", libgenMirror, libgenQuery, libgenMaxResults)
		fmt.Println("Requesting URL:", apiURL)
		fmt.Println("Note: LibGen search is via web scraping and may be slow or break if the site structure changes.")
		time.Sleep(1 * time.Second)

		client := &http.Client{Timeout: 30 * time.Second}
		req, _ := http.NewRequest("GET", apiURL, nil)
		req.Header.Set("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")

		resp, err := client.Do(req)
		if err != nil {
			log.Fatal().Err(err).Msg("Error making GET request to LibGen mirror")
		}
		defer resp.Body.Close()
		log.Debug().Str("status", resp.Status).Int("statusCode", resp.StatusCode).Msg("LibGen mirror response received")

		if resp.StatusCode != http.StatusOK {
			log.Error().Str("status", resp.Status).Msg("LibGen mirror request failed")
			fmt.Printf("Error: LibGen mirror request failed with status %s\n", resp.Status)
			return
		}

		htmlBody, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			log.Fatal().Err(err).Msg("Error reading LibGen HTML response body")
		}

		htmlFilePath := "/home/ubuntu/libgen_page.html"
		err = ioutil.WriteFile(htmlFilePath, htmlBody, 0644)
		if err != nil {
			log.Error().Err(err).Str("path", htmlFilePath).Msg("Failed to save LibGen HTML to file")
		} else {
			log.Info().Str("path", htmlFilePath).Msg("LibGen HTML response saved to file")
			fmt.Printf("LibGen HTML response saved to: %s\n", htmlFilePath)
		}

		doc, err := goquery.NewDocumentFromReader(strings.NewReader(string(htmlBody)))
		if err != nil {
			log.Fatal().Err(err).Msg("Error parsing HTML response from LibGen")
		}
		log.Debug().Msg("LibGen HTML response parsed successfully")

		var entries []LibgenEntry
		log.Debug().Msg("Attempting to find result rows with selector: table.catalog tbody tr")
		resultRows := doc.Find("table.catalog tbody tr")
		log.Debug().Int("num_rows_found_by_selector", resultRows.Length()).Msg("Initial rows found by selector")

		resultRows.Each(func(i int, row *goquery.Selection) {
			if len(entries) >= libgenMaxResults {
				return 
			}
			log.Debug().Int("row_index", i).Msg("Processing result row")

			cells := row.Find("td")
			var entry LibgenEntry

			if cells.Length() == 5 {
				entry.Authors = strings.TrimSpace(cells.Eq(0).Text())
				
				articleCell := cells.Eq(1)
				titleLink := articleCell.Find("p a").First()
				entry.Title = strings.TrimSpace(titleLink.Text())
				if href, exists := titleLink.Attr("href"); exists {
					parsedHref, _ := url.Parse(href)
					entry.DownloadURL = apiURLObject.ResolveReference(parsedHref).String() // Use apiURLObject as base
				}
				doiText := articleCell.Find("p").Eq(1).Text()
				if strings.HasPrefix(strings.ToLower(doiText), "doi:") {
				    entry.DOI = strings.TrimSpace(strings.TrimPrefix(strings.ToLower(doiText), "doi:"))
				} else {
				    doiRegex := regexp.MustCompile(`10\.\d{4,9}/[-._;()/:A-Z0-9a-z]+`)
				    foundDOI := doiRegex.FindString(doiText)
				    if foundDOI != "" {
				        entry.DOI = foundDOI
				    }
				}

				journalCell := cells.Eq(2)
				var journalParts []string
				journalCell.Find("p").Each(func(_ int, p *goquery.Selection) {
					journalParts = append(journalParts, strings.TrimSpace(p.Text()))
				})
				entry.JournalInfo = strings.Join(journalParts, "; ")

				fileCell := cells.Eq(3)
				entry.FileSize = strings.TrimSpace(fileCell.Contents().Not("a").Not("br").Text())
				if editLink, exists := fileCell.Find("a[title=\"edit metadata\"]").Attr("href"); exists {
					parsedEditLink, _ := url.Parse(editLink)
					entry.EditLink = apiURLObject.ResolveReference(parsedEditLink).String() // Use apiURLObject as base
				}

				mirrorsCell := cells.Eq(4)
				mirrorsCell.Find("ul.record_mirrors li a").Each(func(_ int, link *goquery.Selection) {
					if href, exists := link.Attr("href"); exists {
						parsedMirrorLink, err := url.Parse(href)
						if err == nil {
							resolvedHref := apiURLObject.ResolveReference(parsedMirrorLink).String() // Use apiURLObject as base
							entry.MirrorLinks = append(entry.MirrorLinks, resolvedHref)
						}
					}
				})
				
				if entry.Title != "" {
				    entries = append(entries, entry)
				    log.Debug().Str("title", entry.Title).Msg("Parsed entry (libgen.is/scimag structure)")
				} else {
				    log.Warn().Int("row_index", i).Msg("Skipping row, could not parse title (libgen.is/scimag structure)")
				}
			} else {
				log.Warn().Int("row_index", i).Int("num_cells", cells.Length()).Msg("Skipping row due to unexpected cell count for libgen.is/scimag structure")
			}
		})

		if len(entries) == 0 {
			fmt.Println("No results found, or failed to parse results from the mirror.")
			fmt.Println("The structure of LibGen mirrors changes frequently. This scraper might need an update.")
			fmt.Println("Please check the saved libgen_page.html to analyze the structure and update selectors in libgen.go if issues persist.")
			log.Info().Msg("No entries parsed from LibGen HTML")
			return
		}
		log.Debug().Int("num_entries_parsed", len(entries)).Msg("Finished parsing LibGen results")

		fmt.Printf("\nFound %d results (showing up to %d):\n", len(entries), libgenMaxResults)
		fmt.Println("--------------------------------------------------")

		for i, entry := range entries {
			if i >= libgenMaxResults { break }
			fmt.Printf("Result %d:\n", i+1)
			fmt.Printf("  Title: %s\n", entry.Title)
			fmt.Printf("  Authors: %s\n", entry.Authors)
			if entry.JournalInfo != "" { fmt.Printf("  Journal Info: %s\n", entry.JournalInfo) }
			if entry.DOI != "" { fmt.Printf("  DOI: %s\n", entry.DOI) }
			if entry.FileSize != "" { fmt.Printf("  File Size: %s\n", entry.FileSize) }
			if entry.DownloadURL != "" { fmt.Printf("  Article/Details Link: %s\n", entry.DownloadURL) }
			if entry.EditLink != "" { fmt.Printf("  Edit Metadata Link: %s\n", entry.EditLink) }
			if len(entry.MirrorLinks) > 0 {
				fmt.Printf("  Mirror Links: %s\n", strings.Join(entry.MirrorLinks, ", "))
			}
			fmt.Println("--------------------------------------------------")
		}
	},
}

func init() {
	rootCmd.AddCommand(libgenCmd)
	libgenCmd.Flags().StringVarP(&libgenQuery, "query", "q", "", "Search query for LibGen (e.g., \"artificial intelligence\", \"ISBN:9783319994912\") (required)")
	libgenCmd.Flags().IntVarP(&libgenMaxResults, "max_results", "n", 10, "Maximum number of results to display")
	libgenCmd.Flags().StringVarP(&libgenMirror, "mirror", "m", "https://libgen.is", "LibGen mirror URL (e.g., https://libgen.is, http://libgen.st)")
}

