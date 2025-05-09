package cmd

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"time"

	"github.com/scrapybara/academic-search-cli/pkg/libgen"
	"github.com/scrapybara/academic-search-cli/pkg/openalex"
	"github.com/spf13/cobra"
)

var (
	findFullTextPreferVersion string
	findFullTextOutput        string
	findFullTextEmail         string
)

// FullTextResult represents the result of finding full text
type FullTextResult struct {
	PDFURL    string `json:"pdf_url"`
	Source    string `json:"source"`
	OAStatus  string `json:"oa_status,omitempty"`
	License   string `json:"license,omitempty"`
	MD5       string `json:"md5,omitempty"`
}

// UnpaywallResponse represents the response from Unpaywall API
type UnpaywallResponse struct {
	DOI           string `json:"doi"`
	IsOA          bool   `json:"is_oa"`
	OAStatus      string `json:"oa_status"`
	BestOALocation struct {
		URL              string `json:"url"`
		VersionString    string `json:"version"`
		LicenseNormalized string `json:"license"`
	} `json:"best_oa_location"`
}

var findFullTextCmd = &cobra.Command{
	Use:   "find-full-text",
	Short: "Find full text for a scholarly work",
	Long:  `Hand back the best PDF (or HTML) URL for a work, trying legal OA sources first and falling back to LibGen.`,
	RunE: func(cmd *cobra.Command, args []string) error {
		// Need at least DOI or title
		doi, _ := cmd.Flags().GetString("doi")
		title, _ := cmd.Flags().GetString("title")
		
		if doi == "" && title == "" {
			return fmt.Errorf("at least one of --doi or --title must be provided")
		}
		
		var result FullTextResult
		
		// Try OpenAlex first if DOI is provided
		if doi != "" {
			client := openalex.NewClient(findFullTextEmail)
			work, err := client.GetWork(doi)
			if err == nil {
				// Check if the work is open access
				if work.OpenAccess.IsOA {
					// Try to find a full text URL
					pdfURL := ""
					version := ""
					license := ""
					
					// Check primary location first
					if work.PrimaryLocation.IsOA {
						if work.PrimaryLocation.PDF != "" {
							pdfURL = work.PrimaryLocation.PDF
							version = work.PrimaryLocation.Version
						} else if work.PrimaryLocation.HTML != "" {
							pdfURL = work.PrimaryLocation.HTML
							version = work.PrimaryLocation.Version
						}
					}
					
					// If no PDF in primary location, check other locations
					if pdfURL == "" {
						for _, location := range work.Locations {
							isPreferredVersion := strings.Contains(strings.ToLower(location.Version), findFullTextPreferVersion)
							
							if location.IsOA && isPreferredVersion && location.PDF != "" {
								pdfURL = location.PDF
								version = location.Version
								break
							}
						}
						
						// If still no preferred version, just take any OA version
						if pdfURL == "" {
							for _, location := range work.Locations {
								if location.IsOA && location.PDF != "" {
									pdfURL = location.PDF
									version = location.Version
									break
								}
							}
						}
					}
					
					if pdfURL != "" {
						result.PDFURL = pdfURL
						result.Source = "openalex"
						result.OAStatus = work.OpenAccess.OAStatus
						result.License = license
						
						// Output results and return
						outputResult(result)
						return nil
					}
				}
			}
			
			// Try Unpaywall API
			httpClient := &http.Client{Timeout: 30 * time.Second}
			unpaywallURL := fmt.Sprintf("https://api.unpaywall.org/v2/%s?email=%s", url.PathEscape(doi), url.QueryEscape(findFullTextEmail))
			
			req, err := http.NewRequest("GET", unpaywallURL, nil)
			if err == nil {
				resp, err := httpClient.Do(req)
				if err == nil && resp.StatusCode == http.StatusOK {
					defer resp.Body.Close()
					
					body, err := io.ReadAll(resp.Body)
					if err == nil {
						var unpaywallResp UnpaywallResponse
						err = json.Unmarshal(body, &unpaywallResp)
						if err == nil && unpaywallResp.IsOA && unpaywallResp.BestOALocation.URL != "" {
							isPreferredVersion := strings.Contains(strings.ToLower(unpaywallResp.BestOALocation.VersionString), findFullTextPreferVersion)
							
							if isPreferredVersion || findFullTextPreferVersion == "" {
								result.PDFURL = unpaywallResp.BestOALocation.URL
								result.Source = "unpaywall"
								result.OAStatus = unpaywallResp.OAStatus
								result.License = unpaywallResp.BestOALocation.LicenseNormalized
								
								// Output results and return
								outputResult(result)
								return nil
							}
						}
					}
				}
			}
		}
		
		// If we get here, try LibGen
		if title == "" && doi != "" {
			// Try to get title from OpenAlex or DOI
			client := openalex.NewClient(findFullTextEmail)
			work, err := client.GetWork(doi)
			if err == nil {
				title = work.Title
			}
		}
		
		if title != "" {
			libgenClient := libgen.NewClient()
			books, err := libgenClient.Search(libgen.SearchOptions{
				Query: title,
				Limit: 1,
			})
			
			if err == nil && len(books) > 0 {
				result.PDFURL = libgen.GetDownloadURL(books[0])
				result.Source = "libgen"
				result.MD5 = books[0].MD5
				
				// Output results and return
				outputResult(result)
				return nil
			}
		}
		
		// If we get here, we couldn't find full text
		return fmt.Errorf("could not find full text for the specified work")
	},
}

func outputResult(result FullTextResult) {
	if findFullTextOutput == "json" {
		// Output as JSON
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
	} else {
		// Output as text
		fmt.Printf("Full Text URL: %s\n", result.PDFURL)
		fmt.Printf("Source: %s\n", result.Source)
		
		if result.OAStatus != "" {
			fmt.Printf("OA Status: %s\n", result.OAStatus)
		}
		
		if result.License != "" {
			fmt.Printf("License: %s\n", result.License)
		}
		
		if result.MD5 != "" {
			fmt.Printf("MD5: %s\n", result.MD5)
		}
	}
}

func init() {
	rootCmd.AddCommand(findFullTextCmd)
	
	findFullTextCmd.Flags().String("doi", "", "DOI of the work")
	findFullTextCmd.Flags().String("title", "", "Title of the work")
	findFullTextCmd.Flags().StringVar(&findFullTextPreferVersion, "prefer-version", "published", "Preferred version (published, accepted, submitted)")
	findFullTextCmd.Flags().StringVar(&findFullTextOutput, "output", "text", "Output format (text, json)")
	findFullTextCmd.Flags().StringVar(&findFullTextEmail, "email", "user@example.com", "Email for API requests")
}