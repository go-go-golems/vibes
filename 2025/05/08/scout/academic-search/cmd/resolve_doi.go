package cmd

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/scrapybara/academic-search-cli/pkg/crossref"
	"github.com/scrapybara/academic-search-cli/pkg/openalex"
	"github.com/spf13/cobra"
)

var (
	resolveDOIOutput string
)

// ResolvedWork represents a merged work from multiple sources
type ResolvedWork struct {
	ID             string             `json:"id"`
	DOI            string             `json:"doi"`
	Title          string             `json:"title"`
	Authors        []string           `json:"authors"`
	Year           int                `json:"year"`
	Journal        string             `json:"journal,omitempty"`
	IsOA           bool               `json:"is_oa"`
	OAStatus       string             `json:"oa_status,omitempty"`
	License        string             `json:"license,omitempty"`
	CitationCount  int                `json:"citation_count"`
	ReferencedWorks []string          `json:"referenced_works,omitempty"`
	CitedByCount   int                `json:"cited_by_count"`
	Concepts       []openalex.Concept `json:"concepts,omitempty"`
}

var resolveDOICmd = &cobra.Command{
	Use:   "resolve-doi [doi]",
	Short: "Resolve a DOI to get complete metadata",
	Long:  `Fetch complete metadata for a DOI from both Crossref and OpenAlex, merging into a single rich record.`,
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		doi := args[0]
		
		// Validate DOI format (basic check)
		if !strings.HasPrefix(doi, "10.") {
			return fmt.Errorf("invalid DOI format, must start with '10.'")
		}
		
		// Create a merged work
		var resolvedWork ResolvedWork
		resolvedWork.DOI = doi
		
		// Get data from OpenAlex
		openalexClient := openalex.NewClient("")
		openalexWork, err := openalexClient.GetWork(doi)
		if err != nil {
			fmt.Printf("Warning: OpenAlex lookup failed: %v\n", err)
		} else {
			// Populate from OpenAlex data
			resolvedWork.ID = openalexWork.ID
			resolvedWork.Title = openalexWork.Title
			
			// Authors
			for _, authorship := range openalexWork.Authorships {
				resolvedWork.Authors = append(resolvedWork.Authors, authorship.Author.Name)
			}
			
			resolvedWork.Year = openalexWork.PublicationYear
			
			if openalexWork.PrimaryLocation.Source.Name != "" {
				resolvedWork.Journal = openalexWork.PrimaryLocation.Source.Name
			}
			
			resolvedWork.IsOA = openalexWork.OpenAccess.IsOA
			resolvedWork.OAStatus = openalexWork.OpenAccess.OAStatus
			resolvedWork.CitationCount = openalexWork.CitationCount
			resolvedWork.ReferencedWorks = openalexWork.ReferencedWorks
			resolvedWork.CitedByCount = openalexWork.CitationCount
			resolvedWork.Concepts = openalexWork.Concepts
		}
		
		// Get data from Crossref
		crossrefClient := crossref.NewClient("")
		crossrefWorks, err := crossrefClient.Search(crossref.SearchOptions{
			Query: fmt.Sprintf("doi:%s", doi),
			Rows:  1,
		})
		if err != nil || len(crossrefWorks.Message.Items) == 0 {
			fmt.Printf("Warning: Crossref lookup failed: %v\n", err)
		} else {
			crossrefWork := crossrefWorks.Message.Items[0]
			
			// If we don't have a title from OpenAlex, use Crossref's
			if resolvedWork.Title == "" {
				resolvedWork.Title = crossref.GetTitle(crossrefWork)
			}
			
			// If we don't have authors from OpenAlex, use Crossref's
			if len(resolvedWork.Authors) == 0 {
				var authors []string
				for _, author := range crossrefWork.Author {
					authors = append(authors, fmt.Sprintf("%s %s", author.Given, author.Family))
				}
				resolvedWork.Authors = authors
			}
			
			// If we don't have a year from OpenAlex, use Crossref's
			if resolvedWork.Year == 0 && len(crossrefWork.Published.DateParts) > 0 && len(crossrefWork.Published.DateParts[0]) > 0 {
				resolvedWork.Year = crossrefWork.Published.DateParts[0][0]
			}
			
			// If we don't have a journal from OpenAlex, use Crossref's
			if resolvedWork.Journal == "" {
				resolvedWork.Journal = crossrefWork.Publisher
			}
			
			// License from Crossref always takes precedence
			if len(crossrefWork.Link) > 0 {
				for _, link := range crossrefWork.Link {
					if strings.Contains(strings.ToLower(link.ContentType), "license") {
						resolvedWork.License = link.URL
						break
					}
				}
			}
		}
		
		// Output results
		if resolveDOIOutput == "json" {
			// Output as JSON
			jsonData, err := json.MarshalIndent(resolvedWork, "", "  ")
			if err != nil {
				return err
			}
			fmt.Println(string(jsonData))
		} else {
			// Output as text
			fmt.Printf("DOI: %s\n", resolvedWork.DOI)
			fmt.Printf("Title: %s\n", resolvedWork.Title)
			
			if len(resolvedWork.Authors) > 0 {
				fmt.Printf("Authors: %s\n", strings.Join(resolvedWork.Authors, ", "))
			}
			
			if resolvedWork.Year > 0 {
				fmt.Printf("Year: %d\n", resolvedWork.Year)
			}
			
			if resolvedWork.Journal != "" {
				fmt.Printf("Journal: %s\n", resolvedWork.Journal)
			}
			
			fmt.Printf("Open Access: %v\n", resolvedWork.IsOA)
			if resolvedWork.OAStatus != "" {
				fmt.Printf("OA Status: %s\n", resolvedWork.OAStatus)
			}
			
			if resolvedWork.License != "" {
				fmt.Printf("License: %s\n", resolvedWork.License)
			}
			
			fmt.Printf("Citation Count: %d\n", resolvedWork.CitationCount)
			
			if len(resolvedWork.ReferencedWorks) > 0 {
				fmt.Printf("References: %d works\n", len(resolvedWork.ReferencedWorks))
			}
			
			if resolvedWork.CitedByCount > 0 {
				fmt.Printf("Cited By: %d works\n", resolvedWork.CitedByCount)
			}
			
			if len(resolvedWork.Concepts) > 0 {
				concepts := []string{}
				for i, concept := range resolvedWork.Concepts {
					if i < 5 { // Show top 5 concepts
						concepts = append(concepts, fmt.Sprintf("%s (%.2f)", concept.Name, concept.Score))
					}
				}
				fmt.Printf("Concepts: %s\n", strings.Join(concepts, ", "))
			}
			
			if resolvedWork.ID != "" {
				fmt.Printf("OpenAlex ID: %s\n", resolvedWork.ID)
			}
		}
		
		return nil
	},
}

func init() {
	rootCmd.AddCommand(resolveDOICmd)
	
	resolveDOICmd.Flags().StringVar(&resolveDOIOutput, "output", "text", "Output format (text, json)")
}