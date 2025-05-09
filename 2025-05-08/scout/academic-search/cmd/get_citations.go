package cmd

import (
	"encoding/json"
	"fmt"

	"github.com/scrapybara/academic-search-cli/pkg/openalex"
	"github.com/spf13/cobra"
)

var (
	getCitationsDirection string
	getCitationsLimit     int
	getCitationsCursor    string
	getCitationsOutput    string
)

// CitationsResult represents the result of citation retrieval
type CitationsResult struct {
	Citations   []Citation `json:"citations"`
	NextCursor  string     `json:"next_cursor,omitempty"`
}

// Citation represents a minimal citation record
type Citation struct {
	ID     string `json:"id"`
	DOI    string `json:"doi,omitempty"`
	Title  string `json:"title"`
	Year   int    `json:"year,omitempty"`
}

var getCitationsCmd = &cobra.Command{
	Use:   "get-citations [work_id]",
	Short: "Get citations for a scholarly work",
	Long:  `Retrieve one hop of the citation graph—either the works this paper cites (outgoing) or the works that cite it (incoming).`,
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		workID := args[0]
		
		client := openalex.NewClient("")
		var result CitationsResult
		
		if getCitationsDirection == "refs" {
			// Get references (works this paper cites)
			work, err := client.GetWork(workID)
			if err != nil {
				return err
			}
			
			// For each referenced work, get basic details
			count := 0
			for _, refID := range work.ReferencedWorks {
				if count >= getCitationsLimit {
					break
				}
				
				refWork, err := client.GetWork(refID)
				if err != nil {
					fmt.Printf("Warning: Could not retrieve work %s: %v\n", refID, err)
					continue
				}
				
				citation := Citation{
					ID:    refID,
					DOI:   refWork.DOI,
					Title: refWork.Title,
					Year:  refWork.PublicationYear,
				}
				
				result.Citations = append(result.Citations, citation)
				count++
			}
			
		} else if getCitationsDirection == "cited_by" {
			// Get works that cite this paper
			resp, err := client.GetCitations(workID, getCitationsLimit, getCitationsCursor)
			if err != nil {
				return err
			}
			
			for _, work := range resp.Works {
				citation := Citation{
					ID:    work.ID,
					DOI:   work.DOI,
					Title: work.Title,
					Year:  work.PublicationYear,
				}
				
				result.Citations = append(result.Citations, citation)
			}
			
			result.NextCursor = resp.Meta.NextCursor
		} else {
			return fmt.Errorf("invalid direction: %s (must be 'refs' or 'cited_by')", getCitationsDirection)
		}
		
		// Output results
		if getCitationsOutput == "json" {
			// Output as JSON
			jsonData, err := json.MarshalIndent(result, "", "  ")
			if err != nil {
				return err
			}
			fmt.Println(string(jsonData))
		} else {
			// Output as text
			if getCitationsDirection == "refs" {
				fmt.Printf("References for work %s:\n\n", workID)
			} else {
				fmt.Printf("Citations of work %s:\n\n", workID)
			}
			
			for i, citation := range result.Citations {
				fmt.Printf("%d. %s\n", i+1, citation.Title)
				
				if citation.Year > 0 {
					fmt.Printf("   Year: %d\n", citation.Year)
				}
				
				if citation.DOI != "" {
					fmt.Printf("   DOI: %s\n", citation.DOI)
				}
				
				fmt.Printf("   ID: %s\n\n", citation.ID)
			}
			
			if result.NextCursor != "" {
				fmt.Printf("Next page cursor: %s\n", result.NextCursor)
				fmt.Printf("Use --cursor %s to get the next page\n", result.NextCursor)
			}
		}
		
		return nil
	},
}

func init() {
	rootCmd.AddCommand(getCitationsCmd)
	
	getCitationsCmd.Flags().StringVar(&getCitationsDirection, "direction", "cited_by", "Direction of citations (refs, cited_by)")
	getCitationsCmd.Flags().IntVar(&getCitationsLimit, "limit", 100, "Maximum number of citations to return")
	getCitationsCmd.Flags().StringVar(&getCitationsCursor, "cursor", "", "Cursor for pagination")
	getCitationsCmd.Flags().StringVar(&getCitationsOutput, "output", "text", "Output format (text, json)")
}