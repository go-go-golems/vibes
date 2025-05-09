package cmd

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/scrapybara/academic-search-cli/pkg/openalex"
	"github.com/spf13/cobra"
)

var (
	getMetricsOutput string
)

// MetricsResult represents quantitative metrics for a work
type MetricsResult struct {
	CitationCount   int         `json:"citation_count"`
	CitedByCount    int         `json:"cited_by_count"`
	ReferenceCount  int         `json:"reference_count"`
	IsOA            bool        `json:"is_oa"`
	OAStatus        string      `json:"oa_status"`
	Altmetrics      Altmetrics  `json:"altmetrics"`
}

// Altmetrics represents alternative metrics
type Altmetrics struct {
	Views      int  `json:"views"`
	Downloads  int  `json:"downloads"`
}

var getMetricsCmd = &cobra.Command{
	Use:   "get-metrics [work_id]",
	Short: "Get metrics for a scholarly work",
	Long:  `Pull headline quantitative metrics for quick triage of a work. Work ID can be OpenAlex ID (W...) or DOI (10...)`,
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		workID := args[0]
		
		client := openalex.NewClient("")
		work, err := client.GetWork(workID)
		if err != nil {
			return err
		}
		
		// Calculate metrics
		metrics := MetricsResult{
			CitationCount:  work.CitationCount,
			CitedByCount:   work.CitationCount, // OpenAlex uses the same value for both
			ReferenceCount: len(work.ReferencedWorks),
			IsOA:           work.OpenAccess.IsOA,
			OAStatus:       work.OpenAccess.OAStatus,
			// Note: OpenAlex doesn't provide views/downloads, these would be 0
		}
		
		// Output results
		if getMetricsOutput == "json" {
			// Output as JSON
			jsonData, err := json.MarshalIndent(metrics, "", "  ")
			if err != nil {
				return err
			}
			fmt.Println(string(jsonData))
		} else {
			// Output as text
			fmt.Printf("Metrics for: %s\n\n", work.Title)
			
			fmt.Printf("Citation Count: %d\n", metrics.CitationCount)
			fmt.Printf("Cited By Count: %d\n", metrics.CitedByCount)
			fmt.Printf("Reference Count: %d\n", metrics.ReferenceCount)
			fmt.Printf("Open Access: %v\n", metrics.IsOA)
			
			if metrics.OAStatus != "" {
				fmt.Printf("OA Status: %s\n", metrics.OAStatus)
			}
			
			// For completeness, but these will be 0
			if metrics.Altmetrics.Views > 0 {
				fmt.Printf("Views: %d\n", metrics.Altmetrics.Views)
			}
			
			if metrics.Altmetrics.Downloads > 0 {
				fmt.Printf("Downloads: %d\n", metrics.Altmetrics.Downloads)
			}
		}
		
		return nil
	},
}

func init() {
	rootCmd.AddCommand(getMetricsCmd)
	
	getMetricsCmd.Flags().StringVar(&getMetricsOutput, "output", "text", "Output format (text, json)")
}