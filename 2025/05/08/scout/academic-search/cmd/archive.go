package cmd

import (
	"fmt"
	"strings"

	"github.com/scrapybara/academic-search-cli/pkg/archive"
	"github.com/spf13/cobra"
)

var (
	archiveRows      int
	archivePage      int
	archiveSort      string
	archiveMediaType string
)

var archiveCmd = &cobra.Command{
	Use:   "archive [query]",
	Short: "Search documents on Internet Archive",
	Long:  `Search for documents and papers on Internet Archive using their API.`,
	Args:  cobra.MinimumNArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		query := strings.Join(args, " ")
		client := archive.NewClient()
		
		opts := archive.SearchOptions{
			Query:     query,
			Rows:      archiveRows,
			Page:      archivePage,
			Sort:      archiveSort,
			MediaType: archiveMediaType,
		}
		
		resp, err := client.Search(opts)
		if err != nil {
			return err
		}
		
		fmt.Printf("Found %d results (showing %d)\n\n", resp.Response.NumFound, len(resp.Response.Docs))
		
		for i, doc := range resp.Response.Docs {
			fmt.Printf("%d. %s\n", i+1, doc.Title)
			
			// Creators
			creators := archive.GetCreators(doc)
			fmt.Printf("   Creators: %s\n", creators)
			
			// Publication details
			if doc.Year != "" {
				fmt.Printf("   Year: %s\n", doc.Year)
			}
			
			if len(doc.Publisher) > 0 {
				fmt.Printf("   Publisher: %s\n", strings.Join(doc.Publisher, ", "))
			}
			
			// Media type
			if doc.MediaType != "" {
				fmt.Printf("   Media Type: %s\n", doc.MediaType)
			}
			
			// Collections
			if len(doc.Collection) > 0 {
				fmt.Printf("   Collections: %s\n", strings.Join(doc.Collection[:min(3, len(doc.Collection))], ", "))
			}
			
			// Description (truncated)
			if doc.Description != "" {
				description := doc.Description
				if len(description) > 200 {
					description = description[:200] + "..."
				}
				fmt.Printf("   Description: %s\n", strings.ReplaceAll(description, "\n", " "))
			}
			
			// Formats
			if len(doc.Format) > 0 {
				fmt.Printf("   Formats: %s\n", strings.Join(doc.Format[:min(5, len(doc.Format))], ", "))
			}
			
			// Download count
			if doc.NumDownloads > 0 {
				fmt.Printf("   Downloads: %d\n", doc.NumDownloads)
			}
			
			// URLs
			viewURL := archive.GetViewURL(doc)
			fmt.Printf("   View: %s\n", viewURL)
			
			downloadURL := archive.GetDownloadURL(doc)
			fmt.Printf("   Download: %s\n", downloadURL)
			
			fmt.Println()
		}
		
		return nil
	},
}

// min returns the minimum of two integers
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func init() {
	rootCmd.AddCommand(archiveCmd)
	
	archiveCmd.Flags().IntVar(&archiveRows, "rows", 5, "Number of results")
	archiveCmd.Flags().IntVar(&archivePage, "page", 1, "Page number")
	archiveCmd.Flags().StringVar(&archiveSort, "sort", "downloads desc", "Sort order (e.g., downloads desc, date desc)")
	archiveCmd.Flags().StringVar(&archiveMediaType, "type", "texts", "Media type (e.g., texts, audio, movies)")
}