package cmd

import (
	"fmt"
	"strings"

	"github.com/scrapybara/academic-search-cli/pkg/libgen"
	"github.com/spf13/cobra"
)

var (
	libgenLimit  int
	libgenPage   int
	libgenFields string
)

var libgenCmd = &cobra.Command{
	Use:   "libgen [query]",
	Short: "Search papers on Library Genesis",
	Long:  `Search for scientific papers and books on Library Genesis.`,
	Args:  cobra.MinimumNArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		query := strings.Join(args, " ")
		client := libgen.NewClient()
		
		fmt.Printf("Searching Libgen for: %s\n", query)
		
		opts := libgen.SearchOptions{
			Query:    query,
			Limit:    libgenLimit,
			Page:     libgenPage,
			Fields:   libgenFields,
		}
		
		books, err := client.Search(opts)
		if err != nil {
			return fmt.Errorf("Libgen search error: %v", err)
		}
		
		fmt.Printf("Found %d results\n\n", len(books))
		
		for i, book := range books {
			fmt.Printf("%d. %s\n", i+1, book.Title)
			
			// Authors
			if book.Author != "" {
				fmt.Printf("   Authors: %s\n", libgen.FormatAuthors(book.Author))
			}
			
			// Publication details
			if book.Year != "" {
				fmt.Printf("   Year: %s\n", book.Year)
			}
			
			if book.Publisher != "" {
				fmt.Printf("   Publisher: %s\n", book.Publisher)
			}
			
			if book.Extension != "" {
				fmt.Printf("   Format: %s\n", strings.ToUpper(book.Extension))
			}
			
			if book.Pages != "" {
				fmt.Printf("   Pages: %s\n", book.Pages)
			}
			
			if book.Size != "" {
				fmt.Printf("   Size: %s\n", book.Size)
			}
			
			if book.Language != "" {
				fmt.Printf("   Language: %s\n", book.Language)
			}
			
			// Download links
			fmt.Printf("   MD5: %s\n", book.MD5)
			
			// Primary download link
			primaryURL := libgen.GetPrimaryDownloadURL(book)
			fmt.Printf("   Download: %s\n", primaryURL)
			
			// If there are additional download mirrors, show them
			if len(book.DownloadLinks) > 1 {
				fmt.Printf("   Mirrors:\n")
				for j, link := range book.DownloadLinks {
					if j > 0 { // Skip the first link as it's already shown above
						fmt.Printf("     %d. %s\n", j, link)
					}
					if j >= 2 { // Show at most 3 mirrors (including the primary one)
						break
					}
				}
			}
			
			fmt.Println()
		}
		
		return nil
	},
}

func init() {
	rootCmd.AddCommand(libgenCmd)
	
	libgenCmd.Flags().IntVar(&libgenLimit, "limit", 5, "Number of results to return")
	libgenCmd.Flags().IntVar(&libgenPage, "page", 1, "Page number")
	libgenCmd.Flags().StringVar(&libgenFields, "fields", "title", "Field to search (title, author, publisher)")
}