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
		
		opts := libgen.SearchOptions{
			Query:    query,
			Limit:    libgenLimit,
			Page:     libgenPage,
			Fields:   libgenFields,
		}
		
		books, err := client.Search(opts)
		if err != nil {
			return err
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
			
			// DOI
			if book.DOI != "" {
				fmt.Printf("   DOI: %s\n", book.DOI)
			}
			
			// Download URL
			downloadURL := libgen.GetDownloadURL(book)
			fmt.Printf("   Download: %s\n", downloadURL)
			
			fmt.Println()
		}
		
		return nil
	},
}

func init() {
	rootCmd.AddCommand(libgenCmd)
	
	libgenCmd.Flags().IntVar(&libgenLimit, "limit", 5, "Number of results to return")
	libgenCmd.Flags().IntVar(&libgenPage, "page", 1, "Page number")
	libgenCmd.Flags().StringVar(&libgenFields, "fields", "title,author,year,publisher", "Fields to search (comma-separated)")
}