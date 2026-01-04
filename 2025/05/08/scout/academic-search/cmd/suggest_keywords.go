package cmd

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/scrapybara/academic-search-cli/pkg/openalex"
	"github.com/spf13/cobra"
)

var (
	suggestKeywordsMaxKeywords int
	suggestKeywordsOutput     string
)

// KeywordsResult represents the result of keyword suggestions
type KeywordsResult struct {
	Keywords []Keyword `json:"keywords"`
}

// Keyword represents a controlled-vocabulary concept
type Keyword struct {
	ID        string  `json:"id"`
	Name      string  `json:"display_name"`
	Relevance float64 `json:"relevance"`
}

var suggestKeywordsCmd = &cobra.Command{
	Use:   "suggest-keywords [text]",
	Short: "Suggest keywords from text",
	Long:  `Return controlled-vocabulary concepts for a work title or arbitrary text to seed further searches.`,
	Args:  cobra.MinimumNArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		text := strings.Join(args, " ")
		
		client := openalex.NewClient("")
		concepts, err := client.SuggestKeywords(text, suggestKeywordsMaxKeywords)
		if err != nil {
			return err
		}
		
		// Convert to our output format
		var result KeywordsResult
		for _, concept := range concepts {
			result.Keywords = append(result.Keywords, Keyword{
				ID:        concept.ID,
				Name:      concept.Name,
				Relevance: concept.Score,
			})
		}
		
		// Output results
		if suggestKeywordsOutput == "json" {
			// Output as JSON
			jsonData, err := json.MarshalIndent(result, "", "  ")
			if err != nil {
				return err
			}
			fmt.Println(string(jsonData))
		} else {
			// Output as text
			fmt.Printf("Keywords for: %s\n\n", text)
			
			for i, keyword := range result.Keywords {
				fmt.Printf("%d. %s (%.2f)\n", i+1, keyword.Name, keyword.Relevance)
				fmt.Printf("   ID: %s\n", keyword.ID)
			}
		}
		
		return nil
	},
}

func init() {
	rootCmd.AddCommand(suggestKeywordsCmd)
	
	suggestKeywordsCmd.Flags().IntVar(&suggestKeywordsMaxKeywords, "max", 10, "Maximum number of keywords to return")
	suggestKeywordsCmd.Flags().StringVar(&suggestKeywordsOutput, "output", "text", "Output format (text, json)")
}