package stats

import (
	"encoding/json"
	"fmt"
	"os"
	"sort"
	"strings"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/scrapybara/ttmp/pkg/model"
	"github.com/scrapybara/ttmp/pkg/parser"
)

// StatsResult represents statistics about a collection of TTMP documents
type StatsResult struct {
	TotalDocuments int `json:"total_documents"`
	DocumentTypes  map[string]int `json:"document_types"`
	StatusCounts   map[string]int `json:"status_counts"`
	TopTags        []TagCount `json:"top_tags"`
	OldestDocument   *DocumentRef `json:"oldest_document"`
	NewestDocument   *DocumentRef `json:"newest_document"`
	LastUpdatedDocument *DocumentRef `json:"last_updated_document"`
	TotalWordCount   int `json:"total_word_count"`
	AverageWordCount int `json:"average_word_count"`
	ShortestDocument *DocumentRef `json:"shortest_document"`
	LongestDocument  *DocumentRef `json:"longest_document"`
	UniqueSources int `json:"unique_sources"`
	TopSources    []SourceCount `json:"top_sources"`
}

// TagCount represents a tag and its frequency
type TagCount struct {
	Tag   string `json:"tag"`
	Count int    `json:"count"`
}

// SourceCount represents a source file and its reference count
type SourceCount struct {
	Source string `json:"source"`
	Count  int    `json:"count"`
}

// DocumentRef represents a reference to a document
type DocumentRef struct {
	ID       string `json:"id"`
	Title    string `json:"title"`
	Path     string `json:"path"`
	WordCount int    `json:"word_count,omitempty"`
	Date     *time.Time `json:"date,omitempty"`
}

// NewStatsCommand creates a new stats command
func NewStatsCommand(logger *logrus.Logger, parser *parser.TTMPParser) *cobra.Command {
	var (
		jsonFormat bool
		recursive  bool
	)
	
	cmd := &cobra.Command{
		Use:   "stats [directory]",
		Short: "Show statistics about TTMP documents",
		Long:  `Analyze TTMP documents and show statistics such as document types, tags, word counts, etc.`,
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			directory := args[0]
			
			// Check if directory exists
			info, err := os.Stat(directory)
			if os.IsNotExist(err) {
				logger.Fatalf("Directory does not exist: %s", directory)
			}
			
			if !info.IsDir() {
				logger.Fatalf("Not a directory: %s", directory)
			}
			
			// Load collection
			collection, err := parser.LoadCollection(directory, recursive)
			if err != nil {
				logger.Fatalf("Error loading documents: %v", err)
			}
			
			if len(collection.Documents) == 0 {
				logger.Warn("No documents found in the specified directory")
				return
			}
			
			// Calculate statistics
			stats := calculateStats(collection)
			
			// Output results
			if jsonFormat {
				outputJSON(stats)
			} else {
				outputText(stats)
			}
		},
	}
	
	cmd.Flags().BoolVar(&jsonFormat, "json", false, "Output results in JSON format")
	cmd.Flags().BoolVarP(&recursive, "recursive", "r", false, "Recursively process subdirectories")
	
	return cmd
}

// calculateStats calculates statistics for a collection of documents
func calculateStats(collection *model.TTMPCollection) StatsResult {
	stats := StatsResult{
		TotalDocuments: len(collection.Documents),
		DocumentTypes:  make(map[string]int),
		StatusCounts:   make(map[string]int),
	}
	
	// Maps to track frequency
	tagCounts := make(map[string]int)
	sourceCounts := make(map[string]int)
	
	// Variables to track min/max values
	var oldestDoc, newestDoc, lastUpdatedDoc *DocumentRef
	var shortestDoc, longestDoc *DocumentRef
	totalWordCount := 0
	
	// Analyze each document
	for _, doc := range collection.Documents {
		// Count document types
		if doc.DocumentType != "" {
			stats.DocumentTypes[doc.DocumentType]++
		}
		
		// Count statuses
		if doc.Status != "" {
			stats.StatusCounts[doc.Status]++
		} else {
			stats.StatusCounts["unknown"]++
		}
		
		// Count tags
		for _, tag := range doc.Tags {
			tagCounts[tag]++
		}
		
		// Count source files
		for _, source := range doc.SourceFiles {
			sourceCounts[source]++
		}
		
		// Track oldest document
		if doc.Created != nil {
			if oldestDoc == nil || doc.Created.Before(*oldestDoc.Date) {
				oldestDoc = &DocumentRef{
					ID:    doc.ID,
					Title: doc.GetTitle(),
					Path:  doc.FilePath,
					Date:  doc.Created,
				}
			}
			
			if newestDoc == nil || doc.Created.After(*newestDoc.Date) {
				newestDoc = &DocumentRef{
					ID:    doc.ID,
					Title: doc.GetTitle(),
					Path:  doc.FilePath,
					Date:  doc.Created,
				}
			}
		}
		
		// Track last updated document
		if doc.Updated != nil {
			if lastUpdatedDoc == nil || doc.Updated.After(*lastUpdatedDoc.Date) {
				lastUpdatedDoc = &DocumentRef{
					ID:    doc.ID,
					Title: doc.GetTitle(),
					Path:  doc.FilePath,
					Date:  doc.Updated,
				}
			}
		}
		
		// Track word count statistics
		wordCount := doc.CalculateWordCount()
		totalWordCount += wordCount
		
		if shortestDoc == nil || wordCount < shortestDoc.WordCount {
			shortestDoc = &DocumentRef{
				ID:        doc.ID,
				Title:     doc.GetTitle(),
				Path:      doc.FilePath,
				WordCount: wordCount,
			}
		}
		
		if longestDoc == nil || wordCount > longestDoc.WordCount {
			longestDoc = &DocumentRef{
				ID:        doc.ID,
				Title:     doc.GetTitle(),
				Path:      doc.FilePath,
				WordCount: wordCount,
			}
		}
	}
	
	// Calculate average word count
	averageWordCount := 0
	if stats.TotalDocuments > 0 {
		averageWordCount = totalWordCount / stats.TotalDocuments
	}
	
	// Set word count statistics
	stats.TotalWordCount = totalWordCount
	stats.AverageWordCount = averageWordCount
	stats.ShortestDocument = shortestDoc
	stats.LongestDocument = longestDoc
	
	// Set date statistics
	stats.OldestDocument = oldestDoc
	stats.NewestDocument = newestDoc
	stats.LastUpdatedDocument = lastUpdatedDoc
	
	// Set tag statistics (top 10)
	stats.TopTags = getTopTags(tagCounts, 10)
	
	// Set source statistics
	stats.UniqueSources = len(sourceCounts)
	stats.TopSources = getTopSources(sourceCounts, 10)
	
	return stats
}

// getTopTags returns the top N tags by frequency
func getTopTags(tagCounts map[string]int, n int) []TagCount {
	var topTags []TagCount
	
	for tag, count := range tagCounts {
		topTags = append(topTags, TagCount{Tag: tag, Count: count})
	}
	
	// Sort by count (descending)
	sort.Slice(topTags, func(i, j int) bool {
		return topTags[i].Count > topTags[j].Count
	})
	
	// Limit to top N
	if len(topTags) > n {
		topTags = topTags[:n]
	}
	
	return topTags
}

// getTopSources returns the top N sources by frequency
func getTopSources(sourceCounts map[string]int, n int) []SourceCount {
	var topSources []SourceCount
	
	for source, count := range sourceCounts {
		topSources = append(topSources, SourceCount{Source: source, Count: count})
	}
	
	// Sort by count (descending)
	sort.Slice(topSources, func(i, j int) bool {
		return topSources[i].Count > topSources[j].Count
	})
	
	// Limit to top N
	if len(topSources) > n {
		topSources = topSources[:n]
	}
	
	return topSources
}

// outputJSON outputs the statistics in JSON format
func outputJSON(stats StatsResult) {
	jsonData, err := json.MarshalIndent(stats, "", "  ")
	if err != nil {
		fmt.Printf("Error formatting JSON: %v\n", err)
		return
	}
	
	fmt.Println(string(jsonData))
}

// outputText outputs the statistics in human-readable text format
func outputText(stats StatsResult) {
	fmt.Printf("TTMP Document Statistics\n")
	fmt.Printf("=======================\n\n")
	
	fmt.Printf("Total Documents: %d\n\n", stats.TotalDocuments)
	
	// Document types
	fmt.Printf("Document Types:\n")
	if len(stats.DocumentTypes) == 0 {
		fmt.Printf("  No document types found\n")
	} else {
		for docType, count := range stats.DocumentTypes {
			fmt.Printf("  %s: %d\n", docType, count)
		}
	}
	fmt.Println()
	
	// Status counts
	fmt.Printf("Status Distribution:\n")
	if len(stats.StatusCounts) == 0 {
		fmt.Printf("  No status information found\n")
	} else {
		for status, count := range stats.StatusCounts {
			fmt.Printf("  %s: %d\n", status, count)
		}
	}
	fmt.Println()
	
	// Top tags
	fmt.Printf("Top Tags:\n")
	if len(stats.TopTags) == 0 {
		fmt.Printf("  No tags found\n")
	} else {
		for _, tag := range stats.TopTags {
			fmt.Printf("  %s: %d\n", tag.Tag, tag.Count)
		}
	}
	fmt.Println()
	
	// Timeline
	fmt.Printf("Timeline:\n")
	if stats.OldestDocument != nil && stats.OldestDocument.Date != nil {
		fmt.Printf("  Oldest Document: %s (%s) - %s\n",
			stats.OldestDocument.Title,
			stats.OldestDocument.ID,
			stats.OldestDocument.Date.Format("2006-01-02"))
	}
	
	if stats.NewestDocument != nil && stats.NewestDocument.Date != nil {
		fmt.Printf("  Newest Document: %s (%s) - %s\n",
			stats.NewestDocument.Title,
			stats.NewestDocument.ID,
			stats.NewestDocument.Date.Format("2006-01-02"))
	}
	
	if stats.LastUpdatedDocument != nil && stats.LastUpdatedDocument.Date != nil {
		fmt.Printf("  Last Updated: %s (%s) - %s\n",
			stats.LastUpdatedDocument.Title,
			stats.LastUpdatedDocument.ID,
			stats.LastUpdatedDocument.Date.Format("2006-01-02"))
	}
	fmt.Println()
	
	// Word count statistics
	fmt.Printf("Word Count Statistics:\n")
	fmt.Printf("  Total Word Count: %d\n", stats.TotalWordCount)
	fmt.Printf("  Average Word Count: %d\n", stats.AverageWordCount)
	
	if stats.ShortestDocument != nil {
		fmt.Printf("  Shortest Document: %s (%s) - %d words\n",
			stats.ShortestDocument.Title,
			stats.ShortestDocument.ID,
			stats.ShortestDocument.WordCount)
	}
	
	if stats.LongestDocument != nil {
		fmt.Printf("  Longest Document: %s (%s) - %d words\n",
			stats.LongestDocument.Title,
			stats.LongestDocument.ID,
			stats.LongestDocument.WordCount)
	}
	fmt.Println()
	
	// Source file statistics
	fmt.Printf("Source File Statistics:\n")
	fmt.Printf("  Unique Source Files: %d\n", stats.UniqueSources)
	
	if len(stats.TopSources) > 0 {
		fmt.Printf("  Top Referenced Source Files:\n")
		for _, source := range stats.TopSources {
			fmt.Printf("    %s: %d references\n", source.Source, source.Count)
		}
	}
}