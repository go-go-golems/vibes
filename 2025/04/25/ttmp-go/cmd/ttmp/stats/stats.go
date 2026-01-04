package stats

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"

	"github.com/user/ttmp-go/pkg/model"
	"github.com/user/ttmp-go/pkg/parser"
	"github.com/user/ttmp-go/pkg/util/fileutil"
)

// NewStatsCommand creates a new stats command
func NewStatsCommand() *cobra.Command {
	var recursive bool

	cmd := &cobra.Command{
		Use:   "stats [path]",
		Short: "Show statistics about TTMP documents",
		Long:  `Shows statistics about TTMP documents in a directory.`,
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			path := args[0]

			// Check if path exists
			fileInfo, err := os.Stat(path)
			if err != nil {
				return fmt.Errorf("failed to access path: %s", path)
			}

			if !fileInfo.IsDir() {
				return fmt.Errorf("path is not a directory: %s", path)
			}

			// Find all markdown files
			var files []string
			if recursive {
				files, err = fileutil.FindMarkdownFilesRecursive(path)
			} else {
				files, err = fileutil.FindMarkdownFiles(path)
			}

			if err != nil {
				return fmt.Errorf("failed to find markdown files: %s", path)
			}

			// Parse all files
			p := parser.NewParser()
			var docs []*model.TTMPDocument
			for _, file := range files {
				doc, err := p.ParseFile(file)
				if err != nil {
					fmt.Fprintf(os.Stderr, "Warning: Failed to parse %s: %v\n", file, err)
					continue
				}
				docs = append(docs, doc)
			}

			// Display statistics
			fmt.Printf("Total documents: %d\n\n", len(docs))

			// Count document types
			typeCount := make(map[string]int)
			for _, doc := range docs {
				typeCount[doc.Type]++
			}

			fmt.Println("Document types:")
			for docType, count := range typeCount {
				fmt.Printf("  %s: %d\n", docType, count)
			}

			// Count tags
			tagCount := make(map[string]int)
			for _, doc := range docs {
				for _, tag := range doc.Tags {
					tagCount[tag]++
				}
			}

			fmt.Println("\nMost used tags:")
			// Find top 10 tags (or fewer if there are fewer than 10)
			topTags := findTopTags(tagCount, 10)
			for _, tag := range topTags {
				fmt.Printf("  %s: %d\n", tag.Name, tag.Count)
			}

			return nil
		},
	}

	cmd.Flags().BoolVarP(&recursive, "recursive", "r", false, "Recursively search documents in subdirectories")

	return cmd
}

type tagStat struct {
	Name  string
	Count int
}

// findTopTags finds the top N tags by count
func findTopTags(tagCount map[string]int, n int) []tagStat {
	// Convert map to slice
	var tags []tagStat
	for tag, count := range tagCount {
		tags = append(tags, tagStat{tag, count})
	}

	// Sort tags by count (descending)
	for i := 0; i < len(tags); i++ {
		for j := i + 1; j < len(tags); j++ {
			if tags[i].Count < tags[j].Count {
				tags[i], tags[j] = tags[j], tags[i]
			}
		}
	}

	// Return top N (or all if there are fewer than N)
	if len(tags) < n {
		return tags
	}
	return tags[:n]
}