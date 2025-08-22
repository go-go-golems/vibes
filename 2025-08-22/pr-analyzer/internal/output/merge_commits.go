package output

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/olekukonko/tablewriter"
	"gopkg.in/yaml.v3"
	"pr-analyzer/internal/git"
)

// PrintMergeCommitsTable outputs merge commits in table format
func PrintMergeCommitsTable(commits []*git.MergeCommitInfo) error {
	fmt.Printf("Merge Commits\n")
	fmt.Printf("=============\n\n")

	table := tablewriter.NewWriter(os.Stdout)
	table.Header("Hash", "Date", "Author", "Message")

	for _, commit := range commits {
		table.Append([]string{
			commit.ShortHash,
			commit.Date.Format("2006-01-02 15:04"),
			commit.Author,
			truncateString(commit.Message, 60),
		})
	}
	
	table.Render()
	fmt.Printf("\nTotal: %d merge commits\n", len(commits))
	fmt.Printf("\nTo analyze a specific merge commit, use:\n")
	fmt.Printf("  pr-analyzer analyze --merge-commit <hash>\n\n")
	
	return nil
}

// PrintMergeCommitsJSON outputs merge commits in JSON format
func PrintMergeCommitsJSON(commits []*git.MergeCommitInfo) error {
	result := map[string]interface{}{
		"merge_commits": commits,
		"total":         len(commits),
	}
	
	jsonData, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal JSON: %w", err)
	}

	fmt.Println(string(jsonData))
	return nil
}

// PrintMergeCommitsYAML outputs merge commits in YAML format
func PrintMergeCommitsYAML(commits []*git.MergeCommitInfo) error {
	result := map[string]interface{}{
		"merge_commits": commits,
		"total":         len(commits),
	}
	
	yamlData, err := yaml.Marshal(result)
	if err != nil {
		return fmt.Errorf("failed to marshal YAML: %w", err)
	}

	fmt.Println(string(yamlData))
	return nil
}

// truncateString truncates a string to the specified length
func truncateString(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}

