package cmd

import (
	"fmt"
	"strings"

	"github.com/spf13/cobra"
	"pr-analyzer/internal/git"
	"pr-analyzer/internal/output"
)

var (
	listLimit  int
	listSince  string
	listAuthor string
)

var listCmd = &cobra.Command{
	Use:   "list",
	Short: "List merge commits in the repository",
	Long: `List merge commits that can be analyzed. This helps identify
pull request merge commits for analysis.

Examples:
  # List last 10 merge commits
  pr-analyzer list

  # List merge commits from last week
  pr-analyzer list --since "1 week ago"

  # List merge commits by specific author
  pr-analyzer list --author "john@example.com"

  # List more commits
  pr-analyzer list --limit 50`,
	RunE: runList,
}

func init() {
	rootCmd.AddCommand(listCmd)

	listCmd.Flags().IntVar(&listLimit, "limit", 10, "Maximum number of merge commits to list")
	listCmd.Flags().StringVar(&listSince, "since", "", "Show commits since date (e.g., '1 week ago', '2023-01-01')")
	listCmd.Flags().StringVar(&listAuthor, "author", "", "Filter by author email or name")
}

func runList(cmd *cobra.Command, args []string) error {
	// Open repository
	repo, err := git.OpenRepository(repoPath)
	if err != nil {
		return fmt.Errorf("failed to open repository: %w", err)
	}

	// Get merge commits
	mergeCommits, err := repo.GetMergeCommits(listLimit, listSince, listAuthor)
	if err != nil {
		return fmt.Errorf("failed to get merge commits: %w", err)
	}

	if len(mergeCommits) == 0 {
		fmt.Println("No merge commits found.")
		return nil
	}

	// Output results
	switch strings.ToLower(outputFormat) {
	case "json":
		return output.PrintMergeCommitsJSON(mergeCommits)
	case "yaml", "yml":
		return output.PrintMergeCommitsYAML(mergeCommits)
	case "table":
		return output.PrintMergeCommitsTable(mergeCommits)
	default:
		return fmt.Errorf("unsupported output format: %s (supported: table, json, yaml)", outputFormat)
	}
}

