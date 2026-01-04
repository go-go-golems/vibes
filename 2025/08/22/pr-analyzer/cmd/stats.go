package cmd

import (
	"fmt"
	"strings"

	"github.com/spf13/cobra"
	"pr-analyzer/internal/analysis"
	"pr-analyzer/internal/git"
	"pr-analyzer/internal/output"
)

var (
	statsLimit int
	statsSince string
	statsUseDefaults bool
)

var statsCmd = &cobra.Command{
	Use:   "stats",
	Short: "Show repository statistics and trends",
	Long: `Analyze multiple merge commits to show repository statistics and trends.
This provides insights into development patterns, language usage, and
cross-system changes over time.

Examples:
  # Analyze last 20 merge commits
  pr-analyzer stats

  # Analyze merge commits from last month
  pr-analyzer stats --since "1 month ago"

  # Analyze more commits for better trends
  pr-analyzer stats --limit 50`,
	RunE: runStats,
}

func init() {
	rootCmd.AddCommand(statsCmd)

	statsCmd.Flags().IntVar(&statsLimit, "limit", 20, "Number of merge commits to analyze")
	statsCmd.Flags().StringVar(&statsSince, "since", "", "Analyze commits since date")
	statsCmd.Flags().BoolVar(&statsUseDefaults, "use-defaults", false, "Use default category patterns")
}

func runStats(cmd *cobra.Command, args []string) error {
	// Open repository
	repo, err := git.OpenRepository(repoPath)
	if err != nil {
		return fmt.Errorf("failed to open repository: %w", err)
	}

	// Get merge commits
	mergeCommits, err := repo.GetMergeCommits(statsLimit, statsSince, "")
	if err != nil {
		return fmt.Errorf("failed to get merge commits: %w", err)
	}

	if len(mergeCommits) == 0 {
		fmt.Println("No merge commits found for analysis.")
		return nil
	}

	// Create analyzer
	analyzer := analysis.NewAnalyzer(repo)

	// Configure categories if using defaults
	if statsUseDefaults {
		categoryMap := analysis.GetDefaultCategories()
		analyzer.SetCategories(categoryMap)
	}

	// Analyze all merge commits
	var allResults []*analysis.PRAnalysisResult
	for _, mergeCommit := range mergeCommits {
		result, err := analyzer.AnalyzeMergeCommit(mergeCommit.Hash)
		if err != nil {
			fmt.Printf("Warning: Failed to analyze commit %s: %v\n", mergeCommit.ShortHash, err)
			continue
		}
		allResults = append(allResults, result)
	}

	if len(allResults) == 0 {
		return fmt.Errorf("no commits could be analyzed")
	}

	// Calculate aggregate statistics
	stats := analysis.CalculateAggregateStats(allResults)

	// Output results
	switch strings.ToLower(outputFormat) {
	case "json":
		return output.PrintStatsJSON(stats)
	case "yaml", "yml":
		return output.PrintStatsYAML(stats)
	case "table":
		return output.PrintStatsTable(stats)
	default:
		return fmt.Errorf("unsupported output format: %s (supported: table, json, yaml)", outputFormat)
	}
}

