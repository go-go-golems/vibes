package cmd

import (
	"fmt"
	"strings"

	"github.com/spf13/cobra"
	"github.com/rs/zerolog/log"
	"pr-analyzer/internal/analysis"
	"pr-analyzer/internal/git"
	"pr-analyzer/internal/output"
)

var (
	prBranch     string
	baseBranch   string
	mergeCommit  string
	categories   string
	excludes     string
	useDefaults  bool
)

var analyzeCmd = &cobra.Command{
	Use:   "analyze",
	Short: "Analyze a pull request or branch for language and cross-system statistics",
	Long: `Analyze commits in a pull request to compute:
- Language-based statistics (percentage of changes by programming language)
- Cross-subsystem analysis (commits touching multiple systems)
- Custom categorization using glob patterns

Examples:
  # Analyze current branch against main
  pr-analyzer analyze --pr-branch feature/new-api --base-branch main

  # Analyze specific merge commit
  pr-analyzer analyze --merge-commit abc123def456

  # Use custom categories
  pr-analyzer analyze --pr-branch feature/ui --categories "frontend:frontend/**,ui/**;backend:backend/**,api/**"

  # Use default categories
  pr-analyzer analyze --pr-branch feature/full-stack --use-defaults

  # Exclude certain files
  pr-analyzer analyze --pr-branch feature/docs --excludes "*.md,docs/**"`,
	RunE: runAnalyze,
}

func init() {
	rootCmd.AddCommand(analyzeCmd)

	analyzeCmd.Flags().StringVar(&prBranch, "pr-branch", "", "Branch to analyze as PR (required unless using --merge-commit)")
	analyzeCmd.Flags().StringVar(&baseBranch, "base-branch", "main", "Base branch to compare against")
	analyzeCmd.Flags().StringVar(&mergeCommit, "merge-commit", "", "Specific merge commit to analyze")
	analyzeCmd.Flags().StringVar(&categories, "categories", "", "Custom categories in format 'name1:pattern1,pattern2;name2:pattern3'")
	analyzeCmd.Flags().StringVar(&excludes, "excludes", "", "Comma-separated exclude patterns")
	analyzeCmd.Flags().BoolVar(&useDefaults, "use-defaults", false, "Use default category patterns")
}

func runAnalyze(cmd *cobra.Command, args []string) error {
	// Validate arguments
	if mergeCommit == "" && prBranch == "" {
		return fmt.Errorf("either --pr-branch or --merge-commit must be specified")
	}

	if mergeCommit != "" && prBranch != "" {
		return fmt.Errorf("cannot specify both --pr-branch and --merge-commit")
	}

	log.Debug().Str("repo", repoPath).Str("base", baseBranch).Str("pr", prBranch).Str("merge", mergeCommit).Str("output", outputFormat).Msg("starting analysis")

	// Open repository
	repo, err := git.OpenRepository(repoPath)
	if err != nil {
		return fmt.Errorf("failed to open repository: %w", err)
	}

	// Create analyzer
	analyzer := analysis.NewAnalyzer(repo)

	// Configure categories
	if err := configureCategories(analyzer); err != nil {
		return fmt.Errorf("failed to configure categories: %w", err)
	}

	// Configure excludes
	if excludes != "" {
		excludePatterns := strings.Split(excludes, ",")
		for i, pattern := range excludePatterns {
			excludePatterns[i] = strings.TrimSpace(pattern)
		}
		analyzer.AddExcludePatterns(excludePatterns)
		log.Debug().Int("exclude_count", len(excludePatterns)).Msg("applied exclude patterns")
	}

	// Perform analysis
	var result *analysis.PRAnalysisResult
	if mergeCommit != "" {
		log.Debug().Str("merge", mergeCommit).Msg("mode=merge-commit")
		result, err = analyzer.AnalyzeMergeCommit(mergeCommit)
		if err != nil {
			return fmt.Errorf("failed to analyze merge commit: %w", err)
		}
	} else {
		// Validate branches exist
		if !repo.BranchExists(baseBranch) {
			return fmt.Errorf("base branch '%s' does not exist", baseBranch)
		}
		if !repo.BranchExists(prBranch) {
			return fmt.Errorf("PR branch '%s' does not exist", prBranch)
		}

		log.Debug().Str("base", baseBranch).Str("pr", prBranch).Msg("mode=branch-range")
		result, err = analyzer.AnalyzePR(baseBranch, prBranch)
		if err != nil {
			return fmt.Errorf("failed to analyze PR: %w", err)
		}
	}

	log.Debug().Int("commits", result.PRInfo.TotalCommits).Int("files", result.PRInfo.TotalFiles).Int("lines", result.PRInfo.TotalLines).Msg("analysis complete")

	// Output results
	switch strings.ToLower(outputFormat) {
	case "json":
		log.Debug().Msg("output=json")
		return output.PrintJSON(result)
	case "yaml", "yml":
		log.Debug().Msg("output=yaml")
		return output.PrintYAML(result)
	case "table":
		log.Debug().Msg("output=table")
		return output.PrintTable(result)
	default:
		return fmt.Errorf("unsupported output format: %s (supported: table, json, yaml)", outputFormat)
	}
}

func configureCategories(analyzer *analysis.Analyzer) error {
	var categoryMap map[string][]string

	if useDefaults {
		// Use default categories
		categoryMap = analysis.GetDefaultCategories()
		log.Debug().Int("category_count", len(categoryMap)).Msg("using default categories")
	} else if categories != "" {
		// Parse custom categories
		categoryMap = analysis.ParseCategoriesString(categories)
		if len(categoryMap) == 0 {
			return fmt.Errorf("failed to parse categories string: %s", categories)
		}
		log.Debug().Int("category_count", len(categoryMap)).Msg("using custom categories")
	}

	if len(categoryMap) > 0 {
		analyzer.SetCategories(categoryMap)
	}

	return nil
}

