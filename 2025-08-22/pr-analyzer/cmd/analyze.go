package cmd

import (
	"fmt"
	"strings"
	"context"

	"github.com/spf13/cobra"
	"github.com/rs/zerolog/log"
	"pr-analyzer/internal/analysis"
	"pr-analyzer/internal/git"
	"pr-analyzer/internal/output"
	"pr-analyzer/internal/db"
)

var (
	prBranch     string
	baseBranch   string
	commitHash   string
	categories   string
	excludes     string
	useDefaults  bool
	saveToDB     bool
	dbPath       string
)

var analyzeCmd = &cobra.Command{
	Use:   "analyze",
	Short: "Analyze a pull request, commit, or branch range",
	Long: `Analyze commits in a pull request or a single commit to compute:
- Language-based statistics (percentage of changes by programming language)
- Cross-subsystem analysis (commits touching multiple systems)
- Custom categorization using glob patterns

Examples:
  # Analyze current branch against main
  pr-analyzer analyze --pr-branch feature/new-api --base-branch main

  # Analyze specific commit (merge or non-merge)
  pr-analyzer analyze --commit abc123def456

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

	analyzeCmd.Flags().StringVar(&prBranch, "pr-branch", "", "Branch to analyze as PR (required unless using --commit)")
	analyzeCmd.Flags().StringVar(&baseBranch, "base-branch", "main", "Base branch to compare against")
	analyzeCmd.Flags().StringVar(&commitHash, "commit", "", "Specific commit to analyze (merge or non-merge)")
	analyzeCmd.Flags().StringVar(&categories, "categories", "", "Custom categories in format 'name1:pattern1,pattern2;name2:pattern3'")
	analyzeCmd.Flags().StringVar(&excludes, "excludes", "", "Comma-separated exclude patterns")
	analyzeCmd.Flags().BoolVar(&useDefaults, "use-defaults", false, "Use default category patterns")
	analyzeCmd.Flags().BoolVar(&saveToDB, "save-to-db", false, "Save analysis to sqlite database (use --db-path)")
	analyzeCmd.Flags().StringVar(&dbPath, "db-path", "pr-analyzer.sqlite", "Path to sqlite database file")
}

func runAnalyze(cmd *cobra.Command, args []string) error {
	// Validate arguments
	if commitHash == "" && prBranch == "" {
		return fmt.Errorf("either --pr-branch or --commit must be specified")
	}

	if commitHash != "" && prBranch != "" {
		return fmt.Errorf("cannot specify both --pr-branch and --commit")
	}

	log.Debug().Str("repo", repoPath).Str("base", baseBranch).Str("pr", prBranch).Str("commit", commitHash).Str("output", outputFormat).Msg("starting analysis")

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
	if commitHash != "" {
		log.Debug().Str("commit", commitHash).Msg("mode=commit")
		result, err = analyzer.AnalyzeCommit(commitHash)
		if err != nil {
			return fmt.Errorf("failed to analyze commit: %w", err)
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

	// Optionally save to sqlite
	if saveToDB {
		ctx := context.Background()
		store, err := db.Open(ctx, dbPath)
		if err != nil {
			return fmt.Errorf("open db: %w", err)
		}
		defer store.Close()
		if _, err := store.InsertAnalysis(ctx, result); err != nil {
			return fmt.Errorf("insert analysis: %w", err)
		}
		log.Info().Str("db", dbPath).Msg("analysis saved to database")
	}

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

