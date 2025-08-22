package analysis

import (
	"fmt"
	"sort"
	"pr-analyzer/internal/git"
	"github.com/go-git/go-git/v5/plumbing/object"
	"github.com/rs/zerolog/log"
)

// Analyzer performs PR analysis
type Analyzer struct {
	repo             *git.Repository
	languageDetector *LanguageDetector
	categoryMatcher  *CategoryMatcher
}

// NewAnalyzer creates a new analyzer instance
func NewAnalyzer(repo *git.Repository) *Analyzer {
	return &Analyzer{
		repo:             repo,
		languageDetector: NewLanguageDetector(),
		categoryMatcher:  NewCategoryMatcher(),
	}
}

// SetCategories configures the category matcher with custom categories
func (a *Analyzer) SetCategories(categories map[string][]string) {
	a.categoryMatcher = NewCategoryMatcher()
	for name, patterns := range categories {
		a.categoryMatcher.AddCategory(name, patterns)
	}
	log.Debug().Int("categories", len(categories)).Msg("set categories")
}

// AddExcludePatterns adds patterns to exclude from analysis
func (a *Analyzer) AddExcludePatterns(patterns []string) {
	for _, pattern := range patterns {
		a.categoryMatcher.AddExcludePattern(pattern)
	}
	log.Debug().Int("excludes", len(patterns)).Msg("added exclude patterns")
}

// AnalyzePR performs complete PR analysis
func (a *Analyzer) AnalyzePR(baseBranch, prBranch string) (*PRAnalysisResult, error) {
	// Get commits between branches
	commits, err := a.repo.GetCommitsBetween(baseBranch, prBranch)
	if err != nil {
		return nil, fmt.Errorf("failed to get commits: %w", err)
	}
	log.Debug().Str("base", baseBranch).Str("head", prBranch).Int("commits", len(commits)).Msg("analyzing PR")

	return a.analyzeCommits(commits, baseBranch, prBranch, "")
}

// AnalyzeMergeCommit analyzes a specific merge commit
func (a *Analyzer) AnalyzeMergeCommit(mergeCommitHash string) (*PRAnalysisResult, error) {
	commits, err := a.repo.GetCommitsFromMerge(mergeCommitHash)
	if err != nil {
		return nil, fmt.Errorf("failed to get commits from merge: %w", err)
	}
	log.Debug().Str("merge", mergeCommitHash).Int("commits", len(commits)).Msg("analyzing merge commit")

	return a.analyzeCommits(commits, "", "", mergeCommitHash)
}

// analyzeCommits performs the core analysis on a set of commits
func (a *Analyzer) analyzeCommits(commits []*object.Commit, baseBranch, prBranch, mergeCommit string) (*PRAnalysisResult, error) {
	// Get diffs for all commits
	var diffs []*git.CommitDiff
	var commitInfos []CommitInfo

	for _, commit := range commits {
		diff, err := a.repo.GetCommitDiff(commit)
		if err != nil {
			return nil, fmt.Errorf("failed to get diff for commit %s: %w", commit.Hash.String(), err)
		}
		diffs = append(diffs, diff)

		// Create commit info
		commitInfo := CommitInfo{
			Hash:         commit.Hash.String(),
			Message:      commit.Message,
			Author:       commit.Author.Name,
			Date:         commit.Author.When,
			FilesChanged: make([]string, len(diff.Files)),
			Languages:    make(map[string]int),
			Categories:   make(map[string]int),
			LinesAdded:   diff.TotalAdded,
			LinesDeleted: diff.TotalDeleted,
		}

		// Analyze files in this commit
		for i, fileDiff := range diff.Files {
			commitInfo.FilesChanged[i] = fileDiff.Path

			// Detect language
			language := a.languageDetector.DetectLanguage(fileDiff.Path)
			commitInfo.Languages[language]++

			// Categorize file
			categories := a.categoryMatcher.CategorizeFile(fileDiff.Path)
			log.Trace().Str("file", fileDiff.Path).Strs("categories", categories).Msg("categorized file")
			for _, category := range categories {
				commitInfo.Categories[category]++
			}
		}

		commitInfos = append(commitInfos, commitInfo)
	}

	// Calculate language statistics
	languageStats := a.calculateLanguageStats(diffs)

	// Calculate cross-system statistics
	crossSystemStats := a.calculateCrossSystemStats(commitInfos)

	// Calculate totals
	totalAdded, totalDeleted := git.GetTotalChanges(diffs)
	fileChanges := git.GetFileChanges(diffs)

	result := &PRAnalysisResult{
		PRInfo: PRInfo{
			BaseBranch:   baseBranch,
			PRBranch:     prBranch,
			MergeCommit:  mergeCommit,
			TotalFiles:   len(fileChanges),
			TotalLines:   totalAdded + totalDeleted,
			TotalCommits: len(commits),
		},
		LanguageStats:    languageStats,
		CrossSystemStats: crossSystemStats,
		Commits:          commitInfos,
		Categories:       a.categoryMatcher.GetCategories(),
	}

	log.Debug().Int("commits", len(commits)).Int("files", result.PRInfo.TotalFiles).Int("lines", result.PRInfo.TotalLines).Msg("completed analysis")
	return result, nil
}

// calculateLanguageStats computes language-based statistics
func (a *Analyzer) calculateLanguageStats(diffs []*git.CommitDiff) []LanguageStats {
	languageMap := make(map[string]*LanguageStats)
	totalLines := 0

	// Aggregate by language
	for _, diff := range diffs {
		for _, fileDiff := range diff.Files {
			language := a.languageDetector.DetectLanguage(fileDiff.Path)
			
			if _, exists := languageMap[language]; !exists {
				languageMap[language] = &LanguageStats{
					Language: language,
				}
			}

			stats := languageMap[language]
			stats.FilesChanged++
			stats.LinesAdded += fileDiff.LinesAdded
			stats.LinesDeleted += fileDiff.LinesDeleted
			stats.LinesModified += fileDiff.LinesAdded + fileDiff.LinesDeleted

			totalLines += fileDiff.LinesAdded + fileDiff.LinesDeleted
		}
	}

	// Calculate percentages and convert to slice
	var result []LanguageStats
	for _, stats := range languageMap {
		if totalLines > 0 {
			stats.Percentage = float64(stats.LinesModified) / float64(totalLines) * 100
		}
		result = append(result, *stats)
	}

	// Sort by percentage (descending)
	sort.Slice(result, func(i, j int) bool {
		return result[i].Percentage > result[j].Percentage
	})

	log.Debug().Int("languages", len(result)).Int("total_lines", totalLines).Msg("calculated language stats")
	return result
}

// calculateCrossSystemStats computes cross-subsystem statistics
func (a *Analyzer) calculateCrossSystemStats(commits []CommitInfo) CrossSystemStats {
	totalCommits := len(commits)
	singleSystemCommits := 0
	multiSystemCommits := 0
	systemTouchMatrix := make(map[string]map[string]int)
	systemTouchCount := make(map[string]int)

	for _, commit := range commits {
		// Get unique systems touched by this commit
		systemsInCommit := make(map[string]bool)
		for system := range commit.Categories {
			if system != "uncategorized" {
				systemsInCommit[system] = true
			}
		}

		// Count systems touched
		systemCount := len(systemsInCommit)
		if systemCount <= 1 {
			singleSystemCommits++
		} else {
			multiSystemCommits++
		}

		// Update system touch counts
		for system := range systemsInCommit {
			systemTouchCount[system]++
		}

		// Update system touch matrix (co-occurrence)
		systems := make([]string, 0, len(systemsInCommit))
		for system := range systemsInCommit {
			systems = append(systems, system)
		}

		for i, system1 := range systems {
			if systemTouchMatrix[system1] == nil {
				systemTouchMatrix[system1] = make(map[string]int)
			}
			for j, system2 := range systems {
				if i != j {
					systemTouchMatrix[system1][system2]++
				}
			}
		}
	}

	// Create sorted list of most touched systems
	var mostTouched []SystemTouch
	for system, count := range systemTouchCount {
		mostTouched = append(mostTouched, SystemTouch{
			System: system,
			Count:  count,
		})
	}
	sort.Slice(mostTouched, func(i, j int) bool {
		return mostTouched[i].Count > mostTouched[j].Count
	})

	// Calculate cross-system rate
	crossSystemRate := 0.0
	if totalCommits > 0 {
		crossSystemRate = float64(multiSystemCommits) / float64(totalCommits) * 100
	}

	stats := CrossSystemStats{
		TotalCommits:        totalCommits,
		SingleSystemCommits: singleSystemCommits,
		MultiSystemCommits:  multiSystemCommits,
		CrossSystemRate:     crossSystemRate,
		SystemTouchMatrix:   systemTouchMatrix,
		MostTouchedSystems:  mostTouched,
	}
	log.Debug().Int("total_commits", totalCommits).Float64("cross_rate", crossSystemRate).Msg("calculated cross-system stats")
	return stats
}

