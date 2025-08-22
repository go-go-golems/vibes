package analysis

import (
	"sort"
	"time"
)

// AggregateStats represents statistics across multiple PRs
type AggregateStats struct {
	Summary              AggregateSummary              `json:"summary"`
	LanguageTrends       []LanguageTrend               `json:"language_trends"`
	CrossSystemTrends    CrossSystemTrends             `json:"cross_system_trends"`
	MostActiveLanguages  []LanguageActivity            `json:"most_active_languages"`
	MostActiveSystems    []SystemActivity              `json:"most_active_systems"`
	PRSizeDistribution   PRSizeDistribution            `json:"pr_size_distribution"`
	TimeRange            TimeRange                     `json:"time_range"`
}

type AggregateSummary struct {
	TotalPRs            int     `json:"total_prs"`
	TotalCommits        int     `json:"total_commits"`
	TotalFiles          int     `json:"total_files"`
	TotalLines          int     `json:"total_lines"`
	AvgFilesPerPR       float64 `json:"avg_files_per_pr"`
	AvgLinesPerPR       float64 `json:"avg_lines_per_pr"`
	AvgCommitsPerPR     float64 `json:"avg_commits_per_pr"`
	CrossSystemRate     float64 `json:"cross_system_rate"`
}

type LanguageTrend struct {
	Language    string  `json:"language"`
	TotalFiles  int     `json:"total_files"`
	TotalLines  int     `json:"total_lines"`
	Percentage  float64 `json:"percentage"`
	PRCount     int     `json:"pr_count"`
}

type CrossSystemTrends struct {
	SingleSystemPRs   int     `json:"single_system_prs"`
	MultiSystemPRs    int     `json:"multi_system_prs"`
	CrossSystemRate   float64 `json:"cross_system_rate"`
	AvgSystemsPerPR   float64 `json:"avg_systems_per_pr"`
	MaxSystemsInPR    int     `json:"max_systems_in_pr"`
}

type LanguageActivity struct {
	Language string `json:"language"`
	PRCount  int    `json:"pr_count"`
	Files    int    `json:"files"`
	Lines    int    `json:"lines"`
}

type SystemActivity struct {
	System  string `json:"system"`
	PRCount int    `json:"pr_count"`
	Files   int    `json:"files"`
}

type PRSizeDistribution struct {
	Small  int `json:"small"`  // < 10 files
	Medium int `json:"medium"` // 10-50 files
	Large  int `json:"large"`  // 50-200 files
	XLarge int `json:"xlarge"` // > 200 files
}

type TimeRange struct {
	Start time.Time `json:"start"`
	End   time.Time `json:"end"`
	Days  int       `json:"days"`
}

// CalculateAggregateStats calculates aggregate statistics from multiple PR analysis results
func CalculateAggregateStats(results []*PRAnalysisResult) *AggregateStats {
	if len(results) == 0 {
		return &AggregateStats{}
	}

	// Initialize counters
	languageMap := make(map[string]*LanguageActivity)
	systemMap := make(map[string]*SystemActivity)
	
	var totalFiles, totalLines, totalCommits int
	var singleSystemPRs, multiSystemPRs int
	var totalSystems int
	var maxSystems int
	var sizeDistribution PRSizeDistribution
	
	var earliestTime, latestTime time.Time

	// Process each PR result
	for i, result := range results {
		totalFiles += result.PRInfo.TotalFiles
		totalLines += result.PRInfo.TotalLines
		totalCommits += result.PRInfo.TotalCommits

		// Cross-system analysis
		if result.CrossSystemStats.MultiSystemCommits > 0 {
			multiSystemPRs++
		} else {
			singleSystemPRs++
		}

		// Count unique systems in this PR
		systemsInPR := len(result.CrossSystemStats.MostTouchedSystems)
		totalSystems += systemsInPR
		if systemsInPR > maxSystems {
			maxSystems = systemsInPR
		}

		// PR size distribution
		switch {
		case result.PRInfo.TotalFiles < 10:
			sizeDistribution.Small++
		case result.PRInfo.TotalFiles < 50:
			sizeDistribution.Medium++
		case result.PRInfo.TotalFiles < 200:
			sizeDistribution.Large++
		default:
			sizeDistribution.XLarge++
		}

		// Language statistics
		for _, langStat := range result.LanguageStats {
			if activity, exists := languageMap[langStat.Language]; exists {
				activity.PRCount++
				activity.Files += langStat.FilesChanged
				activity.Lines += langStat.LinesModified
			} else {
				languageMap[langStat.Language] = &LanguageActivity{
					Language: langStat.Language,
					PRCount:  1,
					Files:    langStat.FilesChanged,
					Lines:    langStat.LinesModified,
				}
			}
		}

		// System statistics
		for _, systemStat := range result.CrossSystemStats.MostTouchedSystems {
			if activity, exists := systemMap[systemStat.System]; exists {
				activity.PRCount++
				// Note: We don't have file count per system in the current structure
			} else {
				systemMap[systemStat.System] = &SystemActivity{
					System:  systemStat.System,
					PRCount: 1,
				}
			}
		}

		// Time range (if we had commit dates, we'd track this)
		// For now, we'll use current time as placeholder
		if i == 0 {
			earliestTime = time.Now().AddDate(0, 0, -len(results))
			latestTime = time.Now()
		}
	}

	// Calculate averages
	totalPRs := len(results)
	avgFilesPerPR := float64(totalFiles) / float64(totalPRs)
	avgLinesPerPR := float64(totalLines) / float64(totalPRs)
	avgCommitsPerPR := float64(totalCommits) / float64(totalPRs)
	crossSystemRate := float64(multiSystemPRs) / float64(totalPRs) * 100
	avgSystemsPerPR := float64(totalSystems) / float64(totalPRs)

	// Convert maps to sorted slices
	var languageTrends []LanguageTrend
	for _, activity := range languageMap {
		percentage := float64(activity.Lines) / float64(totalLines) * 100
		languageTrends = append(languageTrends, LanguageTrend{
			Language:   activity.Language,
			TotalFiles: activity.Files,
			TotalLines: activity.Lines,
			Percentage: percentage,
			PRCount:    activity.PRCount,
		})
	}
	sort.Slice(languageTrends, func(i, j int) bool {
		return languageTrends[i].TotalLines > languageTrends[j].TotalLines
	})

	var mostActiveLanguages []LanguageActivity
	for _, activity := range languageMap {
		mostActiveLanguages = append(mostActiveLanguages, *activity)
	}
	sort.Slice(mostActiveLanguages, func(i, j int) bool {
		return mostActiveLanguages[i].PRCount > mostActiveLanguages[j].PRCount
	})

	var mostActiveSystems []SystemActivity
	for _, activity := range systemMap {
		mostActiveSystems = append(mostActiveSystems, *activity)
	}
	sort.Slice(mostActiveSystems, func(i, j int) bool {
		return mostActiveSystems[i].PRCount > mostActiveSystems[j].PRCount
	})

	// Calculate time range
	days := int(latestTime.Sub(earliestTime).Hours() / 24)

	return &AggregateStats{
		Summary: AggregateSummary{
			TotalPRs:        totalPRs,
			TotalCommits:    totalCommits,
			TotalFiles:      totalFiles,
			TotalLines:      totalLines,
			AvgFilesPerPR:   avgFilesPerPR,
			AvgLinesPerPR:   avgLinesPerPR,
			AvgCommitsPerPR: avgCommitsPerPR,
			CrossSystemRate: crossSystemRate,
		},
		LanguageTrends: languageTrends,
		CrossSystemTrends: CrossSystemTrends{
			SingleSystemPRs: singleSystemPRs,
			MultiSystemPRs:  multiSystemPRs,
			CrossSystemRate: crossSystemRate,
			AvgSystemsPerPR: avgSystemsPerPR,
			MaxSystemsInPR:  maxSystems,
		},
		MostActiveLanguages: mostActiveLanguages,
		MostActiveSystems:   mostActiveSystems,
		PRSizeDistribution:  sizeDistribution,
		TimeRange: TimeRange{
			Start: earliestTime,
			End:   latestTime,
			Days:  days,
		},
	}
}

