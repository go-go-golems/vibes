package analysis

import "time"

// LanguageStats represents statistics for a specific programming language
type LanguageStats struct {
	Language      string  `json:"language" yaml:"language"`
	FilesChanged  int     `json:"files_changed" yaml:"files_changed"`
	LinesAdded    int     `json:"lines_added" yaml:"lines_added"`
	LinesDeleted  int     `json:"lines_deleted" yaml:"lines_deleted"`
	LinesModified int     `json:"lines_modified" yaml:"lines_modified"`
	Percentage    float64 `json:"percentage" yaml:"percentage"`
}

// CommitInfo represents information about a single commit
type CommitInfo struct {
	Hash         string            `json:"hash" yaml:"hash"`
	Message      string            `json:"message" yaml:"message"`
	Author       string            `json:"author" yaml:"author"`
	Date         time.Time         `json:"date" yaml:"date"`
	FilesChanged []string          `json:"files_changed" yaml:"files_changed"`
	Languages    map[string]int    `json:"languages" yaml:"languages"`
	Categories   map[string]int    `json:"categories" yaml:"categories"`
	LinesAdded   int               `json:"lines_added" yaml:"lines_added"`
	LinesDeleted int               `json:"lines_deleted" yaml:"lines_deleted"`
}

// SystemTouch represents how many times a system/category was touched
type SystemTouch struct {
	System string `json:"system" yaml:"system"`
	Count  int    `json:"count" yaml:"count"`
}

// CrossSystemStats represents cross-subsystem analysis results
type CrossSystemStats struct {
	TotalCommits        int                        `json:"total_commits" yaml:"total_commits"`
	SingleSystemCommits int                        `json:"single_system_commits" yaml:"single_system_commits"`
	MultiSystemCommits  int                        `json:"multi_system_commits" yaml:"multi_system_commits"`
	CrossSystemRate     float64                    `json:"cross_system_rate" yaml:"cross_system_rate"`
	SystemTouchMatrix   map[string]map[string]int  `json:"system_touch_matrix" yaml:"system_touch_matrix"`
	MostTouchedSystems  []SystemTouch              `json:"most_touched_systems" yaml:"most_touched_systems"`
}

// PRAnalysisResult represents the complete analysis result
type PRAnalysisResult struct {
	PRInfo           PRInfo             `json:"pr_info" yaml:"pr_info"`
	LanguageStats    []LanguageStats    `json:"language_stats" yaml:"language_stats"`
	CrossSystemStats CrossSystemStats   `json:"cross_system_stats" yaml:"cross_system_stats"`
	Commits          []CommitInfo       `json:"commits" yaml:"commits"`
	Categories       map[string][]string `json:"categories" yaml:"categories"`
}

// PRInfo represents basic information about the analyzed PR
type PRInfo struct {
	BaseBranch   string `json:"base_branch" yaml:"base_branch"`
	PRBranch     string `json:"pr_branch" yaml:"pr_branch"`
	MergeCommit  string `json:"merge_commit,omitempty" yaml:"merge_commit,omitempty"`
	TotalFiles   int    `json:"total_files" yaml:"total_files"`
	TotalLines   int    `json:"total_lines" yaml:"total_lines"`
	TotalCommits int    `json:"total_commits" yaml:"total_commits"`
	RepoPath     string `json:"repo_path" yaml:"repo_path"`
	// Merge commit metadata (when applicable)
	MergeAuthorName  string    `json:"merge_author_name,omitempty" yaml:"merge_author_name,omitempty"`
	MergeAuthorEmail string    `json:"merge_author_email,omitempty" yaml:"merge_author_email,omitempty"`
	MergeAuthorDate  time.Time `json:"merge_author_date,omitempty" yaml:"merge_author_date,omitempty"`
	MergeCommitterName  string    `json:"merge_committer_name,omitempty" yaml:"merge_committer_name,omitempty"`
	MergeCommitterEmail string    `json:"merge_committer_email,omitempty" yaml:"merge_committer_email,omitempty"`
	MergeCommitterDate  time.Time `json:"merge_committer_date,omitempty" yaml:"merge_committer_date,omitempty"`
	MergeSummary        string    `json:"merge_summary,omitempty" yaml:"merge_summary,omitempty"`
}

