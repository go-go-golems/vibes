package models

import (
	"encoding/json"
	"time"
)

// Review represents a code review
type Review struct {
	ID           string    `json:"id" db:"id"`
	Title        string    `json:"title" db:"title"`
	Branch       string    `json:"branch,omitempty" db:"branch"`
	Commit       string    `json:"commit,omitempty" db:"commit"`
	BaseCommit   string    `json:"base_commit,omitempty" db:"base_commit"`
	Reviewer     string    `json:"reviewer" db:"reviewer"`
	Created      time.Time `json:"created" db:"created"`
	Updated      time.Time `json:"updated" db:"updated"`
	Status       string    `json:"status" db:"status"`
	FilesChanged int       `json:"files_changed" db:"files_changed"`
	LinesAdded   int       `json:"lines_added" db:"lines_added"`
	LinesRemoved int       `json:"lines_removed" db:"lines_removed"`
	Tags         []string  `json:"tags,omitempty" db:"-"`
	TagsJSON     string    `json:"-" db:"tags"`
}

// BeforeSave prepares the review for database storage
func (r *Review) BeforeSave() error {
	if len(r.Tags) > 0 {
		tagsJSON, err := json.Marshal(r.Tags)
		if err != nil {
			return err
		}
		r.TagsJSON = string(tagsJSON)
	}
	return nil
}

// AfterLoad processes the review after loading from database
func (r *Review) AfterLoad() error {
	if r.TagsJSON != "" {
		return json.Unmarshal([]byte(r.TagsJSON), &r.Tags)
	}
	return nil
}

// ReviewStatus constants
const (
	StatusPending          = "pending"
	StatusApproved         = "approved"
	StatusChangesRequested = "changes_requested"
	StatusDraft            = "draft"
)

// ReviewSummary provides summary statistics for a review
type ReviewSummary struct {
	ReviewID        string `json:"review_id"`
	TotalAnnotations int   `json:"total_annotations"`
	IssuesCount     int    `json:"issues_count"`
	SuggestionsCount int   `json:"suggestions_count"`
	PraiseCount     int    `json:"praise_count"`
	QuestionsCount  int    `json:"questions_count"`
	CriticalCount   int    `json:"critical_count"`
	MajorCount      int    `json:"major_count"`
	MinorCount      int    `json:"minor_count"`
	OpenCount       int    `json:"open_count"`
	ResolvedCount   int    `json:"resolved_count"`
}
