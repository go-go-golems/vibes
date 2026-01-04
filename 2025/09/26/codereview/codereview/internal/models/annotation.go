package models

import "time"

// Annotation represents a code review annotation
type Annotation struct {
	ID        int       `json:"id" db:"id"`
	ReviewID  string    `json:"review_id" db:"review_id"`
	File      string    `json:"file" db:"file"`
	Line      *int      `json:"line,omitempty" db:"line"`
	LineStart *int      `json:"line_start,omitempty" db:"line_start"`
	LineEnd   *int      `json:"line_end,omitempty" db:"line_end"`
	Type      string    `json:"type" db:"type"`
	Severity  string    `json:"severity" db:"severity"`
	Message   string    `json:"message" db:"message"`
	Suggestion string   `json:"suggestion,omitempty" db:"suggestion"`
	Status    string    `json:"status" db:"status"`
	Created   time.Time `json:"created" db:"created"`
	Updated   time.Time `json:"updated" db:"updated"`
	Threads   []Thread  `json:"threads,omitempty" db:"-"`
}

// Annotation type constants
const (
	TypeIssue      = "issue"
	TypeSuggestion = "suggestion"
	TypePraise     = "praise"
	TypeQuestion   = "question"
)

// Annotation severity constants
const (
	SeverityMinor    = "minor"
	SeverityMajor    = "major"
	SeverityCritical = "critical"
)

// Annotation status constants
const (
	AnnotationStatusOpen         = "open"
	AnnotationStatusResolved     = "resolved"
	AnnotationStatusAcknowledged = "acknowledged"
)

// Thread represents a discussion thread for an annotation
type Thread struct {
	ID           int       `json:"id" db:"id"`
	AnnotationID int       `json:"annotation_id" db:"annotation_id"`
	Author       string    `json:"author" db:"author"`
	Message      string    `json:"message" db:"message"`
	Timestamp    time.Time `json:"timestamp" db:"timestamp"`
}

// IsValidType checks if the annotation type is valid
func (a *Annotation) IsValidType() bool {
	switch a.Type {
	case TypeIssue, TypeSuggestion, TypePraise, TypeQuestion:
		return true
	default:
		return false
	}
}

// IsValidSeverity checks if the annotation severity is valid
func (a *Annotation) IsValidSeverity() bool {
	switch a.Severity {
	case SeverityMinor, SeverityMajor, SeverityCritical:
		return true
	default:
		return false
	}
}

// IsValidStatus checks if the annotation status is valid
func (a *Annotation) IsValidStatus() bool {
	switch a.Status {
	case AnnotationStatusOpen, AnnotationStatusResolved, AnnotationStatusAcknowledged:
		return true
	default:
		return false
	}
}

// HasLineRange returns true if the annotation covers a line range
func (a *Annotation) HasLineRange() bool {
	return a.LineStart != nil && a.LineEnd != nil
}

// HasSingleLine returns true if the annotation is for a single line
func (a *Annotation) HasSingleLine() bool {
	return a.Line != nil
}

// IsFileLevel returns true if the annotation is at file level (no specific line)
func (a *Annotation) IsFileLevel() bool {
	return a.Line == nil && a.LineStart == nil && a.LineEnd == nil
}
