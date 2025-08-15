package metadata

import (
	"time"
)

// Document represents the metadata structure for markdown documents
type Document struct {
	// Core identification
	Schema string `yaml:"schema" json:"schema"`
	DocID  string `yaml:"doc_id" json:"doc_id"`
	Title  string `yaml:"title" json:"title"`
	Slug   string `yaml:"slug,omitempty" json:"slug,omitempty"`

	// Status and lifecycle
	Status  string     `yaml:"status" json:"status"`
	Summary string     `yaml:"summary,omitempty" json:"summary,omitempty"`
	Tags    []string   `yaml:"tags,omitempty" json:"tags,omitempty"`
	TTLDays int        `yaml:"ttl_days,omitempty" json:"ttl_days,omitempty"`

	// Ownership and responsibility
	Owners       []string `yaml:"owners,omitempty" json:"owners,omitempty"`
	Stakeholders []string `yaml:"stakeholders,omitempty" json:"stakeholders,omitempty"`
	Audience     []string `yaml:"audience,omitempty" json:"audience,omitempty"`

	// Timestamps
	CreatedAt         *time.Time `yaml:"created_at,omitempty" json:"created_at,omitempty"`
	UpdatedAt         *time.Time `yaml:"updated_at,omitempty" json:"updated_at,omitempty"`
	LastReviewedAt    *time.Time `yaml:"last_reviewed_at,omitempty" json:"last_reviewed_at,omitempty"`
	NextReviewDueAt   *time.Time `yaml:"next_review_due_at,omitempty" json:"next_review_due_at,omitempty"`

	// Repository and location
	Repo string `yaml:"repo,omitempty" json:"repo,omitempty"`
	Path string `yaml:"path,omitempty" json:"path,omitempty"`

	// Security and access
	Visibility string `yaml:"visibility" json:"visibility"`
	DataClass  string `yaml:"data_class" json:"data_class"`

	// Relationships (for future use)
	Relations map[string][]string `yaml:"relations,omitempty" json:"relations,omitempty"`

	// LLM provenance (for future use)
	LLM *LLMProvenance `yaml:"llm,omitempty" json:"llm,omitempty"`
}

// LLMProvenance tracks AI assistance in document creation/editing
type LLMProvenance struct {
	Generated bool         `yaml:"generated" json:"generated"`
	Assisted  bool         `yaml:"assisted" json:"assisted"`
	Sessions  []LLMSession `yaml:"sessions,omitempty" json:"sessions,omitempty"`
}

// LLMSession represents a single AI assistance session
type LLMSession struct {
	Timestamp time.Time `yaml:"ts" json:"ts"`
	Model     string    `yaml:"model" json:"model"`
	Mode      string    `yaml:"mode" json:"mode"` // create, edit, review, etc.
}

// Valid status values
const (
	StatusDraft      = "draft"
	StatusInProgress = "in_progress"
	StatusReview     = "review"
	StatusFinal      = "final"
	StatusArchived   = "archived"
)

// Valid visibility values
const (
	VisibilityPublic       = "public"
	VisibilityInternal     = "internal"
	VisibilityConfidential = "confidential"
)

// Valid data class values
const (
	DataClassNone   = "none"
	DataClassPII    = "pii"
	DataClassSecret = "secret"
)

// IsValidStatus checks if a status value is valid
func IsValidStatus(status string) bool {
	switch status {
	case StatusDraft, StatusInProgress, StatusReview, StatusFinal, StatusArchived:
		return true
	default:
		return false
	}
}

// IsValidVisibility checks if a visibility value is valid
func IsValidVisibility(visibility string) bool {
	switch visibility {
	case VisibilityPublic, VisibilityInternal, VisibilityConfidential:
		return true
	default:
		return false
	}
}

// IsValidDataClass checks if a data class value is valid
func IsValidDataClass(dataClass string) bool {
	switch dataClass {
	case DataClassNone, DataClassPII, DataClassSecret:
		return true
	default:
		return false
	}
}

// NewDocument creates a new document with default values
func NewDocument(title string) *Document {
	now := time.Now()
	return &Document{
		Schema:     "mdmeta/v1",
		DocID:      generateULID(),
		Title:      title,
		Status:     StatusDraft,
		CreatedAt:  &now,
		UpdatedAt:  &now,
		Visibility: VisibilityInternal,
		DataClass:  DataClassNone,
	}
}

