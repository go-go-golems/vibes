package model

import (
	"fmt"
	"path/filepath"
	"strings"
	"time"
)

// TTMPDocument represents a TTMP (YAML Text with Text Metadata Preamble) document
type TTMPDocument struct {
	// Core Identity & Lifecycle
	ID        string     `yaml:"id,omitempty" json:"id,omitempty"`
	Title     string     `yaml:"title,omitempty" json:"title,omitempty"`
	Created   *time.Time `yaml:"created,omitempty" json:"created,omitempty"`
	Updated   *time.Time `yaml:"updated,omitempty" json:"updated,omitempty"`
	Status    string     `yaml:"status,omitempty" json:"status,omitempty"`
	Owner     string     `yaml:"owner,omitempty" json:"owner,omitempty"`
	Audience  string     `yaml:"audience,omitempty" json:"audience,omitempty"`
	
	// Discovery & Classification
	Tags        []string `yaml:"tags,omitempty" json:"tags,omitempty"`
	Category    string   `yaml:"category,omitempty" json:"category,omitempty"`
	Longevity   string   `yaml:"longevity,omitempty" json:"longevity,omitempty"`
	Keywords    string   `yaml:"keywords,omitempty" json:"keywords,omitempty"`
	DocumentType string  `yaml:"document_type,omitempty" json:"document_type,omitempty"`
	
	// Code & Concept Links
	SourceFiles     []string `yaml:"source_files,omitempty" json:"source_files,omitempty"`
	TrackedFunctions []string `yaml:"tracked_functions,omitempty" json:"tracked_functions,omitempty"`
	Concepts        []string `yaml:"concepts,omitempty" json:"concepts,omitempty"`
	
	// Relationships to Other Docs
	SeeAlso     []string `yaml:"see_also,omitempty" json:"see_also,omitempty"`
	Predecessor string   `yaml:"predecessor,omitempty" json:"predecessor,omitempty"`
	Successor   string   `yaml:"successor,omitempty" json:"successor,omitempty"`
	Imports     []string `yaml:"imports,omitempty" json:"imports,omitempty"`
	
	// Revision & Governance
	SchemaVersion string           `yaml:"schema_version,omitempty" json:"schema_version,omitempty"`
	ApprovedBy    []string         `yaml:"approved_by,omitempty" json:"approved_by,omitempty"`
	ReviewCycle   string           `yaml:"review_cycle,omitempty" json:"review_cycle,omitempty"`
	Changelog     []ChangelogEntry `yaml:"changelog,omitempty" json:"changelog,omitempty"`
	
	// Automation Hooks
	UpdateCommand   string `yaml:"update_command,omitempty" json:"update_command,omitempty"`
	NotifyOnChange  string `yaml:"notify_on_change,omitempty" json:"notify_on_change,omitempty"`
	AutoSync        bool   `yaml:"auto_sync,omitempty" json:"auto_sync,omitempty"`
	
	// Content Metrics & Summaries
	WordCount int    `yaml:"word_count,omitempty" json:"word_count,omitempty"`
	Abstract  string `yaml:"abstract,omitempty" json:"abstract,omitempty"`
	TOCDepth  int    `yaml:"toc_depth,omitempty" json:"toc_depth,omitempty"`
	
	// Execution-Time Context
	RunID  string `yaml:"run_id,omitempty" json:"run_id,omitempty"`
	Env    string `yaml:"env,omitempty" json:"env,omitempty"`
	Commit string `yaml:"commit,omitempty" json:"commit,omitempty"`
	
	// The content of the document (everything after the frontmatter)
	Content string `yaml:"-" json:"content,omitempty"`
	
	// File path to the original document
	FilePath string `yaml:"-" json:"file_path,omitempty"`
	
	// Computed fields (not stored in YAML)
	RelativeFilePath string `yaml:"-" json:"relative_file_path,omitempty"`
	LastModified     time.Time `yaml:"-" json:"last_modified,omitempty"`
}

// ChangelogEntry represents a single entry in the changelog
type ChangelogEntry struct {
	Date   time.Time `yaml:"date" json:"date"`
	Change string    `yaml:"change" json:"change"`
	Author string    `yaml:"author" json:"author"`
}

// NewTTMPDocument creates a new empty TTMP document
func NewTTMPDocument() *TTMPDocument {
	return &TTMPDocument{
		Tags:             make([]string, 0),
		SourceFiles:      make([]string, 0),
		TrackedFunctions: make([]string, 0),
		Concepts:         make([]string, 0),
		SeeAlso:          make([]string, 0),
		ApprovedBy:       make([]string, 0),
		Changelog:        make([]ChangelogEntry, 0),
	}
}

// CalculateWordCount calculates the word count of the document content
func (d *TTMPDocument) CalculateWordCount() int {
	if d.Content == "" {
		return 0
	}
	
	// Simple word count by splitting by whitespace
	words := strings.Fields(d.Content)
	return len(words)
}

// UpdateWordCount updates the word count based on the current content
func (d *TTMPDocument) UpdateWordCount() {
	d.WordCount = d.CalculateWordCount()
}

// GetFirstHeading returns the first heading in the content
func (d *TTMPDocument) GetFirstHeading() string {
	if d.Content == "" {
		return ""
	}
	
	lines := strings.Split(d.Content, "\n")
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "# ") {
			return strings.TrimPrefix(trimmed, "# ")
		}
	}
	
	return ""
}

// GetRelativeFilePath returns the file path relative to a base directory
func (d *TTMPDocument) GetRelativeFilePath(baseDir string) string {
	if d.FilePath == "" {
		return ""
	}
	
	rel, err := filepath.Rel(baseDir, d.FilePath)
	if err != nil {
		return d.FilePath
	}
	
	return rel
}

// SetRelativeFilePath sets the relative file path for the document
func (d *TTMPDocument) SetRelativeFilePath(baseDir string) {
	d.RelativeFilePath = d.GetRelativeFilePath(baseDir)
}

// GenerateID generates an ID from the document's file path if not already set
func (d *TTMPDocument) GenerateID() {
	if d.ID != "" {
		return
	}
	
	if d.FilePath != "" {
		base := filepath.Base(d.FilePath)
		ext := filepath.Ext(base)
		d.ID = strings.TrimSuffix(base, ext)
	}
}

// IsValid checks if the document has the essential fields
func (d *TTMPDocument) IsValid() bool {
	// A valid document must have an ID
	return d.ID != ""
}

// GetStatus returns the status with a default if not set
func (d *TTMPDocument) GetStatus() string {
	if d.Status == "" {
		return "draft"
	}
	return d.Status
}

// GetTitle returns the best available title
func (d *TTMPDocument) GetTitle() string {
	if d.Title != "" {
		return d.Title
	}
	
	// Try to get the first heading
	firstHeading := d.GetFirstHeading()
	if firstHeading != "" {
		return firstHeading
	}
	
	// Fall back to ID if available
	if d.ID != "" {
		return d.ID
	}
	
	// Last resort, use the file name
	if d.FilePath != "" {
		return filepath.Base(d.FilePath)
	}
	
	return "Untitled Document"
}

// GetDocumentTypeName returns a human-readable name for the document type
func (d *TTMPDocument) GetDocumentTypeName() string {
	switch strings.ToLower(d.DocumentType) {
	case "spec", "specification":
		return "Specification"
	case "guide":
		return "Guide"
	case "tutorial":
		return "Tutorial"
	case "howto":
		return "How-To"
	case "reference":
		return "Reference"
	case "architecture":
		return "Architecture"
	case "design":
		return "Design"
	case "debugging":
		return "Debugging"
	case "prd", "product":
		return "Product Requirements"
	default:
		if d.DocumentType != "" {
			return d.DocumentType
		}
		return "Document"
	}
}

// Summary represents a shorter view of a TTMP document for listings
type Summary struct {
	ID           string    `json:"id"`
	Title        string    `json:"title"`
	DocumentType string    `json:"document_type"`
	Status       string    `json:"status"`
	Created      time.Time `json:"created,omitempty"`
	Updated      time.Time `json:"updated,omitempty"`
	Tags         []string  `json:"tags"`
	FilePath     string    `json:"file_path"`
}

// ToSummary converts a TTMPDocument to a Summary
func (d *TTMPDocument) ToSummary() Summary {
	summary := Summary{
		ID:           d.ID,
		Title:        d.GetTitle(),
		DocumentType: d.DocumentType,
		Status:       d.GetStatus(),
		Tags:         d.Tags,
		FilePath:     d.FilePath,
	}
	
	if d.Created != nil {
		summary.Created = *d.Created
	}
	
	if d.Updated != nil {
		summary.Updated = *d.Updated
	} else if !d.LastModified.IsZero() {
		summary.Updated = d.LastModified
	}
	
	return summary
}

// String returns a string representation of the document
func (d *TTMPDocument) String() string {
	return fmt.Sprintf("TTMPDocument{ID: %s, Title: %s, Type: %s}", d.ID, d.GetTitle(), d.DocumentType)
}