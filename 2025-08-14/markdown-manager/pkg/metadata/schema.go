package metadata

import (
	"time"
)

// DocumentMetadata represents the YAML frontmatter structure
type DocumentMetadata struct {
	// Core identification
	Title       string    `yaml:"title" json:"title"`
	Description string    `yaml:"description,omitempty" json:"description,omitempty"`
	Tags        []string  `yaml:"tags,omitempty" json:"tags,omitempty"`
	Category    string    `yaml:"category,omitempty" json:"category,omitempty"`
	
	// Timestamps
	Created    time.Time  `yaml:"created" json:"created"`
	Modified   time.Time  `yaml:"modified" json:"modified"`
	LastUsed   *time.Time `yaml:"last_used,omitempty" json:"last_used,omitempty"`
	
	// Project/context information
	Project     string   `yaml:"project,omitempty" json:"project,omitempty"`
	Repository  string   `yaml:"repository,omitempty" json:"repository,omitempty"`
	Branch      string   `yaml:"branch,omitempty" json:"branch,omitempty"`
	
	// Relationships
	RelatedFiles []string `yaml:"related_files,omitempty" json:"related_files,omitempty"`
	Dependencies []string `yaml:"dependencies,omitempty" json:"dependencies,omitempty"`
	References   []string `yaml:"references,omitempty" json:"references,omitempty"`
	
	// Status and workflow
	Status      string `yaml:"status,omitempty" json:"status,omitempty"` // draft, review, final, archived
	Priority    string `yaml:"priority,omitempty" json:"priority,omitempty"` // low, medium, high, critical
	Version     string `yaml:"version,omitempty" json:"version,omitempty"`
	
	// Author information
	Author      string   `yaml:"author,omitempty" json:"author,omitempty"`
	Contributors []string `yaml:"contributors,omitempty" json:"contributors,omitempty"`
	
	// Technical metadata
	Language    string            `yaml:"language,omitempty" json:"language,omitempty"`
	Format      string            `yaml:"format,omitempty" json:"format,omitempty"`
	Template    string            `yaml:"template,omitempty" json:"template,omitempty"`
	Custom      map[string]interface{} `yaml:"custom,omitempty" json:"custom,omitempty"`
}

// DocumentFile represents a markdown file with its metadata and content
type DocumentFile struct {
	Path     string           `json:"path"`
	Metadata DocumentMetadata `json:"metadata"`
	Content  string           `json:"content,omitempty"`
	Size     int64            `json:"size"`
	ModTime  time.Time        `json:"mod_time"`
}

// SearchCriteria defines search parameters
type SearchCriteria struct {
	Title       string
	Tags        []string
	Category    string
	Project     string
	Status      string
	Priority    string
	Author      string
	DateFrom    *time.Time
	DateTo      *time.Time
	ContentText string
}

