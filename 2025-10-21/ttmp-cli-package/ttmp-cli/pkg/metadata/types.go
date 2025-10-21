package metadata

import "time"

// DocumentMetadata represents the YAML frontmatter of a ttmp document
type DocumentMetadata struct {
	Title        string    `yaml:"Title,omitempty"`
	Ticket       string    `yaml:"Ticket,omitempty"`
	Status       string    `yaml:"Status,omitempty"`        // draft | active | review | archived
	Topics       []string  `yaml:"Topics,omitempty"`
	DocType      string    `yaml:"DocType,omitempty"`       // index | design-doc | reference | etc.
	Intent       string    `yaml:"Intent,omitempty"`        // short-term | long-term | throwaway
	Owners       []string  `yaml:"Owners,omitempty"`
	RelatedFiles []string  `yaml:"RelatedFiles,omitempty"`
	Summary      string    `yaml:"Summary,omitempty"`
	LastUpdated  string    `yaml:"LastUpdated,omitempty"`
}

// TicketInfo represents a parsed ticket directory
type TicketInfo struct {
	Ticket      string
	Slug        string
	Path        string
	IndexPath   string
	HasIndex    bool
	Documents   []DocumentInfo
	Status      string
	Topics      []string
	Owners      []string
	LastUpdated time.Time
}

// DocumentInfo represents a single document with its metadata
type DocumentInfo struct {
	Path     string
	Filename string
	Metadata DocumentMetadata
}

// HealthIssue represents a problem found by the doctor command
type HealthIssue struct {
	Severity string // error | warning | info
	Ticket   string
	File     string
	Message  string
}

