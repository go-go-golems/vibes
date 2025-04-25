package model

// TTMPSchema defines the schema for TTMP documents
type TTMPSchema struct {
	// Schema version
	Version string
	
	// Required fields
	RequiredFields []string
	
	// Valid values for specific fields
	ValidStatuses      []string
	ValidLongevity     []string
	ValidAudiences     []string
	ValidDocumentTypes []string
}

// NewDefaultSchema creates a new default TTMP schema
func NewDefaultSchema() *TTMPSchema {
	return &TTMPSchema{
		Version: "1.0",
		
		RequiredFields: []string{
			"id",
		},
		
		ValidStatuses: []string{
			"draft",
			"active",
			"deprecated",
			"proposed",
			"in-review",
			"approved",
			"rejected",
			"archived",
		},
		
		ValidLongevity: []string{
			"short",
			"long",
		},
		
		ValidAudiences: []string{
			"internal-dev",
			"external-docs",
			"partners",
			"public",
		},
		
		ValidDocumentTypes: []string{
			"architecture",
			"spec",
			"specification",
			"guide",
			"tutorial",
			"how-to",
			"howto",
			"reference",
			"debugging",
			"prd",
			"report",
			"design",
			"proposal",
			"concept",
			"documentation",
			"log",
			"runlog",
		},
	}
}

// ValidateStatus checks if the status is valid according to the schema
func (s *TTMPSchema) ValidateStatus(status string) bool {
	if status == "" {
		return true
	}
	
	for _, validStatus := range s.ValidStatuses {
		if status == validStatus {
			return true
		}
	}
	
	return false
}

// ValidateLongevity checks if the longevity is valid according to the schema
func (s *TTMPSchema) ValidateLongevity(longevity string) bool {
	if longevity == "" {
		return true
	}
	
	for _, validLongevity := range s.ValidLongevity {
		if longevity == validLongevity {
			return true
		}
	}
	
	return false
}

// ValidateAudience checks if the audience is valid according to the schema
func (s *TTMPSchema) ValidateAudience(audience string) bool {
	if audience == "" {
		return true
	}
	
	for _, validAudience := range s.ValidAudiences {
		if audience == validAudience {
			return true
		}
	}
	
	return false
}

// ValidateDocumentType checks if the document type is valid according to the schema
func (s *TTMPSchema) ValidateDocumentType(documentType string) bool {
	if documentType == "" {
		return true
	}
	
	for _, validDocumentType := range s.ValidDocumentTypes {
		if documentType == validDocumentType {
			return true
		}
	}
	
	return false
}

// IsValidField checks if a field name is valid in the TTMP schema
func (s *TTMPSchema) IsValidField(field string) bool {
	validFields := []string{
		// Core Identity & Lifecycle
		"id",
		"title",
		"created",
		"updated",
		"status",
		"owner",
		"audience",
		
		// Discovery & Classification
		"tags",
		"category",
		"longevity",
		"keywords",
		"document_type",
		
		// Code & Concept Links
		"source_files",
		"tracked_functions",
		"concepts",
		
		// Relationships to Other Docs
		"see_also",
		"predecessor",
		"successor",
		"imports",
		
		// Revision & Governance
		"schema_version",
		"approved_by",
		"review_cycle",
		"changelog",
		
		// Automation Hooks
		"update_command",
		"notify_on_change",
		"auto_sync",
		
		// Content Metrics & Summaries
		"word_count",
		"abstract",
		"toc_depth",
		
		// Execution-Time Context
		"run_id",
		"env",
		"commit",
	}
	
	for _, validField := range validFields {
		if field == validField {
			return true
		}
	}
	
	return false
}