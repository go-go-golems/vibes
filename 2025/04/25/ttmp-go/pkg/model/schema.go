package model

// TTMPSchema defines the schema for validating TTMP documents
type TTMPSchema struct {
	Version       string            `yaml:"version"`
	RequiredFields []string          `yaml:"requiredFields"`
	AllowedTypes   []string          `yaml:"allowedTypes"`
	FieldTypes     map[string]string `yaml:"fieldTypes"`
}

// NewDefaultSchema creates a new schema with default values
func NewDefaultSchema() *TTMPSchema {
	return &TTMPSchema{
		Version:       "1.0",
		RequiredFields: []string{"id", "type", "title", "created"},
		AllowedTypes:   []string{"concept", "note", "reference", "task", "project", "person"},
		FieldTypes: map[string]string{
			"id":          "string",
			"type":        "string",
			"title":       "string",
			"description": "string",
			"created":     "date",
			"modified":    "date",
			"tags":        "string[]",
			"links":       "object[]",
		},
	}
}