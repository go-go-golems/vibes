package validator

import (
	"fmt"
	"regexp"
	"strings"
	"time"

	"ttmp-go/pkg/errors"
	"ttmp-go/pkg/model"
)

// TTMPValidator is responsible for validating TTMP documents against a defined schema
type TTMPValidator struct {
	// Configuration options can be added here
	RequiredFields []string
	AllowedTypes   []string
}

// NewValidator creates a new TTMPValidator
func NewValidator() *TTMPValidator {
	return &TTMPValidator{
		RequiredFields: []string{"id", "type", "title", "created"},
		AllowedTypes:   []string{"concept", "note", "reference", "task", "project", "person"},
	}
}

// ValidateDocument validates a TTMP document against a defined schema
func (v *TTMPValidator) ValidateDocument(doc *model.TTMPDocument) error {
	ve := errors.NewValidationErrors()

	// Validate required fields
	for _, field := range v.RequiredFields {
		switch field {
		case "id":
			if doc.ID == "" {
				ve.AddError("Missing required field 'id'", "Document must have a unique identifier")
			} else if !isValidID(doc.ID) {
				ve.AddError("Invalid 'id' format", "ID must be alphanumeric with optional hyphens and underscores")
			}
		case "type":
			if doc.Type == "" {
				ve.AddError("Missing required field 'type'", "Document must have a type")
			} else if !v.isValidType(doc.Type) {
				ve.AddError(
					fmt.Sprintf("Invalid 'type': %s", doc.Type),
					fmt.Sprintf("Type must be one of: %s", strings.Join(v.AllowedTypes, ", ")),
				)
			}
		case "title":
			if doc.Title == "" {
				ve.AddError("Missing required field 'title'", "Document must have a title")
			}
		case "created":
			if doc.Created.IsZero() {
				ve.AddError("Missing required field 'created'", "Document must have a creation date")
			}
		}
	}

	// Validate optional fields with specific formats
	if doc.Modified != nil && doc.Modified.Before(doc.Created) {
		ve.AddError(
			"Invalid 'modified' date",
			fmt.Sprintf("Modified date (%s) cannot be before created date (%s)", 
			doc.Modified.Format(time.RFC3339), 
			doc.Created.Format(time.RFC3339)),
		)
	}

	// Validate tags (if present)
	for i, tag := range doc.Tags {
		if !isValidTag(tag) {
			ve.AddError(
				fmt.Sprintf("Invalid tag at index %d: '%s'", i, tag),
				"Tags must be alphanumeric with optional hyphens",
			)
		}
	}

	// Validate links (if present)
	for i, link := range doc.Links {
		if link.URL == "" {
			ve.AddError(
				fmt.Sprintf("Missing URL for link at index %d", i),
				"Links must have a URL",
			)
		}
	}

	if ve.HasErrors() {
		return ve
	}

	return nil
}

// isValidID checks if an ID follows the correct format
func isValidID(id string) bool {
	// Alphanumeric with optional hyphens and underscores
	pattern := `^[a-zA-Z0-9_-]+$`
	match, _ := regexp.MatchString(pattern, id)
	return match
}

// isValidType checks if a type is one of the allowed types
func (v *TTMPValidator) isValidType(docType string) bool {
	for _, allowedType := range v.AllowedTypes {
		if docType == allowedType {
			return true
		}
	}
	return false
}

// isValidTag checks if a tag follows the correct format
func isValidTag(tag string) bool {
	// Alphanumeric with optional hyphens
	pattern := `^[a-zA-Z0-9-]+$`
	match, _ := regexp.MatchString(pattern, tag)
	return match
}