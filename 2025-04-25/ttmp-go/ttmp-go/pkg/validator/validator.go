package validator

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/sirupsen/logrus"

	ttmperrors "github.com/scrapybara/ttmp/pkg/errors"
	"github.com/scrapybara/ttmp/pkg/model"
)

// Validator validates TTMP documents
type Validator struct {
	logger *logrus.Logger
	schema *model.TTMPSchema
}

// NewValidator creates a new Validator instance
func NewValidator(logger *logrus.Logger) *Validator {
	return &Validator{
		logger: logger,
		schema: model.NewDefaultSchema(),
	}
}

// ValidationResultType represents the result level of a validation
type ValidationResultType string

const (
	// ValidationResultError indicates an error that violates the schema
	ValidationResultError ValidationResultType = "error"
	
	// ValidationResultWarning indicates a potential issue but doesn't violate the schema
	ValidationResultWarning ValidationResultType = "warning"
	
	// ValidationResultSuggestion indicates a suggestion for improvement
	ValidationResultSuggestion ValidationResultType = "suggestion"
)

// ValidationIssue represents a single validation issue
type ValidationIssue struct {
	Type        ValidationResultType
	Message     string
	Field       string
	Value       interface{}
	Code        string
	Suggestion  string
	LineNumber  int
	ColumnStart int
	ColumnEnd   int
}

// ValidationResult holds the result of a validation operation
type ValidationResult struct {
	Valid       bool
	Errors      []ValidationIssue
	Warnings    []ValidationIssue
	Suggestions []ValidationIssue
	DocumentID  string
	FilePath    string
}

// NewValidationResult creates a new empty validation result
func NewValidationResult() ValidationResult {
	return ValidationResult{
		Valid:       true,
		Errors:      []ValidationIssue{},
		Warnings:    []ValidationIssue{},
		Suggestions: []ValidationIssue{},
	}
}

// AddError adds an error to the validation result
func (r *ValidationResult) AddError(message string, field string, code string) {
	r.Valid = false
	r.Errors = append(r.Errors, ValidationIssue{
		Type:    ValidationResultError,
		Message: message,
		Field:   field,
		Code:    code,
	})
}

// AddWarning adds a warning to the validation result
func (r *ValidationResult) AddWarning(message string, field string, code string) {
	r.Warnings = append(r.Warnings, ValidationIssue{
		Type:    ValidationResultWarning,
		Message: message,
		Field:   field,
		Code:    code,
	})
}

// AddSuggestion adds a suggestion to the validation result
func (r *ValidationResult) AddSuggestion(message string, field string, suggestion string) {
	r.Suggestions = append(r.Suggestions, ValidationIssue{
		Type:       ValidationResultSuggestion,
		Message:    message,
		Field:      field,
		Suggestion: suggestion,
	})
}

// GetErrorStrings returns all error messages as strings
func (r *ValidationResult) GetErrorStrings() []string {
	var errors []string
	for _, err := range r.Errors {
		errors = append(errors, err.Message)
	}
	return errors
}

// GetWarningStrings returns all warning messages as strings
func (r *ValidationResult) GetWarningStrings() []string {
	var warnings []string
	for _, warning := range r.Warnings {
		warnings = append(warnings, warning.Message)
	}
	return warnings
}

// GetSuggestionStrings returns all suggestion messages as strings
func (r *ValidationResult) GetSuggestionStrings() []string {
	var suggestions []string
	for _, suggestion := range r.Suggestions {
		suggestions = append(suggestions, suggestion.Message)
	}
	return suggestions
}

// HasIssues returns true if there are any errors, warnings, or suggestions
func (r *ValidationResult) HasIssues() bool {
	return len(r.Errors) > 0 || len(r.Warnings) > 0 || len(r.Suggestions) > 0
}

// Validate performs validation checks on a TTMP document
func (v *Validator) Validate(doc *model.TTMPDocument) (ValidationResult, error) {
	result := NewValidationResult()
	result.DocumentID = doc.ID
	
	// Perform validation
	v.validateRequiredFields(doc, &result)
	v.validateDates(doc, &result)
	v.validateStatuses(doc, &result)
	v.validateValues(doc, &result)
	v.validateContent(doc, &result)
	v.provideSuggestions(doc, &result)
	
	return result, nil
}

// validateRequiredFields checks that required fields are present
func (v *Validator) validateRequiredFields(doc *model.TTMPDocument, result *ValidationResult) {
	// Check required fields defined in schema
	for _, field := range v.schema.RequiredFields {
		switch field {
		case "id":
			if doc.ID == "" {
				result.AddError("Missing required field: id", "id", "missing_required_field")
			}
		case "title":
			if doc.Title == "" {
				result.AddWarning("Missing recommended field: title", "title", "missing_recommended_field")
			}
		case "document_type":
			if doc.DocumentType == "" {
				result.AddWarning("Missing recommended field: document_type", "document_type", "missing_recommended_field")
			}
		}
	}
	
	// Validate ID format
	if doc.ID != "" {
		// Check for spaces in ID
		if strings.Contains(doc.ID, " ") {
			result.AddWarning("ID should not contain spaces", "id", "invalid_id_format")
		}
		
		// Check for special characters
		idPattern := regexp.MustCompile(`^[a-zA-Z0-9\-_]+$`)
		if !idPattern.MatchString(doc.ID) {
			result.AddWarning("ID should only contain alphanumeric characters, hyphens, and underscores", "id", "invalid_id_format")
		}
	}
}

// validateDates checks date-related fields
func (v *Validator) validateDates(doc *model.TTMPDocument, result *ValidationResult) {
	now := time.Now()
	
	// Check if created date is in the future
	if doc.Created != nil && doc.Created.After(now) {
		result.AddWarning("Created date is in the future", "created", "future_date")
	}
	
	// Check if updated date is in the future
	if doc.Updated != nil && doc.Updated.After(now) {
		result.AddWarning("Updated date is in the future", "updated", "future_date")
	}
	
	// Check if updated date is before created date
	if doc.Created != nil && doc.Updated != nil && doc.Updated.Before(*doc.Created) {
		result.AddError("Updated date is before created date", "updated", "date_order_invalid")
	}
	
	// Check if created date is a reasonable date (not older than 1980)
	oldestReasonableDate := time.Date(1980, 1, 1, 0, 0, 0, 0, time.UTC)
	if doc.Created != nil && doc.Created.Before(oldestReasonableDate) {
		result.AddWarning("Created date is unusually old (before 1980)", "created", "date_suspicious")
	}
}

// validateStatuses checks status values
func (v *Validator) validateStatuses(doc *model.TTMPDocument, result *ValidationResult) {
	// Validate status values
	if !v.schema.ValidateStatus(doc.Status) {
		validStatusesStr := strings.Join(v.schema.ValidStatuses, ", ")
		result.AddWarning(
			fmt.Sprintf("Status '%s' is not one of the standard values: %s", doc.Status, validStatusesStr),
			"status",
			"non_standard_status")
	}
	
	// Validate longevity values
	if !v.schema.ValidateLongevity(doc.Longevity) {
		validLongevityStr := strings.Join(v.schema.ValidLongevity, ", ")
		result.AddWarning(
			fmt.Sprintf("Longevity '%s' is not one of the standard values: %s", doc.Longevity, validLongevityStr),
			"longevity",
			"non_standard_longevity")
	}
	
	// Validate audience values
	if !v.schema.ValidateAudience(doc.Audience) {
		validAudienceStr := strings.Join(v.schema.ValidAudiences, ", ")
		result.AddWarning(
			fmt.Sprintf("Audience '%s' is not one of the standard values: %s", doc.Audience, validAudienceStr),
			"audience",
			"non_standard_audience")
	}
	
	// Validate document type values
	if !v.schema.ValidateDocumentType(doc.DocumentType) {
		result.AddWarning(
			fmt.Sprintf("Document type '%s' is not one of the standard values", doc.DocumentType),
			"document_type",
			"non_standard_document_type")
	}
}

// validateValues checks the values of fields
func (v *Validator) validateValues(doc *model.TTMPDocument, result *ValidationResult) {
	// Validate review cycle format
	if doc.ReviewCycle != "" {
		cyclePeriodRegex := regexp.MustCompile(`^(\d+)(d|w|m|y)$`)
		if !cyclePeriodRegex.MatchString(doc.ReviewCycle) {
			result.AddWarning(
				fmt.Sprintf("Review cycle '%s' is not in a valid format (e.g., 30d, 4w, 6m, 1y)", doc.ReviewCycle),
				"review_cycle",
				"invalid_review_cycle_format")
		}
	}
	
	// Validate tags don't have spaces
	for i, tag := range doc.Tags {
		if strings.Contains(tag, " ") {
			result.AddWarning(
				fmt.Sprintf("Tag '%s' contains spaces, which may cause issues with filtering", tag),
				fmt.Sprintf("tags[%d]", i),
				"tag_contains_spaces")
		}
	}
	
	// Validate source_files existence
	if doc.FilePath != "" { // Only check if we know where the document is stored
		for i, sourceFile := range doc.SourceFiles {
			// Construct path relative to document location
			docDir := filepath.Dir(doc.FilePath)
			relativePath := filepath.Join(docDir, sourceFile)
			
			// Try both absolute and relative paths
			if _, err := os.Stat(sourceFile); errors.Is(err, os.ErrNotExist) {
				if _, err := os.Stat(relativePath); errors.Is(err, os.ErrNotExist) {
					result.AddWarning(
						fmt.Sprintf("Source file does not exist: %s", sourceFile),
						fmt.Sprintf("source_files[%d]", i),
						"source_file_not_found")
				}
			}
		}
	}
	
	// Validate see_also references
	if doc.FilePath != "" { // Only check if we know where the document is stored
		for i, ref := range doc.SeeAlso {
			// Check if it looks like a file path with .md extension
			if strings.HasSuffix(ref, ".md") {
				// Construct path relative to document location
				docDir := filepath.Dir(doc.FilePath)
				relativePath := filepath.Join(docDir, ref)
				
				// Try both absolute and relative paths
				if _, err := os.Stat(ref); errors.Is(err, os.ErrNotExist) {
					if _, err := os.Stat(relativePath); errors.Is(err, os.ErrNotExist) {
						result.AddWarning(
							fmt.Sprintf("Referenced document does not exist: %s", ref),
							fmt.Sprintf("see_also[%d]", i),
							"referenced_document_not_found")
					}
				}
			}
		}
	}
	
	// Schema version check
	if doc.SchemaVersion != "" && doc.SchemaVersion != v.schema.Version {
		result.AddWarning(
			fmt.Sprintf("Document uses schema version %s, but validator uses version %s",
				doc.SchemaVersion, v.schema.Version),
			"schema_version",
			"schema_version_mismatch")
	}
}

// validateContent performs validation on the document content structure
func (v *Validator) validateContent(doc *model.TTMPDocument, result *ValidationResult) {
	if doc.Content == "" {
		result.AddWarning("Document has no content", "content", "empty_content")
		return
	}
	
	// Check if document has a title heading
	hasTitleHeading := false
	lines := strings.Split(doc.Content, "\n")
	for _, line := range lines {
		if strings.HasPrefix(line, "# ") {
			hasTitleHeading = true
			break
		}
	}
	
	if !hasTitleHeading {
		result.AddSuggestion(
			"Document should start with a level 1 heading (# Title)",
			"content",
			"Add a level 1 heading to the document")
	}
	
	// Check for empty sections
	sectionPattern := regexp.MustCompile(`(?m)^#+\s+(.+)$`)
	sections := sectionPattern.FindAllStringSubmatch(doc.Content, -1)
	
	if len(sections) == 0 {
		result.AddSuggestion(
			"Document has no section headings",
			"content",
			"Add section headings to structure the document")
	}
}

// provideSuggestions adds suggestions for improving the document
func (v *Validator) provideSuggestions(doc *model.TTMPDocument, result *ValidationResult) {
	// Suggest tags for discoverability
	if doc.Tags == nil || len(doc.Tags) == 0 {
		result.AddSuggestion(
			"Consider adding tags for better discoverability",
			"tags",
			"Add relevant tags to make the document easier to find")
	}
	
	// Suggest abstract for searchability
	if doc.Abstract == "" {
		result.AddSuggestion(
			"Adding an abstract would improve document searchability",
			"abstract",
			"Add a brief summary of the document")
	}
	
	// Suggest document type
	if doc.DocumentType == "" {
		result.AddSuggestion(
			"Adding a document_type would help categorize this document",
			"document_type",
			"Add a document type such as 'guide', 'spec', 'tutorial', etc.")
	}
	
	// Suggest review cycle for long-term documents
	if doc.Longevity == "long" && doc.ReviewCycle == "" {
		result.AddSuggestion(
			"Long-term documents should have a review cycle",
			"review_cycle",
			"Add a review cycle (e.g., '90d') to ensure the document stays up to date")
	}
}

// ValidateFile validates a TTMP document from a file path
func (v *Validator) ValidateFile(filePath string, doc *model.TTMPDocument) (ValidationResult, error) {
	result, err := v.Validate(doc)
	if err != nil {
		return result, err
	}
	
	result.FilePath = filePath
	
	// Additional file-specific validations
	fileInfo, err := os.Stat(filePath)
	if err != nil {
		if os.IsNotExist(err) {
			return result, ttmperrors.NewError(ttmperrors.ErrFileNotFound, "File not found").WithFile(filePath)
		}
		if os.IsPermission(err) {
			return result, ttmperrors.NewError(ttmperrors.ErrFilePermission, "Permission denied").WithFile(filePath)
		}
		return result, ttmperrors.NewError(ttmperrors.ErrFile, "Error accessing file").WithFile(filePath).WithCause(err)
	}
	
	// Check if the file's modification time matches the updated time
	modTime := fileInfo.ModTime()
	if doc.Updated != nil && !doc.Updated.Equal(modTime) {
		timeDiff := modTime.Sub(*doc.Updated).Hours() / 24 // difference in days
		if timeDiff > 30 {
			result.AddWarning(
				fmt.Sprintf("File was last modified %.0f days ago, but document's updated field is different", timeDiff),
				"updated",
				"timestamp_divergence")
		}
	}
	
	// Check if ID matches filename
	base := filepath.Base(filePath)
	ext := filepath.Ext(base)
	filename := strings.TrimSuffix(base, ext)
	
	if doc.ID != filename && !strings.Contains(doc.ID, filename) && !strings.Contains(filename, doc.ID) {
		result.AddSuggestion(
			fmt.Sprintf("Consider aligning document ID (%s) with filename (%s)", doc.ID, filename),
			"id",
			"Rename file or update ID for consistency")
	}
	
	return result, nil
}

// ValidateCollection performs validation on a collection of documents
func (v *Validator) ValidateCollection(collection *model.TTMPCollection) ([]ValidationResult, error) {
	var results []ValidationResult
	
	// Validate individual documents
	for _, doc := range collection.Documents {
		result, err := v.Validate(doc)
		if err != nil {
			return results, err
		}
		
		if doc.FilePath != "" {
			result, err = v.ValidateFile(doc.FilePath, doc)
			if err != nil {
				return results, err
			}
		}
		
		results = append(results, result)
	}
	
	// Check for collection-level issues
	collectionResult := NewValidationResult()
	collectionResult.DocumentID = "collection"
	
	// Check for ID uniqueness
	idMap := make(map[string][]string)
	for _, doc := range collection.Documents {
		if doc.ID != "" {
			idMap[doc.ID] = append(idMap[doc.ID], doc.FilePath)
		}
	}
	
	for id, paths := range idMap {
		if len(paths) > 1 {
			collectionResult.AddWarning(
				fmt.Sprintf("Duplicate ID '%s' found in multiple files: %s", id, strings.Join(paths, ", ")),
				"id",
				"duplicate_id")
		}
	}
	
	// Check document references
	for _, doc := range collection.Documents {
		// Check see_also references
		for _, refPath := range doc.SeeAlso {
			if !strings.HasSuffix(refPath, ".md") {
				continue
			}
			
			refDoc := collection.GetDocumentByPath(refPath)
			if refDoc == nil {
				// Try relative path
				dir := filepath.Dir(doc.FilePath)
				absPath := filepath.Join(dir, refPath)
				refDoc = collection.GetDocumentByPath(absPath)
				
				if refDoc == nil {
					collectionResult.AddWarning(
						fmt.Sprintf("Document %s references non-existent document: %s", doc.FilePath, refPath),
						"see_also",
						"broken_reference")
				}
			}
		}
		
		// Check predecessor/successor references
		if doc.Predecessor != "" {
			predDoc := collection.GetDocumentByID(doc.Predecessor)
			if predDoc == nil {
				collectionResult.AddWarning(
					fmt.Sprintf("Document %s references non-existent predecessor: %s", doc.FilePath, doc.Predecessor),
					"predecessor",
					"broken_reference")
			}
		}
		
		if doc.Successor != "" {
			succDoc := collection.GetDocumentByID(doc.Successor)
			if succDoc == nil {
				collectionResult.AddWarning(
					fmt.Sprintf("Document %s references non-existent successor: %s", doc.FilePath, doc.Successor),
					"successor",
					"broken_reference")
			}
		}
	}
	
	if collectionResult.HasIssues() {
		results = append(results, collectionResult)
	}
	
	return results, nil
}

// SetSchema sets a custom schema for the validator
func (v *Validator) SetSchema(schema *model.TTMPSchema) {
	v.schema = schema
}