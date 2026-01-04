package errors

import (
	"fmt"
	"strings"
)

// ErrorType represents the type of error
type ErrorType string

const (
	// ErrParsing represents parsing-related errors
	ErrParsing ErrorType = "PARSING"
	
	// ErrValidation represents validation-related errors
	ErrValidation ErrorType = "VALIDATION"
	
	// ErrQuery represents query-related errors
	ErrQuery ErrorType = "QUERY"
	
	// ErrIO represents I/O related errors
	ErrIO ErrorType = "IO"
	
	// ErrInternal represents internal errors
	ErrInternal ErrorType = "INTERNAL"
)

// TTMPError represents a custom error in the TTMP system
type TTMPError struct {
	Type    ErrorType
	Message string
	Cause   error
	Details []string
}

// Error implements the error interface
func (e *TTMPError) Error() string {
	if len(e.Details) > 0 {
		return fmt.Sprintf("[%s] %s\nDetails: %s", e.Type, e.Message, strings.Join(e.Details, ", "))
	}
	
	if e.Cause != nil {
		return fmt.Sprintf("[%s] %s\nCause: %s", e.Type, e.Message, e.Cause.Error())
	}
	
	return fmt.Sprintf("[%s] %s", e.Type, e.Message)
}

// AddDetail adds a detail to the error
func (e *TTMPError) AddDetail(detail string) *TTMPError {
	e.Details = append(e.Details, detail)
	return e
}

// NewParsingError creates a new parsing error
func NewParsingError(message string, cause error) *TTMPError {
	return &TTMPError{
		Type:    ErrParsing,
		Message: message,
		Cause:   cause,
	}
}

// NewValidationError creates a new validation error
func NewValidationError(message string, details ...string) *TTMPError {
	return &TTMPError{
		Type:    ErrValidation,
		Message: message,
		Details: details,
	}
}

// NewQueryError creates a new query error
func NewQueryError(message string, cause error) *TTMPError {
	return &TTMPError{
		Type:    ErrQuery,
		Message: message,
		Cause:   cause,
	}
}

// NewIOError creates a new I/O error
func NewIOError(message string, cause error) *TTMPError {
	return &TTMPError{
		Type:    ErrIO,
		Message: message,
		Cause:   cause,
	}
}

// NewInternalError creates a new internal error
func NewInternalError(message string, cause error) *TTMPError {
	return &TTMPError{
		Type:    ErrInternal,
		Message: message,
		Cause:   cause,
	}
}

// ValidationErrors represents a collection of validation errors
type ValidationErrors struct {
	Errors []*TTMPError
}

// NewValidationErrors creates a new ValidationErrors
func NewValidationErrors() *ValidationErrors {
	return &ValidationErrors{
		Errors: make([]*TTMPError, 0),
	}
}

// AddError adds an error to the collection
func (ve *ValidationErrors) AddError(message string, details ...string) {
	ve.Errors = append(ve.Errors, NewValidationError(message, details...))
}

// HasErrors returns true if there are any errors in the collection
func (ve *ValidationErrors) HasErrors() bool {
	return len(ve.Errors) > 0
}

// Error implements the error interface
func (ve *ValidationErrors) Error() string {
	if !ve.HasErrors() {
		return "No validation errors"
	}
	
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Found %d validation errors:\n", len(ve.Errors)))
	
	for i, err := range ve.Errors {
		sb.WriteString(fmt.Sprintf("%d. %s\n", i+1, err.Error()))
	}
	
	return sb.String()
}