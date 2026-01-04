package errors

import (
	"fmt"
	"strings"
)

// ErrorType defines different categories of errors
type ErrorType string

const (
	// Parse errors
	ErrParse          ErrorType = "ParseError"
	ErrParseYAML      ErrorType = "YAMLParseError"
	ErrParseContent   ErrorType = "ContentParseError"
	
	// File errors
	ErrFile           ErrorType = "FileError"
	ErrFileNotFound   ErrorType = "FileNotFoundError"
	ErrFilePermission ErrorType = "FilePermissionError"
	
	// Validation errors
	ErrValidation        ErrorType = "ValidationError"
	ErrValidationSchema  ErrorType = "SchemaValidationError"
	ErrValidationMissing ErrorType = "MissingFieldError"
	ErrValidationFormat  ErrorType = "FormatValidationError"
	
	// Query errors
	ErrQuery          ErrorType = "QueryError"
	ErrQueryNoResults ErrorType = "NoResultsError"
	
	// System errors
	ErrSystem         ErrorType = "SystemError"
	ErrInternal       ErrorType = "InternalError"
	
	// Input errors
	ErrInput          ErrorType = "InputError"
	ErrInputFormat    ErrorType = "InputFormatError"
	ErrInputValue     ErrorType = "InputValueError"
)

// TTMPError represents a custom error type for TTMP operations
type TTMPError struct {
	Type    ErrorType
	Message string
	File    string
	Line    int
	Cause   error
}

// Error returns the string representation of the error
func (e *TTMPError) Error() string {
	parts := []string{fmt.Sprintf("[%s] %s", e.Type, e.Message)}
	
	if e.File != "" {
		if e.Line > 0 {
			parts = append(parts, fmt.Sprintf("in %s at line %d", e.File, e.Line))
		} else {
			parts = append(parts, fmt.Sprintf("in %s", e.File))
		}
	}
	
	if e.Cause != nil {
		parts = append(parts, fmt.Sprintf("caused by: %v", e.Cause))
	}
	
	return strings.Join(parts, " ")
}

// Is implements the standard Go error comparison method
func (e *TTMPError) Is(target error) bool {
	t, ok := target.(*TTMPError)
	if !ok {
		return false
	}
	return e.Type == t.Type
}

// Unwrap returns the cause of the error
func (e *TTMPError) Unwrap() error {
	return e.Cause
}

// NewError creates a new TTMP error
func NewError(errType ErrorType, message string) *TTMPError {
	return &TTMPError{
		Type:    errType,
		Message: message,
	}
}

// WithFile adds file information to the error
func (e *TTMPError) WithFile(file string) *TTMPError {
	e.File = file
	return e
}

// WithLine adds line information to the error
func (e *TTMPError) WithLine(line int) *TTMPError {
	e.Line = line
	return e
}

// WithCause adds a cause to the error
func (e *TTMPError) WithCause(cause error) *TTMPError {
	e.Cause = cause
	return e
}

// UserError returns a user-friendly error message
func (e *TTMPError) UserError() string {
	switch e.Type {
	case ErrParse, ErrParseYAML:
		if e.Line > 0 {
			return fmt.Sprintf("Error parsing document at line %d: %s", e.Line, e.Message)
		}
		return fmt.Sprintf("Error parsing document: %s", e.Message)
		
	case ErrParseContent:
		return fmt.Sprintf("Error processing document content: %s", e.Message)
		
	case ErrFileNotFound:
		return fmt.Sprintf("File not found: %s", e.File)
		
	case ErrFilePermission:
		return fmt.Sprintf("Permission denied: %s", e.File)
		
	case ErrValidation, ErrValidationSchema, ErrValidationMissing, ErrValidationFormat:
		return fmt.Sprintf("Validation error: %s", e.Message)
		
	case ErrQueryNoResults:
		return "No documents found matching the query criteria"
		
	case ErrInternal:
		return fmt.Sprintf("Internal error: %s", e.Message)
		
	case ErrInputFormat, ErrInputValue:
		return fmt.Sprintf("Invalid input: %s", e.Message)
		
	default:
		return e.Message
	}
}

// IsFileNotFound checks if the error is a file not found error
func IsFileNotFound(err error) bool {
	var e *TTMPError
	if ok := AsError(err, &e); ok {
		return e.Type == ErrFileNotFound
	}
	return false
}

// IsParseError checks if the error is a parse error
func IsParseError(err error) bool {
	var e *TTMPError
	if ok := AsError(err, &e); ok {
		return e.Type == ErrParse || e.Type == ErrParseYAML || e.Type == ErrParseContent
	}
	return false
}

// IsValidationError checks if the error is a validation error
func IsValidationError(err error) bool {
	var e *TTMPError
	if ok := AsError(err, &e); ok {
		return e.Type == ErrValidation || e.Type == ErrValidationSchema || 
			   e.Type == ErrValidationMissing || e.Type == ErrValidationFormat
	}
	return false
}

// AsError attempt to convert an error to a TTMPError
func AsError(err error, target interface{}) bool {
	return AsErrorWithType(err, target, "")
}

// AsErrorWithType attempt to convert an error to a TTMPError of a specific type
func AsErrorWithType(err error, target interface{}, errType ErrorType) bool {
	te, ok := err.(*TTMPError)
	if !ok {
		return false
	}
	
	if errType != "" && te.Type != errType {
		return false
	}
	
	tt, ok := target.(**TTMPError)
	if !ok {
		return false
	}
	
	*tt = te
	return true
}