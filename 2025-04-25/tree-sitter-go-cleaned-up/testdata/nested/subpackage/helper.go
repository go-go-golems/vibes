package subpackage

import (
	"errors"
	"strings"
)

// Helper is a utility struct for testing nested packages
type Helper struct {
	prefix string
	cache  map[string]string
}

// NewHelper creates a new Helper instance
func NewHelper(prefix string) *Helper {
	return &Helper{
		prefix: prefix,
		cache:  make(map[string]string),
	}
}

// FormatString formats a string with the helper's prefix
func (h *Helper) FormatString(input string) string {
	// Check cache first
	if formatted, ok := h.cache[input]; ok {
		return formatted
	}
	
	// Format the string
	formatted := h.prefix + ": " + input
	
	// Store in cache
	h.cache[input] = formatted
	
	return formatted
}

// ClearCache clears the helper's cache
func (h *Helper) ClearCache() {
	h.cache = make(map[string]string)
}

// Validate checks if a string meets certain criteria
func Validate(input string) error {
	// Check if empty
	if input == "" {
		return errors.New("input cannot be empty")
	}
	
	// Check if too short
	if len(input) < 3 {
		return errors.New("input must be at least 3 characters long")
	}
	
	// Check if contains spaces
	if strings.Contains(input, " ") {
		return errors.New("input cannot contain spaces")
	}
	
	return nil
}