package query

import (
	"fmt"
	"reflect"
	"regexp"
	"strings"
	"time"

	"ttmp-go/pkg/errors"
	"ttmp-go/pkg/model"
)

// Operator represents a comparison operator
type Operator string

const (
	// Equal checks if values are equal
	Equal Operator = "eq"
	
	// NotEqual checks if values are not equal
	NotEqual Operator = "ne"
	
	// Contains checks if a string contains another string
	Contains Operator = "contains"
	
	// StartsWith checks if a string starts with another string
	StartsWith Operator = "startswith"
	
	// EndsWith checks if a string ends with another string
	EndsWith Operator = "endswith"
	
	// GreaterThan checks if a value is greater than another value
	GreaterThan Operator = "gt"
	
	// LessThan checks if a value is less than another value
	LessThan Operator = "lt"
	
	// GreaterThanOrEqual checks if a value is greater than or equal to another value
	GreaterThanOrEqual Operator = "gte"
	
	// LessThanOrEqual checks if a value is less than or equal to another value
	LessThanOrEqual Operator = "lte"
	
	// In checks if a value is in a list of values
	In Operator = "in"
	
	// NotIn checks if a value is not in a list of values
	NotIn Operator = "notin"
	
	// Matches checks if a string matches a regex pattern
	Matches Operator = "matches"
)

// QueryCondition represents a condition for querying TTMP documents
type QueryCondition struct {
	Field    string
	Operator Operator
	Value    interface{}
}

// TTMPQuerier is responsible for querying TTMP documents
type TTMPQuerier struct {
	// Configuration options can be added here
}

// NewQuerier creates a new TTMPQuerier
func NewQuerier() *TTMPQuerier {
	return &TTMPQuerier{}
}

// Query searches a collection of TTMP documents based on conditions
func (q *TTMPQuerier) Query(documents []*model.TTMPDocument, conditions []QueryCondition) ([]*QueryResult, error) {
	if len(conditions) == 0 {
		return nil, errors.NewQueryError("No query conditions provided", nil)
	}

	results := make([]*QueryResult, 0)

	for _, doc := range documents {
		matches := true

		for _, condition := range conditions {
			match, err := q.evaluateCondition(doc, condition)
			if err != nil {
				return nil, err
			}

			if !match {
				matches = false
				break
			}
		}

		if matches {
			results = append(results, NewQueryResult(doc))
		}
	}

	return results, nil
}

// evaluateCondition evaluates a single condition against a document
func (q *TTMPQuerier) evaluateCondition(doc *model.TTMPDocument, condition QueryCondition) (bool, error) {
	// Get the field value
	fieldValue, err := q.getFieldValue(doc, condition.Field)
	if err != nil {
		return false, err
	}

	// If field doesn't exist or is nil, return false (unless checking for existence)
	if fieldValue == nil {
		return false, nil
	}

	// Evaluate based on operator
	switch condition.Operator {
	case Equal:
		return q.evaluateEqual(fieldValue, condition.Value)
	case NotEqual:
		matches, err := q.evaluateEqual(fieldValue, condition.Value)
		return !matches, err
	case Contains:
		return q.evaluateContains(fieldValue, condition.Value)
	case StartsWith:
		return q.evaluateStartsWith(fieldValue, condition.Value)
	case EndsWith:
		return q.evaluateEndsWith(fieldValue, condition.Value)
	case GreaterThan:
		return q.evaluateGreaterThan(fieldValue, condition.Value)
	case LessThan:
		return q.evaluateLessThan(fieldValue, condition.Value)
	case GreaterThanOrEqual:
		return q.evaluateGreaterThanOrEqual(fieldValue, condition.Value)
	case LessThanOrEqual:
		return q.evaluateLessThanOrEqual(fieldValue, condition.Value)
	case In:
		return q.evaluateIn(fieldValue, condition.Value)
	case NotIn:
		matches, err := q.evaluateIn(fieldValue, condition.Value)
		return !matches, err
	case Matches:
		return q.evaluateMatches(fieldValue, condition.Value)
	default:
		return false, errors.NewQueryError(
			fmt.Sprintf("Unknown operator: %s", condition.Operator),
			nil,
		)
	}
}

// getFieldValue retrieves the value of a field from a document
func (q *TTMPQuerier) getFieldValue(doc *model.TTMPDocument, field string) (interface{}, error) {
	// Check standard fields first
	switch strings.ToLower(field) {
	case "id":
		return doc.ID, nil
	case "type":
		return doc.Type, nil
	case "title":
		return doc.Title, nil
	case "description":
		return doc.Description, nil
	case "created":
		return doc.Created, nil
	case "modified":
		if doc.Modified != nil {
			return *doc.Modified, nil
		}
		return nil, nil
	case "tags":
		return doc.Tags, nil
	case "links":
		return doc.Links, nil
	case "content":
		return doc.Content, nil
	}

	// Check if it's a custom field
	if val, ok := doc.Custom[field]; ok {
		return val, nil
	}

	// Field not found
	return nil, nil
}

// evaluateEqual checks if two values are equal
func (q *TTMPQuerier) evaluateEqual(fieldValue, conditionValue interface{}) (bool, error) {
	// Handle special cases
	switch typedVal := fieldValue.(type) {
	case time.Time:
		// For dates, attempt to convert the condition value to a time
		switch cv := conditionValue.(type) {
		case time.Time:
			return typedVal.Equal(cv), nil
		case string:
			// Try to parse the string as a date
			parsedTime, err := parseTimeString(cv)
			if err != nil {
				return false, errors.NewQueryError(
					fmt.Sprintf("Cannot compare time with string: %s", cv),
					err,
				)
			}
			return typedVal.Equal(parsedTime), nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot compare time with %T", conditionValue),
				nil,
			)
		}
	case []string:
		// For string arrays, check if arrays are equal
		switch cv := conditionValue.(type) {
		case []string:
			if len(typedVal) != len(cv) {
				return false, nil
			}
			for i, v := range typedVal {
				if v != cv[i] {
					return false, nil
				}
			}
			return true, nil
		case string:
			// Check if the array contains the string
			for _, v := range typedVal {
				if v == cv {
					return true, nil
				}
			}
			return false, nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot compare string array with %T", conditionValue),
				nil,
			)
		}
	}

	// For other types, just compare directly
	return fieldValue == conditionValue, nil
}

// evaluateContains checks if a string contains another string
func (q *TTMPQuerier) evaluateContains(fieldValue, conditionValue interface{}) (bool, error) {
	switch typedField := fieldValue.(type) {
	case string:
		// For string fields, check if it contains the condition value
		switch cv := conditionValue.(type) {
		case string:
			return strings.Contains(typedField, cv), nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot use contains with non-string value: %v", conditionValue),
				nil,
			)
		}
	case []string:
		// For string arrays, check if the array contains the condition value
		switch cv := conditionValue.(type) {
		case string:
			for _, v := range typedField {
				if v == cv {
					return true, nil
				}
			}
			return false, nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot use contains with non-string value: %v", conditionValue),
				nil,
			)
		}
	default:
		return false, errors.NewQueryError(
			fmt.Sprintf("Cannot use contains with field type %T", fieldValue),
			nil,
		)
	}
}

// evaluateStartsWith checks if a string starts with another string
func (q *TTMPQuerier) evaluateStartsWith(fieldValue, conditionValue interface{}) (bool, error) {
	switch typedField := fieldValue.(type) {
	case string:
		// For string fields, check if it starts with the condition value
		switch cv := conditionValue.(type) {
		case string:
			return strings.HasPrefix(typedField, cv), nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot use startswith with non-string value: %v", conditionValue),
				nil,
			)
		}
	default:
		return false, errors.NewQueryError(
			fmt.Sprintf("Cannot use startswith with field type %T", fieldValue),
			nil,
		)
	}
}

// evaluateEndsWith checks if a string ends with another string
func (q *TTMPQuerier) evaluateEndsWith(fieldValue, conditionValue interface{}) (bool, error) {
	switch typedField := fieldValue.(type) {
	case string:
		// For string fields, check if it ends with the condition value
		switch cv := conditionValue.(type) {
		case string:
			return strings.HasSuffix(typedField, cv), nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot use endswith with non-string value: %v", conditionValue),
				nil,
			)
		}
	default:
		return false, errors.NewQueryError(
			fmt.Sprintf("Cannot use endswith with field type %T", fieldValue),
			nil,
		)
	}
}

// evaluateGreaterThan checks if a value is greater than another value
func (q *TTMPQuerier) evaluateGreaterThan(fieldValue, conditionValue interface{}) (bool, error) {
	switch typedField := fieldValue.(type) {
	case time.Time:
		// Handle time comparison
		switch cv := conditionValue.(type) {
		case time.Time:
			return typedField.After(cv), nil
		case string:
			parsedTime, err := parseTimeString(cv)
			if err != nil {
				return false, errors.NewQueryError(
					fmt.Sprintf("Cannot compare time with string: %s", cv),
					err,
				)
			}
			return typedField.After(parsedTime), nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot compare time with %T", conditionValue),
				nil,
			)
		}
	case int:
		// Handle int comparison
		switch cv := conditionValue.(type) {
		case int:
			return typedField > cv, nil
		case float64:
			return float64(typedField) > cv, nil
		case string:
			val, err := parseNumericString(cv)
			if err != nil {
				return false, errors.NewQueryError(
					fmt.Sprintf("Cannot compare int with string: %s", cv),
					err,
				)
			}
			return float64(typedField) > val, nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot compare int with %T", conditionValue),
				nil,
			)
		}
	case float64:
		// Handle float comparison
		switch cv := conditionValue.(type) {
		case int:
			return typedField > float64(cv), nil
		case float64:
			return typedField > cv, nil
		case string:
			val, err := parseNumericString(cv)
			if err != nil {
				return false, errors.NewQueryError(
					fmt.Sprintf("Cannot compare float with string: %s", cv),
					err,
				)
			}
			return typedField > val, nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot compare float with %T", conditionValue),
				nil,
			)
		}
	case string:
		// Handle string comparison
		switch cv := conditionValue.(type) {
		case string:
			return typedField > cv, nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot compare string with %T", conditionValue),
				nil,
			)
		}
	default:
		return false, errors.NewQueryError(
			fmt.Sprintf("Cannot use greater than with field type %T", fieldValue),
			nil,
		)
	}
}

// evaluateLessThan checks if a value is less than another value
func (q *TTMPQuerier) evaluateLessThan(fieldValue, conditionValue interface{}) (bool, error) {
	switch typedField := fieldValue.(type) {
	case time.Time:
		// Handle time comparison
		switch cv := conditionValue.(type) {
		case time.Time:
			return typedField.Before(cv), nil
		case string:
			parsedTime, err := parseTimeString(cv)
			if err != nil {
				return false, errors.NewQueryError(
					fmt.Sprintf("Cannot compare time with string: %s", cv),
					err,
				)
			}
			return typedField.Before(parsedTime), nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot compare time with %T", conditionValue),
				nil,
			)
		}
	case int:
		// Handle int comparison
		switch cv := conditionValue.(type) {
		case int:
			return typedField < cv, nil
		case float64:
			return float64(typedField) < cv, nil
		case string:
			val, err := parseNumericString(cv)
			if err != nil {
				return false, errors.NewQueryError(
					fmt.Sprintf("Cannot compare int with string: %s", cv),
					err,
				)
			}
			return float64(typedField) < val, nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot compare int with %T", conditionValue),
				nil,
			)
		}
	case float64:
		// Handle float comparison
		switch cv := conditionValue.(type) {
		case int:
			return typedField < float64(cv), nil
		case float64:
			return typedField < cv, nil
		case string:
			val, err := parseNumericString(cv)
			if err != nil {
				return false, errors.NewQueryError(
					fmt.Sprintf("Cannot compare float with string: %s", cv),
					err,
				)
			}
			return typedField < val, nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot compare float with %T", conditionValue),
				nil,
			)
		}
	case string:
		// Handle string comparison
		switch cv := conditionValue.(type) {
		case string:
			return typedField < cv, nil
		default:
			return false, errors.NewQueryError(
				fmt.Sprintf("Cannot compare string with %T", conditionValue),
				nil,
			)
		}
	default:
		return false, errors.NewQueryError(
			fmt.Sprintf("Cannot use less than with field type %T", fieldValue),
			nil,
		)
	}
}

// evaluateGreaterThanOrEqual checks if a value is greater than or equal to another value
func (q *TTMPQuerier) evaluateGreaterThanOrEqual(fieldValue, conditionValue interface{}) (bool, error) {
	gt, err := q.evaluateGreaterThan(fieldValue, conditionValue)
	if err != nil {
		return false, err
	}
	
	eq, err := q.evaluateEqual(fieldValue, conditionValue)
	if err != nil {
		return false, err
	}
	
	return gt || eq, nil
}

// evaluateLessThanOrEqual checks if a value is less than or equal to another value
func (q *TTMPQuerier) evaluateLessThanOrEqual(fieldValue, conditionValue interface{}) (bool, error) {
	lt, err := q.evaluateLessThan(fieldValue, conditionValue)
	if err != nil {
		return false, err
	}
	
	eq, err := q.evaluateEqual(fieldValue, conditionValue)
	if err != nil {
		return false, err
	}
	
	return lt || eq, nil
}

// evaluateIn checks if a value is in a list of values
func (q *TTMPQuerier) evaluateIn(fieldValue, conditionValue interface{}) (bool, error) {
	// conditionValue must be a slice
	condValSlice := reflect.ValueOf(conditionValue)
	if condValSlice.Kind() != reflect.Slice {
		return false, errors.NewQueryError(
			fmt.Sprintf("In operator requires a slice, got %T", conditionValue),
			nil,
		)
	}
	
	// Check if fieldValue is in the slice
	for i := 0; i < condValSlice.Len(); i++ {
		equal, err := q.evaluateEqual(fieldValue, condValSlice.Index(i).Interface())
		if err != nil {
			continue
		}
		
		if equal {
			return true, nil
		}
	}
	
	return false, nil
}

// evaluateMatches checks if a string matches a regex pattern
func (q *TTMPQuerier) evaluateMatches(fieldValue, conditionValue interface{}) (bool, error) {
	fieldStr, ok := fieldValue.(string)
	if !ok {
		return false, errors.NewQueryError(
			fmt.Sprintf("Matches operator requires a string field, got %T", fieldValue),
			nil,
		)
	}
	
	patternStr, ok := conditionValue.(string)
	if !ok {
		return false, errors.NewQueryError(
			fmt.Sprintf("Matches operator requires a string pattern, got %T", conditionValue),
			nil,
		)
	}
	
	// Compile the regex pattern
	pattern, err := regexp.Compile(patternStr)
	if err != nil {
		return false, errors.NewQueryError(
			fmt.Sprintf("Invalid regex pattern: %s", patternStr),
			err,
		)
	}
	
	return pattern.MatchString(fieldStr), nil
}

// parseTimeString parses a string into a time.Time
func parseTimeString(timeStr string) (time.Time, error) {
	// Try common formats
	formats := []string{
		time.RFC3339,
		"2006-01-02",
		"2006-01-02 15:04:05",
		"2006-01-02T15:04:05",
		"2006-01-02T15:04:05Z07:00",
	}
	
	for _, format := range formats {
		t, err := time.Parse(format, timeStr)
		if err == nil {
			return t, nil
		}
	}
	
	return time.Time{}, fmt.Errorf("could not parse time string: %s", timeStr)
}

// parseNumericString parses a string into a float64
func parseNumericString(numStr string) (float64, error) {
	var val float64
	var err error
	
	// Try parsing as float
	val, err = parseFloat(numStr)
	if err != nil {
		return 0, fmt.Errorf("could not parse numeric string: %s", numStr)
	}
	
	return val, nil
}

// parseFloat parses a string into a float64
func parseFloat(s string) (float64, error) {
	// Remove any commas
	s = strings.ReplaceAll(s, ",", "")
	
	// Parse float
	var val float64
	_, err := fmt.Sscanf(s, "%f", &val)
	if err != nil {
		return 0, err
	}
	
	return val, nil
}