package main

import (
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
)

// loadFieldsForResults loads fields for a slice of log results
func (q *LogQuerier) loadFieldsForResults(results []LogResult) error {
	if len(results) == 0 {
		return nil
	}

	// Build list of log IDs
	logIDs := make([]string, len(results))
	logIDMap := make(map[int64]*LogResult)
	
	for i, result := range results {
		logIDs[i] = fmt.Sprintf("%d", result.ID)
		logIDMap[result.ID] = &results[i]
	}

	// Query all fields for these log IDs
	query := fmt.Sprintf(`
		SELECT log_id, field_name, field_value, field_type 
		FROM log_fields 
		WHERE log_id IN (%s)
		ORDER BY log_id, field_name
	`, strings.Join(logIDs, ","))

	rows, err := q.db.GetDB().Query(query)
	if err != nil {
		return fmt.Errorf("failed to query fields: %w", err)
	}
	defer rows.Close()

	for rows.Next() {
		var logID int64
		var fieldName, fieldValue, fieldType string

		if err := rows.Scan(&logID, &fieldName, &fieldValue, &fieldType); err != nil {
			return fmt.Errorf("failed to scan field row: %w", err)
		}

		if result, exists := logIDMap[logID]; exists {
			if result.Fields == nil {
				result.Fields = make(map[string]interface{})
			}
			result.Fields[fieldName] = parseFieldValue(fieldValue, fieldType)
		}
	}

	return rows.Err()
}

// parseFieldValue converts a string field value back to its original type
func parseFieldValue(value, fieldType string) interface{} {
	switch fieldType {
	case "string":
		return value
	case "number":
		// Try int first, then float
		if intVal, err := strconv.ParseInt(value, 10, 64); err == nil {
			return intVal
		}
		if floatVal, err := strconv.ParseFloat(value, 64); err == nil {
			return floatVal
		}
		return value // fallback to string
	case "boolean":
		if boolVal, err := strconv.ParseBool(value); err == nil {
			return boolVal
		}
		return value // fallback to string
	case "object":
		// Try to parse as JSON
		var obj interface{}
		if err := json.Unmarshal([]byte(value), &obj); err == nil {
			return obj
		}
		return value // fallback to string
	default:
		return value
	}
}

