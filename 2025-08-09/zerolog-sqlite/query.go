package main

import (
	"database/sql"
	"fmt"
	"strings"
	"time"
)

// QueryOptions defines options for querying logs
type QueryOptions struct {
	Levels     []string          // Filter by log levels
	StartTime  *time.Time        // Start time filter
	EndTime    *time.Time        // End time filter
	Message    string            // Message substring filter
	Caller     string            // Caller filter
	Fields     map[string]string // Field filters (field_name -> field_value)
	Limit      int               // Maximum number of results
	Offset     int               // Offset for pagination
	OrderBy    string            // Order by field (timestamp, level, etc.)
	OrderDesc  bool              // Order descending
}

// LogResult represents a log entry returned from queries
type LogResult struct {
	ID        int64                  `json:"id"`
	Timestamp time.Time              `json:"timestamp"`
	Level     string                 `json:"level"`
	Message   string                 `json:"message"`
	Caller    string                 `json:"caller"`
	Stack     string                 `json:"stack"`
	Fields    map[string]interface{} `json:"fields"`
	CreatedAt time.Time              `json:"created_at"`
}

// QueryStats represents statistics about query results
type QueryStats struct {
	TotalCount int            `json:"total_count"`
	LevelCounts map[string]int `json:"level_counts"`
	TimeRange   struct {
		Start time.Time `json:"start"`
		End   time.Time `json:"end"`
	} `json:"time_range"`
}

// LogQuerier provides methods for querying logs
type LogQuerier struct {
	db *Database
}

// NewLogQuerier creates a new log querier
func NewLogQuerier(db *Database) *LogQuerier {
	return &LogQuerier{db: db}
}

// QueryLogs retrieves logs based on the provided options
func (q *LogQuerier) QueryLogs(options QueryOptions) ([]LogResult, error) {
	query, args := q.buildQuery(options, false)
	
	rows, err := q.db.GetDB().Query(query, args...)
	if err != nil {
		return nil, fmt.Errorf("failed to execute query: %w", err)
	}
	defer rows.Close()

	var results []LogResult
	for rows.Next() {
		var result LogResult
		var createdAt time.Time

		err := rows.Scan(
			&result.ID,
			&result.Timestamp,
			&result.Level,
			&result.Message,
			&result.Caller,
			&result.Stack,
			&createdAt,
		)
		if err != nil {
			return nil, fmt.Errorf("failed to scan row: %w", err)
		}

		result.CreatedAt = createdAt
		result.Fields = make(map[string]interface{})

		results = append(results, result)
	}

	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("error iterating rows: %w", err)
	}

	// Load fields for all results
	if len(results) > 0 {
		if err := q.loadFieldsForResults(results); err != nil {
			return nil, fmt.Errorf("failed to load fields: %w", err)
		}
	}

	return results, nil
}

// CountLogs returns the count of logs matching the criteria
func (q *LogQuerier) CountLogs(options QueryOptions) (int, error) {
	query, args := q.buildQuery(options, true)
	
	var count int
	err := q.db.GetDB().QueryRow(query, args...).Scan(&count)
	if err != nil {
		return 0, fmt.Errorf("failed to count logs: %w", err)
	}

	return count, nil
}

// GetLogStats returns statistics about logs matching the criteria
func (q *LogQuerier) GetLogStats(options QueryOptions) (*QueryStats, error) {
	stats := &QueryStats{
		LevelCounts: make(map[string]int),
	}

	// Get total count
	count, err := q.CountLogs(options)
	if err != nil {
		return nil, fmt.Errorf("failed to get total count: %w", err)
	}
	stats.TotalCount = count

	// Get level counts
	levelQuery, levelArgs := q.buildLevelCountQuery(options)
	rows, err := q.db.GetDB().Query(levelQuery, levelArgs...)
	if err != nil {
		return nil, fmt.Errorf("failed to query level counts: %w", err)
	}
	defer rows.Close()

	for rows.Next() {
		var level string
		var count int
		if err := rows.Scan(&level, &count); err != nil {
			return nil, fmt.Errorf("failed to scan level count: %w", err)
		}
		stats.LevelCounts[level] = count
	}

	// Get time range
	timeQuery, timeArgs := q.buildTimeRangeQuery(options)
	var startTimeStr, endTimeStr sql.NullString
	err = q.db.GetDB().QueryRow(timeQuery, timeArgs...).Scan(&startTimeStr, &endTimeStr)
	if err != nil && err != sql.ErrNoRows {
		return nil, fmt.Errorf("failed to get time range: %w", err)
	}

	if startTimeStr.Valid {
		if t, err := time.Parse(time.RFC3339Nano, startTimeStr.String); err == nil {
			stats.TimeRange.Start = t
		}
	}
	if endTimeStr.Valid {
		if t, err := time.Parse(time.RFC3339Nano, endTimeStr.String); err == nil {
			stats.TimeRange.End = t
		}
	}

	return stats, nil
}

// GetLogByID retrieves a specific log entry by ID
func (q *LogQuerier) GetLogByID(id int64) (*LogResult, error) {
	query := `
		SELECT id, timestamp, level, message, caller, stack, created_at
		FROM logs
		WHERE id = ?
	`

	var result LogResult
	var createdAt time.Time

	err := q.db.GetDB().QueryRow(query, id).Scan(
		&result.ID,
		&result.Timestamp,
		&result.Level,
		&result.Message,
		&result.Caller,
		&result.Stack,
		&createdAt,
	)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, fmt.Errorf("log with ID %d not found", id)
		}
		return nil, fmt.Errorf("failed to get log by ID: %w", err)
	}

	result.CreatedAt = createdAt
	result.Fields = make(map[string]interface{})

	// Load fields for this single result
	results := []LogResult{result}
	if err := q.loadFieldsForResults(results); err != nil {
		return nil, fmt.Errorf("failed to load fields: %w", err)
	}

	return &results[0], nil
}

// DeleteOldLogs deletes logs older than the specified duration
func (q *LogQuerier) DeleteOldLogs(olderThan time.Duration) (int64, error) {
	cutoffTime := time.Now().Add(-olderThan)
	
	result, err := q.db.GetDB().Exec("DELETE FROM logs WHERE timestamp < ?", cutoffTime)
	if err != nil {
		return 0, fmt.Errorf("failed to delete old logs: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return 0, fmt.Errorf("failed to get rows affected: %w", err)
	}

	return rowsAffected, nil
}

// buildQuery constructs the SQL query based on options
func (q *LogQuerier) buildQuery(options QueryOptions, countOnly bool) (string, []interface{}) {
	var query strings.Builder
	var args []interface{}

	if countOnly {
		if len(options.Fields) > 0 {
			query.WriteString("SELECT COUNT(DISTINCT l.id) FROM logs l")
		} else {
			query.WriteString("SELECT COUNT(*) FROM logs l")
		}
	} else {
		query.WriteString("SELECT l.id, l.timestamp, l.level, l.message, l.caller, l.stack, l.created_at FROM logs l")
	}

	// Add JOIN for field filtering if needed
	if len(options.Fields) > 0 {
		query.WriteString(" INNER JOIN log_fields lf ON l.id = lf.log_id")
	}

	// Build WHERE clause
	conditions := []string{}

	// Level filter
	if len(options.Levels) > 0 {
		placeholders := make([]string, len(options.Levels))
		for i, level := range options.Levels {
			placeholders[i] = "?"
			args = append(args, level)
		}
		conditions = append(conditions, fmt.Sprintf("l.level IN (%s)", strings.Join(placeholders, ",")))
	}

	// Time filters
	if options.StartTime != nil {
		conditions = append(conditions, "l.timestamp >= ?")
		args = append(args, *options.StartTime)
	}
	if options.EndTime != nil {
		conditions = append(conditions, "l.timestamp <= ?")
		args = append(args, *options.EndTime)
	}

	// Message filter
	if options.Message != "" {
		conditions = append(conditions, "l.message LIKE ?")
		args = append(args, "%"+options.Message+"%")
	}

	// Caller filter
	if options.Caller != "" {
		conditions = append(conditions, "l.caller LIKE ?")
		args = append(args, "%"+options.Caller+"%")
	}

	// Field filters using JOIN
	if len(options.Fields) > 0 {
		fieldConditions := []string{}
		for fieldName, fieldValue := range options.Fields {
			fieldConditions = append(fieldConditions, "(lf.field_name = ? AND lf.field_value = ?)")
			args = append(args, fieldName, fieldValue)
		}
		if len(fieldConditions) == 1 {
			conditions = append(conditions, fieldConditions[0])
		} else {
			// For multiple field filters, we need to ensure all conditions are met
			conditions = append(conditions, fmt.Sprintf("(%s)", strings.Join(fieldConditions, " OR ")))
		}
	}

	// Add WHERE clause if we have conditions
	if len(conditions) > 0 {
		query.WriteString(" WHERE ")
		query.WriteString(strings.Join(conditions, " AND "))
	}

	// Add GROUP BY for field filtering to avoid duplicates
	if len(options.Fields) > 0 && !countOnly {
		query.WriteString(" GROUP BY l.id, l.timestamp, l.level, l.message, l.caller, l.stack, l.created_at")
		if len(options.Fields) > 1 {
			query.WriteString(fmt.Sprintf(" HAVING COUNT(DISTINCT lf.field_name) = %d", len(options.Fields)))
		}
	}

	// Add ORDER BY and LIMIT for non-count queries
	if !countOnly {
		orderBy := "l.timestamp"
		if options.OrderBy != "" {
			orderBy = "l." + options.OrderBy
		}
		
		query.WriteString(fmt.Sprintf(" ORDER BY %s", orderBy))
		if options.OrderDesc {
			query.WriteString(" DESC")
		}

		if options.Limit > 0 {
			query.WriteString(" LIMIT ?")
			args = append(args, options.Limit)
		}

		if options.Offset > 0 {
			query.WriteString(" OFFSET ?")
			args = append(args, options.Offset)
		}
	}

	return query.String(), args
}

// buildLevelCountQuery builds a query to count logs by level
func (q *LogQuerier) buildLevelCountQuery(options QueryOptions) (string, []interface{}) {
	var query strings.Builder
	var args []interface{}

	query.WriteString("SELECT level, COUNT(*) FROM logs")

	// Build WHERE clause (similar to buildQuery but without level filter)
	conditions := []string{}

	if options.StartTime != nil {
		conditions = append(conditions, "timestamp >= ?")
		args = append(args, *options.StartTime)
	}
	if options.EndTime != nil {
		conditions = append(conditions, "timestamp <= ?")
		args = append(args, *options.EndTime)
	}
	if options.Message != "" {
		conditions = append(conditions, "message LIKE ?")
		args = append(args, "%"+options.Message+"%")
	}
	if options.Caller != "" {
		conditions = append(conditions, "caller LIKE ?")
		args = append(args, "%"+options.Caller+"%")
	}

	for fieldName, fieldValue := range options.Fields {
		conditions = append(conditions, "JSON_EXTRACT(fields, ?) IS NOT NULL AND JSON_EXTRACT(fields, ?) = ?")
		args = append(args, "$."+fieldName)
		args = append(args, "$."+fieldName)
		args = append(args, fieldValue)
	}

	if len(conditions) > 0 {
		query.WriteString(" WHERE ")
		query.WriteString(strings.Join(conditions, " AND "))
	}

	query.WriteString(" GROUP BY level")

	return query.String(), args
}

// buildTimeRangeQuery builds a query to get the time range of matching logs
func (q *LogQuerier) buildTimeRangeQuery(options QueryOptions) (string, []interface{}) {
	var query strings.Builder
	var args []interface{}

	query.WriteString("SELECT MIN(timestamp), MAX(timestamp) FROM logs")

	// Build WHERE clause (same as buildQuery)
	conditions := []string{}

	if len(options.Levels) > 0 {
		placeholders := make([]string, len(options.Levels))
		for i, level := range options.Levels {
			placeholders[i] = "?"
			args = append(args, level)
		}
		conditions = append(conditions, fmt.Sprintf("level IN (%s)", strings.Join(placeholders, ",")))
	}

	if options.StartTime != nil {
		conditions = append(conditions, "timestamp >= ?")
		args = append(args, *options.StartTime)
	}
	if options.EndTime != nil {
		conditions = append(conditions, "timestamp <= ?")
		args = append(args, *options.EndTime)
	}
	if options.Message != "" {
		conditions = append(conditions, "message LIKE ?")
		args = append(args, "%"+options.Message+"%")
	}
	if options.Caller != "" {
		conditions = append(conditions, "caller LIKE ?")
		args = append(args, "%"+options.Caller+"%")
	}

	for fieldName, fieldValue := range options.Fields {
		conditions = append(conditions, "JSON_EXTRACT(fields, ?) IS NOT NULL AND JSON_EXTRACT(fields, ?) = ?")
		args = append(args, "$."+fieldName)
		args = append(args, "$."+fieldName)
		args = append(args, fieldValue)
	}

	if len(conditions) > 0 {
		query.WriteString(" WHERE ")
		query.WriteString(strings.Join(conditions, " AND "))
	}

	return query.String(), args
}

