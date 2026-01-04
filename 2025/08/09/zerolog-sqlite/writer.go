package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"sync"
	"time"
)

// SQLiteWriter implements io.Writer interface for zerolog
type SQLiteWriter struct {
	db               *Database
	insertStmt       *sql.Stmt
	insertFieldsStmt *sql.Stmt
	mutex            sync.Mutex
	batchSize        int
	batchBuffer      []LogEntry
	batchMutex       sync.Mutex
}

// LogEntry represents a parsed log entry
type LogEntry struct {
	Timestamp time.Time              `json:"time"`
	Level     string                 `json:"level"`
	Message   string                 `json:"message"`
	Caller    string                 `json:"caller"`
	Stack     string                 `json:"stack"`
	Fields    map[string]interface{} `json:"-"`
}

// NewSQLiteWriter creates a new SQLite writer for zerolog
func NewSQLiteWriter(db *Database, options ...WriterOption) (*SQLiteWriter, error) {
	writer := &SQLiteWriter{
		db:          db,
		batchSize:   1, // Default to immediate writes
		batchBuffer: make([]LogEntry, 0),
	}

	// Apply options
	for _, option := range options {
		option(writer)
	}

	// Prepare statements
	if err := writer.prepareStatements(); err != nil {
		return nil, fmt.Errorf("failed to prepare statements: %w", err)
	}

	return writer, nil
}

// WriterOption defines configuration options for SQLiteWriter
type WriterOption func(*SQLiteWriter)

// WithBatchSize sets the batch size for bulk inserts
func WithBatchSize(size int) WriterOption {
	return func(w *SQLiteWriter) {
		if size > 0 {
			w.batchSize = size
		}
	}
}

// prepareStatements prepares SQL statements for inserting logs
func (w *SQLiteWriter) prepareStatements() error {
	// Prepare main insert statement (removed fields column)
	insertSQL := `
		INSERT INTO logs (timestamp, level, message, caller, stack)
		VALUES (?, ?, ?, ?, ?)
	`
	stmt, err := w.db.GetDB().Prepare(insertSQL)
	if err != nil {
		return fmt.Errorf("failed to prepare insert statement: %w", err)
	}
	w.insertStmt = stmt

	// Prepare fields insert statement
	fieldsSQL := `
		INSERT INTO log_fields (log_id, field_name, field_value, field_type)
		VALUES (?, ?, ?, ?)
	`
	stmt, err = w.db.GetDB().Prepare(fieldsSQL)
	if err != nil {
		return fmt.Errorf("failed to prepare fields insert statement: %w", err)
	}
	w.insertFieldsStmt = stmt

	return nil
}

// Write implements io.Writer interface
func (w *SQLiteWriter) Write(p []byte) (n int, err error) {
	// Parse the log entry
	entry, err := w.parseLogEntry(p)
	if err != nil {
		// If parsing fails, we still return the number of bytes to avoid breaking the logger
		return len(p), fmt.Errorf("failed to parse log entry: %w", err)
	}

	// Handle batching
	if w.batchSize > 1 {
		return w.writeBatched(entry, len(p))
	}

	// Write immediately
	if err := w.writeEntry(entry); err != nil {
		return len(p), fmt.Errorf("failed to write log entry: %w", err)
	}

	return len(p), nil
}

// parseLogEntry parses a JSON log entry from zerolog
func (w *SQLiteWriter) parseLogEntry(data []byte) (*LogEntry, error) {
	// Parse as generic JSON first
	var raw map[string]interface{}
	if err := json.Unmarshal(data, &raw); err != nil {
		return nil, fmt.Errorf("failed to unmarshal JSON: %w", err)
	}

	entry := &LogEntry{
		Fields: make(map[string]interface{}),
	}

	// Extract standard fields
	if timeStr, ok := raw["time"].(string); ok {
		if t, err := time.Parse(time.RFC3339Nano, timeStr); err == nil {
			entry.Timestamp = t
		} else {
			entry.Timestamp = time.Now()
		}
		delete(raw, "time")
	} else {
		entry.Timestamp = time.Now()
	}

	if level, ok := raw["level"].(string); ok {
		entry.Level = level
		delete(raw, "level")
	}

	if message, ok := raw["message"].(string); ok {
		entry.Message = message
		delete(raw, "message")
	}

	if caller, ok := raw["caller"].(string); ok {
		entry.Caller = caller
		delete(raw, "caller")
	}

	if stack, ok := raw["stack"].(string); ok {
		entry.Stack = stack
		delete(raw, "stack")
	}

	// Store remaining fields
	entry.Fields = raw

	return entry, nil
}

// writeBatched handles batched writing
func (w *SQLiteWriter) writeBatched(entry *LogEntry, bytesWritten int) (int, error) {
	w.batchMutex.Lock()
	defer w.batchMutex.Unlock()

	w.batchBuffer = append(w.batchBuffer, *entry)

	if len(w.batchBuffer) >= w.batchSize {
		if err := w.flushBatch(); err != nil {
			return bytesWritten, fmt.Errorf("failed to flush batch: %w", err)
		}
	}

	return bytesWritten, nil
}

// flushBatch writes all buffered entries to the database
func (w *SQLiteWriter) flushBatch() error {
	if len(w.batchBuffer) == 0 {
		return nil
	}

	tx, err := w.db.GetDB().Begin()
	if err != nil {
		return fmt.Errorf("failed to begin transaction: %w", err)
	}
	defer tx.Rollback()

	for _, entry := range w.batchBuffer {
		if err := w.writeEntryTx(tx, &entry); err != nil {
			return fmt.Errorf("failed to write entry in batch: %w", err)
		}
	}

	if err := tx.Commit(); err != nil {
		return fmt.Errorf("failed to commit batch: %w", err)
	}

	// Clear buffer
	w.batchBuffer = w.batchBuffer[:0]
	return nil
}

// writeEntry writes a single log entry to the database
func (w *SQLiteWriter) writeEntry(entry *LogEntry) error {
	w.mutex.Lock()
	defer w.mutex.Unlock()

	tx, err := w.db.GetDB().Begin()
	if err != nil {
		return fmt.Errorf("failed to begin transaction: %w", err)
	}
	defer tx.Rollback()

	if err := w.writeEntryTx(tx, entry); err != nil {
		return err
	}

	return tx.Commit()
}

// writeEntryTx writes a log entry within a transaction
func (w *SQLiteWriter) writeEntryTx(tx *sql.Tx, entry *LogEntry) error {
	// Insert main log entry (removed fields column)
	result, err := tx.Stmt(w.insertStmt).Exec(
		entry.Timestamp,
		entry.Level,
		entry.Message,
		entry.Caller,
		entry.Stack,
	)
	if err != nil {
		return fmt.Errorf("failed to insert log entry: %w", err)
	}

	// Always insert fields in key-value table if there are any
	if len(entry.Fields) > 0 {
		logID, err := result.LastInsertId()
		if err != nil {
			return fmt.Errorf("failed to get last insert ID: %w", err)
		}

		for fieldName, fieldValue := range entry.Fields {
			fieldType := getFieldType(fieldValue)
			fieldValueStr := formatFieldValue(fieldValue)

			_, err := tx.Stmt(w.insertFieldsStmt).Exec(logID, fieldName, fieldValueStr, fieldType)
			if err != nil {
				return fmt.Errorf("failed to insert field %s: %w", fieldName, err)
			}
		}
	}

	return nil
}

// getFieldType determines the type of a field value
func getFieldType(value interface{}) string {
	switch value.(type) {
	case string:
		return "string"
	case int, int8, int16, int32, int64, uint, uint8, uint16, uint32, uint64, float32, float64:
		return "number"
	case bool:
		return "boolean"
	default:
		return "object"
	}
}

// formatFieldValue converts a field value to its string representation
func formatFieldValue(value interface{}) string {
	switch v := value.(type) {
	case string:
		return v
	case int, int8, int16, int32, int64, uint, uint8, uint16, uint32, uint64, float32, float64, bool:
		return fmt.Sprintf("%v", v)
	default:
		// For complex objects, serialize to JSON
		if jsonBytes, err := json.Marshal(v); err == nil {
			return string(jsonBytes)
		}
		return fmt.Sprintf("%v", v)
	}
}

// Flush forces any buffered entries to be written
func (w *SQLiteWriter) Flush() error {
	w.batchMutex.Lock()
	defer w.batchMutex.Unlock()
	return w.flushBatch()
}

// Close closes the writer and flushes any remaining entries
func (w *SQLiteWriter) Close() error {
	// Flush any remaining entries
	if err := w.Flush(); err != nil {
		return fmt.Errorf("failed to flush on close: %w", err)
	}

	// Close prepared statements
	if w.insertStmt != nil {
		w.insertStmt.Close()
	}
	if w.insertFieldsStmt != nil {
		w.insertFieldsStmt.Close()
	}

	return nil
}

