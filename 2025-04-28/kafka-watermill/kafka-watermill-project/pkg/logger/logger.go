package logger

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"runtime"
	"strings"
	"time"

	"github.com/google/uuid"
)

// LogLevel defines the severity of a log message
type LogLevel string

const (
	// Log levels
	DebugLevel LogLevel = "DEBUG"
	InfoLevel  LogLevel = "INFO"
	WarnLevel  LogLevel = "WARN"
	ErrorLevel LogLevel = "ERROR"
	FatalLevel LogLevel = "FATAL"
)

// StructuredLogger provides structured logging capabilities
type StructuredLogger struct {
	serviceName string
	output      io.Writer
	minLevel    LogLevel
	fields      map[string]interface{}
}

// LogEntry represents a structured log entry
type LogEntry struct {
	Timestamp   string                 `json:"timestamp"`
	Level       string                 `json:"level"`
	Message     string                 `json:"message"`
	ServiceName string                 `json:"service_name"`
	CorrelationID string               `json:"correlation_id,omitempty"`
	TraceID     string                 `json:"trace_id,omitempty"`
	SpanID      string                 `json:"span_id,omitempty"`
	File        string                 `json:"file,omitempty"`
	Line        int                    `json:"line,omitempty"`
	Fields      map[string]interface{} `json:"fields,omitempty"`
}

// NewStructuredLogger creates a new structured logger
func NewStructuredLogger(serviceName string) *StructuredLogger {
	// Default is to write to stdout
	return &StructuredLogger{
		serviceName: serviceName,
		output:      os.Stdout,
		minLevel:    InfoLevel,
		fields:      make(map[string]interface{}),
	}
}

// SetOutput sets the output writer for the logger
func (l *StructuredLogger) SetOutput(w io.Writer) *StructuredLogger {
	l.output = w
	return l
}

// SetMinLevel sets the minimum log level to output
func (l *StructuredLogger) SetMinLevel(level LogLevel) *StructuredLogger {
	l.minLevel = level
	return l
}

// With returns a logger with the additional fields
func (l *StructuredLogger) With(fields map[string]interface{}) *StructuredLogger {
	newLogger := &StructuredLogger{
		serviceName: l.serviceName,
		output:      l.output,
		minLevel:    l.minLevel,
		fields:      make(map[string]interface{}),
	}

	// Copy existing fields
	for k, v := range l.fields {
		newLogger.fields[k] = v
	}

	// Add new fields
	for k, v := range fields {
		newLogger.fields[k] = v
	}

	return newLogger
}

// WithField returns a logger with an additional field
func (l *StructuredLogger) WithField(key string, value interface{}) *StructuredLogger {
	return l.With(map[string]interface{}{key: value})
}

// WithCorrelationID returns a logger with a correlation ID
func (l *StructuredLogger) WithCorrelationID(correlationID string) *StructuredLogger {
	if correlationID == "" {
		correlationID = uuid.New().String()
	}
	return l.WithField("correlation_id", correlationID)
}

// WithRequestID returns a logger with a request ID
func (l *StructuredLogger) WithRequestID(requestID string) *StructuredLogger {
	return l.WithField("request_id", requestID)
}

// WithOrderID returns a logger with an order ID
func (l *StructuredLogger) WithOrderID(orderID string) *StructuredLogger {
	return l.WithField("order_id", orderID)
}

// shouldLog returns true if the message should be logged based on level
func (l *StructuredLogger) shouldLog(level LogLevel) bool {
	// Compare log levels based on string comparison
	// DEBUG < INFO < WARN < ERROR < FATAL
	if level == DebugLevel && l.minLevel != DebugLevel {
		return false
	}
	if level == InfoLevel && (l.minLevel == WarnLevel || l.minLevel == ErrorLevel || l.minLevel == FatalLevel) {
		return false
	}
	if level == WarnLevel && (l.minLevel == ErrorLevel || l.minLevel == FatalLevel) {
		return false
	}
	if level == ErrorLevel && l.minLevel == FatalLevel {
		return false
	}
	return true
}

// log formats and writes a log entry
func (l *StructuredLogger) log(level LogLevel, msg string, fields map[string]interface{}) {
	if !l.shouldLog(level) {
		return
	}

	// Get caller information
	_, file, line, ok := runtime.Caller(2)
	if !ok {
		file = "unknown"
		line = 0
	} else {
		// Extract just the file name, not the full path
		parts := strings.Split(file, "/")
		file = parts[len(parts)-1]
	}

	// Create log entry
	entry := LogEntry{
		Timestamp:   time.Now().UTC().Format(time.RFC3339Nano),
		Level:       string(level),
		Message:     msg,
		ServiceName: l.serviceName,
		File:        file,
		Line:        line,
		Fields:      make(map[string]interface{}),
	}

	// Copy standard fields
	for k, v := range l.fields {
		if k == "correlation_id" {
			entry.CorrelationID = fmt.Sprintf("%v", v)
		} else if k == "trace_id" {
			entry.TraceID = fmt.Sprintf("%v", v)
		} else if k == "span_id" {
			entry.SpanID = fmt.Sprintf("%v", v)
		} else {
			entry.Fields[k] = v
		}
	}

	// Add additional fields
	for k, v := range fields {
		entry.Fields[k] = v
	}

	// Marshal to JSON
	jsonData, err := json.Marshal(entry)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error marshaling log entry: %v", err)
		return
	}

	// Write to output
	fmt.Fprintln(l.output, string(jsonData))

	// Exit if fatal
	if level == FatalLevel {
		os.Exit(1)
	}
}

// Debug logs a message at debug level
func (l *StructuredLogger) Debug(msg string, fields ...map[string]interface{}) {
	fieldMap := make(map[string]interface{})
	if len(fields) > 0 {
		fieldMap = fields[0]
	}
	l.log(DebugLevel, msg, fieldMap)
}

// Info logs a message at info level
func (l *StructuredLogger) Info(msg string, fields ...map[string]interface{}) {
	fieldMap := make(map[string]interface{})
	if len(fields) > 0 {
		fieldMap = fields[0]
	}
	l.log(InfoLevel, msg, fieldMap)
}

// Warn logs a message at warn level
func (l *StructuredLogger) Warn(msg string, fields ...map[string]interface{}) {
	fieldMap := make(map[string]interface{})
	if len(fields) > 0 {
		fieldMap = fields[0]
	}
	l.log(WarnLevel, msg, fieldMap)
}

// Error logs a message at error level
func (l *StructuredLogger) Error(msg string, fields ...map[string]interface{}) {
	fieldMap := make(map[string]interface{})
	if len(fields) > 0 {
		fieldMap = fields[0]
	}
	l.log(ErrorLevel, msg, fieldMap)
}

// Fatal logs a message at fatal level and exits the program
func (l *StructuredLogger) Fatal(msg string, fields ...map[string]interface{}) {
	fieldMap := make(map[string]interface{})
	if len(fields) > 0 {
		fieldMap = fields[0]
	}
	l.log(FatalLevel, msg, fieldMap)
	// log will call os.Exit(1)
}