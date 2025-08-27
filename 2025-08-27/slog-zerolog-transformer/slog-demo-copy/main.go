package main

import (
	"context"
	"os"
	"time"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

func main() {
	// Create a console writer that writes to stdout
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stdout})

	// Basic logging examples
	log.Info().
		Str("version", "1.0.0").
		Str("build_time", time.Now().Format(time.RFC3339)).
		Msg("Application starting")
	
	// Different log levels
	log.Debug().
		Str("component", "main").
		Str("debug_info", "startup_sequence").
		Msg("Debug message for troubleshooting")
	log.Info().
		Int("user_id", 12345).
		Str("action", "login").
		Msg("Information message")
	log.Warn().
		Str("memory_usage", "85%").
		Str("threshold", "80%").
		Msg("Warning message")
	log.Error().
		Str("error", "connection timeout").
		Int("retry_count", 3).
		Msg("Error occurred")

	// Structured logging with various data types
	log.Info().
		Int("user_id", 67890).
		Str("username", "john_doe").
		Str("email", "john@example.com").
		Time("last_login", time.Now().Add(-24*time.Hour)).
		Bool("is_premium", true).
		Int("login_count", 42).
		Msg("User activity")

	// Logging with context (zerolog doesn't use context the same way)
	ctx := context.Background()
	ctx = context.WithValue(ctx, "request_id", "req-123456")
	ctx = context.WithValue(ctx, "session_id", "sess-abcdef")

	log.Info().
		Str("endpoint", "/api/users").
		Str("method", "GET").
		Int("duration_ms", 150).
		Msg("Processing request")

	// Group logging (zerolog uses nested objects)
	log.Info().
		Str("database.host", "localhost").
		Int("database.port", 5432).
		Str("database.name", "myapp").
		Str("query.table", "users").
		Str("query.operation", "SELECT").
		Int("query.rows_affected", 25).
		Msg("Database operation")

	// Logging with attributes (simplified for zerolog)
	log.Info().
		Str("service", "user-service").
		Int("port", 8080).
		Bool("tls_enabled", true).
		Msg("Service configuration")

	// Performance logging
	start := time.Now()
	time.Sleep(100 * time.Millisecond) // Simulate work
	duration := time.Since(start)

	log.Info().
		Str("operation", "data_processing").
		Dur("duration", duration).
		Int("records_processed", 1000).
		Float64("success_rate", 0.98).
		Msg("Operation completed")

	// Error handling example
	err := simulateError()
	if err != nil {
		log.Error().
			Err(err).
			Str("component", "data_processor").
			Str("retry_after", "30s").
			Msg("Failed to process data")
	}

	// Final log message
	log.Info().
		Dur("uptime", time.Since(time.Now().Add(-5*time.Minute))).
		Msg("Application shutting down")
}

func simulateError() error {
	return &CustomError{
		Code:    "DATA_001",
		Message: "Invalid data format",
		Details: map[string]interface{}{
			"expected": "JSON",
			"received": "XML",
			"line":     42,
		},
	}
}

type CustomError struct {
	Code    string
	Message string
	Details map[string]interface{}
}

func (e *CustomError) Error() string {
	return e.Message
}

