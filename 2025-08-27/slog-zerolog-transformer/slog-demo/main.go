package main

import (
	"context"
	"log/slog"
	"os"
	"time"
)

func main() {
	// Create a text handler that writes to stdout
	handler := slog.NewTextHandler(os.Stdout, &slog.HandlerOptions{
		Level: slog.LevelDebug,
	})
	logger := slog.New(handler)

	// Set as default logger
	slog.SetDefault(logger)

	// Basic logging examples
	slog.Info("Application starting", "version", "1.0.0", "build_time", time.Now().Format(time.RFC3339))
	
	// Different log levels
	slog.Debug("Debug message for troubleshooting", "component", "main", "debug_info", "startup_sequence")
	slog.Info("Information message", "user_id", 12345, "action", "login")
	slog.Warn("Warning message", "memory_usage", "85%", "threshold", "80%")
	slog.Error("Error occurred", "error", "connection timeout", "retry_count", 3)

	// Structured logging with various data types
	slog.Info("User activity",
		"user_id", 67890,
		"username", "john_doe",
		"email", "john@example.com",
		"last_login", time.Now().Add(-24*time.Hour),
		"is_premium", true,
		"login_count", 42,
	)

	// Logging with context
	ctx := context.Background()
	ctx = context.WithValue(ctx, "request_id", "req-123456")
	ctx = context.WithValue(ctx, "session_id", "sess-abcdef")

	slog.InfoContext(ctx, "Processing request",
		"endpoint", "/api/users",
		"method", "GET",
		"duration_ms", 150,
	)

	// Group logging
	slog.Info("Database operation",
		slog.Group("database",
			"host", "localhost",
			"port", 5432,
			"name", "myapp",
		),
		slog.Group("query",
			"table", "users",
			"operation", "SELECT",
			"rows_affected", 25,
		),
	)

	// Logging with attributes
	attrs := []slog.Attr{
		slog.String("service", "user-service"),
		slog.Int("port", 8080),
		slog.Bool("tls_enabled", true),
	}
	
	logger.LogAttrs(context.Background(), slog.LevelInfo, "Service configuration", attrs...)

	// Performance logging
	start := time.Now()
	time.Sleep(100 * time.Millisecond) // Simulate work
	duration := time.Since(start)

	slog.Info("Operation completed",
		"operation", "data_processing",
		"duration", duration,
		"records_processed", 1000,
		"success_rate", 0.98,
	)

	// Error handling example
	err := simulateError()
	if err != nil {
		slog.Error("Failed to process data",
			"error", err.Error(),
			"component", "data_processor",
			"retry_after", "30s",
		)
	}

	// Final log message
	slog.Info("Application shutting down", "uptime", time.Since(time.Now().Add(-5*time.Minute)))
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

