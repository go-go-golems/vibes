package logger

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"time"

	"github.com/rs/zerolog"
	"github.com/spf13/viper"

	"github.com/user/youtube-analyzer-go/internal/config"
)

// Logger wraps zerolog with additional functionality
type Logger struct {
	zerolog.Logger
	config    *config.Config
	sessionID string
	logFile   *os.File
}

// New creates a new logger instance
func New(cfg *config.Config, sessionID string) *Logger {
	// Set up zerolog
	zerolog.TimeFieldFormat = time.RFC3339

	// Determine log level
	var level zerolog.Level
	logLevelStr := viper.GetString("log-level")
	if logLevelStr == "" {
		logLevelStr = cfg.LogLevel
	}

	switch logLevelStr {
	case "debug":
		level = zerolog.DebugLevel
	case "info":
		level = zerolog.InfoLevel
	case "warn":
		level = zerolog.WarnLevel
	case "error":
		level = zerolog.ErrorLevel
	default:
		level = zerolog.InfoLevel
	}

	// Override with debug flag if set
	if viper.GetBool("log-debug") {
		level = zerolog.DebugLevel
	}

	// Determine log file path
	logFilePath := viper.GetString("log-file")
	if logFilePath == "" {
		logDir := filepath.Join(cfg.OutputDir, "logs")
		if err := os.MkdirAll(logDir, 0755); err != nil {
			fmt.Fprintf(os.Stderr, "Failed to create log directory: %v\n", err)
		}
		logFilePath = filepath.Join(logDir, fmt.Sprintf("analysis_%s.log", sessionID))
	} else {
		// Ensure directory exists for custom log file path
		logDir := filepath.Dir(logFilePath)
		if err := os.MkdirAll(logDir, 0755); err != nil {
			fmt.Fprintf(os.Stderr, "Failed to create log directory %s: %v\n", logDir, err)
		}
	}

	// Create log file
	var logFile *os.File
	var writers []io.Writer

	// Always write to stderr for console output
	if !cfg.Quiet {
		consoleWriter := zerolog.ConsoleWriter{
			Out:        os.Stderr,
			TimeFormat: "15:04:05",
			NoColor:    cfg.NoColor,
		}
		writers = append(writers, consoleWriter)
	}

	// Add file writer if log file specified
	var err error
	logFile, err = os.OpenFile(
		logFilePath,
		os.O_CREATE|os.O_WRONLY|os.O_APPEND,
		0666,
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to create log file %s: %v\n", logFilePath, err)
		logFile = nil
	} else {
		fmt.Fprintf(os.Stderr, "📝 Logging to: %s\n", logFilePath)
		writers = append(writers, logFile)
	}

	// Create multi-writer
	var writer io.Writer
	if len(writers) == 1 {
		writer = writers[0]
	} else if len(writers) > 1 {
		writer = io.MultiWriter(writers...)
	} else {
		writer = os.Stderr
	}

	// Create logger with caller information
	logger := zerolog.New(writer).
		Level(level).
		With().
		Timestamp().
		Caller().
		Str("session", sessionID).
		Logger()

	return &Logger{
		Logger:    logger,
		config:    cfg,
		sessionID: sessionID,
		logFile:   logFile,
	}
}

// Close closes the log file
func (l *Logger) Close() error {
	if l.logFile != nil {
		return l.logFile.Close()
	}
	return nil
}

// Step logs a step with special formatting
func (l *Logger) Step(stepNum int, stepName, stepType string, details map[string]interface{}) {
	event := l.Info().
		Int("step_num", stepNum).
		Str("step_name", stepName).
		Str("step_type", stepType).
		Str("session", l.sessionID)

	for k, v := range details {
		event = event.Interface(k, v)
	}

	switch stepType {
	case "error":
		l.Error().Fields(details).Msgf("Step %d: %s", stepNum, stepName)
	case "success":
		event.Msgf("✅ Step %d: %s", stepNum, stepName)
	case "processing":
		event.Msgf("⚙️ Step %d: %s", stepNum, stepName)
	default:
		event.Msgf("📝 Step %d: %s", stepNum, stepName)
	}
}

// APICall logs API call details
func (l *Logger) APICall(callNum int, model, operation string, duration time.Duration, success bool) {
	event := l.Info().
		Int("api_call_num", callNum).
		Str("model", model).
		Str("operation", operation).
		Int64("duration_ms", duration.Milliseconds()).
		Bool("success", success).
		Str("session", l.sessionID)

	if success {
		event.Msgf("🌐 API Call %d: %s (%s) completed in %v", callNum, operation, model, duration)
	} else {
		l.Error().
			Int("api_call_num", callNum).
			Str("model", model).
			Str("operation", operation).
			Int64("duration_ms", duration.Milliseconds()).
			Str("session", l.sessionID).
			Msgf("🚨 API Call %d: %s (%s) failed after %v", callNum, operation, model, duration)
	}
}

// Progress logs progress information
func (l *Logger) Progress(current, total int, operation string) {
	percentage := float64(current) / float64(total) * 100
	l.Info().
		Int("current", current).
		Int("total", total).
		Float64("percentage", percentage).
		Str("operation", operation).
		Str("session", l.sessionID).
		Msgf("📊 Progress: %d/%d (%.1f%%) - %s", current, total, percentage, operation)
}

// TUIEvent logs TUI events for debugging
func (l *Logger) TUIEvent(screen, event string, details map[string]interface{}) {
	event_log := l.Debug().
		Str("screen", screen).
		Str("event", event).
		Str("session", l.sessionID)

	for k, v := range details {
		event_log = event_log.Interface(k, v)
	}

	event_log.Msgf("🖥️ TUI: %s -> %s", screen, event)
}

// StreamingEvent logs streaming events
func (l *Logger) StreamingEvent(eventType string, details map[string]interface{}) {
	event_log := l.Debug().
		Str("event_type", eventType).
		Str("session", l.sessionID)

	for k, v := range details {
		event_log = event_log.Interface(k, v)
	}

	event_log.Msgf("🎬 Streaming: %s", eventType)
}
