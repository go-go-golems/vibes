package logger

import (
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/fatih/color"
	"github.com/sirupsen/logrus"

	"github.com/user/youtube-analyzer-go/internal/config"
)

// Logger wraps logrus with additional functionality
type Logger struct {
	*logrus.Logger
	config    *config.Config
	sessionID string
	logFile   *os.File
}

// New creates a new logger instance
func New(cfg *config.Config, sessionID string) *Logger {
	log := logrus.New()

	// Set log level
	level, err := logrus.ParseLevel(cfg.LogLevel)
	if err != nil {
		level = logrus.InfoLevel
	}
	log.SetLevel(level)

	// Create log file
	logDir := filepath.Join(cfg.OutputDir, "logs")
	if err := os.MkdirAll(logDir, 0755); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to create log directory: %v\n", err)
	}

	logFile, err := os.OpenFile(
		filepath.Join(logDir, fmt.Sprintf("analysis_%s.log", sessionID)),
		os.O_CREATE|os.O_WRONLY|os.O_APPEND,
		0666,
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to create log file: %v\n", err)
		logFile = nil
	}

	// Configure formatter
	if cfg.NoColor {
		log.SetFormatter(&logrus.TextFormatter{
			DisableColors:   true,
			FullTimestamp:   true,
			TimestampFormat: "2006-01-02 15:04:05",
		})
	} else {
		log.SetFormatter(&ColoredFormatter{})
	}

	// Set output
	if !cfg.Quiet {
		if logFile != nil {
			log.SetOutput(logFile)
		} else {
			log.SetOutput(os.Stderr)
		}
	} else {
		log.SetOutput(logFile)
	}

	return &Logger{
		Logger:    log,
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
	fields := logrus.Fields{
		"step_num":  stepNum,
		"step_name": stepName,
		"step_type": stepType,
		"session":   l.sessionID,
	}

	for k, v := range details {
		fields[k] = v
	}

	entry := l.WithFields(fields)

	switch stepType {
	case "error":
		entry.Error(fmt.Sprintf("Step %d: %s", stepNum, stepName))
	case "success":
		entry.Info(fmt.Sprintf("✅ Step %d: %s", stepNum, stepName))
	case "processing":
		entry.Info(fmt.Sprintf("⚙️ Step %d: %s", stepNum, stepName))
	default:
		entry.Info(fmt.Sprintf("📝 Step %d: %s", stepNum, stepName))
	}
}

// APICall logs API call details
func (l *Logger) APICall(callNum int, model, operation string, duration time.Duration, success bool) {
	fields := logrus.Fields{
		"api_call_num": callNum,
		"model":        model,
		"operation":    operation,
		"duration_ms":  duration.Milliseconds(),
		"success":      success,
		"session":      l.sessionID,
	}

	entry := l.WithFields(fields)

	if success {
		entry.Info(fmt.Sprintf("🔗 API Call %d: %s (%s) - %.2fs", callNum, operation, model, duration.Seconds()))
	} else {
		entry.Error(fmt.Sprintf("❌ API Call %d: %s (%s) - Failed after %.2fs", callNum, operation, model, duration.Seconds()))
	}
}

// Progress logs progress updates
func (l *Logger) Progress(current, total int, message string) {
	percentage := float64(current) / float64(total) * 100
	l.WithFields(logrus.Fields{
		"current":    current,
		"total":      total,
		"percentage": percentage,
		"session":    l.sessionID,
	}).Info(fmt.Sprintf("📊 Progress: %.1f%% - %s", percentage, message))
}

// ColoredFormatter provides colored log output
type ColoredFormatter struct{}

// Format implements the logrus.Formatter interface
func (f *ColoredFormatter) Format(entry *logrus.Entry) ([]byte, error) {
	timestamp := entry.Time.Format("15:04:05")

	var levelColor *color.Color
	var levelText string

	switch entry.Level {
	case logrus.DebugLevel:
		levelColor = color.New(color.FgMagenta)
		levelText = "DEBUG"
	case logrus.InfoLevel:
		levelColor = color.New(color.FgBlue)
		levelText = "INFO "
	case logrus.WarnLevel:
		levelColor = color.New(color.FgYellow)
		levelText = "WARN "
	case logrus.ErrorLevel:
		levelColor = color.New(color.FgRed)
		levelText = "ERROR"
	default:
		levelColor = color.New(color.FgWhite)
		levelText = "UNKNOWN"
	}

	// Format: [15:04:05] LEVEL | message
	formatted := fmt.Sprintf("[%s] %s | %s\n",
		color.New(color.FgCyan).Sprint(timestamp),
		levelColor.Sprint(levelText),
		entry.Message,
	)

	return []byte(formatted), nil
}
