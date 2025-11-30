package logging

import (
	"os"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

// InitLogger initializes the global logger with default settings
func InitLogger() {
	zerolog.TimeFieldFormat = zerolog.TimeFormatUnix
	log.Logger = zerolog.New(os.Stderr).With().Timestamp().Logger()
}

// InitLoggerWithLevel initializes the logger with a specific log level
func InitLoggerWithLevel(level string) {
	InitLogger()
	
	var logLevel zerolog.Level
	switch level {
	case "debug":
		logLevel = zerolog.DebugLevel
	case "info":
		logLevel = zerolog.InfoLevel
	case "warn":
		logLevel = zerolog.WarnLevel
	case "error":
		logLevel = zerolog.ErrorLevel
	default:
		logLevel = zerolog.InfoLevel
	}
	
	zerolog.SetGlobalLevel(logLevel)
}

// GetLogger returns the global logger instance
func GetLogger() zerolog.Logger {
	return log.Logger
}

