package util

import (
	"github.com/sirupsen/logrus"
)

var logger *logrus.Logger

// SetLogger sets the global logger instance
func SetLogger(l *logrus.Logger) {
	logger = l
}

// GetLogger returns the global logger instance
func GetLogger() *logrus.Logger {
	if logger == nil {
		logger = logrus.New()
	}
	return logger
}

