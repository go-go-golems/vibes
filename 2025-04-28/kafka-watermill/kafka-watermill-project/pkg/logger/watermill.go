package logger

import (
	"github.com/ThreeDotsLabs/watermill"
)

type WatermillAdapter struct {
	logger *StructuredLogger
}

// NewWatermillAdapter creates a new Watermill logger adapter
func NewWatermillAdapter(logger *StructuredLogger) watermill.LoggerAdapter {
	return &WatermillAdapter{
		logger: logger,
	}
}

func (w *WatermillAdapter) Error(msg string, err error, fields watermill.LogFields) {
	logFields := make(map[string]interface{}, len(fields)+1)
	for k, v := range fields {
		logFields[k] = v
	}
	if err != nil {
		logFields["error"] = err.Error()
	}
	w.logger.Error(msg, logFields)
}

func (w *WatermillAdapter) Info(msg string, fields watermill.LogFields) {
	logFields := make(map[string]interface{}, len(fields))
	for k, v := range fields {
		logFields[k] = v
	}
	w.logger.Info(msg, logFields)
}

func (w *WatermillAdapter) Debug(msg string, fields watermill.LogFields) {
	logFields := make(map[string]interface{}, len(fields))
	for k, v := range fields {
		logFields[k] = v
	}
	w.logger.Debug(msg, logFields)
}

func (w *WatermillAdapter) Trace(msg string, fields watermill.LogFields) {
	// Since we don't have a trace level, we'll use debug
	w.Debug(msg, fields)
}

func (w *WatermillAdapter) With(fields watermill.LogFields) watermill.LoggerAdapter {
	logFields := make(map[string]interface{}, len(fields))
	for k, v := range fields {
		logFields[k] = v
	}
	return &WatermillAdapter{
		logger: w.logger.With(logFields),
	}
}
