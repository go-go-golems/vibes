package watermill

import (
	"fmt"
	"os"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/rs/zerolog"
)

// LogLevel represents different log levels
type LogLevel string

const (
	LogLevelTrace LogLevel = "trace"
	LogLevelDebug LogLevel = "debug"
	LogLevelInfo  LogLevel = "info"
	LogLevelWarn  LogLevel = "warn"
	LogLevelError LogLevel = "error"
	LogLevelFatal LogLevel = "fatal"
)

// LogConfig holds logging configuration
type LogConfig struct {
	Level      LogLevel `json:"level"`
	Pretty     bool     `json:"pretty"`
	TimeFormat string   `json:"time_format"`
	Output     string   `json:"output"` // "stdout", "stderr", or file path
}

// DefaultLogConfig returns default logging configuration
func DefaultLogConfig() LogConfig {
	return LogConfig{
		Level:      LogLevelInfo,
		Pretty:     true,
		TimeFormat: time.RFC3339,
		Output:     "stdout",
	}
}

// WatermillZerologAdapter adapts zerolog to Watermill's logger interface
type WatermillZerologAdapter struct {
	logger zerolog.Logger
}

// NewWatermillZerologAdapter creates a new adapter
func NewWatermillZerologAdapter(logger zerolog.Logger) *WatermillZerologAdapter {
	return &WatermillZerologAdapter{logger: logger}
}

// Error logs an error message
func (w *WatermillZerologAdapter) Error(msg string, err error, fields watermill.LogFields) {
	event := w.logger.Error().Err(err)
	for key, value := range fields {
		event = event.Interface(key, value)
	}
	event.Msg(msg)
}

// Info logs an info message
func (w *WatermillZerologAdapter) Info(msg string, fields watermill.LogFields) {
	event := w.logger.Info()
	for key, value := range fields {
		event = event.Interface(key, value)
	}
	event.Msg(msg)
}

// Debug logs a debug message
func (w *WatermillZerologAdapter) Debug(msg string, fields watermill.LogFields) {
	event := w.logger.Debug()
	for key, value := range fields {
		event = event.Interface(key, value)
	}
	event.Msg(msg)
}

// Trace logs a trace message
func (w *WatermillZerologAdapter) Trace(msg string, fields watermill.LogFields) {
	event := w.logger.Trace()
	for key, value := range fields {
		event = event.Interface(key, value)
	}
	event.Msg(msg)
}

// With returns a logger with additional fields
func (w *WatermillZerologAdapter) With(fields watermill.LogFields) watermill.LoggerAdapter {
	ctx := w.logger.With()
	for key, value := range fields {
		ctx = ctx.Interface(key, value)
	}
	return &WatermillZerologAdapter{logger: ctx.Logger()}
}

// ConfigureLogger configures zerolog with the given configuration
func ConfigureLogger(config LogConfig) (zerolog.Logger, error) {
	// Set global log level
	switch config.Level {
	case LogLevelTrace:
		zerolog.SetGlobalLevel(zerolog.TraceLevel)
	case LogLevelDebug:
		zerolog.SetGlobalLevel(zerolog.DebugLevel)
	case LogLevelInfo:
		zerolog.SetGlobalLevel(zerolog.InfoLevel)
	case LogLevelWarn:
		zerolog.SetGlobalLevel(zerolog.WarnLevel)
	case LogLevelError:
		zerolog.SetGlobalLevel(zerolog.ErrorLevel)
	case LogLevelFatal:
		zerolog.SetGlobalLevel(zerolog.FatalLevel)
	default:
		return zerolog.Logger{}, fmt.Errorf("invalid log level: %s", config.Level)
	}
	
	// Set time format
	zerolog.TimeFieldFormat = config.TimeFormat
	
	// Configure output
	var output *os.File
	switch config.Output {
	case "stdout":
		output = os.Stdout
	case "stderr":
		output = os.Stderr
	default:
		// Assume it's a file path
		file, err := os.OpenFile(config.Output, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
		if err != nil {
			return zerolog.Logger{}, fmt.Errorf("failed to open log file %s: %w", config.Output, err)
		}
		output = file
	}
	
	// Create logger
	var logger zerolog.Logger
	if config.Pretty {
		logger = zerolog.New(zerolog.ConsoleWriter{
			Out:        output,
			TimeFormat: config.TimeFormat,
		}).With().Timestamp().Logger()
	} else {
		logger = zerolog.New(output).With().Timestamp().Logger()
	}
	
	return logger, nil
}

// LogEvent represents a structured log event for analysis
type LogEvent struct {
	Timestamp   time.Time              `json:"timestamp"`
	Level       string                 `json:"level"`
	Component   string                 `json:"component"`
	Message     string                 `json:"message"`
	Fields      map[string]interface{} `json:"fields,omitempty"`
	Error       string                 `json:"error,omitempty"`
	MessageUUID string                 `json:"message_uuid,omitempty"`
	Topic       string                 `json:"topic,omitempty"`
	HandlerID   string                 `json:"handler_id,omitempty"`
	PubSubID    string                 `json:"pubsub_id,omitempty"`
}

// LogAnalyzer analyzes log events for testing and validation
type LogAnalyzer struct {
	events []LogEvent
	logger zerolog.Logger
}

// NewLogAnalyzer creates a new log analyzer
func NewLogAnalyzer(logger zerolog.Logger) *LogAnalyzer {
	return &LogAnalyzer{
		events: make([]LogEvent, 0),
		logger: logger,
	}
}

// AddEvent adds a log event for analysis
func (la *LogAnalyzer) AddEvent(event LogEvent) {
	la.events = append(la.events, event)
}

// GetEvents returns all collected events
func (la *LogAnalyzer) GetEvents() []LogEvent {
	return la.events
}

// GetEventsByLevel returns events filtered by level
func (la *LogAnalyzer) GetEventsByLevel(level string) []LogEvent {
	var filtered []LogEvent
	for _, event := range la.events {
		if event.Level == level {
			filtered = append(filtered, event)
		}
	}
	return filtered
}

// GetEventsByComponent returns events filtered by component
func (la *LogAnalyzer) GetEventsByComponent(component string) []LogEvent {
	var filtered []LogEvent
	for _, event := range la.events {
		if event.Component == component {
			filtered = append(filtered, event)
		}
	}
	return filtered
}

// GetEventsByTopic returns events filtered by topic
func (la *LogAnalyzer) GetEventsByTopic(topic string) []LogEvent {
	var filtered []LogEvent
	for _, event := range la.events {
		if event.Topic == topic {
			filtered = append(filtered, event)
		}
	}
	return filtered
}

// GetMessageFlow returns the flow of a specific message through the system
func (la *LogAnalyzer) GetMessageFlow(messageUUID string) []LogEvent {
	var flow []LogEvent
	for _, event := range la.events {
		if event.MessageUUID == messageUUID {
			flow = append(flow, event)
		}
	}
	return flow
}

// ValidateMessageFlow validates that a message follows the expected flow
func (la *LogAnalyzer) ValidateMessageFlow(messageUUID string) (bool, []string) {
	flow := la.GetMessageFlow(messageUUID)
	if len(flow) == 0 {
		return false, []string{"No events found for message"}
	}
	
	var issues []string
	expectedStages := []string{"Publishing message", "Message published successfully", "Processing message in JS handler"}
	
	stagesSeen := make(map[string]bool)
	for _, event := range flow {
		stagesSeen[event.Message] = true
	}
	
	for _, stage := range expectedStages {
		if !stagesSeen[stage] {
			issues = append(issues, fmt.Sprintf("Missing stage: %s", stage))
		}
	}
	
	return len(issues) == 0, issues
}

// GetStatistics returns statistics about the collected events
func (la *LogAnalyzer) GetStatistics() map[string]interface{} {
	stats := make(map[string]interface{})
	
	// Count by level
	levelCounts := make(map[string]int)
	componentCounts := make(map[string]int)
	topicCounts := make(map[string]int)
	
	for _, event := range la.events {
		levelCounts[event.Level]++
		if event.Component != "" {
			componentCounts[event.Component]++
		}
		if event.Topic != "" {
			topicCounts[event.Topic]++
		}
	}
	
	stats["total_events"] = len(la.events)
	stats["by_level"] = levelCounts
	stats["by_component"] = componentCounts
	stats["by_topic"] = topicCounts
	
	if len(la.events) > 0 {
		stats["first_event"] = la.events[0].Timestamp
		stats["last_event"] = la.events[len(la.events)-1].Timestamp
		stats["duration"] = la.events[len(la.events)-1].Timestamp.Sub(la.events[0].Timestamp)
	}
	
	return stats
}

// Clear clears all collected events
func (la *LogAnalyzer) Clear() {
	la.events = make([]LogEvent, 0)
}

// LoggingMiddleware creates a middleware that logs message processing
func LoggingMiddleware(logger zerolog.Logger) func(next func(*message.Message) error) func(*message.Message) error {
	return func(next func(*message.Message) error) func(*message.Message) error {
		return func(msg *message.Message) error {
			start := time.Now()
			
			logger.Debug().
				Str("message_uuid", msg.UUID).
				Str("topic", msg.Metadata["topic"]).
				Msg("Starting message processing")
			
			err := next(msg)
			
			duration := time.Since(start)
			
			if err != nil {
				logger.Error().
					Err(err).
					Str("message_uuid", msg.UUID).
					Dur("duration", duration).
					Msg("Message processing failed")
			} else {
				logger.Debug().
					Str("message_uuid", msg.UUID).
					Dur("duration", duration).
					Msg("Message processing completed")
			}
			
			return err
		}
	}
}

