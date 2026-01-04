// Package tracing defines interfaces and implementations for tracing agent execution
package tracing

import (
	"context"
	"time"

	"github.com/goagent/framework/goagent/types"
	"github.com/google/uuid"
)

// Tracer interface defines the functionality of a tracing system
type Tracer interface {
	// StartSpan starts a new span
	StartSpan(ctx context.Context, name string) (context.Context, types.Span)
	
	// LogEvent logs an event
	LogEvent(ctx context.Context, event types.Event)
	
	// GetEvents returns all events
	GetEvents() []interface{}
}

// SimpleTracer implements the Tracer interface
type SimpleTracer struct {
	events []interface{}
	spans  []types.Span
}

// NewSimpleTracer creates a new SimpleTracer
func NewSimpleTracer() *SimpleTracer {
	return &SimpleTracer{
		events: make([]interface{}, 0),
		spans:  make([]types.Span, 0),
	}
}

// StartSpan starts a new span
func (t *SimpleTracer) StartSpan(ctx context.Context, name string) (context.Context, types.Span) {
	span := &SimpleSpan{
		ID:        uuid.New().String(),
		Name:      name,
		StartTime: time.Now().UnixNano(),
		tracer:    t,
	}
	
	t.spans = append(t.spans, span)
	t.events = append(t.events, map[string]interface{}{
		"type":      "span_start",
		"span_id":   span.ID,
		"name":      name,
		"timestamp": span.StartTime,
	})
	
	return ctx, span
}

// LogEvent logs an event
func (t *SimpleTracer) LogEvent(ctx context.Context, event types.Event) {
	if event.Timestamp == 0 {
		event.Timestamp = time.Now().UnixNano()
	}
	
	t.events = append(t.events, event)
}

// GetEvents returns all events
func (t *SimpleTracer) GetEvents() []interface{} {
	return t.events
}

// SimpleSpan implements the Span interface
type SimpleSpan struct {
	ID        string
	Name      string
	StartTime int64
	EndTime   int64
	tracer    *SimpleTracer
}

// End ends the span
func (s *SimpleSpan) End() {
	s.EndTime = time.Now().UnixNano()
	
	s.tracer.events = append(s.tracer.events, map[string]interface{}{
		"type":      "span_end",
		"span_id":   s.ID,
		"name":      s.Name,
		"timestamp": s.EndTime,
		"duration":  s.EndTime - s.StartTime,
	})
}
