package progress

import (
	"encoding/json"
	"time"
)

// Event represents a progress event in the pelican genome sequencing pipeline
type Event struct {
	JobID       string    `json:"job_id"`
	Stage       string    `json:"stage"`   // "fetch", "analyze", "done", "error"
	Fetched     int       `json:"fetched"`
	Indexed     int       `json:"indexed"`
	RateLimited bool      `json:"rate_limited"`
	Err         string    `json:"err,omitempty"`
	Ts          time.Time `json:"ts"`
}

// Sink defines the interface for sending progress events
type Sink interface {
	Send(event Event) error
}

// Source defines the interface for receiving progress events
type Source interface {
	Subscribe(jobID string) (<-chan Event, error)
	Close() error
}

// ToJSON converts an event to JSON bytes
func (e Event) ToJSON() ([]byte, error) {
	return json.Marshal(e)
}

// FromJSON creates an event from JSON bytes
func FromJSON(data []byte) (Event, error) {
	var event Event
	err := json.Unmarshal(data, &event)
	return event, err
}

// TopicName returns the Watermill topic name for a job
func TopicName(jobID string) string {
	return "jobs.progress" // Single topic for all jobs
}

