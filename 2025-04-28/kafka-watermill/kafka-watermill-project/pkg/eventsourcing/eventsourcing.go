1	package eventsourcing
2	
3	import (
4		"fmt"
5		"sync"
6		"time"
7	
8		"github.com/google/uuid"
9	)
10	
11	// Event is the base interface for all events
12	type Event interface {
13		GetAggregateID() string
14		GetEventType() string
15		GetTimestamp() time.Time
16	}
17	
18	// Aggregate is the base interface for event-sourced aggregates
19	type Aggregate interface {
20		GetID() string
21		ApplyEvent(event Event) error
22		GetUncommittedEvents() []Event
23		ClearUncommittedEvents()
24	}
25	
26	// BaseEvent provides common functionality for all events
27	type BaseEvent struct {
28		AggregateID string    `json:"aggregate_id"`
29		EventType   string    `json:"event_type"`
30		Timestamp   time.Time `json:"timestamp"`