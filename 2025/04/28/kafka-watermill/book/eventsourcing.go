package eventsourcing

import (
	"fmt"
	"sync"
	"time"
)

// Event is the base interface for all events
type Event interface {
	GetAggregateID() string
	GetEventType() string
	GetTimestamp() time.Time
}

// Aggregate is the base interface for event-sourced aggregates
type Aggregate interface {
	GetID() string
	ApplyEvent(event Event) error
	GetUncommittedEvents() []Event
	ClearUncommittedEvents()
}

// BaseEvent provides common functionality for all events
type BaseEvent struct {
	AggregateID string    `json:"aggregate_id"`
	EventType   string    `json:"event_type"`
	Timestamp   time.Time `json:"timestamp"`
}

// GetAggregateID returns the aggregate ID
func (e BaseEvent) GetAggregateID() string {
	return e.AggregateID
}

// GetEventType returns the event type
func (e BaseEvent) GetEventType() string {
	return e.EventType
}

// GetTimestamp returns the event timestamp
func (e BaseEvent) GetTimestamp() time.Time {
	return e.Timestamp
}

// BaseAggregate provides common functionality for all aggregates
type BaseAggregate struct {
	ID                string
	uncommittedEvents []Event
	mutex             sync.Mutex
}

// GetID returns the aggregate ID
func (a *BaseAggregate) GetID() string {
	return a.ID
}

// GetUncommittedEvents returns all uncommitted events
func (a *BaseAggregate) GetUncommittedEvents() []Event {
	a.mutex.Lock()
	defer a.mutex.Unlock()

	return a.uncommittedEvents
}

// ClearUncommittedEvents clears all uncommitted events
func (a *BaseAggregate) ClearUncommittedEvents() {
	a.mutex.Lock()
	defer a.mutex.Unlock()

	a.uncommittedEvents = nil
}

// AddEvent adds an event to the uncommitted events list
func (a *BaseAggregate) AddEvent(event Event) {
	a.mutex.Lock()
	defer a.mutex.Unlock()

	a.uncommittedEvents = append(a.uncommittedEvents, event)
}

// EventStore is the interface for storing and retrieving events
type EventStore interface {
	SaveEvents(aggregateID string, events []Event) error
	GetEvents(aggregateID string) ([]Event, error)
}

// InMemoryEventStore is a simple in-memory implementation of EventStore
type InMemoryEventStore struct {
	events map[string][]Event
	mutex  sync.RWMutex
}

// NewInMemoryEventStore creates a new in-memory event store
func NewInMemoryEventStore() *InMemoryEventStore {
	return &InMemoryEventStore{
		events: make(map[string][]Event),
	}
}

// SaveEvents saves events for an aggregate
func (s *InMemoryEventStore) SaveEvents(aggregateID string, events []Event) error {
	s.mutex.Lock()
	defer s.mutex.Unlock()

	for _, event := range events {
		s.events[aggregateID] = append(s.events[aggregateID], event)
	}

	return nil
}

// GetEvents gets all events for an aggregate
func (s *InMemoryEventStore) GetEvents(aggregateID string) ([]Event, error) {
	s.mutex.RLock()
	defer s.mutex.RUnlock()

	if events, ok := s.events[aggregateID]; ok {
		return events, nil
	}

	return []Event{}, nil
}

// Repository is the interface for loading and saving aggregates
type Repository[T Aggregate] interface {
	Load(id string) (T, error)
	Save(aggregate T) error
}

// GenericRepository is a generic implementation of Repository
type GenericRepository[T Aggregate] struct {
	eventStore       EventStore
	aggregateFactory func(id string) T
	eventHandlerMap  map[string]func(T, Event) error
}

// NewGenericRepository creates a new generic repository
func NewGenericRepository[T Aggregate](
	eventStore EventStore,
	aggregateFactory func(id string) T,
	eventHandlerMap map[string]func(T, Event) error,
) *GenericRepository[T] {
	return &GenericRepository[T]{
		eventStore:       eventStore,
		aggregateFactory: aggregateFactory,
		eventHandlerMap:  eventHandlerMap,
	}
}

// Load loads an aggregate from the event store
func (r *GenericRepository[T]) Load(id string) (T, error) {
	// Create a new aggregate instance
	aggregate := r.aggregateFactory(id)

	// Get all events for this aggregate
	events, err := r.eventStore.GetEvents(id)
	if err != nil {
		return aggregate, fmt.Errorf("error loading events: %w", err)
	}

	// Apply each event to the aggregate
	for _, event := range events {
		if err := aggregate.ApplyEvent(event); err != nil {
			return aggregate, fmt.Errorf("error applying event: %w", err)
		}
	}

	return aggregate, nil
}

// Save saves uncommitted events to the event store
func (r *GenericRepository[T]) Save(aggregate T) error {
	// Get uncommitted events
	events := aggregate.GetUncommittedEvents()
	if len(events) == 0 {
		return nil
	}

	// Save events to the event store
	if err := r.eventStore.SaveEvents(aggregate.GetID(), events); err != nil {
		return fmt.Errorf("error saving events: %w", err)
	}

	// Clear uncommitted events
	aggregate.ClearUncommittedEvents()

	return nil
}
