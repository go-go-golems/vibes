package mediator

import (
	"context"
	"errors"
	"fmt"
	"log"
	"sync"
)

// Command represents a command to be executed
type Command interface {
	CommandType() string
}

// CommandHandler handles a specific command type
type CommandHandler interface {
	Handle(ctx context.Context, command Command) (interface{}, error)
}

// Event represents something that has happened
type Event interface {
	EventType() string
}

// EventHandler handles a specific event type
type EventHandler interface {
	Handle(ctx context.Context, event Event) error
}

// Query represents a data query
type Query interface {
	QueryType() string
}

// QueryHandler handles a specific query type
type QueryHandler interface {
	Handle(ctx context.Context, query Query) (interface{}, error)
}

// Mediator coordinates command and event handling
type Mediator interface {
	RegisterCommandHandler(commandType string, handler CommandHandler)
	RegisterEventHandler(eventType string, handler EventHandler)
	RegisterQueryHandler(queryType string, handler QueryHandler)
	Send(ctx context.Context, command Command) (interface{}, error)
	Publish(ctx context.Context, event Event) error
	Query(ctx context.Context, query Query) (interface{}, error)
}

// DefaultMediator is a simple implementation of Mediator
type DefaultMediator struct {
	commandHandlers    map[string]CommandHandler
	eventHandlers      map[string][]EventHandler
	queryHandlers      map[string]QueryHandler
	commandHandlersMux sync.RWMutex
	eventHandlersMux   sync.RWMutex
	queryHandlersMux   sync.RWMutex
}

// NewMediator creates a new mediator
func NewMediator() *DefaultMediator {
	return &DefaultMediator{
		commandHandlers: make(map[string]CommandHandler),
		eventHandlers:   make(map[string][]EventHandler),
		queryHandlers:   make(map[string]QueryHandler),
	}
}

// RegisterCommandHandler registers a command handler
func (m *DefaultMediator) RegisterCommandHandler(commandType string, handler CommandHandler) {
	m.commandHandlersMux.Lock()
	defer m.commandHandlersMux.Unlock()
	m.commandHandlers[commandType] = handler
}

// RegisterEventHandler registers an event handler
func (m *DefaultMediator) RegisterEventHandler(eventType string, handler EventHandler) {
	m.eventHandlersMux.Lock()
	defer m.eventHandlersMux.Unlock()
	if m.eventHandlers[eventType] == nil {
		m.eventHandlers[eventType] = []EventHandler{}
	}
	m.eventHandlers[eventType] = append(m.eventHandlers[eventType], handler)
}

// RegisterQueryHandler registers a query handler
func (m *DefaultMediator) RegisterQueryHandler(queryType string, handler QueryHandler) {
	m.queryHandlersMux.Lock()
	defer m.queryHandlersMux.Unlock()
	m.queryHandlers[queryType] = handler
}

// Send sends a command to its handler
func (m *DefaultMediator) Send(ctx context.Context, command Command) (interface{}, error) {
	m.commandHandlersMux.RLock()
	handler, exists := m.commandHandlers[command.CommandType()]
	m.commandHandlersMux.RUnlock()

	if !exists {
		return nil, fmt.Errorf("no handler registered for command type %s", command.CommandType())
	}

	return handler.Handle(ctx, command)
}

// Publish publishes an event to all its handlers
func (m *DefaultMediator) Publish(ctx context.Context, event Event) error {
	m.eventHandlersMux.RLock()
	handlers, exists := m.eventHandlers[event.EventType()]
	m.eventHandlersMux.RUnlock()

	if !exists || len(handlers) == 0 {
		// It's not an error if no handlers are registered
		return nil
	}

	errs := []error{}
	for _, handler := range handlers {
		if err := handler.Handle(ctx, event); err != nil {
			errs = append(errs, fmt.Errorf("error handling event %s: %w", event.EventType(), err))
			log.Printf("Error handling event %s: %v", event.EventType(), err)
		}
	}

	if len(errs) > 0 {
		return errors.Join(errs...)
	}

	return nil
}

// Query sends a query to its handler
func (m *DefaultMediator) Query(ctx context.Context, query Query) (interface{}, error) {
	m.queryHandlersMux.RLock()
	handler, exists := m.queryHandlers[query.QueryType()]
	m.queryHandlersMux.RUnlock()

	if !exists {
		return nil, fmt.Errorf("no handler registered for query type %s", query.QueryType())
	}

	return handler.Handle(ctx, query)
}