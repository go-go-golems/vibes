# Implementing an Event-Driven Architecture in GoAgent Framework

This comprehensive guide provides step-by-step instructions for transforming the GoAgent framework into an event-driven system while maintaining an elegant top-level API. The implementation is based on patterns observed in the go-go-golems/geppetto and go-go-golems/pinocchio repositories.

## Table of Contents

1. [Introduction](#introduction)
2. [Prerequisites](#prerequisites)
3. [Understanding Event-Driven Architecture](#understanding-event-driven-architecture)
4. [Implementation Guide](#implementation-guide)
   - [Step 1: Core Event System](#step-1-core-event-system)
   - [Step 2: Component Refactoring](#step-2-component-refactoring)
   - [Step 3: UI Integration](#step-3-ui-integration)
   - [Step 4: API Refinement](#step-4-api-refinement)
5. [Example Applications](#example-applications)
6. [Testing and Debugging](#testing-and-debugging)
7. [Performance Considerations](#performance-considerations)
8. [Advanced Topics](#advanced-topics)

## Introduction

Event-driven architecture is a design pattern where components communicate through events rather than direct method calls. This approach offers several advantages for an LLM agent framework:

- **Real-time updates**: UI components can receive and display updates as they happen
- **Decoupled components**: System components can communicate without direct dependencies
- **Extensibility**: New components can subscribe to existing events without modifying core code
- **Observability**: Events provide a natural way to trace and monitor system behavior

This guide will show you how to transform the GoAgent framework into an event-driven system while maintaining an elegant, simple API for end users.

## Prerequisites

Before starting the implementation, ensure you have:

- Go 1.18 or later
- The GoAgent framework codebase
- Basic understanding of event-driven architecture
- Familiarity with the Watermill library for Go
- (Optional) Familiarity with Bubbletea for terminal UI applications

## Understanding Event-Driven Architecture

### Key Concepts

1. **Events**: Messages that represent something that has happened in the system
2. **Event Bus**: Central component that facilitates communication between different parts of the system
3. **Publishers**: Components that generate events and send them to the event bus
4. **Subscribers**: Components that receive and process events from the event bus

### Event Flow in GoAgent

In our event-driven GoAgent framework, events will flow as follows:

1. Agent receives a user query
2. Agent publishes an "agent:start" event
3. Agent thinks about the query and publishes "agent:thinking" events
4. Agent decides to use the LLM and publishes an "llm:request" event
5. LLM processes the request and publishes "llm:partial" events as it generates the response
6. LLM completes the response and publishes an "llm:complete" event
7. Agent decides to call a tool and publishes a "tool:call" event
8. Tool executes and publishes a "tool:result" event
9. Agent continues processing and eventually publishes an "agent:complete" event

UI components can subscribe to any of these events to provide real-time updates to users.

## Implementation Guide

### Step 1: Core Event System

First, we'll implement the core event system that will serve as the foundation for our event-driven architecture.

#### 1.1 Create Event Types

Create a new file `events/types.go`:

```go
package events

import (
	"encoding/json"
	"time"
)

// EventType represents the type of an event
type EventType string

const (
	// Agent events
	EventTypeAgentStart        EventType = "agent:start"
	EventTypeAgentThinking     EventType = "agent:thinking"
	EventTypeAgentAction       EventType = "agent:action"
	EventTypeAgentComplete     EventType = "agent:complete"
	
	// LLM events
	EventTypeLLMRequest        EventType = "llm:request"
	EventTypeLLMPartial        EventType = "llm:partial"
	EventTypeLLMComplete       EventType = "llm:complete"
	
	// Tool events
	EventTypeToolCall          EventType = "tool:call"
	EventTypeToolResult        EventType = "tool:result"
	
	// Memory events
	EventTypeMemoryStore       EventType = "memory:store"
	EventTypeMemoryRetrieve    EventType = "memory:retrieve"
	
	// Error events
	EventTypeError             EventType = "error"
)

// Event is the base interface for all events
type Event interface {
	Type() EventType
	Metadata() EventMetadata
	Payload() []byte
	Time() time.Time
	SequenceNumber() uint64
}

// EventMetadata contains metadata for an event
type EventMetadata struct {
	ID        string            `json:"id"`
	ParentID  string            `json:"parent_id,omitempty"`
	AgentID   string            `json:"agent_id,omitempty"`
	SessionID string            `json:"session_id,omitempty"`
	Custom    map[string]string `json:"custom,omitempty"`
}

// BaseEvent provides common functionality for all events
type BaseEvent struct {
	type_       EventType    `json:"type"`
	metadata    EventMetadata `json:"metadata"`
	payload     []byte       `json:"-"`
	timestamp   time.Time    `json:"timestamp"`
	sequenceNum uint64       `json:"sequence_number"`
}

func (e *BaseEvent) Type() EventType {
	return e.type_
}

func (e *BaseEvent) Metadata() EventMetadata {
	return e.metadata
}

func (e *BaseEvent) Payload() []byte {
	return e.payload
}

func (e *BaseEvent) Time() time.Time {
	return e.timestamp
}

func (e *BaseEvent) SequenceNumber() uint64 {
	return e.sequenceNum
}

// NewBaseEvent creates a new base event
func NewBaseEvent(eventType EventType, metadata EventMetadata) *BaseEvent {
	return &BaseEvent{
		type_:     eventType,
		metadata:  metadata,
		timestamp: time.Now(),
	}
}

// ToJSON converts an event to JSON
func ToJSON(event Event) ([]byte, error) {
	return json.Marshal(event)
}

// FromJSON converts JSON to an event
func FromJSON(data []byte) (Event, error) {
	var baseEvent BaseEvent
	if err := json.Unmarshal(data, &baseEvent); err != nil {
		return nil, err
	}
	
	// Set the payload
	baseEvent.payload = data
	
	// Based on the event type, unmarshal to the specific event type
	switch baseEvent.type_ {
	case EventTypeAgentThinking:
		var event AgentThinkingEvent
		if err := json.Unmarshal(data, &event); err != nil {
			return nil, err
		}
		event.BaseEvent = baseEvent
		return &event, nil
	case EventTypeLLMPartial:
		var event LLMPartialEvent
		if err := json.Unmarshal(data, &event); err != nil {
			return nil, err
		}
		event.BaseEvent = baseEvent
		return &event, nil
	// Add cases for other event types
	default:
		return &baseEvent, nil
	}
}
```

#### 1.2 Create Specific Event Types

Create a new file `events/agent_events.go`:

```go
package events

import (
	"github.com/google/uuid"
)

// AgentStartEvent represents the start of an agent run
type AgentStartEvent struct {
	BaseEvent
	Input string `json:"input"`
}

// NewAgentStartEvent creates a new agent start event
func NewAgentStartEvent(agentID, sessionID, input string) *AgentStartEvent {
	metadata := EventMetadata{
		ID:        uuid.New().String(),
		AgentID:   agentID,
		SessionID: sessionID,
	}
	
	event := &AgentStartEvent{
		BaseEvent: *NewBaseEvent(EventTypeAgentStart, metadata),
		Input:     input,
	}
	
	return event
}

// AgentThinkingEvent represents an agent thinking step
type AgentThinkingEvent struct {
	BaseEvent
	Thought string `json:"thought"`
}

// NewAgentThinkingEvent creates a new agent thinking event
func NewAgentThinkingEvent(agentID, sessionID, parentID, thought string) *AgentThinkingEvent {
	metadata := EventMetadata{
		ID:        uuid.New().String(),
		ParentID:  parentID,
		AgentID:   agentID,
		SessionID: sessionID,
	}
	
	event := &AgentThinkingEvent{
		BaseEvent: *NewBaseEvent(EventTypeAgentThinking, metadata),
		Thought:   thought,
	}
	
	return event
}

// AgentActionEvent represents an agent action
type AgentActionEvent struct {
	BaseEvent
	Action     string `json:"action"`
	ActionType string `json:"action_type"`
	Input      string `json:"input"`
}

// NewAgentActionEvent creates a new agent action event
func NewAgentActionEvent(agentID, sessionID, parentID, action, actionType, input string) *AgentActionEvent {
	metadata := EventMetadata{
		ID:        uuid.New().String(),
		ParentID:  parentID,
		AgentID:   agentID,
		SessionID: sessionID,
	}
	
	event := &AgentActionEvent{
		BaseEvent:  *NewBaseEvent(EventTypeAgentAction, metadata),
		Action:     action,
		ActionType: actionType,
		Input:      input,
	}
	
	return event
}

// AgentCompleteEvent represents the completion of an agent run
type AgentCompleteEvent struct {
	BaseEvent
	Result string `json:"result"`
}

// NewAgentCompleteEvent creates a new agent complete event
func NewAgentCompleteEvent(agentID, sessionID, parentID, result string) *AgentCompleteEvent {
	metadata := EventMetadata{
		ID:        uuid.New().String(),
		ParentID:  parentID,
		AgentID:   agentID,
		SessionID: sessionID,
	}
	
	event := &AgentCompleteEvent{
		BaseEvent: *NewBaseEvent(EventTypeAgentComplete, metadata),
		Result:    result,
	}
	
	return event
}
```

Create similar files for LLM events (`events/llm_events.go`), tool events (`events/tool_events.go`), and memory events (`events/memory_events.go`).

#### 1.3 Implement Event Manager

Create a new file `events/manager.go`:

```go
package events

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/ThreeDotsLabs/watermill/pubsub/gochannel"
)

// Publisher interface for components that publish events
type Publisher interface {
	Publish(topic string, event Event) error
}

// Subscriber interface for components that subscribe to events
type Subscriber interface {
	Subscribe(ctx context.Context, topic string) (<-chan Event, error)
	Unsubscribe(topic string) error
}

// EventManager manages event publishing and subscription
type EventManager struct {
	pubSub        *gochannel.GoChannel
	sequenceNumber uint64
	mutex          sync.Mutex
	subscriptions  map[string][]chan Event
	subMutex       sync.RWMutex
}

// NewEventManager creates a new event manager
func NewEventManager() *EventManager {
	// Create a new GoChannel pubsub with default configuration
	pubSub := gochannel.NewGoChannel(
		gochannel.Config{
			OutputChannelBuffer: 100,
			Persistent:          false,
		},
		watermill.NewStdLogger(false, false),
	)
	
	return &EventManager{
		pubSub:        pubSub,
		subscriptions: make(map[string][]chan Event),
	}
}

// Publish publishes an event to a topic
func (m *EventManager) Publish(topic string, event Event) error {
	m.mutex.Lock()
	
	// Set sequence number
	baseEvent, ok := event.(*BaseEvent)
	if ok {
		baseEvent.sequenceNum = m.sequenceNumber
	}
	m.sequenceNumber++
	
	m.mutex.Unlock()
	
	// Convert event to JSON
	payload, err := ToJSON(event)
	if err != nil {
		return fmt.Errorf("failed to marshal event: %w", err)
	}
	
	// Create watermill message
	msg := message.NewMessage(watermill.NewUUID(), payload)
	
	// Publish to watermill
	err = m.pubSub.Publish(topic, msg)
	if err != nil {
		return fmt.Errorf("failed to publish message: %w", err)
	}
	
	return nil
}

// PublishBlind publishes an event without returning an error
func (m *EventManager) PublishBlind(topic string, event Event) {
	err := m.Publish(topic, event)
	if err != nil {
		// Just log the error
		fmt.Printf("Error publishing event: %v\n", err)
	}
}

// Subscribe subscribes to events on a topic
func (m *EventManager) Subscribe(ctx context.Context, topic string) (<-chan Event, error) {
	// Create a channel for events
	eventChan := make(chan Event, 100)
	
	// Subscribe to watermill
	messages, err := m.pubSub.Subscribe(ctx, topic)
	if err != nil {
		return nil, fmt.Errorf("failed to subscribe to topic: %w", err)
	}
	
	// Store subscription
	m.subMutex.Lock()
	m.subscriptions[topic] = append(m.subscriptions[topic], eventChan)
	m.subMutex.Unlock()
	
	// Forward messages to event channel
	go func() {
		defer close(eventChan)
		
		for {
			select {
			case <-ctx.Done():
				return
			case msg, ok := <-messages:
				if !ok {
					return
				}
				
				// Convert message to event
				event, err := FromJSON(msg.Payload)
				if err != nil {
					fmt.Printf("Error unmarshaling event: %v\n", err)
					msg.Ack()
					continue
				}
				
				// Send event to channel
				select {
				case eventChan <- event:
				case <-time.After(time.Second):
					fmt.Println("Event channel full, dropping event")
				}
				
				msg.Ack()
			}
		}
	}()
	
	return eventChan, nil
}

// Unsubscribe unsubscribes from a topic
func (m *EventManager) Unsubscribe(topic string) error {
	m.subMutex.Lock()
	defer m.subMutex.Unlock()
	
	// Remove subscription
	delete(m.subscriptions, topic)
	
	return nil
}

// Close closes the event manager
func (m *EventManager) Close() error {
	return m.pubSub.Close()
}
```

### Step 2: Component Refactoring

Now we'll refactor the core components of the GoAgent framework to use the event system.

#### 2.1 Refactor Agent Interface

Update the agent interface in `agent/agent.go`:

```go
package agent

import (
	"context"

	"github.com/goagent/framework/goagent/events"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/memory"
	"github.com/goagent/framework/goagent/tools"
	"github.com/goagent/framework/goagent/types"
	"github.com/google/uuid"
)

// Agent interface with event-driven methods
type Agent interface {
	// Run starts the agent with the given input and returns the final result
	Run(ctx context.Context, input string) (string, error)
	
	// RunWithEvents starts the agent and returns a channel of events
	RunWithEvents(ctx context.Context, input string) (<-chan events.Event, error)
	
	// AddTool adds a tool to the agent
	AddTool(tool tools.Tool)
	
	// SetMemory sets the memory for the agent
	SetMemory(mem memory.Memory)
	
	// SetEventManager sets the event manager for the agent
	SetEventManager(manager *events.EventManager)
}

// BaseAgent provides common functionality for all agent types
type BaseAgent struct {
	tools        []tools.Tool
	memory       memory.Memory
	eventManager *events.EventManager
	agentID      string
}

// NewBaseAgent creates a new base agent
func NewBaseAgent() *BaseAgent {
	return &BaseAgent{
		tools:   []tools.Tool{},
		agentID: uuid.New().String(),
	}
}

// AddTool adds a tool to the agent
func (a *BaseAgent) AddTool(tool tools.Tool) {
	a.tools = append(a.tools, tool)
}

// SetMemory sets the memory for the agent
func (a *BaseAgent) SetMemory(mem memory.Memory) {
	a.memory = mem
}

// SetEventManager sets the event manager for the agent
func (a *BaseAgent) SetEventManager(manager *events.EventManager) {
	a.eventManager = manager
}

// GetTools returns the tools available to the agent
func (a *BaseAgent) GetTools() []tools.Tool {
	return a.tools
}

// GetMemory returns the memory used by the agent
func (a *BaseAgent) GetMemory() memory.Memory {
	return a.memory
}

// GetEventManager returns the event manager used by the agent
func (a *BaseAgent) GetEventManager() *events.EventManager {
	return a.eventManager
}

// GetAgentID returns the agent ID
func (a *BaseAgent) GetAgentID() string {
	return a.agentID
}

// PublishEvent publishes an event
func (a *BaseAgent) PublishEvent(topic string, event events.Event) {
	if a.eventManager != nil {
		a.eventManager.PublishBlind(topic, event)
	}
}
```

#### 2.2 Refactor ReAct Agent

Update the ReAct agent implementation in `agent/react.go`:

```go
package agent

import (
	"context"
	"fmt"

	"github.com/goagent/framework/goagent/events"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/types"
	"github.com/google/uuid"
)

// ReActAgent implements the ReAct (Reasoning + Acting) pattern
type ReActAgent struct {
	BaseAgent
	llm      llm.LLM
	maxSteps int
}

// NewReActAgent creates a new ReAct agent
func NewReActAgent(llmModel llm.LLM, maxSteps int) *ReActAgent {
	return &ReActAgent{
		BaseAgent: *NewBaseAgent(),
		llm:       llmModel,
		maxSteps:  maxSteps,
	}
}

// Run runs the agent with the given input
func (a *ReActAgent) Run(ctx context.Context, input string) (string, error) {
	// Create a session ID
	sessionID := uuid.New().String()
	
	// Publish agent start event
	startEvent := events.NewAgentStartEvent(a.GetAgentID(), sessionID, input)
	a.PublishEvent("agent:start", startEvent)
	
	// Initialize conversation
	messages := []types.Message{
		{
			Role:    "system",
			Content: "You are a helpful assistant that follows the ReAct pattern: Reason, then Act.",
		},
		{
			Role:    "user",
			Content: input,
		},
	}
	
	// Main loop
	var finalResult string
	var lastEventID string = startEvent.Metadata().ID
	
	for step := 0; step < a.maxSteps; step++ {
		// Generate thinking
		thinking := fmt.Sprintf("Step %d: I need to analyze the request and determine the next action.", step+1)
		
		// Publish thinking event
		thinkingEvent := events.NewAgentThinkingEvent(a.GetAgentID(), sessionID, lastEventID, thinking)
		a.PublishEvent("agent:thinking", thinkingEvent)
		lastEventID = thinkingEvent.Metadata().ID
		
		// Generate LLM response
		llmResponse, err := a.llm.Generate(ctx, messages)
		if err != nil {
			// Publish error event
			errorEvent := events.NewErrorEvent(a.GetAgentID(), sessionID, lastEventID, err.Error())
			a.PublishEvent("error", errorEvent)
			return "", err
		}
		
		// Parse response to extract reasoning and action
		reasoning, action, actionInput, err := parseReActResponse(llmResponse)
		if err != nil {
			// Publish error event
			errorEvent := events.NewErrorEvent(a.GetAgentID(), sessionID, lastEventID, err.Error())
			a.PublishEvent("error", errorEvent)
			return "", err
		}
		
		// If no action, we're done
		if action == "FINISH" {
			finalResult = reasoning
			break
		}
		
		// Publish action event
		actionEvent := events.NewAgentActionEvent(a.GetAgentID(), sessionID, lastEventID, action, "tool", actionInput)
		a.PublishEvent("agent:action", actionEvent)
		lastEventID = actionEvent.Metadata().ID
		
		// Execute tool
		var toolResult string
		for _, tool := range a.GetTools() {
			if tool.Name() == action {
				// Publish tool call event
				toolCallEvent := events.NewToolCallEvent(a.GetAgentID(), sessionID, lastEventID, action, actionInput)
				a.PublishEvent("tool:call", toolCallEvent)
				
				// Execute tool
				toolResult, err = tool.Execute(ctx, actionInput)
				if err != nil {
					// Publish error event
					errorEvent := events.NewErrorEvent(a.GetAgentID(), sessionID, lastEventID, err.Error())
					a.PublishEvent("error", errorEvent)
					return "", err
				}
				
				// Publish tool result event
				toolResultEvent := events.NewToolResultEvent(a.GetAgentID(), sessionID, toolCallEvent.Metadata().ID, action, toolResult)
				a.PublishEvent("tool:result", toolResultEvent)
				lastEventID = toolResultEvent.Metadata().ID
				
				break
			}
		}
		
		// Add assistant message
		messages = append(messages, types.Message{
			Role:    "assistant",
			Content: llmResponse,
		})
		
		// Add tool result message
		messages = append(messages, types.Message{
			Role:    "tool",
			Content: fmt.Sprintf("%s result: %s", action, toolResult),
		})
	}
	
	// Publish agent complete event
	completeEvent := events.NewAgentCompleteEvent(a.GetAgentID(), sessionID, lastEventID, finalResult)
	a.PublishEvent("agent:complete", completeEvent)
	
	return finalResult, nil
}

// RunWithEvents runs the agent and returns a channel of events
func (a *ReActAgent) RunWithEvents(ctx context.Context, input string) (<-chan events.Event, error) {
	// Create event manager if not set
	if a.GetEventManager() == nil {
		a.SetEventManager(events.NewEventManager())
	}
	
	// Subscribe to all agent events
	eventChan, err := a.GetEventManager().Subscribe(ctx, "agent:*")
	if err != nil {
		return nil, err
	}
	
	// Run agent in goroutine
	go func() {
		_, err := a.Run(ctx, input)
		if err != nil {
			fmt.Printf("Error running agent: %v\n", err)
		}
	}()
	
	return eventChan, nil
}

// Helper function to parse ReAct response
func parseReActResponse(response string) (reasoning, action, actionInput string, err error) {
	// Simplified implementation - in a real system, you would use regex or a more robust parser
	reasoning = "This is the reasoning part"
	action = "search_web"
	actionInput = "quantum computing"
	return
}
```

#### 2.3 Refactor LLM Interface

Update the LLM interface in `llm/llm.go`:

```go
package llm

import (
	"context"

	"github.com/goagent/framework/goagent/events"
	"github.com/goagent/framework/goagent/types"
)

// LLM interface with event-driven methods
type LLM interface {
	// Generate generates a response to the given messages
	Generate(ctx context.Context, messages []types.Message) (string, error)
	
	// GenerateStream generates a streaming response
	GenerateStream(ctx context.Context, messages []types.Message) error
	
	// GenerateEmbedding generates an embedding for the given text
	GenerateEmbedding(ctx context.Context, text string) ([]float32, error)
	
	// SetEventManager sets the event manager for the LLM
	SetEventManager(manager *events.EventManager)
}

// BaseLLM provides common functionality for LLM implementations
type BaseLLM struct {
	eventManager *events.EventManager
}

// SetEventManager sets the event manager for the LLM
func (l *BaseLLM) SetEventManager(manager *events.EventManager) {
	l.eventManager = manager
}

// GetEventManager returns the event manager used by the LLM
func (l *BaseLLM) GetEventManager() *events.EventManager {
	return l.eventManager
}

// PublishEvent publishes an event
func (l *BaseLLM) PublishEvent(topic string, event events.Event) {
	if l.eventManager != nil {
		l.eventManager.PublishBlind(topic, event)
	}
}
```

#### 2.4 Refactor Mock LLM

Update the mock LLM implementation in `llm/mock.go`:

```go
package llm

import (
	"context"
	"fmt"
	"strings"
	"time"

	"github.com/goagent/framework/goagent/events"
	"github.com/goagent/framework/goagent/types"
	"github.com/google/uuid"
)

// MockLLM is a mock implementation of the LLM interface for testing
type MockLLM struct {
	BaseLLM
	responses map[string]string
}

// NewMockLLM creates a new mock LLM
func NewMockLLM() *MockLLM {
	return &MockLLM{
		responses: make(map[string]string),
	}
}

// AddResponse adds a response for a specific input
func (m *MockLLM) AddResponse(input, response string) {
	m.responses[input] = response
}

// Generate generates a response to the given messages
func (m *MockLLM) Generate(ctx context.Context, messages []types.Message) (string, error) {
	// Get the last user message
	var lastUserMessage string
	for i := len(messages) - 1; i >= 0; i-- {
		if messages[i].Role == "user" {
			lastUserMessage = messages[i].Content
			break
		}
	}
	
	// Create a request ID
	requestID := uuid.New().String()
	
	// Publish LLM request event
	requestEvent := events.NewLLMRequestEvent(requestID, lastUserMessage)
	m.PublishEvent("llm:request", requestEvent)
	
	// Check if we have a predefined response
	response, ok := m.responses[lastUserMessage]
	if !ok {
		// Default response
		response = fmt.Sprintf("This is a mock response to: %s", lastUserMessage)
	}
	
	// Publish LLM complete event
	completeEvent := events.NewLLMCompleteEvent(requestID, requestEvent.Metadata().ID, response)
	m.PublishEvent("llm:complete", completeEvent)
	
	return response, nil
}

// GenerateStream generates a streaming response
func (m *MockLLM) GenerateStream(ctx context.Context, messages []types.Message) error {
	// Get the last user message
	var lastUserMessage string
	for i := len(messages) - 1; i >= 0; i-- {
		if messages[i].Role == "user" {
			lastUserMessage = messages[i].Content
			break
		}
	}
	
	// Create a request ID
	requestID := uuid.New().String()
	
	// Publish LLM request event
	requestEvent := events.NewLLMRequestEvent(requestID, lastUserMessage)
	m.PublishEvent("llm:request", requestEvent)
	
	// Check if we have a predefined response
	response, ok := m.responses[lastUserMessage]
	if !ok {
		// Default response
		response = fmt.Sprintf("This is a mock response to: %s", lastUserMessage)
	}
	
	// Split response into chunks
	chunks := splitIntoChunks(response, 10)
	
	// Publish partial events
	var fullResponse string
	for _, chunk := range chunks {
		select {
		case <-ctx.Done():
			return ctx.Err()
		default:
			fullResponse += chunk
			
			// Publish partial event
			partialEvent := events.NewLLMPartialEvent(requestID, requestEvent.Metadata().ID, chunk, fullResponse)
			m.PublishEvent("llm:partial", partialEvent)
			
			// Simulate delay
			time.Sleep(100 * time.Millisecond)
		}
	}
	
	// Publish complete event
	completeEvent := events.NewLLMCompleteEvent(requestID, requestEvent.Metadata().ID, fullResponse)
	m.PublishEvent("llm:complete", completeEvent)
	
	return nil
}

// GenerateEmbedding generates an embedding for the given text
func (m *MockLLM) GenerateEmbedding(ctx context.Context, text string) ([]float32, error) {
	// Mock embedding - in a real system, this would call an embedding model
	embedding := make([]float32, 10)
	for i := range embedding {
		embedding[i] = float32(i) / 10.0
	}
	
	return embedding, nil
}

// Helper function to split text into chunks
func splitIntoChunks(text string, chunkSize int) []string {
	var chunks []string
	words := strings.Fields(text)
	
	for i := 0; i < len(words); i += chunkSize {
		end := i + chunkSize
		if end > len(words) {
			end = len(words)
		}
		
		chunk := strings.Join(words[i:end], " ")
		chunks = append(chunks, chunk)
	}
	
	return chunks
}
```

#### 2.5 Refactor Tool Interface

Update the tool interface in `tools/tools.go`:

```go
package tools

import (
	"context"

	"github.com/goagent/framework/goagent/events"
	"github.com/goagent/framework/goagent/types"
)

// Tool interface with event-driven methods
type Tool interface {
	// Name returns the name of the tool
	Name() string
	
	// Description returns the description of the tool
	Description() string
	
	// Execute executes the tool with the given input
	Execute(ctx context.Context, input string) (string, error)
	
	// Parameters returns the parameters schema for the tool
	Parameters() map[string]types.ParameterSchema
	
	// SetEventManager sets the event manager for the tool
	SetEventManager(manager *events.EventManager)
}

// BaseTool provides common functionality for tool implementations
type BaseTool struct {
	name         string
	description  string
	parameters   map[string]types.ParameterSchema
	eventManager *events.EventManager
}

// NewBaseTool creates a new base tool
func NewBaseTool(name, description string, parameters map[string]types.ParameterSchema) *BaseTool {
	return &BaseTool{
		name:        name,
		description: description,
		parameters:  parameters,
	}
}

// Name returns the name of the tool
func (t *BaseTool) Name() string {
	return t.name
}

// Description returns the description of the tool
func (t *BaseTool) Description() string {
	return t.description
}

// Parameters returns the parameters schema for the tool
func (t *BaseTool) Parameters() map[string]types.ParameterSchema {
	return t.parameters
}

// SetEventManager sets the event manager for the tool
func (t *BaseTool) SetEventManager(manager *events.EventManager) {
	t.eventManager = manager
}

// GetEventManager returns the event manager used by the tool
func (t *BaseTool) GetEventManager() *events.EventManager {
	return t.eventManager
}

// PublishEvent publishes an event
func (t *BaseTool) PublishEvent(topic string, event events.Event) {
	if t.eventManager != nil {
		t.eventManager.PublishBlind(topic, event)
	}
}
```

### Step 3: UI Integration

Now we'll implement UI integration to display events in real-time.

#### 3.1 Create UI Adapter

Create a new file `ui/adapter.go`:

```go
package ui

import (
	"context"
	"fmt"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/goagent/framework/goagent/events"
)

// UIAdapter connects the event system to the UI
type UIAdapter struct {
	eventManager *events.EventManager
	program      *tea.Program
}

// NewUIAdapter creates a new UI adapter
func NewUIAdapter(eventManager *events.EventManager, program *tea.Program) *UIAdapter {
	return &UIAdapter{
		eventManager: eventManager,
		program:      program,
	}
}

// Start starts the UI adapter
func (a *UIAdapter) Start(ctx context.Context) error {
	// Subscribe to relevant topics
	agentEvents, err := a.eventManager.Subscribe(ctx, "agent:*")
	if err != nil {
		return err
	}
	
	llmEvents, err := a.eventManager.Subscribe(ctx, "llm:*")
	if err != nil {
		return err
	}
	
	toolEvents, err := a.eventManager.Subscribe(ctx, "tool:*")
	if err != nil {
		return err
	}
	
	// Forward events to the UI
	go func() {
		for {
			select {
			case <-ctx.Done():
				return
			case event := <-agentEvents:
				a.handleAgentEvent(event)
			case event := <-llmEvents:
				a.handleLLMEvent(event)
			case event := <-toolEvents:
				a.handleToolEvent(event)
			}
		}
	}()
	
	return nil
}

// Event handlers convert events to UI messages
func (a *UIAdapter) handleAgentEvent(event events.Event) {
	switch event.Type() {
	case events.EventTypeAgentStart:
		if e, ok := event.(*events.AgentStartEvent); ok {
			a.program.Send(AgentStartMsg{
				Input: e.Input,
			})
		}
	case events.EventTypeAgentThinking:
		if e, ok := event.(*events.AgentThinkingEvent); ok {
			a.program.Send(AgentThinkingMsg{
				Thought: e.Thought,
			})
		}
	case events.EventTypeAgentAction:
		if e, ok := event.(*events.AgentActionEvent); ok {
			a.program.Send(AgentActionMsg{
				Action:     e.Action,
				ActionType: e.ActionType,
				Input:      e.Input,
			})
		}
	case events.EventTypeAgentComplete:
		if e, ok := event.(*events.AgentCompleteEvent); ok {
			a.program.Send(AgentCompleteMsg{
				Result: e.Result,
			})
		}
	}
}

func (a *UIAdapter) handleLLMEvent(event events.Event) {
	switch event.Type() {
	case events.EventTypeLLMRequest:
		if e, ok := event.(*events.LLMRequestEvent); ok {
			a.program.Send(LLMRequestMsg{
				Input: e.Input,
			})
		}
	case events.EventTypeLLMPartial:
		if e, ok := event.(*events.LLMPartialEvent); ok {
			a.program.Send(LLMPartialMsg{
				Delta:      e.Delta,
				Completion: e.Completion,
			})
		}
	case events.EventTypeLLMComplete:
		if e, ok := event.(*events.LLMCompleteEvent); ok {
			a.program.Send(LLMCompleteMsg{
				Response: e.Response,
			})
		}
	}
}

func (a *UIAdapter) handleToolEvent(event events.Event) {
	switch event.Type() {
	case events.EventTypeToolCall:
		if e, ok := event.(*events.ToolCallEvent); ok {
			a.program.Send(ToolCallMsg{
				ToolName:  e.ToolName,
				ToolInput: e.ToolInput,
			})
		}
	case events.EventTypeToolResult:
		if e, ok := event.(*events.ToolResultEvent); ok {
			a.program.Send(ToolResultMsg{
				ToolName:   e.ToolName,
				ToolResult: e.ToolResult,
			})
		}
	}
}
```

#### 3.2 Create UI Messages

Create a new file `ui/messages.go`:

```go
package ui

// AgentStartMsg represents the start of an agent run
type AgentStartMsg struct {
	Input string
}

// AgentThinkingMsg represents an agent thinking step
type AgentThinkingMsg struct {
	Thought string
}

// AgentActionMsg represents an agent action
type AgentActionMsg struct {
	Action     string
	ActionType string
	Input      string
}

// AgentCompleteMsg represents the completion of an agent run
type AgentCompleteMsg struct {
	Result string
}

// LLMRequestMsg represents an LLM request
type LLMRequestMsg struct {
	Input string
}

// LLMPartialMsg represents a partial LLM response
type LLMPartialMsg struct {
	Delta      string
	Completion string
}

// LLMCompleteMsg represents a complete LLM response
type LLMCompleteMsg struct {
	Response string
}

// ToolCallMsg represents a tool call
type ToolCallMsg struct {
	ToolName  string
	ToolInput string
}

// ToolResultMsg represents a tool result
type ToolResultMsg struct {
	ToolName   string
	ToolResult string
}
```

#### 3.3 Create Terminal UI

Create a new file `ui/terminal.go`:

```go
package ui

import (
	"context"
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/goagent/framework/goagent/events"
)

var (
	agentStyle    = lipgloss.NewStyle().Foreground(lipgloss.Color("205"))
	llmStyle      = lipgloss.NewStyle().Foreground(lipgloss.Color("39"))
	toolStyle     = lipgloss.NewStyle().Foreground(lipgloss.Color("220"))
	thinkingStyle = lipgloss.NewStyle().Foreground(lipgloss.Color("240")).Italic(true)
	resultStyle   = lipgloss.NewStyle().Foreground(lipgloss.Color("46")).Bold(true)
)

// TerminalUI represents a terminal UI for displaying agent events
type TerminalUI struct {
	eventManager *events.EventManager
	program      *tea.Program
	adapter      *UIAdapter
}

// NewTerminalUI creates a new terminal UI
func NewTerminalUI(eventManager *events.EventManager) *TerminalUI {
	model := initialModel()
	program := tea.NewProgram(model)
	
	adapter := NewUIAdapter(eventManager, program)
	
	return &TerminalUI{
		eventManager: eventManager,
		program:      program,
		adapter:      adapter,
	}
}

// Start starts the terminal UI
func (ui *TerminalUI) Start(ctx context.Context) error {
	// Start the UI adapter
	err := ui.adapter.Start(ctx)
	if err != nil {
		return err
	}
	
	// Run the program
	go func() {
		if _, err := ui.program.Run(); err != nil {
			fmt.Printf("Error running program: %v\n", err)
		}
	}()
	
	return nil
}

// Model represents the UI state
type model struct {
	conversation []string
	thinking     string
	loading      bool
	width        int
	height       int
}

func initialModel() model {
	return model{
		conversation: []string{},
		thinking:     "",
		loading:      false,
		width:        80,
		height:       24,
	}
}

// Init initializes the model
func (m model) Init() tea.Cmd {
	return nil
}

// Update updates the model based on messages
func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		return m, nil
		
	case AgentStartMsg:
		m.conversation = append(m.conversation, agentStyle.Render("Agent started with input: ")+msg.Input)
		return m, nil
		
	case AgentThinkingMsg:
		m.thinking = msg.Thought
		return m, nil
		
	case AgentActionMsg:
		m.conversation = append(m.conversation, agentStyle.Render("Agent action: ")+msg.Action+" with input: "+msg.Input)
		return m, nil
		
	case AgentCompleteMsg:
		m.conversation = append(m.conversation, agentStyle.Render("Agent completed with result: ")+resultStyle.Render(msg.Result))
		m.thinking = ""
		return m, nil
		
	case LLMRequestMsg:
		m.conversation = append(m.conversation, llmStyle.Render("LLM request: ")+msg.Input)
		m.loading = true
		return m, nil
		
	case LLMPartialMsg:
		// Update the last message if it's an LLM response
		if len(m.conversation) > 0 && strings.HasPrefix(m.conversation[len(m.conversation)-1], llmStyle.Render("LLM response: ")) {
			m.conversation[len(m.conversation)-1] = llmStyle.Render("LLM response: ") + msg.Completion
		} else {
			m.conversation = append(m.conversation, llmStyle.Render("LLM response: ")+msg.Completion)
		}
		return m, nil
		
	case LLMCompleteMsg:
		m.conversation = append(m.conversation, llmStyle.Render("LLM response: ")+msg.Response)
		m.loading = false
		return m, nil
		
	case ToolCallMsg:
		m.conversation = append(m.conversation, toolStyle.Render("Tool call: ")+msg.ToolName+" with input: "+msg.ToolInput)
		return m, nil
		
	case ToolResultMsg:
		m.conversation = append(m.conversation, toolStyle.Render("Tool result: ")+msg.ToolResult)
		return m, nil
	}
	
	return m, nil
}

// View renders the model
func (m model) View() string {
	// Calculate available height for conversation
	availableHeight := m.height - 4 // Reserve space for thinking and status
	
	// Limit conversation to available height
	startIdx := 0
	if len(m.conversation) > availableHeight {
		startIdx = len(m.conversation) - availableHeight
	}
	
	// Build conversation view
	var sb strings.Builder
	sb.WriteString("=== GoAgent Terminal UI ===\n\n")
	
	for i := startIdx; i < len(m.conversation); i++ {
		sb.WriteString(m.conversation[i] + "\n")
	}
	
	// Add thinking if present
	if m.thinking != "" {
		sb.WriteString("\n" + thinkingStyle.Render("Thinking: "+m.thinking) + "\n")
	}
	
	// Add loading indicator
	if m.loading {
		sb.WriteString("\nLoading...\n")
	}
	
	return sb.String()
}
```

### Step 4: API Refinement

Finally, we'll refine the API to make it elegant and easy to use.

#### 4.1 Create Builder Pattern

Create a new file `goagent/options.go`:

```go
package goagent

import (
	"github.com/goagent/framework/goagent/agent"
	"github.com/goagent/framework/goagent/events"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/memory"
	"github.com/goagent/framework/goagent/tools"
)

// AgentOption is a function that configures an agent
type AgentOption func(agent agent.Agent) error

// WithEventManager sets the event manager for the agent
func WithEventManager(manager *events.EventManager) AgentOption {
	return func(a agent.Agent) error {
		a.SetEventManager(manager)
		return nil
	}
}

// WithMemory sets the memory for the agent
func WithMemory(mem memory.Memory) AgentOption {
	return func(a agent.Agent) error {
		a.SetMemory(mem)
		return nil
	}
}

// WithTool adds a tool to the agent
func WithTool(tool tools.Tool) AgentOption {
	return func(a agent.Agent) error {
		a.AddTool(tool)
		return nil
	}
}

// WithTools adds multiple tools to the agent
func WithTools(tools ...tools.Tool) AgentOption {
	return func(a agent.Agent) error {
		for _, tool := range tools {
			a.AddTool(tool)
		}
		return nil
	}
}

// ReActAgentOption is a function that configures a ReAct agent
type ReActAgentOption func(agent *agent.ReActAgent) error

// WithMaxSteps sets the maximum number of steps for a ReAct agent
func WithMaxSteps(maxSteps int) ReActAgentOption {
	return func(a *agent.ReActAgent) error {
		// This would need to be implemented in the ReActAgent
		// a.SetMaxSteps(maxSteps)
		return nil
	}
}
```

#### 4.2 Create Top-Level API

Create a new file `goagent/goagent.go`:

```go
package goagent

import (
	"github.com/goagent/framework/goagent/agent"
	"github.com/goagent/framework/goagent/events"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/memory"
	"github.com/goagent/framework/goagent/tools"
	"github.com/goagent/framework/goagent/ui"
)

// NewEventManager creates a new event manager
func NewEventManager() *events.EventManager {
	return events.NewEventManager()
}

// NewReActAgent creates a new ReAct agent with the given options
func NewReActAgent(llmModel llm.LLM, options ...AgentOption) agent.Agent {
	reactAgent := agent.NewReActAgent(llmModel, 10)
	
	for _, option := range options {
		option(reactAgent)
	}
	
	return reactAgent
}

// NewPlanAndExecuteAgent creates a new Plan-and-Execute agent with the given options
func NewPlanAndExecuteAgent(plannerLLM, executorLLM llm.LLM, options ...AgentOption) agent.Agent {
	// This would need to be implemented
	// planAndExecuteAgent := agent.NewPlanAndExecuteAgent(plannerLLM, executorLLM, 10)
	
	// for _, option := range options {
	//     option(planAndExecuteAgent)
	// }
	
	// return planAndExecuteAgent
	
	// Placeholder
	return nil
}

// NewTerminalUI creates a new terminal UI
func NewTerminalUI(eventManager *events.EventManager) *ui.TerminalUI {
	return ui.NewTerminalUI(eventManager)
}

// NewWebSearchTool creates a new web search tool
func NewWebSearchTool() tools.Tool {
	// This would need to be implemented
	// return tools.NewWebSearchTool()
	
	// Placeholder
	return nil
}

// NewCalculatorTool creates a new calculator tool
func NewCalculatorTool() tools.Tool {
	// This would need to be implemented
	// return tools.NewCalculatorTool()
	
	// Placeholder
	return nil
}

// NewVectorMemory creates a new vector memory
func NewVectorMemory(llmModel llm.LLM) memory.Memory {
	// This would need to be implemented
	// return memory.NewVectorMemory(llmModel)
	
	// Placeholder
	return nil
}
```

## Example Applications

Here are some example applications that demonstrate how to use the event-driven GoAgent framework.

### Example 1: Simple Research Agent

```go
package main

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/goagent/framework/goagent"
	"github.com/goagent/framework/goagent/llm"
)

func main() {
	// Create event manager
	eventManager := goagent.NewEventManager()
	
	// Create mock LLM
	mockLLM := llm.NewMockLLM()
	mockLLM.SetEventManager(eventManager)
	
	// Add some mock responses
	mockLLM.AddResponse("Research quantum computing", "Quantum computing is a type of computing that uses quantum-mechanical phenomena, such as superposition and entanglement, to perform operations on data.")
	
	// Create web search tool
	webSearchTool := goagent.NewWebSearchTool()
	
	// Create agent
	agent := goagent.NewReActAgent(
		mockLLM,
		goagent.WithEventManager(eventManager),
		goagent.WithTool(webSearchTool),
	)
	
	// Create terminal UI
	ui := goagent.NewTerminalUI(eventManager)
	
	// Start UI
	ctx := context.Background()
	err := ui.Start(ctx)
	if err != nil {
		fmt.Printf("Error starting UI: %v\n", err)
		os.Exit(1)
	}
	
	// Run agent
	result, err := agent.Run(ctx, "Research quantum computing")
	if err != nil {
		fmt.Printf("Error running agent: %v\n", err)
		os.Exit(1)
	}
	
	// Wait for UI to display results
	time.Sleep(1 * time.Second)
	
	fmt.Println("Final result:", result)
}
```

### Example 2: Streaming LLM Responses

```go
package main

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/goagent/framework/goagent"
	"github.com/goagent/framework/goagent/events"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/types"
)

func main() {
	// Create event manager
	eventManager := goagent.NewEventManager()
	
	// Create mock LLM
	mockLLM := llm.NewMockLLM()
	mockLLM.SetEventManager(eventManager)
	
	// Add some mock responses
	mockLLM.AddResponse("Tell me about artificial intelligence", "Artificial intelligence (AI) is intelligence demonstrated by machines, as opposed to natural intelligence displayed by animals including humans. AI research has been defined as the field of study of intelligent agents, which refers to any system that perceives its environment and takes actions that maximize its chance of achieving its goals.")
	
	// Create terminal UI
	ui := goagent.NewTerminalUI(eventManager)
	
	// Start UI
	ctx := context.Background()
	err := ui.Start(ctx)
	if err != nil {
		fmt.Printf("Error starting UI: %v\n", err)
		os.Exit(1)
	}
	
	// Subscribe to LLM events
	llmEvents, err := eventManager.Subscribe(ctx, "llm:*")
	if err != nil {
		fmt.Printf("Error subscribing to events: %v\n", err)
		os.Exit(1)
	}
	
	// Create messages
	messages := []types.Message{
		{
			Role:    "user",
			Content: "Tell me about artificial intelligence",
		},
	}
	
	// Generate streaming response
	go func() {
		err := mockLLM.GenerateStream(ctx, messages)
		if err != nil {
			fmt.Printf("Error generating stream: %v\n", err)
		}
	}()
	
	// Process events
	go func() {
		for event := range llmEvents {
			if event.Type() == events.EventTypeLLMComplete {
				if e, ok := event.(*events.LLMCompleteEvent); ok {
					fmt.Println("\nFinal response:", e.Response)
				}
			}
		}
	}()
	
	// Wait for streaming to complete
	time.Sleep(5 * time.Second)
}
```

## Testing and Debugging

### Unit Testing Event-Driven Components

When testing event-driven components, you can use a test event manager to capture and verify events:

```go
func TestReActAgent(t *testing.T) {
	// Create event manager
	eventManager := events.NewEventManager()
	
	// Create mock LLM
	mockLLM := llm.NewMockLLM()
	mockLLM.SetEventManager(eventManager)
	
	// Add mock response
	mockLLM.AddResponse("Test query", "Test response")
	
	// Create agent
	agent := agent.NewReActAgent(mockLLM, 10)
	agent.SetEventManager(eventManager)
	
	// Subscribe to agent events
	ctx := context.Background()
	agentEvents, err := eventManager.Subscribe(ctx, "agent:*")
	if err != nil {
		t.Fatalf("Failed to subscribe to events: %v", err)
	}
	
	// Run agent in goroutine
	go func() {
		_, err := agent.Run(ctx, "Test query")
		if err != nil {
			t.Errorf("Error running agent: %v", err)
		}
	}()
	
	// Collect events
	var events []events.Event
	timeout := time.After(5 * time.Second)
	
	for {
		select {
		case event := <-agentEvents:
			events = append(events, event)
			if event.Type() == events.EventTypeAgentComplete {
				// Verify events
				if len(events) < 3 {
					t.Errorf("Expected at least 3 events, got %d", len(events))
				}
				
				// Verify event sequence
				if events[0].Type() != events.EventTypeAgentStart {
					t.Errorf("First event should be agent start, got %s", events[0].Type())
				}
				
				if events[len(events)-1].Type() != events.EventTypeAgentComplete {
					t.Errorf("Last event should be agent complete, got %s", events[len(events)-1].Type())
				}
				
				return
			}
		case <-timeout:
			t.Fatal("Test timed out")
			return
		}
	}
}
```

### Debugging Tips

1. **Enable verbose logging**: Add logging to event publishing and subscription to track event flow.

2. **Use event tracing**: Collect all events in a trace and analyze them after execution.

```go
// Create a trace collector
type TraceCollector struct {
	events []events.Event
	mutex  sync.Mutex
}

func NewTraceCollector() *TraceCollector {
	return &TraceCollector{
		events: []events.Event{},
	}
}

func (c *TraceCollector) AddEvent(event events.Event) {
	c.mutex.Lock()
	defer c.mutex.Unlock()
	c.events = append(c.events, event)
}

func (c *TraceCollector) GetEvents() []events.Event {
	c.mutex.Lock()
	defer c.mutex.Unlock()
	return c.events
}

// Use the trace collector
collector := NewTraceCollector()

// Subscribe to all events
allEvents, _ := eventManager.Subscribe(ctx, "*")
go func() {
	for event := range allEvents {
		collector.AddEvent(event)
	}
}()

// After execution
events := collector.GetEvents()
for i, event := range events {
	fmt.Printf("%d: %s - %s\n", i, event.Type(), event.Time())
}
```

3. **Visualize event flow**: Create a visualization of event flow to understand the sequence of events.

## Performance Considerations

### Event Buffering

When dealing with high-frequency events, consider buffering events to avoid overwhelming subscribers:

```go
// Configure buffer size in event manager
func NewEventManager() *EventManager {
	pubSub := gochannel.NewGoChannel(
		gochannel.Config{
			OutputChannelBuffer: 1000, // Increase buffer size
			Persistent:          false,
		},
		watermill.NewStdLogger(false, false),
	)
	
	return &EventManager{
		pubSub:        pubSub,
		subscriptions: make(map[string][]chan Event),
	}
}
```

### Event Filtering

Implement event filtering to reduce the number of events processed by subscribers:

```go
// Subscribe with filter
func (m *EventManager) SubscribeWithFilter(ctx context.Context, topic string, filter func(Event) bool) (<-chan Event, error) {
	// Create a channel for filtered events
	filteredChan := make(chan Event, 100)
	
	// Subscribe to all events on the topic
	allEvents, err := m.Subscribe(ctx, topic)
	if err != nil {
		return nil, err
	}
	
	// Filter events
	go func() {
		defer close(filteredChan)
		
		for {
			select {
			case <-ctx.Done():
				return
			case event, ok := <-allEvents:
				if !ok {
					return
				}
				
				if filter(event) {
					filteredChan <- event
				}
			}
		}
	}()
	
	return filteredChan, nil
}
```

### Event Batching

For UI updates, consider batching events to reduce the number of UI updates:

```go
// Batch events for UI updates
func batchEvents(ctx context.Context, events <-chan events.Event, batchSize int, maxDelay time.Duration) <-chan []events.Event {
	batchChan := make(chan []events.Event)
	
	go func() {
		defer close(batchChan)
		
		var batch []events.Event
		timer := time.NewTimer(maxDelay)
		timer.Stop()
		
		for {
			select {
			case <-ctx.Done():
				if len(batch) > 0 {
					batchChan <- batch
				}
				return
			case event, ok := <-events:
				if !ok {
					if len(batch) > 0 {
						batchChan <- batch
					}
					return
				}
				
				batch = append(batch, event)
				
				if len(batch) == 1 {
					timer.Reset(maxDelay)
				}
				
				if len(batch) >= batchSize {
					batchChan <- batch
					batch = []events.Event{}
					timer.Stop()
				}
			case <-timer.C:
				if len(batch) > 0 {
					batchChan <- batch
					batch = []events.Event{}
				}
			}
		}
	}()
	
	return batchChan
}
```

## Advanced Topics

### Custom Event Types

You can create custom event types for specific use cases:

```go
// Custom event for visualization
type VisualizationEvent struct {
	BaseEvent
	ChartType string            `json:"chart_type"`
	Data      map[string][]float64 `json:"data"`
}

// Create a new visualization event
func NewVisualizationEvent(id, parentID, chartType string, data map[string][]float64) *VisualizationEvent {
	metadata := EventMetadata{
		ID:       id,
		ParentID: parentID,
	}
	
	event := &VisualizationEvent{
		BaseEvent: *NewBaseEvent(EventType("visualization"), metadata),
		ChartType: chartType,
		Data:      data,
	}
	
	return event
}
```

### Web UI Integration

For web applications, you can integrate the event system with WebSockets:

```go
// WebSocketAdapter connects the event system to WebSockets
type WebSocketAdapter struct {
	eventManager *events.EventManager
	clients      map[string]*websocket.Conn
	mutex        sync.RWMutex
}

// NewWebSocketAdapter creates a new WebSocket adapter
func NewWebSocketAdapter(eventManager *events.EventManager) *WebSocketAdapter {
	return &WebSocketAdapter{
		eventManager: eventManager,
		clients:      make(map[string]*websocket.Conn),
	}
}

// AddClient adds a WebSocket client
func (a *WebSocketAdapter) AddClient(id string, conn *websocket.Conn) {
	a.mutex.Lock()
	defer a.mutex.Unlock()
	a.clients[id] = conn
}

// RemoveClient removes a WebSocket client
func (a *WebSocketAdapter) RemoveClient(id string) {
	a.mutex.Lock()
	defer a.mutex.Unlock()
	delete(a.clients, id)
}

// Start starts the WebSocket adapter
func (a *WebSocketAdapter) Start(ctx context.Context) error {
	// Subscribe to all events
	allEvents, err := a.eventManager.Subscribe(ctx, "*")
	if err != nil {
		return err
	}
	
	// Forward events to WebSocket clients
	go func() {
		for {
			select {
			case <-ctx.Done():
				return
			case event := <-allEvents:
				// Convert event to JSON
				payload, err := events.ToJSON(event)
				if err != nil {
					fmt.Printf("Error marshaling event: %v\n", err)
					continue
				}
				
				// Send to all clients
				a.mutex.RLock()
				for id, conn := range a.clients {
					err = conn.WriteMessage(websocket.TextMessage, payload)
					if err != nil {
						fmt.Printf("Error sending to client %s: %v\n", id, err)
					}
				}
				a.mutex.RUnlock()
			}
		}
	}()
	
	return nil
}
```

### Distributed Event System

For distributed applications, you can use a message broker like RabbitMQ or NATS:

```go
// Create a distributed event manager with RabbitMQ
func NewDistributedEventManager(amqpURL string) (*EventManager, error) {
	// Create RabbitMQ publisher
	publisher, err := amqp.NewPublisher(
		amqp.PublisherConfig{
			AmqpURI:      amqpURL,
			Exchange:     "events",
			ExchangeType: "topic",
		},
		watermill.NewStdLogger(false, false),
	)
	if err != nil {
		return nil, err
	}
	
	// Create RabbitMQ subscriber
	subscriber, err := amqp.NewSubscriber(
		amqp.SubscriberConfig{
			AmqpURI:      amqpURL,
			Exchange:     "events",
			ExchangeType: "topic",
			Queue:        "events_queue",
			QueueDurable: true,
		},
		watermill.NewStdLogger(false, false),
	)
	if err != nil {
		return nil, err
	}
	
	// Create router
	router, err := message.NewRouter(message.RouterConfig{}, watermill.NewStdLogger(false, false))
	if err != nil {
		return nil, err
	}
	
	// Create event manager
	eventManager := &EventManager{
		publisher:     publisher,
		subscriber:    subscriber,
		router:        router,
		subscriptions: make(map[string][]chan Event),
	}
	
	return eventManager, nil
}
```

## Conclusion

This guide has shown you how to transform the GoAgent framework into an event-driven architecture while maintaining an elegant top-level API. By following the implementation steps and best practices outlined in this guide, you can create a powerful, flexible, and extensible agent framework that supports real-time updates, decoupled components, and improved observability.

The event-driven approach allows for:

1. **Real-time UI updates**: Users can see agent progress as it happens
2. **Decoupled components**: System components can communicate without direct dependencies
3. **Extensibility**: New components can subscribe to existing events without modifying core code
4. **Observability**: Events provide a natural way to trace and monitor system behavior

All while maintaining an elegant, simple API that hides the complexity of the event system from end users.

By leveraging patterns from the go-go-golems/geppetto and go-go-golems/pinocchio repositories, we've created a robust event system that can be used in a variety of applications, from simple command-line tools to complex web applications.
