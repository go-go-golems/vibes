# Event-Driven Architecture for GoAgent Framework

This document outlines the design for transforming the GoAgent framework into an event-driven architecture based on patterns observed in the go-go-golems/geppetto and go-go-golems/pinocchio repositories.

## Table of Contents

1. [Overview](#overview)
2. [Core Concepts](#core-concepts)
3. [Architecture Components](#architecture-components)
4. [Event System Design](#event-system-design)
5. [UI Integration](#ui-integration)
6. [Implementation Strategy](#implementation-strategy)

## Overview

The current GoAgent framework uses a synchronous, request-response model for agent interactions. Transforming it into an event-driven architecture will provide several benefits:

- **Real-time updates**: UI components can receive and display updates as they happen
- **Decoupled components**: System components can communicate without direct dependencies
- **Extensibility**: New components can subscribe to existing events without modifying core code
- **Observability**: Events provide a natural way to trace and monitor system behavior

The design draws inspiration from the event systems in geppetto and pinocchio, which use a combination of Watermill for message passing and event publishing, along with Bubbletea for UI rendering.

## Core Concepts

### Events

Events are messages that represent something that has happened in the system. In our architecture, events will include:

- **Agent events**: Reasoning steps, decisions, and actions taken by agents
- **LLM events**: Interactions with language models, including streaming responses
- **Tool events**: Tool invocations and their results
- **Memory events**: Memory operations like storage and retrieval
- **UI events**: User interactions and display updates

### Event Bus

The event bus is the central component that facilitates communication between different parts of the system. It allows components to publish events and subscribe to events they're interested in.

### Publishers and Subscribers

- **Publishers**: Components that generate events and send them to the event bus
- **Subscribers**: Components that receive and process events from the event bus

## Architecture Components

### Event System

The event system will be based on the PublisherManager pattern from geppetto, which provides:

- Topic-based publishing and subscription
- Sequence numbering for events
- Error handling for failed publications

```go
// EventManager manages event publishing and subscription
type EventManager struct {
    publishers     map[string][]Publisher
    sequenceNumber uint64
    mutex          sync.Mutex
}

// Publisher interface for components that publish events
type Publisher interface {
    Publish(topic string, event Event) error
}

// Subscriber interface for components that subscribe to events
type Subscriber interface {
    Subscribe(topic string) (<-chan Event, error)
    Unsubscribe(topic string) error
}
```

### Event Types

We'll define a hierarchy of event types similar to geppetto's approach:

```go
// Event is the base interface for all events
type Event interface {
    Type() EventType
    Metadata() EventMetadata
    Payload() []byte
}

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
```

### Agent Component

The agent component will be refactored to use the event system:

```go
// Agent interface with event-driven methods
type Agent interface {
    // Run starts the agent with the given input and returns a channel of events
    Run(ctx context.Context, input string) (<-chan Event, error)
    
    // AddTool adds a tool to the agent
    AddTool(tool Tool)
    
    // SetMemory sets the memory for the agent
    SetMemory(memory Memory)
    
    // SetEventManager sets the event manager for the agent
    SetEventManager(manager *EventManager)
}
```

### LLM Component

The LLM component will publish events for requests and responses:

```go
// LLM interface with event-driven methods
type LLM interface {
    // Generate generates a response and publishes events
    Generate(ctx context.Context, messages []Message) (string, error)
    
    // GenerateStream generates a streaming response and publishes events
    GenerateStream(ctx context.Context, messages []Message) error
    
    // SetEventManager sets the event manager for the LLM
    SetEventManager(manager *EventManager)
}
```

### Tool Component

Tools will publish events for invocations and results:

```go
// Tool interface with event-driven methods
type Tool interface {
    // Name returns the name of the tool
    Name() string
    
    // Description returns the description of the tool
    Description() string
    
    // Execute executes the tool and publishes events
    Execute(ctx context.Context, input string) (string, error)
    
    // SetEventManager sets the event manager for the tool
    SetEventManager(manager *EventManager)
}
```

### Memory Component

Memory operations will publish events:

```go
// Memory interface with event-driven methods
type Memory interface {
    // Add adds a memory entry and publishes an event
    Add(ctx context.Context, entry MemoryEntry) error
    
    // Search searches for memory entries and publishes an event
    Search(ctx context.Context, query string, limit int) ([]MemoryEntry, error)
    
    // SetEventManager sets the event manager for the memory
    SetEventManager(manager *EventManager)
}
```

## Event System Design

### Event Manager

The EventManager will be the central component for event distribution:

```go
// NewEventManager creates a new event manager
func NewEventManager() *EventManager {
    return &EventManager{
        publishers: make(map[string][]Publisher),
    }
}

// RegisterPublisher registers a publisher for a topic
func (m *EventManager) RegisterPublisher(topic string, publisher Publisher) {
    m.mutex.Lock()
    defer m.mutex.Unlock()
    m.publishers[topic] = append(m.publishers[topic], publisher)
}

// Publish publishes an event to all registered publishers for the topic
func (m *EventManager) Publish(topic string, event Event) error {
    m.mutex.Lock()
    defer m.mutex.Unlock()
    
    // Add sequence number to event metadata
    // Convert event to bytes
    // Distribute to all publishers for the topic
    
    return nil
}

// Subscribe creates a subscription for a topic
func (m *EventManager) Subscribe(topic string) (<-chan Event, error) {
    // Create a channel for the subscription
    // Register a handler to forward events to the channel
    // Return the channel
    
    return nil, nil
}
```

### Event Implementation

Events will be implemented with a common base structure:

```go
// BaseEvent provides common functionality for all events
type BaseEvent struct {
    type_       EventType
    metadata    EventMetadata
    payload     []byte
    timestamp   time.Time
    sequenceNum uint64
}

// EventMetadata contains metadata for an event
type EventMetadata struct {
    ID        string
    ParentID  string
    AgentID   string
    SessionID string
}

// Specific event types will embed BaseEvent
type AgentThinkingEvent struct {
    BaseEvent
    Thought string
}

type LLMPartialEvent struct {
    BaseEvent
    Delta      string
    Completion string
}

type ToolCallEvent struct {
    BaseEvent
    ToolName  string
    ToolInput string
}
```

## UI Integration

### UI Adapter

The UI adapter will connect the event system to the UI framework:

```go
// UIAdapter connects the event system to the UI
type UIAdapter struct {
    eventManager *EventManager
    program      *tea.Program
}

// NewUIAdapter creates a new UI adapter
func NewUIAdapter(eventManager *EventManager, program *tea.Program) *UIAdapter {
    return &UIAdapter{
        eventManager: eventManager,
        program:      program,
    }
}

// Start starts the UI adapter
func (a *UIAdapter) Start(ctx context.Context) error {
    // Subscribe to relevant topics
    agentEvents, _ := a.eventManager.Subscribe("agent:*")
    llmEvents, _ := a.eventManager.Subscribe("llm:*")
    toolEvents, _ := a.eventManager.Subscribe("tool:*")
    
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
func (a *UIAdapter) handleAgentEvent(event Event) {
    // Convert event to UI message
    // Send to program
}

func (a *UIAdapter) handleLLMEvent(event Event) {
    // Convert event to UI message
    // Send to program
}

func (a *UIAdapter) handleToolEvent(event Event) {
    // Convert event to UI message
    // Send to program
}
```

### Bubbletea Integration

For terminal UIs, we'll integrate with Bubbletea similar to pinocchio:

```go
// Model represents the UI state
type Model struct {
    conversation []*Message
    thinking     string
    loading      bool
}

// Init initializes the model
func (m Model) Init() tea.Cmd {
    return nil
}

// Update updates the model based on messages
func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
    switch msg := msg.(type) {
    case AgentThinkingMsg:
        m.thinking = msg.Thought
        return m, nil
    case LLMPartialMsg:
        // Update conversation with partial response
        return m, nil
    case ToolCallMsg:
        // Add tool call to conversation
        return m, nil
    }
    
    return m, nil
}

// View renders the model
func (m Model) View() string {
    // Render conversation, thinking, etc.
    return ""
}
```

## Implementation Strategy

### Phase 1: Core Event System

1. Implement the EventManager
2. Define base Event interface and common event types
3. Create adapters for existing components to publish events

### Phase 2: Component Refactoring

1. Refactor Agent to use event-driven approach
2. Refactor LLM to publish events for requests and responses
3. Refactor Tools to publish events for invocations and results
4. Refactor Memory to publish events for operations

### Phase 3: UI Integration

1. Implement UI adapter for Bubbletea
2. Create message types for UI updates
3. Implement handlers for different event types

### Phase 4: API Refinement

1. Design clean, elegant top-level API
2. Create builder pattern for agent configuration
3. Implement convenience methods for common operations

## Elegant Top-Level API

The goal is to maintain an elegant, simple API while providing the power of event-driven architecture:

```go
// Example of elegant top-level API
func main() {
    // Create event manager
    eventManager := goagent.NewEventManager()
    
    // Create agent with event manager
    agent := goagent.NewReActAgent(
        goagent.WithEventManager(eventManager),
        goagent.WithLLM(openaiLLM),
        goagent.WithMaxSteps(10),
    )
    
    // Add tools
    agent.AddTool(webSearchTool)
    agent.AddTool(calculatorTool)
    
    // Set memory
    agent.SetMemory(vectorMemory)
    
    // Create UI
    ui := goagent.NewTerminalUI(eventManager)
    
    // Start UI
    go ui.Start()
    
    // Run agent
    ctx := context.Background()
    result, err := agent.Run(ctx, "Research quantum computing and summarize recent breakthroughs")
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }
    
    fmt.Println("Result:", result)
}
```

This API hides the complexity of the event system while still allowing components to communicate through events.
