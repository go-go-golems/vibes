# Event-Driven Framework Internals: Architectural Document

## Table of Contents

1. [Introduction](#introduction)
2. [Core Architectural Principles](#core-architectural-principles)
3. [Event System Architecture](#event-system-architecture)
   - [Event Queue](#event-queue)
   - [Event Dispatcher](#event-dispatcher)
   - [Event Handlers](#event-handlers)
4. [Control Flow Through Events](#control-flow-through-events)
   - [Agent Execution Model](#agent-execution-model)
   - [Tool Call Lifecycle](#tool-call-lifecycle)
   - [LLM Interaction Pattern](#llm-interaction-pattern)
5. [Component Communication](#component-communication)
   - [Event-Based Communication](#event-based-communication)
   - [State Management](#state-management)
6. [Implementation Details](#implementation-details)
   - [Event Queue Implementation](#event-queue-implementation)
   - [Event Dispatcher Implementation](#event-dispatcher-implementation)
   - [Event Handler Registration](#event-handler-registration)
7. [Advanced Patterns](#advanced-patterns)
   - [Event Sourcing](#event-sourcing)
   - [Event-Driven State Machines](#event-driven-state-machines)
   - [Saga Pattern for Complex Workflows](#saga-pattern-for-complex-workflows)
8. [Performance Considerations](#performance-considerations)
9. [Debugging and Monitoring](#debugging-and-monitoring)
10. [Conclusion](#conclusion)

## Introduction

This architectural document describes the internal workings of an event-driven framework for LLM agents. Unlike traditional frameworks where control flow is managed through direct method calls, this architecture centers around events as the primary mechanism for communication and control flow between components.

The key insight of this architecture is that all operations—from agent reasoning to tool execution to LLM interactions—are represented as events flowing through a central event queue. Components react to these events rather than being explicitly invoked, creating a highly decoupled system where components can be added, removed, or modified without changing the core architecture.

While maintaining the same top-level abstractions (Agent, LLM, Tool, Memory), the internal implementation is radically different, with all control flow managed through event emission and handling.

## Core Architectural Principles

1. **Events as First-Class Citizens**: All system operations are represented as events.
2. **Decoupled Components**: Components communicate exclusively through events, not direct method calls.
3. **Event-Driven Control Flow**: System progression is driven by event emission and handling.
4. **Single Source of Truth**: The event queue is the single source of truth for system state.
5. **Reactive Programming Model**: Components react to events rather than being explicitly invoked.

## Event System Architecture

### Event Queue

At the heart of the architecture is the event queue, a central component responsible for:

1. **Receiving Events**: Accepting events from any component in the system.
2. **Ordering Events**: Maintaining the temporal order of events.
3. **Dispatching Events**: Delivering events to registered handlers.

The event queue is implemented as a priority queue with the following characteristics:

```go
// EventQueue is the central component for event management
type EventQueue struct {
    queue       *priorityQueue
    dispatcher  *EventDispatcher
    mutex       sync.RWMutex
    isRunning   bool
    stopChan    chan struct{}
    waitGroup   sync.WaitGroup
}

// Event represents something that happened in the system
type Event interface {
    Type() EventType
    ID() string
    ParentID() string
    Timestamp() time.Time
    Priority() int
    Payload() interface{}
    Metadata() map[string]interface{}
}

// NewEventQueue creates a new event queue
func NewEventQueue() *EventQueue {
    dispatcher := NewEventDispatcher()
    return &EventQueue{
        queue:      newPriorityQueue(),
        dispatcher: dispatcher,
        stopChan:   make(chan struct{}),
    }
}

// Enqueue adds an event to the queue
func (eq *EventQueue) Enqueue(event Event) {
    eq.mutex.Lock()
    defer eq.mutex.Unlock()
    
    eq.queue.Push(event)
}

// Start begins processing events from the queue
func (eq *EventQueue) Start() {
    eq.mutex.Lock()
    if eq.isRunning {
        eq.mutex.Unlock()
        return
    }
    eq.isRunning = true
    eq.mutex.Unlock()
    
    eq.waitGroup.Add(1)
    go eq.processEvents()
}

// Stop halts event processing
func (eq *EventQueue) Stop() {
    eq.mutex.Lock()
    if !eq.isRunning {
        eq.mutex.Unlock()
        return
    }
    eq.isRunning = false
    close(eq.stopChan)
    eq.mutex.Unlock()
    
    eq.waitGroup.Wait()
}

// processEvents continuously processes events from the queue
func (eq *EventQueue) processEvents() {
    defer eq.waitGroup.Done()
    
    for {
        select {
        case <-eq.stopChan:
            return
        default:
            eq.mutex.Lock()
            if eq.queue.Len() == 0 {
                eq.mutex.Unlock()
                time.Sleep(10 * time.Millisecond)
                continue
            }
            
            event := eq.queue.Pop().(Event)
            eq.mutex.Unlock()
            
            eq.dispatcher.Dispatch(event)
        }
    }
}
```

### Event Dispatcher

The event dispatcher is responsible for:

1. **Handler Registration**: Allowing components to register interest in specific event types.
2. **Event Routing**: Delivering events to the appropriate handlers.
3. **Concurrency Management**: Managing concurrent event handling.

```go
// EventDispatcher routes events to registered handlers
type EventDispatcher struct {
    handlers     map[EventType][]EventHandler
    middleware   []EventMiddleware
    mutex        sync.RWMutex
    errorHandler func(error)
}

// EventHandler processes events of a specific type
type EventHandler interface {
    Handle(Event) error
}

// EventMiddleware processes events before they reach handlers
type EventMiddleware interface {
    Process(Event) (Event, error)
}

// NewEventDispatcher creates a new event dispatcher
func NewEventDispatcher() *EventDispatcher {
    return &EventDispatcher{
        handlers: make(map[EventType][]EventHandler),
        errorHandler: func(err error) {
            log.Printf("Error handling event: %v", err)
        },
    }
}

// RegisterHandler registers a handler for a specific event type
func (ed *EventDispatcher) RegisterHandler(eventType EventType, handler EventHandler) {
    ed.mutex.Lock()
    defer ed.mutex.Unlock()
    
    ed.handlers[eventType] = append(ed.handlers[eventType], handler)
}

// AddMiddleware adds middleware to the processing pipeline
func (ed *EventDispatcher) AddMiddleware(middleware EventMiddleware) {
    ed.mutex.Lock()
    defer ed.mutex.Unlock()
    
    ed.middleware = append(ed.middleware, middleware)
}

// SetErrorHandler sets the function to call when an error occurs
func (ed *EventDispatcher) SetErrorHandler(handler func(error)) {
    ed.mutex.Lock()
    defer ed.mutex.Unlock()
    
    ed.errorHandler = handler
}

// Dispatch sends an event to all registered handlers
func (ed *EventDispatcher) Dispatch(event Event) {
    ed.mutex.RLock()
    handlers := ed.handlers[event.Type()]
    middleware := ed.middleware
    errorHandler := ed.errorHandler
    ed.mutex.RUnlock()
    
    // Process through middleware
    var err error
    for _, mw := range middleware {
        event, err = mw.Process(event)
        if err != nil {
            errorHandler(err)
            return
        }
    }
    
    // Dispatch to handlers
    for _, handler := range handlers {
        err = handler.Handle(event)
        if err != nil {
            errorHandler(err)
        }
    }
}
```

### Event Handlers

Event handlers are the components that react to events. They implement the `EventHandler` interface and are registered with the dispatcher for specific event types.

```go
// AgentEventHandler handles agent-related events
type AgentEventHandler struct {
    agent *Agent
}

// Handle processes an event
func (h *AgentEventHandler) Handle(event Event) error {
    switch event.Type() {
    case EventTypeToolCallResult:
        // Extract tool result from event
        result := event.Payload().(ToolCallResult)
        
        // Continue agent execution with the tool result
        return h.agent.ContinueWithToolResult(result)
    
    case EventTypeLLMResponse:
        // Extract LLM response from event
        response := event.Payload().(LLMResponse)
        
        // Continue agent execution with the LLM response
        return h.agent.ContinueWithLLMResponse(response)
    
    // Handle other event types...
    
    default:
        return nil
    }
}
```

## Control Flow Through Events

### Agent Execution Model

In this architecture, agent execution is driven entirely by events. The agent's state and progress are maintained through a series of event emissions and handlers.

Here's how a typical agent execution flow works:

1. **Initialization**: An `AgentStartEvent` is enqueued to begin execution.
2. **Thinking**: The agent handler processes the start event and enqueues an `AgentThinkingEvent`.
3. **LLM Request**: The thinking handler determines an LLM is needed and enqueues an `LLMRequestEvent`.
4. **LLM Response**: When the LLM responds, an `LLMResponseEvent` is enqueued.
5. **Tool Call Decision**: The agent processes the LLM response and enqueues a `ToolCallEvent` if needed.
6. **Tool Execution**: A tool handler processes the tool call and enqueues a `ToolCallResultEvent`.
7. **Continuation**: The agent handler processes the tool result and continues execution.
8. **Completion**: Eventually, an `AgentCompleteEvent` is enqueued to signal completion.

```go
// Agent represents an LLM agent
type Agent struct {
    id           string
    state        map[string]*AgentState
    eventQueue   *EventQueue
    mutex        sync.RWMutex
}

// AgentState represents the state of an agent execution
type AgentState struct {
    sessionID     string
    messages      []Message
    currentStep   int
    maxSteps      int
    pendingToolID string
    result        string
}

// NewAgent creates a new agent
func NewAgent(eventQueue *EventQueue) *Agent {
    agent := &Agent{
        id:         uuid.New().String(),
        state:      make(map[string]*AgentState),
        eventQueue: eventQueue,
    }
    
    // Register handlers for relevant events
    handler := &AgentEventHandler{agent: agent}
    eventQueue.dispatcher.RegisterHandler(EventTypeAgentStart, handler)
    eventQueue.dispatcher.RegisterHandler(EventTypeToolCallResult, handler)
    eventQueue.dispatcher.RegisterHandler(EventTypeLLMResponse, handler)
    
    return agent
}

// StartExecution begins a new agent execution
func (a *Agent) StartExecution(input string, maxSteps int) string {
    // Create a new session ID
    sessionID := uuid.New().String()
    
    // Initialize agent state
    a.mutex.Lock()
    a.state[sessionID] = &AgentState{
        sessionID:   sessionID,
        messages:    []Message{{Role: "user", Content: input}},
        currentStep: 0,
        maxSteps:    maxSteps,
    }
    a.mutex.Unlock()
    
    // Enqueue start event
    a.eventQueue.Enqueue(NewAgentStartEvent(a.id, sessionID, input))
    
    return sessionID
}

// ContinueWithToolResult continues execution after a tool call
func (a *Agent) ContinueWithToolResult(result ToolCallResult) error {
    a.mutex.Lock()
    state, ok := a.state[result.SessionID]
    if !ok {
        a.mutex.Unlock()
        return fmt.Errorf("no agent state found for session %s", result.SessionID)
    }
    
    // Verify this is the tool we're waiting for
    if state.pendingToolID != result.ToolCallID {
        a.mutex.Unlock()
        return fmt.Errorf("unexpected tool result: waiting for %s, got %s", 
                         state.pendingToolID, result.ToolCallID)
    }
    
    // Clear pending tool
    state.pendingToolID = ""
    
    // Add tool result to messages
    state.messages = append(state.messages, Message{
        Role:    "tool",
        Content: result.Result,
    })
    
    // Increment step counter
    state.currentStep++
    
    // Check if we've reached the maximum steps
    if state.currentStep >= state.maxSteps {
        // Complete execution
        state.result = "Maximum steps reached. Final result: " + result.Result
        a.mutex.Unlock()
        
        // Enqueue completion event
        a.eventQueue.Enqueue(NewAgentCompleteEvent(a.id, result.SessionID, "", state.result))
        return nil
    }
    
    a.mutex.Unlock()
    
    // Continue execution by requesting LLM response
    a.eventQueue.Enqueue(NewLLMRequestEvent(a.id, result.SessionID, state.messages))
    
    return nil
}

// ContinueWithLLMResponse continues execution after an LLM response
func (a *Agent) ContinueWithLLMResponse(response LLMResponse) error {
    a.mutex.Lock()
    state, ok := a.state[response.SessionID]
    if !ok {
        a.mutex.Unlock()
        return fmt.Errorf("no agent state found for session %s", response.SessionID)
    }
    
    // Add LLM response to messages
    state.messages = append(state.messages, Message{
        Role:    "assistant",
        Content: response.Content,
    })
    
    // Parse response to determine next action
    action, toolName, toolInput, isFinal := parseResponse(response.Content)
    
    if isFinal {
        // Complete execution
        state.result = response.Content
        a.mutex.Unlock()
        
        // Enqueue completion event
        a.eventQueue.Enqueue(NewAgentCompleteEvent(a.id, response.SessionID, "", state.result))
        return nil
    }
    
    if action == "tool_call" {
        // Generate tool call ID
        toolCallID := uuid.New().String()
        
        // Set pending tool
        state.pendingToolID = toolCallID
        a.mutex.Unlock()
        
        // Enqueue tool call event
        a.eventQueue.Enqueue(NewToolCallEvent(a.id, response.SessionID, toolCallID, toolName, toolInput))
        return nil
    }
    
    // Unsupported action
    a.mutex.Unlock()
    return fmt.Errorf("unsupported action: %s", action)
}
```

### Tool Call Lifecycle

Tool calls in this architecture follow a specific lifecycle:

1. **Tool Call Event**: The agent emits a `ToolCallEvent` with tool name and input.
2. **Tool Handler**: A tool handler picks up the event and executes the tool.
3. **Tool Result Event**: The tool handler emits a `ToolCallResultEvent` with the result.
4. **Agent Continuation**: The agent handler processes the result and continues execution.

```go
// ToolRegistry manages available tools
type ToolRegistry struct {
    tools        map[string]Tool
    eventQueue   *EventQueue
    mutex        sync.RWMutex
}

// Tool represents a function that agents can use
type Tool interface {
    Name() string
    Description() string
    Execute(input string) (string, error)
}

// NewToolRegistry creates a new tool registry
func NewToolRegistry(eventQueue *EventQueue) *ToolRegistry {
    registry := &ToolRegistry{
        tools:      make(map[string]Tool),
        eventQueue: eventQueue,
    }
    
    // Register handler for tool call events
    handler := &ToolEventHandler{registry: registry}
    eventQueue.dispatcher.RegisterHandler(EventTypeToolCall, handler)
    
    return registry
}

// RegisterTool adds a tool to the registry
func (r *ToolRegistry) RegisterTool(tool Tool) {
    r.mutex.Lock()
    defer r.mutex.Unlock()
    
    r.tools[tool.Name()] = tool
}

// GetTool retrieves a tool by name
func (r *ToolRegistry) GetTool(name string) (Tool, bool) {
    r.mutex.RLock()
    defer r.mutex.RUnlock()
    
    tool, ok := r.tools[name]
    return tool, ok
}

// ToolEventHandler handles tool-related events
type ToolEventHandler struct {
    registry *ToolRegistry
}

// Handle processes a tool event
func (h *ToolEventHandler) Handle(event Event) error {
    if event.Type() != EventTypeToolCall {
        return nil
    }
    
    // Extract tool call from event
    toolCall := event.Payload().(ToolCall)
    
    // Get the tool
    tool, ok := h.registry.GetTool(toolCall.ToolName)
    if !ok {
        // Tool not found, emit error event
        h.registry.eventQueue.Enqueue(NewErrorEvent(
            "tool_not_found",
            toolCall.AgentID,
            toolCall.SessionID,
            fmt.Sprintf("Tool not found: %s", toolCall.ToolName),
        ))
        return nil
    }
    
    // Execute the tool
    result, err := tool.Execute(toolCall.Input)
    if err != nil {
        // Tool execution failed, emit error event
        h.registry.eventQueue.Enqueue(NewErrorEvent(
            "tool_execution_failed",
            toolCall.AgentID,
            toolCall.SessionID,
            fmt.Sprintf("Tool execution failed: %v", err),
        ))
        return nil
    }
    
    // Emit tool result event
    h.registry.eventQueue.Enqueue(NewToolCallResultEvent(
        toolCall.AgentID,
        toolCall.SessionID,
        toolCall.ID,
        toolCall.ToolName,
        result,
    ))
    
    return nil
}
```

### LLM Interaction Pattern

LLM interactions follow a similar event-driven pattern:

1. **LLM Request Event**: The agent emits an `LLMRequestEvent` with messages.
2. **LLM Handler**: An LLM handler processes the request and calls the LLM.
3. **LLM Response Event**: The handler emits an `LLMResponseEvent` with the response.
4. **Agent Continuation**: The agent handler processes the response and continues execution.

```go
// LLMService manages LLM interactions
type LLMService struct {
    provider     LLMProvider
    eventQueue   *EventQueue
}

// LLMProvider represents an LLM API provider
type LLMProvider interface {
    Generate(messages []Message) (string, error)
    GenerateStream(messages []Message) (<-chan string, error)
}

// NewLLMService creates a new LLM service
func NewLLMService(provider LLMProvider, eventQueue *EventQueue) *LLMService {
    service := &LLMService{
        provider:   provider,
        eventQueue: eventQueue,
    }
    
    // Register handler for LLM request events
    handler := &LLMEventHandler{service: service}
    eventQueue.dispatcher.RegisterHandler(EventTypeLLMRequest, handler)
    
    return service
}

// LLMEventHandler handles LLM-related events
type LLMEventHandler struct {
    service *LLMService
}

// Handle processes an LLM event
func (h *LLMEventHandler) Handle(event Event) error {
    if event.Type() != EventTypeLLMRequest {
        return nil
    }
    
    // Extract request from event
    request := event.Payload().(LLMRequest)
    
    // Call the LLM
    response, err := h.service.provider.Generate(request.Messages)
    if err != nil {
        // LLM call failed, emit error event
        h.service.eventQueue.Enqueue(NewErrorEvent(
            "llm_call_failed",
            request.AgentID,
            request.SessionID,
            fmt.Sprintf("LLM call failed: %v", err),
        ))
        return nil
    }
    
    // Emit LLM response event
    h.service.eventQueue.Enqueue(NewLLMResponseEvent(
        request.AgentID,
        request.SessionID,
        response,
    ))
    
    return nil
}
```

## Component Communication

### Event-Based Communication

All component communication happens through events. This creates a highly decoupled system where components don't need to know about each other directly.

For example, when an agent needs to call a tool, it doesn't call the tool directly. Instead:

1. The agent emits a `ToolCallEvent`.
2. The tool registry handler picks up the event.
3. The handler finds the appropriate tool and executes it.
4. The handler emits a `ToolCallResultEvent`.
5. The agent handler picks up the result event and continues execution.

This pattern applies to all component interactions in the system.

### State Management

State in this architecture is managed in two ways:

1. **Component-Local State**: Components maintain their own state, such as the agent's conversation history.
2. **Event-Sourced State**: The sequence of events can be used to reconstruct state if needed.

The event queue acts as the single source of truth for the system. All state changes are represented as events flowing through the queue.

```go
// StateManager maintains system state
type StateManager struct {
    state        map[string]interface{}
    eventQueue   *EventQueue
    mutex        sync.RWMutex
}

// NewStateManager creates a new state manager
func NewStateManager(eventQueue *EventQueue) *StateManager {
    manager := &StateManager{
        state:      make(map[string]interface{}),
        eventQueue: eventQueue,
    }
    
    // Register handlers for state-changing events
    handler := &StateEventHandler{manager: manager}
    eventQueue.dispatcher.RegisterHandler(EventTypeAgentStart, handler)
    eventQueue.dispatcher.RegisterHandler(EventTypeAgentComplete, handler)
    eventQueue.dispatcher.RegisterHandler(EventTypeToolCall, handler)
    eventQueue.dispatcher.RegisterHandler(EventTypeToolCallResult, handler)
    
    return manager
}

// GetState retrieves state by key
func (m *StateManager) GetState(key string) (interface{}, bool) {
    m.mutex.RLock()
    defer m.mutex.RUnlock()
    
    value, ok := m.state[key]
    return value, ok
}

// SetState sets state by key
func (m *StateManager) SetState(key string, value interface{}) {
    m.mutex.Lock()
    defer m.mutex.Unlock()
    
    m.state[key] = value
}

// StateEventHandler handles state-changing events
type StateEventHandler struct {
    manager *StateManager
}

// Handle processes a state event
func (h *StateEventHandler) Handle(event Event) error {
    switch event.Type() {
    case EventTypeAgentStart:
        // Initialize agent state
        start := event.Payload().(AgentStart)
        key := fmt.Sprintf("agent:%s:session:%s", start.AgentID, start.SessionID)
        h.manager.SetState(key, &AgentState{
            sessionID:   start.SessionID,
            messages:    []Message{{Role: "user", Content: start.Input}},
            currentStep: 0,
            maxSteps:    10, // Default value
        })
    
    case EventTypeAgentComplete:
        // Update agent state with result
        complete := event.Payload().(AgentComplete)
        key := fmt.Sprintf("agent:%s:session:%s", complete.AgentID, complete.SessionID)
        if state, ok := h.manager.GetState(key); ok {
            if agentState, ok := state.(*AgentState); ok {
                agentState.result = complete.Result
                h.manager.SetState(key, agentState)
            }
        }
    
    // Handle other event types...
    }
    
    return nil
}
```

## Implementation Details

### Event Queue Implementation

The event queue is implemented as a priority queue to ensure events are processed in the correct order. Events with higher priority are processed first, and events with the same priority are processed in FIFO order.

```go
// priorityQueue implements a priority queue for events
type priorityQueue struct {
    items      []Event
    comparator func(a, b Event) bool
}

// newPriorityQueue creates a new priority queue
func newPriorityQueue() *priorityQueue {
    return &priorityQueue{
        items: []Event{},
        comparator: func(a, b Event) bool {
            // Higher priority first
            if a.Priority() != b.Priority() {
                return a.Priority() > b.Priority()
            }
            // Then by timestamp (earlier first)
            return a.Timestamp().Before(b.Timestamp())
        },
    }
}

// Push adds an item to the queue
func (pq *priorityQueue) Push(item Event) {
    pq.items = append(pq.items, item)
    pq.heapifyUp(len(pq.items) - 1)
}

// Pop removes and returns the highest priority item
func (pq *priorityQueue) Pop() interface{} {
    if len(pq.items) == 0 {
        return nil
    }
    
    item := pq.items[0]
    pq.items[0] = pq.items[len(pq.items)-1]
    pq.items = pq.items[:len(pq.items)-1]
    
    if len(pq.items) > 0 {
        pq.heapifyDown(0)
    }
    
    return item
}

// Len returns the number of items in the queue
func (pq *priorityQueue) Len() int {
    return len(pq.items)
}

// heapifyUp maintains the heap property by moving an item up
func (pq *priorityQueue) heapifyUp(index int) {
    parent := (index - 1) / 2
    
    if index > 0 && pq.comparator(pq.items[index], pq.items[parent]) {
        pq.items[index], pq.items[parent] = pq.items[parent], pq.items[index]
        pq.heapifyUp(parent)
    }
}

// heapifyDown maintains the heap property by moving an item down
func (pq *priorityQueue) heapifyDown(index int) {
    largest := index
    left := 2*index + 1
    right := 2*index + 2
    
    if left < len(pq.items) && pq.comparator(pq.items[left], pq.items[largest]) {
        largest = left
    }
    
    if right < len(pq.items) && pq.comparator(pq.items[right], pq.items[largest]) {
        largest = right
    }
    
    if largest != index {
        pq.items[index], pq.items[largest] = pq.items[largest], pq.items[index]
        pq.heapifyDown(largest)
    }
}
```

### Event Dispatcher Implementation

The event dispatcher uses a map of event types to handlers to route events to the appropriate handlers. It also supports middleware for cross-cutting concerns like logging and metrics.

```go
// EventDispatcher routes events to registered handlers
type EventDispatcher struct {
    handlers     map[EventType][]EventHandler
    middleware   []EventMiddleware
    mutex        sync.RWMutex
    errorHandler func(error)
}

// Dispatch sends an event to all registered handlers
func (ed *EventDispatcher) Dispatch(event Event) {
    ed.mutex.RLock()
    handlers := ed.handlers[event.Type()]
    middleware := ed.middleware
    errorHandler := ed.errorHandler
    ed.mutex.RUnlock()
    
    // Process through middleware
    var err error
    for _, mw := range middleware {
        event, err = mw.Process(event)
        if err != nil {
            errorHandler(err)
            return
        }
    }
    
    // Dispatch to handlers
    for _, handler := range handlers {
        err = handler.Handle(event)
        if err != nil {
            errorHandler(err)
        }
    }
}
```

### Event Handler Registration

Components register handlers for the event types they're interested in. This allows for a highly modular system where components can be added or removed without changing the core architecture.

```go
// Framework is the main entry point for the event-driven framework
type Framework struct {
    eventQueue    *EventQueue
    agentRegistry *AgentRegistry
    toolRegistry  *ToolRegistry
    llmService    *LLMService
    stateManager  *StateManager
}

// NewFramework creates a new framework instance
func NewFramework() *Framework {
    // Create event queue
    eventQueue := NewEventQueue()
    
    // Create components
    agentRegistry := NewAgentRegistry(eventQueue)
    toolRegistry := NewToolRegistry(eventQueue)
    llmService := NewLLMService(NewOpenAIProvider(), eventQueue)
    stateManager := NewStateManager(eventQueue)
    
    // Create framework
    framework := &Framework{
        eventQueue:    eventQueue,
        agentRegistry: agentRegistry,
        toolRegistry:  toolRegistry,
        llmService:    llmService,
        stateManager:  stateManager,
    }
    
    // Start event processing
    eventQueue.Start()
    
    return framework
}

// RegisterAgent adds an agent to the framework
func (f *Framework) RegisterAgent(agent Agent) {
    f.agentRegistry.RegisterAgent(agent)
}

// RegisterTool adds a tool to the framework
func (f *Framework) RegisterTool(tool Tool) {
    f.toolRegistry.RegisterTool(tool)
}

// StartAgent begins agent execution
func (f *Framework) StartAgent(agentName, input string) string {
    agent, ok := f.agentRegistry.GetAgent(agentName)
    if !ok {
        return ""
    }
    
    return agent.StartExecution(input, 10)
}

// Stop halts the framework
func (f *Framework) Stop() {
    f.eventQueue.Stop()
}
```

## Advanced Patterns

### Event Sourcing

Event sourcing is a pattern where all changes to application state are stored as a sequence of events. This allows for:

1. **Complete Audit Trail**: Every state change is recorded as an event.
2. **Time Travel**: The state at any point in time can be reconstructed.
3. **Event Replay**: Events can be replayed to reconstruct state or test new handlers.

```go
// EventStore stores and retrieves events
type EventStore struct {
    events      []Event
    mutex       sync.RWMutex
}

// NewEventStore creates a new event store
func NewEventStore() *EventStore {
    return &EventStore{
        events: []Event{},
    }
}

// Store adds an event to the store
func (es *EventStore) Store(event Event) {
    es.mutex.Lock()
    defer es.mutex.Unlock()
    
    es.events = append(es.events, event)
}

// GetEvents retrieves events matching the filter
func (es *EventStore) GetEvents(filter func(Event) bool) []Event {
    es.mutex.RLock()
    defer es.mutex.RUnlock()
    
    var result []Event
    for _, event := range es.events {
        if filter(event) {
            result = append(result, event)
        }
    }
    
    return result
}

// EventStoreMiddleware stores events in the event store
type EventStoreMiddleware struct {
    store *EventStore
}

// Process stores the event and passes it through
func (m *EventStoreMiddleware) Process(event Event) (Event, error) {
    m.store.Store(event)
    return event, nil
}
```

### Event-Driven State Machines

Complex agent behaviors can be modeled as state machines driven by events. Each state transition is triggered by an event, and each state has handlers for the events it's interested in.

```go
// StateMachine represents a state machine
type StateMachine struct {
    currentState State
    states       map[string]State
    eventQueue   *EventQueue
    mutex        sync.RWMutex
}

// State represents a state in the state machine
type State interface {
    Name() string
    Enter(context interface{})
    Exit(context interface{})
    HandleEvent(event Event, context interface{}) (string, error)
}

// NewStateMachine creates a new state machine
func NewStateMachine(initialState State, eventQueue *EventQueue) *StateMachine {
    sm := &StateMachine{
        currentState: initialState,
        states:       make(map[string]State),
        eventQueue:   eventQueue,
    }
    
    sm.states[initialState.Name()] = initialState
    
    // Register handler for events
    handler := &StateMachineEventHandler{machine: sm}
    eventQueue.dispatcher.RegisterHandler(EventTypeAll, handler)
    
    return sm
}

// AddState adds a state to the state machine
func (sm *StateMachine) AddState(state State) {
    sm.mutex.Lock()
    defer sm.mutex.Unlock()
    
    sm.states[state.Name()] = state
}

// Transition transitions to a new state
func (sm *StateMachine) Transition(stateName string, context interface{}) error {
    sm.mutex.Lock()
    defer sm.mutex.Unlock()
    
    newState, ok := sm.states[stateName]
    if !ok {
        return fmt.Errorf("state not found: %s", stateName)
    }
    
    sm.currentState.Exit(context)
    sm.currentState = newState
    sm.currentState.Enter(context)
    
    return nil
}

// StateMachineEventHandler handles events for the state machine
type StateMachineEventHandler struct {
    machine *StateMachine
    context interface{}
}

// Handle processes an event
func (h *StateMachineEventHandler) Handle(event Event) error {
    h.machine.mutex.Lock()
    currentState := h.machine.currentState
    h.machine.mutex.Unlock()
    
    nextState, err := currentState.HandleEvent(event, h.context)
    if err != nil {
        return err
    }
    
    if nextState != "" && nextState != currentState.Name() {
        return h.machine.Transition(nextState, h.context)
    }
    
    return nil
}
```

### Saga Pattern for Complex Workflows

The Saga pattern is useful for managing complex, multi-step workflows that span multiple components. Each step in the workflow is triggered by an event, and each step can emit events to trigger the next step or compensating actions if a step fails.

```go
// Saga represents a multi-step workflow
type Saga struct {
    id          string
    steps       []SagaStep
    currentStep int
    data        map[string]interface{}
    eventQueue  *EventQueue
    mutex       sync.RWMutex
}

// SagaStep represents a step in a saga
type SagaStep struct {
    Execute    func(data map[string]interface{}) (Event, error)
    Compensate func(data map[string]interface{}) (Event, error)
}

// NewSaga creates a new saga
func NewSaga(id string, eventQueue *EventQueue) *Saga {
    return &Saga{
        id:          id,
        steps:       []SagaStep{},
        currentStep: -1,
        data:        make(map[string]interface{}),
        eventQueue:  eventQueue,
    }
}

// AddStep adds a step to the saga
func (s *Saga) AddStep(step SagaStep) {
    s.mutex.Lock()
    defer s.mutex.Unlock()
    
    s.steps = append(s.steps, step)
}

// Start begins saga execution
func (s *Saga) Start() error {
    s.mutex.Lock()
    if len(s.steps) == 0 {
        s.mutex.Unlock()
        return fmt.Errorf("saga has no steps")
    }
    
    s.currentStep = 0
    step := s.steps[s.currentStep]
    s.mutex.Unlock()
    
    event, err := step.Execute(s.data)
    if err != nil {
        return s.Rollback(err)
    }
    
    if event != nil {
        s.eventQueue.Enqueue(event)
    }
    
    return nil
}

// Next moves to the next step
func (s *Saga) Next() error {
    s.mutex.Lock()
    s.currentStep++
    if s.currentStep >= len(s.steps) {
        s.mutex.Unlock()
        return nil // Saga completed
    }
    
    step := s.steps[s.currentStep]
    s.mutex.Unlock()
    
    event, err := step.Execute(s.data)
    if err != nil {
        return s.Rollback(err)
    }
    
    if event != nil {
        s.eventQueue.Enqueue(event)
    }
    
    return nil
}

// Rollback performs compensating actions
func (s *Saga) Rollback(cause error) error {
    s.mutex.Lock()
    for i := s.currentStep; i >= 0; i-- {
        step := s.steps[i]
        s.mutex.Unlock()
        
        if step.Compensate != nil {
            event, err := step.Compensate(s.data)
            if err != nil {
                // Log error but continue rollback
                log.Printf("Error during saga rollback: %v", err)
            }
            
            if event != nil {
                s.eventQueue.Enqueue(event)
            }
        }
        
        s.mutex.Lock()
    }
    s.mutex.Unlock()
    
    return cause
}
```

## Performance Considerations

### Event Prioritization

Not all events are equally important. The event queue supports prioritization to ensure critical events are processed first.

```go
// Event priorities
const (
    PriorityLow    = 0
    PriorityNormal = 1
    PriorityHigh   = 2
    PriorityCritical = 3
)

// NewErrorEvent creates a new error event with high priority
func NewErrorEvent(errorType, agentID, sessionID, message string) Event {
    return &BaseEvent{
        type_:     EventTypeError,
        metadata: map[string]interface{}{
            "error_type": errorType,
            "agent_id":   agentID,
            "session_id": sessionID,
            "message":    message,
        },
        timestamp: time.Now(),
        priority:  PriorityHigh,
    }
}
```

### Concurrent Processing

The event dispatcher can process events concurrently to improve throughput. However, this requires careful handling of shared state.

```go
// ConcurrentEventDispatcher processes events concurrently
type ConcurrentEventDispatcher struct {
    handlers     map[EventType][]EventHandler
    middleware   []EventMiddleware
    mutex        sync.RWMutex
    errorHandler func(error)
    workerPool   *WorkerPool
}

// NewConcurrentEventDispatcher creates a new concurrent event dispatcher
func NewConcurrentEventDispatcher(workerCount int) *ConcurrentEventDispatcher {
    return &ConcurrentEventDispatcher{
        handlers:    make(map[EventType][]EventHandler),
        workerPool:  NewWorkerPool(workerCount),
        errorHandler: func(err error) {
            log.Printf("Error handling event: %v", err)
        },
    }
}

// Dispatch sends an event to all registered handlers concurrently
func (ed *ConcurrentEventDispatcher) Dispatch(event Event) {
    ed.mutex.RLock()
    handlers := ed.handlers[event.Type()]
    middleware := ed.middleware
    errorHandler := ed.errorHandler
    ed.mutex.RUnlock()
    
    // Process through middleware
    var err error
    for _, mw := range middleware {
        event, err = mw.Process(event)
        if err != nil {
            errorHandler(err)
            return
        }
    }
    
    // Dispatch to handlers concurrently
    for _, handler := range handlers {
        h := handler // Capture handler for closure
        ed.workerPool.Submit(func() {
            err := h.Handle(event)
            if err != nil {
                errorHandler(err)
            }
        })
    }
}
```

### Batching

For high-throughput systems, batching events can improve performance by reducing the overhead of event processing.

```go
// BatchEventQueue batches events before processing
type BatchEventQueue struct {
    queue       *priorityQueue
    dispatcher  *EventDispatcher
    mutex       sync.RWMutex
    isRunning   bool
    stopChan    chan struct{}
    waitGroup   sync.WaitGroup
    batchSize   int
    batchDelay  time.Duration
}

// NewBatchEventQueue creates a new batch event queue
func NewBatchEventQueue(batchSize int, batchDelay time.Duration) *BatchEventQueue {
    dispatcher := NewEventDispatcher()
    return &BatchEventQueue{
        queue:      newPriorityQueue(),
        dispatcher: dispatcher,
        stopChan:   make(chan struct{}),
        batchSize:  batchSize,
        batchDelay: batchDelay,
    }
}

// processEvents continuously processes events from the queue in batches
func (eq *BatchEventQueue) processEvents() {
    defer eq.waitGroup.Done()
    
    ticker := time.NewTicker(eq.batchDelay)
    defer ticker.Stop()
    
    var batch []Event
    
    for {
        select {
        case <-eq.stopChan:
            return
        case <-ticker.C:
            if len(batch) > 0 {
                // Process batch
                for _, event := range batch {
                    eq.dispatcher.Dispatch(event)
                }
                batch = []Event{}
            }
        default:
            eq.mutex.Lock()
            if eq.queue.Len() == 0 {
                eq.mutex.Unlock()
                time.Sleep(10 * time.Millisecond)
                continue
            }
            
            event := eq.queue.Pop().(Event)
            eq.mutex.Unlock()
            
            batch = append(batch, event)
            
            if len(batch) >= eq.batchSize {
                // Process batch
                for _, event := range batch {
                    eq.dispatcher.Dispatch(event)
                }
                batch = []Event{}
            }
        }
    }
}
```

## Debugging and Monitoring

### Event Logging

Logging events is crucial for debugging and monitoring. A logging middleware can be added to the event dispatcher to log all events.

```go
// LoggingMiddleware logs events
type LoggingMiddleware struct {
    logger Logger
}

// Logger interface for logging
type Logger interface {
    Debug(format string, args ...interface{})
    Info(format string, args ...interface{})
    Warn(format string, args ...interface{})
    Error(format string, args ...interface{})
}

// NewLoggingMiddleware creates a new logging middleware
func NewLoggingMiddleware(logger Logger) *LoggingMiddleware {
    return &LoggingMiddleware{
        logger: logger,
    }
}

// Process logs the event and passes it through
func (m *LoggingMiddleware) Process(event Event) (Event, error) {
    m.logger.Debug("Event: type=%s, id=%s, parent_id=%s, timestamp=%s",
        event.Type(), event.ID(), event.ParentID(), event.Timestamp())
    return event, nil
}
```

### Event Visualization

Visualizing event flow can help understand system behavior. Events can be exported to a format suitable for visualization tools.

```go
// EventVisualizer generates visualizations of event flow
type EventVisualizer struct {
    events []Event
}

// NewEventVisualizer creates a new event visualizer
func NewEventVisualizer() *EventVisualizer {
    return &EventVisualizer{
        events: []Event{},
    }
}

// AddEvent adds an event to the visualizer
func (v *EventVisualizer) AddEvent(event Event) {
    v.events = append(v.events, event)
}

// GenerateDOT generates a DOT graph of event flow
func (v *EventVisualizer) GenerateDOT() string {
    var sb strings.Builder
    
    sb.WriteString("digraph EventFlow {\n")
    sb.WriteString("  rankdir=LR;\n")
    sb.WriteString("  node [shape=box];\n\n")
    
    // Add nodes
    for _, event := range v.events {
        sb.WriteString(fmt.Sprintf("  \"%s\" [label=\"%s\\n%s\"];\n",
            event.ID(), event.Type(), event.Timestamp().Format(time.RFC3339)))
    }
    
    sb.WriteString("\n")
    
    // Add edges
    for _, event := range v.events {
        if event.ParentID() != "" {
            sb.WriteString(fmt.Sprintf("  \"%s\" -> \"%s\";\n",
                event.ParentID(), event.ID()))
        }
    }
    
    sb.WriteString("}\n")
    
    return sb.String()
}
```

### Metrics Collection

Collecting metrics about event processing can help identify performance bottlenecks and monitor system health.

```go
// MetricsMiddleware collects metrics about event processing
type MetricsMiddleware struct {
    metrics *Metrics
}

// Metrics collects system metrics
type Metrics struct {
    eventCounts     map[EventType]int
    processingTimes map[EventType]time.Duration
    mutex           sync.RWMutex
}

// NewMetricsMiddleware creates a new metrics middleware
func NewMetricsMiddleware() *MetricsMiddleware {
    return &MetricsMiddleware{
        metrics: &Metrics{
            eventCounts:     make(map[EventType]int),
            processingTimes: make(map[EventType]time.Duration),
        },
    }
}

// Process collects metrics and passes the event through
func (m *MetricsMiddleware) Process(event Event) (Event, error) {
    startTime := time.Now()
    
    // Add start time to event metadata
    metadata := event.Metadata()
    metadata["processing_start_time"] = startTime
    
    return event, nil
}

// PostProcess collects metrics after event processing
func (m *MetricsMiddleware) PostProcess(event Event) {
    m.metrics.mutex.Lock()
    defer m.metrics.mutex.Unlock()
    
    // Increment event count
    m.metrics.eventCounts[event.Type()]++
    
    // Calculate processing time
    metadata := event.Metadata()
    if startTime, ok := metadata["processing_start_time"].(time.Time); ok {
        processingTime := time.Since(startTime)
        m.metrics.processingTimes[event.Type()] += processingTime
    }
}

// GetMetrics returns the collected metrics
func (m *MetricsMiddleware) GetMetrics() *Metrics {
    return m.metrics
}
```

## Conclusion

This architectural document has described the internal workings of an event-driven framework for LLM agents. The key insights are:

1. **Events as Control Flow**: All control flow is managed through events, creating a highly decoupled system.
2. **Event Queue as Central Component**: The event queue is the single source of truth for the system.
3. **Component Communication Through Events**: Components communicate exclusively through events, not direct method calls.
4. **State Management Through Events**: System state is managed through events and can be reconstructed from the event stream.

This architecture provides several benefits:

1. **Decoupling**: Components are highly decoupled, making the system more modular and easier to extend.
2. **Observability**: The event stream provides a complete record of system behavior, making it easier to debug and monitor.
3. **Scalability**: The event-driven approach allows for easy scaling through techniques like event prioritization and concurrent processing.
4. **Flexibility**: The architecture can accommodate a wide range of agent behaviors and tool integrations without changing the core framework.

By following the patterns and implementation details described in this document, you can build a robust, scalable, and extensible event-driven framework for LLM agents.
