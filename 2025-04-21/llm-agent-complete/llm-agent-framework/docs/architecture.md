# GoAgent Framework Architecture

## Overview

GoAgent is an advanced LLM agent framework written in Go, designed to provide a flexible, extensible, and high-performance foundation for building AI agents. The framework incorporates modern agent patterns, parallel tool execution, streaming responses, and vector-based memory capabilities.

## Core Design Principles

1. **Modularity**: Components are designed to be loosely coupled and independently replaceable
2. **Type Safety**: Leveraging Go's strong typing for reliable agent behavior
3. **Concurrency**: Native support for parallel tool execution and streaming responses
4. **Extensibility**: Easy to extend with new tools, LLM providers, and memory systems
5. **Testability**: Built-in mocking capabilities for all components
6. **Performance**: Optimized for low latency and high throughput

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        GoAgent Framework                         │
├─────────────┬─────────────┬─────────────┬─────────────┬─────────┤
│    Agent    │     LLM     │    Tools    │   Memory    │ Tracing │
│   Patterns  │  Providers  │             │   Systems   │         │
├─────────────┼─────────────┼─────────────┼─────────────┼─────────┤
│  - ReAct    │  - Mock     │ - WebSearch │ - Vector DB │ - Logs  │
│  - Plan &   │  - OpenAI   │ - FileOps   │ - In-Memory │ - Spans │
│    Execute  │  - Anthropic│ - APIs      │ - Persistent│ - Events│
│  - CoT      │  - Custom   │ - Custom    │ - Custom    │         │
└─────────────┴─────────────┴─────────────┴─────────────┴─────────┘
```

## Core Components

### 1. Agent Core

The Agent Core is responsible for orchestrating the interaction between the LLM, tools, and memory systems. It implements different agent patterns and manages the agent's state.

```go
// Agent interface defines the core functionality of an agent
type Agent interface {
    // Run executes the agent with the given input and returns the result
    Run(ctx context.Context, input string) (string, error)
    
    // RunWithStream executes the agent with streaming response
    RunWithStream(ctx context.Context, input string) (<-chan AgentResponse, error)
    
    // AddTool adds a tool to the agent
    AddTool(tool Tool) error
    
    // SetMemory sets the memory system for the agent
    SetMemory(memory Memory) error
}
```

### 2. LLM Providers

LLM Providers are responsible for interacting with language models. The framework includes a mock LLM provider for testing and development.

```go
// LLM interface defines the functionality of a language model
type LLM interface {
    // Generate generates a response for the given messages
    Generate(ctx context.Context, messages []Message) (string, error)
    
    // GenerateWithStream generates a streaming response
    GenerateWithStream(ctx context.Context, messages []Message) (<-chan string, error)
    
    // GenerateEmbedding generates an embedding for the given text
    GenerateEmbedding(ctx context.Context, text string) ([]float32, error)
}
```

### 3. Tools

Tools extend the agent's capabilities by allowing it to interact with external systems and perform specific tasks.

```go
// Tool interface defines the functionality of a tool
type Tool interface {
    // Name returns the name of the tool
    Name() string
    
    // Description returns the description of the tool
    Description() string
    
    // Execute executes the tool with the given input
    Execute(ctx context.Context, input string) (string, error)
    
    // Parameters returns the parameters schema of the tool
    Parameters() map[string]ParameterSchema
}
```

### 4. Memory Systems

Memory Systems provide persistence and retrieval capabilities for the agent, including vector-based similarity search.

```go
// Memory interface defines the functionality of a memory system
type Memory interface {
    // Add adds a memory to the system
    Add(ctx context.Context, memory MemoryEntry) error
    
    // Get retrieves memories by ID
    Get(ctx context.Context, ids []string) ([]MemoryEntry, error)
    
    // Search searches for similar memories
    Search(ctx context.Context, query string, limit int) ([]MemoryEntry, error)
    
    // Clear clears all memories
    Clear(ctx context.Context) error
}
```

### 5. Tracing

Tracing provides observability into the agent's execution, capturing logs, spans, and events for debugging and analysis.

```go
// Tracer interface defines the functionality of a tracer
type Tracer interface {
    // StartSpan starts a new span
    StartSpan(ctx context.Context, name string) (context.Context, Span)
    
    // LogEvent logs an event
    LogEvent(ctx context.Context, event Event)
    
    // GetTrace returns the trace for the given context
    GetTrace(ctx context.Context) Trace
}
```

## Advanced Features

### Parallel Tool Execution

The framework supports executing multiple tools in parallel using Go's goroutines and channels:

```go
// ToolExecutor executes tools in parallel
type ToolExecutor struct {
    tools []Tool
}

// ExecuteParallel executes multiple tools in parallel
func (e *ToolExecutor) ExecuteParallel(ctx context.Context, requests []ToolRequest) []ToolResponse {
    responses := make([]ToolResponse, len(requests))
    var wg sync.WaitGroup
    
    for i, req := range requests {
        wg.Add(1)
        go func(i int, req ToolRequest) {
            defer wg.Done()
            tool := e.findTool(req.ToolName)
            result, err := tool.Execute(ctx, req.Input)
            responses[i] = ToolResponse{
                ToolName: req.ToolName,
                Result:   result,
                Error:    err,
            }
        }(i, req)
    }
    
    wg.Wait()
    return responses
}
```

### Streaming Responses

The framework supports streaming responses from LLMs and agents using Go channels:

```go
// StreamingAgent provides streaming responses
type StreamingAgent struct {
    Agent
    llm LLM
}

// RunWithStream runs the agent with streaming response
func (a *StreamingAgent) RunWithStream(ctx context.Context, input string) (<-chan AgentResponse, error) {
    responseChan := make(chan AgentResponse)
    
    go func() {
        defer close(responseChan)
        
        // Process agent steps and send responses as they become available
        for {
            // Get next step from LLM
            stepChan, _ := a.llm.GenerateWithStream(ctx, a.buildPrompt())
            
            // Process step and send response
            for chunk := range stepChan {
                responseChan <- AgentResponse{
                    Type:    "thinking",
                    Content: chunk,
                }
            }
            
            // Execute tool if needed
            if toolCall := a.extractToolCall(); toolCall != nil {
                result, _ := a.executeTool(ctx, toolCall)
                responseChan <- AgentResponse{
                    Type:    "tool_result",
                    Content: result,
                }
            }
            
            // Check if agent is done
            if a.isDone() {
                break
            }
        }
        
        // Send final response
        responseChan <- AgentResponse{
            Type:    "final",
            Content: a.getFinalResponse(),
        }
    }()
    
    return responseChan, nil
}
```

### Vector Memory

The framework integrates with chromem-go for vector-based memory capabilities:

```go
// VectorMemory implements the Memory interface using chromem-go
type VectorMemory struct {
    db        *chromem.DB
    collection *chromem.Collection
}

// Search searches for similar memories
func (m *VectorMemory) Search(ctx context.Context, query string, limit int) ([]MemoryEntry, error) {
    results, err := m.collection.Query(ctx, query, limit, nil, nil)
    if err != nil {
        return nil, err
    }
    
    entries := make([]MemoryEntry, len(results.IDs))
    for i, id := range results.IDs {
        entries[i] = MemoryEntry{
            ID:      id,
            Content: results.Documents[i],
            Metadata: results.Metadatas[i],
        }
    }
    
    return entries, nil
}
```

## Agent Patterns Implementation

### ReAct Pattern

```go
// ReActAgent implements the ReAct pattern
type ReActAgent struct {
    llm    LLM
    tools  []Tool
    memory Memory
    tracer Tracer
}

// Run executes the agent with the ReAct pattern
func (a *ReActAgent) Run(ctx context.Context, input string) (string, error) {
    ctx, span := a.tracer.StartSpan(ctx, "ReActAgent.Run")
    defer span.End()
    
    messages := []Message{
        {Role: "system", Content: a.buildSystemPrompt()},
        {Role: "user", Content: input},
    }
    
    for i := 0; i < maxIterations; i++ {
        // Get next step from LLM
        response, err := a.llm.Generate(ctx, messages)
        if err != nil {
            return "", err
        }
        
        // Extract thought, action, and action input
        thought, action, actionInput := a.parseResponse(response)
        
        // Log thought
        a.tracer.LogEvent(ctx, Event{
            Type: "thought",
            Data: thought,
        })
        
        // Check if final answer
        if action == "final_answer" {
            return actionInput, nil
        }
        
        // Execute tool
        tool := a.findTool(action)
        if tool == nil {
            messages = append(messages, Message{
                Role: "assistant",
                Content: response,
            })
            messages = append(messages, Message{
                Role: "user",
                Content: "Error: Tool not found. Please use one of the available tools.",
            })
            continue
        }
        
        result, err := tool.Execute(ctx, actionInput)
        if err != nil {
            result = "Error: " + err.Error()
        }
        
        // Log tool execution
        a.tracer.LogEvent(ctx, Event{
            Type: "tool_execution",
            Data: map[string]string{
                "tool":   action,
                "input":  actionInput,
                "result": result,
            },
        })
        
        // Add to messages
        messages = append(messages, Message{
            Role: "assistant",
            Content: response,
        })
        messages = append(messages, Message{
            Role: "user",
            Content: "Observation: " + result,
        })
    }
    
    return "Agent exceeded maximum iterations", nil
}
```

### Plan-and-Execute Pattern

```go
// PlanAndExecuteAgent implements the Plan-and-Execute pattern
type PlanAndExecuteAgent struct {
    planner  LLM
    executor LLM
    tools    []Tool
    memory   Memory
    tracer   Tracer
}

// Run executes the agent with the Plan-and-Execute pattern
func (a *PlanAndExecuteAgent) Run(ctx context.Context, input string) (string, error) {
    ctx, span := a.tracer.StartSpan(ctx, "PlanAndExecuteAgent.Run")
    defer span.End()
    
    // Generate plan
    planMessages := []Message{
        {Role: "system", Content: a.buildPlannerPrompt()},
        {Role: "user", Content: input},
    }
    
    planResponse, err := a.planner.Generate(ctx, planMessages)
    if err != nil {
        return "", err
    }
    
    // Parse plan into steps
    steps := a.parsePlan(planResponse)
    
    // Log plan
    a.tracer.LogEvent(ctx, Event{
        Type: "plan",
        Data: steps,
    })
    
    // Execute each step
    results := make([]string, len(steps))
    for i, step := range steps {
        // Build executor prompt
        executorMessages := []Message{
            {Role: "system", Content: a.buildExecutorPrompt()},
            {Role: "user", Content: fmt.Sprintf("Plan: %s\nCurrent step: %s\nPrevious results: %s", 
                planResponse, step, strings.Join(results[:i], "\n"))},
        }
        
        // Get executor response
        executorResponse, err := a.executor.Generate(ctx, executorMessages)
        if err != nil {
            return "", err
        }
        
        // Extract action and action input
        action, actionInput := a.parseExecutorResponse(executorResponse)
        
        // Execute tool
        tool := a.findTool(action)
        if tool == nil {
            results[i] = "Error: Tool not found"
            continue
        }
        
        result, err := tool.Execute(ctx, actionInput)
        if err != nil {
            result = "Error: " + err.Error()
        }
        
        // Store result
        results[i] = result
        
        // Log step execution
        a.tracer.LogEvent(ctx, Event{
            Type: "step_execution",
            Data: map[string]string{
                "step":   step,
                "tool":   action,
                "input":  actionInput,
                "result": result,
            },
        })
    }
    
    // Generate final answer
    finalMessages := []Message{
        {Role: "system", Content: a.buildFinalizerPrompt()},
        {Role: "user", Content: fmt.Sprintf("Input: %s\nPlan: %s\nResults: %s", 
            input, planResponse, strings.Join(results, "\n"))},
    }
    
    finalResponse, err := a.planner.Generate(ctx, finalMessages)
    if err != nil {
        return "", err
    }
    
    return finalResponse, nil
}
```

## Mock Implementations

### Mock LLM

```go
// MockLLM implements the LLM interface for testing
type MockLLM struct {
    responses       map[string]string
    streamResponses map[string][]string
    embeddings      map[string][]float32
}

// NewMockLLM creates a new MockLLM
func NewMockLLM() *MockLLM {
    return &MockLLM{
        responses:       make(map[string]string),
        streamResponses: make(map[string][]string),
        embeddings:      make(map[string][]float32),
    }
}

// AddResponse adds a response for a given input
func (m *MockLLM) AddResponse(input, response string) {
    m.responses[input] = response
}

// AddStreamResponse adds a streaming response for a given input
func (m *MockLLM) AddStreamResponse(input string, chunks []string) {
    m.streamResponses[input] = chunks
}

// AddEmbedding adds an embedding for a given text
func (m *MockLLM) AddEmbedding(text string, embedding []float32) {
    m.embeddings[text] = embedding
}

// Generate generates a response for the given messages
func (m *MockLLM) Generate(ctx context.Context, messages []Message) (string, error) {
    key := m.messagesKey(messages)
    if response, ok := m.responses[key]; ok {
        return response, nil
    }
    return "I don't know how to respond to that.", nil
}

// GenerateWithStream generates a streaming response
func (m *MockLLM) GenerateWithStream(ctx context.Context, messages []Message) (<-chan string, error) {
    key := m.messagesKey(messages)
    chunks := m.streamResponses[key]
    if chunks == nil {
        chunks = []string{"I don't know how to respond to that."}
    }
    
    ch := make(chan string)
    go func() {
        defer close(ch)
        for _, chunk := range chunks {
            select {
            case <-ctx.Done():
                return
            case ch <- chunk:
                time.Sleep(100 * time.Millisecond)
            }
        }
    }()
    
    return ch, nil
}

// GenerateEmbedding generates an embedding for the given text
func (m *MockLLM) GenerateEmbedding(ctx context.Context, text string) ([]float32, error) {
    if embedding, ok := m.embeddings[text]; ok {
        return embedding, nil
    }
    // Generate random embedding if not found
    embedding := make([]float32, 384)
    for i := range embedding {
        embedding[i] = rand.Float32()
    }
    return embedding, nil
}

// messagesKey generates a key for the given messages
func (m *MockLLM) messagesKey(messages []Message) string {
    var sb strings.Builder
    for _, msg := range messages {
        sb.WriteString(msg.Role)
        sb.WriteString(": ")
        sb.WriteString(msg.Content)
        sb.WriteString("\n")
    }
    return sb.String()
}
```

### Mock Tool

```go
// MockTool implements the Tool interface for testing
type MockTool struct {
    name        string
    description string
    responses   map[string]string
    parameters  map[string]ParameterSchema
}

// NewMockTool creates a new MockTool
func NewMockTool(name, description string) *MockTool {
    return &MockTool{
        name:        name,
        description: description,
        responses:   make(map[string]string),
        parameters:  make(map[string]ParameterSchema),
    }
}

// AddResponse adds a response for a given input
func (t *MockTool) AddResponse(input, response string) {
    t.responses[input] = response
}

// AddParameter adds a parameter schema
func (t *MockTool) AddParameter(name string, schema ParameterSchema) {
    t.parameters[name] = schema
}

// Name returns the name of the tool
func (t *MockTool) Name() string {
    return t.name
}

// Description returns the description of the tool
func (t *MockTool) Description() string {
    return t.description
}

// Execute executes the tool with the given input
func (t *MockTool) Execute(ctx context.Context, input string) (string, error) {
    if response, ok := t.responses[input]; ok {
        return response, nil
    }
    return "No response configured for this input", nil
}

// Parameters returns the parameters schema of the tool
func (t *MockTool) Parameters() map[string]ParameterSchema {
    return t.parameters
}
```

### Mock Memory

```go
// MockMemory implements the Memory interface for testing
type MockMemory struct {
    entries  map[string]MemoryEntry
    searches map[string][]MemoryEntry
}

// NewMockMemory creates a new MockMemory
func NewMockMemory() *MockMemory {
    return &MockMemory{
        entries:  make(map[string]MemoryEntry),
        searches: make(map[string][]MemoryEntry),
    }
}

// Add adds a memory to the system
func (m *MockMemory) Add(ctx context.Context, memory MemoryEntry) error {
    m.entries[memory.ID] = memory
    return nil
}

// Get retrieves memories by ID
func (m *MockMemory) Get(ctx context.Context, ids []string) ([]MemoryEntry, error) {
    var entries []MemoryEntry
    for _, id := range ids {
        if entry, ok := m.entries[id]; ok {
            entries = append(entries, entry)
        }
    }
    return entries, nil
}

// Search searches for similar memories
func (m *MockMemory) Search(ctx context.Context, query string, limit int) ([]MemoryEntry, error) {
    if entries, ok := m.searches[query]; ok {
        if len(entries) > limit {
            return entries[:limit], nil
        }
        return entries, nil
    }
    return []MemoryEntry{}, nil
}

// AddSearchResult adds a search result for a given query
func (m *MockMemory) AddSearchResult(query string, entries []MemoryEntry) {
    m.searches[query] = entries
}

// Clear clears all memories
func (m *MockMemory) Clear(ctx context.Context) error {
    m.entries = make(map[string]MemoryEntry)
    m.searches = make(map[string][]MemoryEntry)
    return nil
}
```

## Conclusion

This architecture provides a solid foundation for building advanced LLM agents in Go. The modular design allows for easy extension and customization, while the built-in support for parallel tool execution, streaming responses, and vector-based memory enables the creation of sophisticated agent applications.

The mock implementations facilitate testing and development without requiring external services, making it easy to get started with the framework.
