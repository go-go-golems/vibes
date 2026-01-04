# Integrating Geppetto with GoAgent Framework

This guide provides comprehensive instructions for integrating the [go-go-golems/geppetto](https://github.com/go-go-golems/geppetto) LLM implementation with the GoAgent framework. Geppetto is a powerful Go library for working with large language models that provides structured conversation management, tool calls, and support for multiple LLM providers.

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Architecture Comparison](#architecture-comparison)
4. [Integration Strategy](#integration-strategy)
5. [Step-by-Step Implementation](#step-by-step-implementation)
   - [Creating a Geppetto LLM Adapter](#creating-a-geppetto-llm-adapter)
   - [Implementing Tool Call Support](#implementing-tool-call-support)
   - [Memory Integration](#memory-integration)
   - [Tracing Integration](#tracing-integration)
6. [Example: Research Agent with Geppetto](#example-research-agent-with-geppetto)
7. [Advanced Features](#advanced-features)
8. [Troubleshooting](#troubleshooting)

## Overview

Geppetto is a Go library for building LLM-powered applications with support for:

- Multiple LLM providers (OpenAI, Claude, Ollama, etc.)
- Structured conversation management
- Tool calls and function execution
- Streaming responses
- Event-based architecture

This guide will show you how to integrate Geppetto with the GoAgent framework to leverage these capabilities while maintaining the agent patterns and abstractions provided by GoAgent.

## Prerequisites

Before starting the integration, ensure you have:

- Go 1.18 or later
- GoAgent framework installed
- Geppetto library installed (`go get github.com/go-go-golems/geppetto`)
- Basic understanding of both frameworks

## Architecture Comparison

### GoAgent Architecture

The GoAgent framework is built around these core components:

- **Agent**: Implements reasoning patterns (ReAct, Plan-and-Execute)
- **LLM**: Interface for language model interactions
- **Tools**: Functions that agents can use to interact with external systems
- **Memory**: Storage for agent context and history
- **Tracing**: System for monitoring agent execution

### Geppetto Architecture

Geppetto is organized around:

- **Conversation**: Manages message history with support for different content types
- **Steps**: Processing units that handle LLM interactions and tool execution
- **Events**: Publish-subscribe system for monitoring execution
- **Settings**: Configuration for LLM providers and API calls

## Integration Strategy

The integration will focus on:

1. Creating a Geppetto adapter that implements the GoAgent LLM interface
2. Mapping GoAgent tools to Geppetto tool calls
3. Connecting Geppetto's event system to GoAgent's tracing
4. Leveraging Geppetto's conversation management with GoAgent's memory system

## Step-by-Step Implementation

### Creating a Geppetto LLM Adapter

First, we'll create an adapter that implements the GoAgent LLM interface using Geppetto:

```go
package llm

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/go-go-golems/geppetto/pkg/conversation"
	"github.com/go-go-golems/geppetto/pkg/events"
	"github.com/go-go-golems/geppetto/pkg/steps"
	"github.com/go-go-golems/geppetto/pkg/steps/ai"
	"github.com/go-go-golems/geppetto/pkg/steps/ai/openai"
	"github.com/go-go-golems/geppetto/pkg/steps/ai/settings"
	"github.com/goagent/framework/goagent/types"
	go_openai "github.com/sashabaranov/go-openai"
)

// GeppettoLLM implements the GoAgent LLM interface using Geppetto
type GeppettoLLM struct {
	apiKey     string
	model      string
	apiType    ai.ApiType
	settings   *settings.StepSettings
	pubManager *events.PublisherManager
}

// NewGeppettoLLM creates a new Geppetto LLM adapter
func NewGeppettoLLM(apiKey, model string, apiType ai.ApiType) *GeppettoLLM {
	// Create default settings
	stepSettings := settings.NewStepSettings()
	stepSettings.API.OpenAIApiKey = apiKey
	
	// Set the model and API type
	stepSettings.Chat.Engine = model
	stepSettings.Chat.ApiType = &apiType
	
	return &GeppettoLLM{
		apiKey:     apiKey,
		model:      model,
		apiType:    apiType,
		settings:   stepSettings,
		pubManager: events.NewPublisherManager(),
	}
}

// convertMessages converts GoAgent messages to Geppetto conversation messages
func (g *GeppettoLLM) convertMessages(messages []types.Message) []*conversation.Message {
	var geppettoMessages []*conversation.Message
	
	for _, msg := range messages {
		role := conversation.Role(msg.Role)
		geppettoMsg := conversation.NewChatMessage(role, msg.Content)
		geppettoMessages = append(geppettoMessages, geppettoMsg)
	}
	
	return geppettoMessages
}

// Generate implements the GoAgent LLM interface
func (g *GeppettoLLM) Generate(ctx context.Context, messages []types.Message) (string, error) {
	// Convert messages to Geppetto format
	geppettoMessages := g.convertMessages(messages)
	
	// Create a chat step
	chatStep, err := openai.NewChatStep(
		g.settings,
		openai.WithChatStepSubscriptionManager(g.pubManager),
	)
	if err != nil {
		return "", fmt.Errorf("failed to create chat step: %w", err)
	}
	
	// Execute the step
	result, err := chatStep.Start(ctx, geppettoMessages)
	if err != nil {
		return "", fmt.Errorf("failed to execute chat step: %w", err)
	}
	
	// Get the result
	stepResult := <-result.Result()
	if stepResult.Err != nil {
		return "", fmt.Errorf("chat step failed: %w", stepResult.Err)
	}
	
	return stepResult.Value, nil
}

// GenerateWithStream implements the GoAgent LLM interface with streaming
func (g *GeppettoLLM) GenerateWithStream(ctx context.Context, messages []types.Message) (<-chan string, error) {
	// Convert messages to Geppetto format
	geppettoMessages := g.convertMessages(messages)
	
	// Enable streaming
	g.settings.Chat.Stream = true
	
	// Create a chat step
	chatStep, err := openai.NewChatStep(
		g.settings,
		openai.WithChatStepSubscriptionManager(g.pubManager),
	)
	if err != nil {
		return nil, fmt.Errorf("failed to create chat step: %w", err)
	}
	
	// Execute the step
	result, err := chatStep.Start(ctx, geppettoMessages)
	if err != nil {
		return nil, fmt.Errorf("failed to execute chat step: %w", err)
	}
	
	// Create a channel for streaming responses
	outputChan := make(chan string)
	
	// Process streaming results
	go func() {
		defer close(outputChan)
		
		// Subscribe to partial completion events
		partialSub := g.pubManager.Subscribe(events.TopicPartialCompletion)
		defer g.pubManager.Unsubscribe(partialSub)
		
		for {
			select {
			case <-ctx.Done():
				return
			case msg := <-partialSub:
				if event, ok := msg.Payload.(events.PartialCompletionEvent); ok {
					outputChan <- event.Delta
				}
			case stepResult := <-result.Result():
				if stepResult.Err != nil {
					// Just log the error since we can't return it
					fmt.Printf("Error in streaming: %v\n", stepResult.Err)
				}
				return
			}
		}
	}()
	
	return outputChan, nil
}

// GenerateEmbedding implements the GoAgent LLM interface
func (g *GeppettoLLM) GenerateEmbedding(ctx context.Context, text string) ([]float32, error) {
	// Create an OpenAI client directly for embeddings
	// (Geppetto doesn't have a direct embedding step, so we use go-openai)
	client := go_openai.NewClient(g.apiKey)
	
	// Create the embedding request
	req := go_openai.EmbeddingRequest{
		Input: []string{text},
		Model: go_openai.AdaEmbeddingV2,
	}
	
	// Get the embedding
	resp, err := client.CreateEmbeddings(ctx, req)
	if err != nil {
		return nil, fmt.Errorf("failed to create embedding: %w", err)
	}
	
	if len(resp.Data) == 0 {
		return nil, fmt.Errorf("no embeddings returned")
	}
	
	return resp.Data[0].Embedding, nil
}
```

### Implementing Tool Call Support

Next, we'll extend the adapter to support tool calls:

```go
// GeppettoToolCallLLM extends GeppettoLLM with tool call support
type GeppettoToolCallLLM struct {
	*GeppettoLLM
	tools []go_openai.Tool
}

// NewGeppettoToolCallLLM creates a new Geppetto LLM adapter with tool call support
func NewGeppettoToolCallLLM(apiKey, model string, apiType ai.ApiType) *GeppettoToolCallLLM {
	return &GeppettoToolCallLLM{
		GeppettoLLM: NewGeppettoLLM(apiKey, model, apiType),
		tools:       []go_openai.Tool{},
	}
}

// AddTool adds a tool to the LLM
func (g *GeppettoToolCallLLM) AddTool(name, description string, parameters map[string]interface{}) {
	// Convert parameters to OpenAI format
	paramSchema := make(map[string]interface{})
	paramSchema["type"] = "object"
	paramSchema["properties"] = parameters
	
	requiredParams := []string{}
	for paramName, paramDef := range parameters {
		if paramDefMap, ok := paramDef.(map[string]interface{}); ok {
			if required, ok := paramDefMap["required"].(bool); ok && required {
				requiredParams = append(requiredParams, paramName)
			}
		}
	}
	paramSchema["required"] = requiredParams
	
	// Create the tool
	tool := go_openai.Tool{
		Type: go_openai.ToolTypeFunction,
		Function: go_openai.FunctionDefinition{
			Name:        name,
			Description: description,
			Parameters:  paramSchema,
		},
	}
	
	g.tools = append(g.tools, tool)
}

// GenerateWithToolCalls generates a response with potential tool calls
func (g *GeppettoToolCallLLM) GenerateWithToolCalls(ctx context.Context, messages []types.Message) (*types.ToolCallResponse, error) {
	// Convert messages to Geppetto format
	geppettoMessages := g.convertMessages(messages)
	
	// Create a chat with tools step
	chatStep, err := openai.NewChatWithToolsStep(
		g.settings,
		g.tools,
		openai.WithChatWithToolsStepSubscriptionManager(g.pubManager),
	)
	if err != nil {
		return nil, fmt.Errorf("failed to create chat with tools step: %w", err)
	}
	
	// Execute the step
	result, err := chatStep.Start(ctx, geppettoMessages)
	if err != nil {
		return nil, fmt.Errorf("failed to execute chat with tools step: %w", err)
	}
	
	// Get the result
	stepResult := <-result.Result()
	if stepResult.Err != nil {
		return nil, fmt.Errorf("chat with tools step failed: %w", stepResult.Err)
	}
	
	// Convert the result to GoAgent format
	toolCallResponse := &types.ToolCallResponse{
		Content: stepResult.Value.Content,
		Role:    stepResult.Value.Role,
	}
	
	// Convert tool calls
	for _, toolCall := range stepResult.Value.ToolCalls {
		goAgentToolCall := types.ToolCall{
			ID:   toolCall.ID,
			Name: toolCall.Function.Name,
			Arguments: toolCall.Function.Arguments,
		}
		toolCallResponse.ToolCalls = append(toolCallResponse.ToolCalls, goAgentToolCall)
	}
	
	return toolCallResponse, nil
}

// ExecuteToolCall executes a tool and returns the result
func (g *GeppettoToolCallLLM) ExecuteToolCall(ctx context.Context, toolCall types.ToolCall, executor func(name, args string) (string, error)) (string, error) {
	// Execute the tool using the provided executor
	result, err := executor(toolCall.Name, toolCall.Arguments)
	if err != nil {
		return "", fmt.Errorf("failed to execute tool %s: %w", toolCall.Name, err)
	}
	
	return result, nil
}

// GenerateWithToolCallsStream generates a response with potential tool calls and streams the results
func (g *GeppettoToolCallLLM) GenerateWithToolCallsStream(ctx context.Context, messages []types.Message) (<-chan types.ToolCallStreamEvent, error) {
	// Convert messages to Geppetto format
	geppettoMessages := g.convertMessages(messages)
	
	// Enable streaming
	g.settings.Chat.Stream = true
	
	// Create a chat with tools step
	chatStep, err := openai.NewChatWithToolsStep(
		g.settings,
		g.tools,
		openai.WithChatWithToolsStepSubscriptionManager(g.pubManager),
	)
	if err != nil {
		return nil, fmt.Errorf("failed to create chat with tools step: %w", err)
	}
	
	// Execute the step
	result, err := chatStep.Start(ctx, geppettoMessages)
	if err != nil {
		return nil, fmt.Errorf("failed to execute chat with tools step: %w", err)
	}
	
	// Create a channel for streaming responses
	outputChan := make(chan types.ToolCallStreamEvent)
	
	// Process streaming results
	go func() {
		defer close(outputChan)
		
		// Subscribe to events
		partialSub := g.pubManager.Subscribe(events.TopicPartialCompletion)
		toolCallSub := g.pubManager.Subscribe(events.TopicToolCall)
		defer g.pubManager.Unsubscribe(partialSub)
		defer g.pubManager.Unsubscribe(toolCallSub)
		
		for {
			select {
			case <-ctx.Done():
				return
			case msg := <-partialSub:
				if event, ok := msg.Payload.(events.PartialCompletionEvent); ok {
					outputChan <- types.ToolCallStreamEvent{
						Type:    "content",
						Content: event.Delta,
					}
				}
			case msg := <-toolCallSub:
				if event, ok := msg.Payload.(events.ToolCallEvent); ok {
					outputChan <- types.ToolCallStreamEvent{
						Type: "tool_call",
						ToolCall: &types.ToolCall{
							ID:        event.ToolCall.ID,
							Name:      event.ToolCall.Name,
							Arguments: event.ToolCall.Input,
						},
					}
				}
			case stepResult := <-result.Result():
				if stepResult.Err != nil {
					outputChan <- types.ToolCallStreamEvent{
						Type:  "error",
						Error: stepResult.Err.Error(),
					}
				}
				return
			}
		}
	}()
	
	return outputChan, nil
}
```

### Memory Integration

Now, let's integrate Geppetto's conversation management with GoAgent's memory system:

```go
package memory

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/go-go-golems/geppetto/pkg/conversation"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/types"
	"github.com/google/uuid"
)

// GeppettoMemory implements the GoAgent Memory interface using Geppetto's conversation system
type GeppettoMemory struct {
	llmModel    llm.LLM
	conversations map[string][]*conversation.Message
}

// NewGeppettoMemory creates a new memory system backed by Geppetto
func NewGeppettoMemory(llmModel llm.LLM) *GeppettoMemory {
	return &GeppettoMemory{
		llmModel:    llmModel,
		conversations: make(map[string][]*conversation.Message),
	}
}

// Add adds a memory entry
func (m *GeppettoMemory) Add(ctx context.Context, entry types.MemoryEntry) error {
	// Convert to Geppetto message
	var role conversation.Role
	
	// Determine role based on metadata
	if roleStr, ok := entry.Metadata["role"]; ok {
		role = conversation.Role(roleStr)
	} else {
		// Default to user role
		role = conversation.RoleUser
	}
	
	// Create Geppetto message
	msg := conversation.NewChatMessage(role, entry.Content)
	
	// Add to conversation (create if doesn't exist)
	conversationID := "default"
	if id, ok := entry.Metadata["conversation_id"]; ok {
		conversationID = id
	}
	
	if _, ok := m.conversations[conversationID]; !ok {
		m.conversations[conversationID] = []*conversation.Message{}
	}
	
	m.conversations[conversationID] = append(m.conversations[conversationID], msg)
	
	return nil
}

// Search searches for memory entries similar to the query
func (m *GeppettoMemory) Search(ctx context.Context, query string, limit int) ([]types.MemoryEntry, error) {
	// For each conversation, compute similarity with query
	var results []types.MemoryEntry
	
	// Get embedding for query
	queryEmbedding, err := m.llmModel.GenerateEmbedding(ctx, query)
	if err != nil {
		return nil, fmt.Errorf("failed to generate embedding for query: %w", err)
	}
	
	// Process each conversation
	for conversationID, messages := range m.conversations {
		for _, msg := range messages {
			// Get content as string
			content := ""
			if chatMsg, ok := msg.Content.(*conversation.ChatMessageContent); ok {
				content = chatMsg.Text
			} else {
				// Skip non-chat messages
				continue
			}
			
			// Get embedding for content
			contentEmbedding, err := m.llmModel.GenerateEmbedding(ctx, content)
			if err != nil {
				continue
			}
			
			// Compute similarity (cosine similarity)
			similarity := cosineSimilarity(queryEmbedding, contentEmbedding)
			
			// Add to results if similarity is high enough
			if similarity > 0.7 {
				// Convert to MemoryEntry
				entry := types.MemoryEntry{
					ID:      uuid.New().String(),
					Content: content,
					Metadata: map[string]string{
						"conversation_id": conversationID,
						"role":            string(msg.Content.(*conversation.ChatMessageContent).Role),
						"similarity":      fmt.Sprintf("%f", similarity),
					},
					Created: msg.Time,
				}
				
				results = append(results, entry)
			}
		}
	}
	
	// Sort by similarity (descending)
	// Note: This is a simplified implementation
	
	// Limit results
	if len(results) > limit {
		results = results[:limit]
	}
	
	return results, nil
}

// Get gets a memory entry by ID
func (m *GeppettoMemory) Get(ctx context.Context, id string) (types.MemoryEntry, error) {
	// This is a simplified implementation
	// In a real implementation, you would store IDs and look them up
	return types.MemoryEntry{}, fmt.Errorf("memory entry not found")
}

// Helper function to compute cosine similarity
func cosineSimilarity(a, b []float32) float64 {
	// Simplified implementation
	var dotProduct float64
	var normA float64
	var normB float64
	
	for i := 0; i < len(a); i++ {
		dotProduct += float64(a[i] * b[i])
		normA += float64(a[i] * a[i])
		normB += float64(b[i] * b[i])
	}
	
	return dotProduct / (math.Sqrt(normA) * math.Sqrt(normB))
}
```

### Tracing Integration

Finally, let's integrate Geppetto's event system with GoAgent's tracing:

```go
package tracing

import (
	"context"
	"sync"
	"time"

	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/go-go-golems/geppetto/pkg/events"
	"github.com/goagent/framework/goagent/types"
	"github.com/google/uuid"
)

// GeppettoTracer implements the GoAgent Tracer interface using Geppetto's event system
type GeppettoTracer struct {
	pubManager *events.PublisherManager
	events     []interface{}
	mu         sync.Mutex
}

// NewGeppettoTracer creates a new tracer backed by Geppetto's event system
func NewGeppettoTracer() *GeppettoTracer {
	tracer := &GeppettoTracer{
		pubManager: events.NewPublisherManager(),
		events:     []interface{}{},
	}
	
	// Subscribe to all events
	sub := tracer.pubManager.Subscribe(events.TopicAll)
	
	// Process events in a goroutine
	go func() {
		for msg := range sub {
			tracer.mu.Lock()
			tracer.events = append(tracer.events, msg.Payload)
			tracer.mu.Unlock()
		}
	}()
	
	return tracer
}

// StartSpan starts a new span
func (t *GeppettoTracer) StartSpan(ctx context.Context, name string) (context.Context, types.Span) {
	spanID := uuid.New()
	
	// Create span
	span := &geppettoSpan{
		id:        spanID,
		name:      name,
		startTime: time.Now(),
		tracer:    t,
	}
	
	// Add span to context
	ctx = context.WithValue(ctx, spanContextKey{}, span)
	
	// Log span start event
	t.LogEvent(ctx, types.Event{
		Type: "span_start",
		Data: map[string]interface{}{
			"span_id":   spanID.String(),
			"span_name": name,
			"timestamp": span.startTime,
		},
	})
	
	return ctx, span
}

// LogEvent logs an event
func (t *GeppettoTracer) LogEvent(ctx context.Context, event types.Event) {
	// Get span from context
	span, _ := ctx.Value(spanContextKey{}).(*geppettoSpan)
	
	// Add span ID to event if available
	if span != nil {
		if event.Data == nil {
			event.Data = make(map[string]interface{})
		}
		event.Data["span_id"] = span.id.String()
	}
	
	// Convert to Geppetto event
	geppettoEvent := events.Event{
		Type:      event.Type,
		Timestamp: time.Now(),
		Data:      event.Data,
	}
	
	// Publish event
	t.pubManager.PublishBlind(geppettoEvent)
	
	// Also store locally
	t.mu.Lock()
	t.events = append(t.events, geppettoEvent)
	t.mu.Unlock()
}

// GetEvents returns all events
func (t *GeppettoTracer) GetEvents() []interface{} {
	t.mu.Lock()
	defer t.mu.Unlock()
	
	// Return a copy to avoid race conditions
	eventsCopy := make([]interface{}, len(t.events))
	copy(eventsCopy, t.events)
	
	return eventsCopy
}

// GetPublisherManager returns the Geppetto publisher manager
func (t *GeppettoTracer) GetPublisherManager() *events.PublisherManager {
	return t.pubManager
}

// spanContextKey is used to store span in context
type spanContextKey struct{}

// geppettoSpan implements the GoAgent Span interface
type geppettoSpan struct {
	id        uuid.UUID
	name      string
	startTime time.Time
	tracer    *GeppettoTracer
}

// End ends the span
func (s *geppettoSpan) End() {
	// Log span end event
	s.tracer.LogEvent(context.Background(), types.Event{
		Type: "span_end",
		Data: map[string]interface{}{
			"span_id":   s.id.String(),
			"span_name": s.name,
			"timestamp": time.Now(),
			"duration":  time.Since(s.startTime).Milliseconds(),
		},
	})
}
```

## Example: Research Agent with Geppetto

Here's a complete example of using the Geppetto integration with a Research Agent:

```go
package main

import (
	"context"
	"fmt"
	"os"

	"github.com/go-go-golems/geppetto/pkg/steps/ai"
	"github.com/goagent/framework/goagent/agent"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/memory"
	"github.com/goagent/framework/goagent/tools"
	"github.com/goagent/framework/goagent/tracing"
	"github.com/goagent/framework/goagent/types"
)

func main() {
	// Get API key from environment
	apiKey := os.Getenv("OPENAI_API_KEY")
	if apiKey == "" {
		fmt.Println("OPENAI_API_KEY environment variable is required")
		os.Exit(1)
	}

	// Create Geppetto LLM
	geppettoLLM := llm.NewGeppettoToolCallLLM(apiKey, "gpt-4-turbo", ai.ApiTypeOpenAI)

	// Add tools
	geppettoLLM.AddTool(
		"search_web",
		"Search the web for information",
		map[string]interface{}{
			"query": map[string]interface{}{
				"type":        "string",
				"description": "The search query",
				"required":    true,
			},
		},
	)

	geppettoLLM.AddTool(
		"write_file",
		"Write content to a file",
		map[string]interface{}{
			"filename": map[string]interface{}{
				"type":        "string",
				"description": "The name of the file to write",
				"required":    true,
			},
			"content": map[string]interface{}{
				"type":        "string",
				"description": "The content to write to the file",
				"required":    true,
			},
		},
	)

	// Create memory
	geppettoMemory := memory.NewGeppettoMemory(geppettoLLM)

	// Create tracer
	geppettoTracer := tracing.NewGeppettoTracer()

	// Create ReAct agent
	reactAgent := agent.NewReActAgent(geppettoLLM, 10)

	// Add tools
	webSearchTool := &tools.WebSearchTool{}
	fileTool := &tools.FileTool{}

	reactAgent.AddTool(webSearchTool)
	reactAgent.AddTool(fileTool)

	// Set memory and tracer
	reactAgent.SetMemory(geppettoMemory)
	reactAgent.SetTracer(geppettoTracer)

	// Run the agent
	ctx := context.Background()
	result, err := reactAgent.Run(ctx, "Research the latest advancements in quantum computing and write a comprehensive article about it.")
	if err != nil {
		fmt.Printf("Error running agent: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Agent result:", result)

	// Print trace events
	fmt.Println("\nTrace events:")
	for i, event := range geppettoTracer.GetEvents() {
		fmt.Printf("%d: %+v\n", i, event)
	}
}
```

## Advanced Features

### Streaming with Tool Calls

Geppetto supports streaming responses with tool calls, which can be integrated with GoAgent:

```go
// Example of using streaming with tool calls
func streamingExample() {
	// Create Geppetto LLM with tool call support
	geppettoLLM := llm.NewGeppettoToolCallLLM(apiKey, "gpt-4-turbo", ai.ApiTypeOpenAI)
	
	// Add tools
	geppettoLLM.AddTool(
		"search_web",
		"Search the web for information",
		map[string]interface{}{
			"query": map[string]interface{}{
				"type":        "string",
				"description": "The search query",
				"required":    true,
			},
		},
	)
	
	// Create messages
	messages := []types.Message{
		{
			Role:    "system",
			Content: "You are a helpful assistant.",
		},
		{
			Role:    "user",
			Content: "What's the weather in New York?",
		},
	}
	
	// Generate with streaming
	ctx := context.Background()
	streamChan, err := geppettoLLM.GenerateWithToolCallsStream(ctx, messages)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}
	
	// Process streaming events
	for event := range streamChan {
		switch event.Type {
		case "content":
			fmt.Print(event.Content)
		case "tool_call":
			fmt.Printf("\nTool call: %s(%s)\n", event.ToolCall.Name, event.ToolCall.Arguments)
			
			// Execute tool
			result, err := executeToolCall(event.ToolCall)
			if err != nil {
				fmt.Printf("Error executing tool: %v\n", err)
				continue
			}
			
			// Add tool result to messages
			messages = append(messages, types.Message{
				Role:    "tool",
				Content: result,
			})
		case "error":
			fmt.Printf("Error: %s\n", event.Error)
		}
	}
}

// Helper function to execute tool calls
func executeToolCall(toolCall types.ToolCall) (string, error) {
	switch toolCall.Name {
	case "search_web":
		var args struct {
			Query string `json:"query"`
		}
		if err := json.Unmarshal([]byte(toolCall.Arguments), &args); err != nil {
			return "", err
		}
		
		// Simulate web search
		return fmt.Sprintf("Search results for '%s'", args.Query), nil
	default:
		return "", fmt.Errorf("unknown tool: %s", toolCall.Name)
	}
}
```

### Multiple LLM Providers

Geppetto supports multiple LLM providers, which can be used with GoAgent:

```go
// Create Claude LLM
claudeLLM := llm.NewGeppettoLLM(apiKey, "claude-3-opus-20240229", ai.ApiTypeClaude)

// Create Ollama LLM
ollamaLLM := llm.NewGeppettoLLM("", "llama3", ai.ApiTypeOllama)
```

## Troubleshooting

### Common Issues

1. **API Key Issues**
   - Ensure the API key is correctly set in the environment
   - Check that the API key has the necessary permissions

2. **Tool Call Format**
   - Geppetto expects tool calls in a specific format
   - Ensure parameters match the expected schema

3. **Streaming Issues**
   - Check that the context is properly propagated
   - Ensure all goroutines are properly managed

### Debugging Tips

1. Use the tracer to capture and analyze events:
   ```go
   events := geppettoTracer.GetEvents()
   for _, event := range events {
       fmt.Printf("%+v\n", event)
   }
   ```

2. Enable verbose logging in Geppetto:
   ```go
   os.Setenv("GEPPETTO_LOG_LEVEL", "debug")
   ```

3. Check for context cancellation issues:
   ```go
   ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
   defer cancel()
   ```

## Conclusion

This integration guide demonstrates how to combine the GoAgent framework with Geppetto's powerful LLM capabilities. By following this approach, you can leverage Geppetto's support for multiple LLM providers, structured conversation management, and tool calls while maintaining the agent patterns and abstractions provided by GoAgent.

The integration focuses on creating adapters that implement the GoAgent interfaces using Geppetto's components, allowing for a seamless integration that preserves the strengths of both frameworks.
