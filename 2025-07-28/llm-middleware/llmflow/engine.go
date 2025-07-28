package llmflow

import (
	"context"
	"fmt"
)

// Engine is the core orchestrator that manages conversation history,
// context store, and middleware chain execution.
type Engine struct {
	Client           LLMClient
	Middlewares      []InputMiddleware
	conversationHistory []Message
	contextStore     map[string]interface{}
	turnCounter      int
}

// NewEngine creates a new Engine instance
func NewEngine(client LLMClient, middlewares ...InputMiddleware) *Engine {
	return &Engine{
		Client:              client,
		Middlewares:         middlewares,
		conversationHistory: make([]Message, 0),
		contextStore:        make(map[string]interface{}),
		turnCounter:         0,
	}
}

// NextTurn assembles the chain, injects the user message, then runs it.
func (e *Engine) NextTurn(ctx context.Context, userText string) (*Turn, error) {
	// Create user message
	userMessage := Message{
		Role:    "user",
		Content: userText,
	}

	// Append to conversation history
	e.conversationHistory = append(e.conversationHistory, userMessage)

	// Create turn with current state
	turn := &Turn{
		Index:    e.turnCounter,
		Messages: make([]Message, len(e.conversationHistory)),
		Context:  e.copyContext(),
		Output:   make(map[string]interface{}),
	}

	// Copy conversation history to turn
	copy(turn.Messages, e.conversationHistory)

	// Build the handler chain: middleware1 → middleware2 → … → terminal
	terminalHandler := func(c context.Context, t *Turn) error {
		raw, err := e.Client.Infer(c, t.Messages)
		if err != nil {
			return fmt.Errorf("LLM inference failed: %w", err)
		}
		t.Output["raw"] = raw
		return nil
	}

	handler := Compose(e.Middlewares...)(terminalHandler)

	// Execute the chain end-to-end
	if err := handler(ctx, turn); err != nil {
		return nil, fmt.Errorf("middleware chain execution failed: %w", err)
	}

	// Update context store with any new artefacts from the turn
	e.updateContextStore(turn.Output)

	// Add assistant response to conversation history if available
	if assistantResponse, ok := turn.Output["assistant_message"].(string); ok {
		assistantMessage := Message{
			Role:    "assistant",
			Content: assistantResponse,
		}
		e.conversationHistory = append(e.conversationHistory, assistantMessage)
	}

	e.turnCounter++
	return turn, nil
}

// copyContext creates a deep copy of the current context store
func (e *Engine) copyContext() map[string]interface{} {
	context := make(map[string]interface{})
	for k, v := range e.contextStore {
		context[k] = v
	}
	return context
}

// updateContextStore merges new artefacts from turn output into the context store
func (e *Engine) updateContextStore(output map[string]interface{}) {
	for k, v := range output {
		// Skip raw output and assistant_message as they're not context artefacts
		if k != "raw" && k != "assistant_message" {
			e.contextStore[k] = v
		}
	}
}

// GetConversationHistory returns the current conversation history
func (e *Engine) GetConversationHistory() []Message {
	history := make([]Message, len(e.conversationHistory))
	copy(history, e.conversationHistory)
	return history
}

// GetContextStore returns a copy of the current context store
func (e *Engine) GetContextStore() map[string]interface{} {
	return e.copyContext()
}

