package llm

import (
	"context"
	"sync"

	"github.com/go-go-golems/geppetto/pkg/conversation"
)

// MockLLM implements the LLM interface for testing
type MockLLM struct {
	mu              sync.RWMutex
	responses       map[string]string
	streamResponses map[string][]string
	embeddings      map[string][]float32
}

var _ LLM = &MockLLM{}

// NewMockLLM creates a new MockLLM
func NewMockLLM() *MockLLM {
	return &MockLLM{
		responses:       make(map[string]string),
		streamResponses: make(map[string][]string),
		embeddings:      make(map[string][]float32),
	}
}

// AddResponse adds a response for a given input. The input key is generated from a list of messages.
func (m *MockLLM) AddResponse(messages []*conversation.Message, response string) {
	m.mu.Lock()
	defer m.mu.Unlock()

	key := messagesToKey(messages)
	m.responses[key] = response
}

// AddStreamResponse adds a streaming response for a given input. The input key is generated from a list of messages.
func (m *MockLLM) AddStreamResponse(messages []*conversation.Message, chunks []string) {
	m.mu.Lock()
	defer m.mu.Unlock()

	key := messagesToKey(messages)
	m.streamResponses[key] = chunks
}

// AddEmbedding adds an embedding for a given text
func (m *MockLLM) AddEmbedding(text string, embedding []float32) {
	m.mu.Lock()
	defer m.mu.Unlock()

	m.embeddings[text] = embedding
}

// Generate generates a response to the given messages
func (m *MockLLM) Generate(ctx context.Context, messages []*conversation.Message) (string, error) {
	m.mu.RLock()
	defer m.mu.RUnlock()

	// Create a key from the messages
	key := messagesToKey(messages)

	// Check if we have a response for this input
	if response, ok := m.responses[key]; ok {
		return response, nil
	}

	// Default response
	return "I don't know how to respond to that.", nil
}

// GenerateWithStream generates a response to the given messages with streaming
func (m *MockLLM) GenerateWithStream(ctx context.Context, messages []*conversation.Message) (<-chan string, error) {
	m.mu.RLock()
	defer m.mu.RUnlock()

	// Create a key from the messages
	key := messagesToKey(messages)

	// Create a channel for the response
	responseChan := make(chan string)

	go func() {
		defer close(responseChan)

		// Check if we have a streaming response for this input
		if chunks, ok := m.streamResponses[key]; ok {
			for _, chunk := range chunks {
				select {
				case <-ctx.Done():
					return
				case responseChan <- chunk:
				}
			}
			return
		}

		// Check if we have a non-streaming response for this input
		if response, ok := m.responses[key]; ok {
			select {
			case <-ctx.Done():
				return
			case responseChan <- response:
			}
			return
		}

		// Default response
		select {
		case <-ctx.Done():
			return
		case responseChan <- "I don't know how to respond to that.":
		}
	}()

	return responseChan, nil
}

// GenerateEmbedding generates an embedding for the given text
func (m *MockLLM) GenerateEmbedding(ctx context.Context, text string) ([]float32, error) {
	m.mu.RLock()
	defer m.mu.RUnlock()

	// Check if we have an embedding for this text
	if embedding, ok := m.embeddings[text]; ok {
		return embedding, nil
	}

	// Default embedding (32-dimensional vector of 0.1)
	embedding := make([]float32, 32)
	for i := range embedding {
		embedding[i] = 0.1
	}

	return embedding, nil
}

// messagesToKey converts a slice of messages to a string key
func messagesToKey(messages []*conversation.Message) string {
	var key string
	for _, msg := range messages {
		if msg.Content.ContentType() == conversation.ContentTypeChatMessage {
			chatContent := msg.Content.(*conversation.ChatMessageContent)
			key += string(chatContent.Role) + ": " + chatContent.Text + "\n"
		} else {
			// Handle other content types if necessary, or skip them
			key += string(msg.Content.ContentType()) + ": [non-text content]\n"
		}
	}
	return key
}
