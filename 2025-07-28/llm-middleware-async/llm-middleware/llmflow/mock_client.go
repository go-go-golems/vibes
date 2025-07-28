package llmflow

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
)

// MockLLMClient is a mock implementation of LLMClient for testing
type MockLLMClient struct {
	responses map[string]string
	callCount int
}

// NewMockLLMClient creates a new mock LLM client with predefined responses
func NewMockLLMClient() *MockLLMClient {
	return &MockLLMClient{
		responses: map[string]string{
			"hello":                    "Hello! How can I help you today?",
			"what is the weather":      "I don't have access to real-time weather data, but I can help you with other questions.",
			"create a blog schema":     `{"title": "string", "content": "string", "author": "string", "publishDate": "date", "tags": ["string"]}`,
			"explain middleware":       "Middleware is a software component that sits between different parts of an application, providing services like logging, authentication, and data transformation.",
			"generate a plan":          "Here's a plan:\n1. Analyze requirements\n2. Design architecture\n3. Implement core features\n4. Test thoroughly\n5. Deploy and monitor",
			"default":                  "I understand your request. This is a mock response for testing the middleware architecture.",
		},
		callCount: 0,
	}
}

// Infer simulates an LLM inference call
func (m *MockLLMClient) Infer(ctx context.Context, msgs []Message) (string, error) {
	m.callCount++
	
	if len(msgs) == 0 {
		return "", fmt.Errorf("no messages provided")
	}

	// Get the last user message to determine response
	var lastUserMessage string
	for i := len(msgs) - 1; i >= 0; i-- {
		if msgs[i].Role == "user" {
			lastUserMessage = strings.ToLower(msgs[i].Content)
			break
		}
	}

	// Find matching response
	for keyword, response := range m.responses {
		if strings.Contains(lastUserMessage, keyword) {
			return response, nil
		}
	}

	// Return default response if no match found
	return m.responses["default"], nil
}

// GetCallCount returns the number of times Infer was called
func (m *MockLLMClient) GetCallCount() int {
	return m.callCount
}

// AddResponse adds a new keyword-response pair to the mock client
func (m *MockLLMClient) AddResponse(keyword, response string) {
	m.responses[keyword] = response
}

// SetJSONResponse sets a response that will be formatted as JSON
func (m *MockLLMClient) SetJSONResponse(keyword string, data interface{}) error {
	jsonBytes, err := json.Marshal(data)
	if err != nil {
		return err
	}
	m.responses[keyword] = string(jsonBytes)
	return nil
}

