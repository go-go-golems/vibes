package async

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"sync/atomic"
	"time"
)

// MockAsyncLLMClient implements LLMClient for testing async operations
type MockAsyncLLMClient struct {
	responses    map[string]string
	callCount    int64
	capabilities ClientCapabilities
	metrics      ClientMetrics
	latency      time.Duration
}

// NewMockAsyncLLMClient creates a new async mock client
func NewMockAsyncLLMClient() *MockAsyncLLMClient {
	return &MockAsyncLLMClient{
		responses: map[string]string{
			"hello":                    "Hello! How can I help you today?",
			"what is the weather":      "I don't have access to real-time weather data, but I can help you with other questions.",
			"create a blog schema":     `{"title": "string", "content": "string", "author": "string", "publishDate": "date", "tags": ["string"]}`,
			"explain middleware":       "Middleware is a software component that sits between different parts of an application, providing services like logging, authentication, and data transformation.",
			"generate a plan":          "Here's a plan:\n1. Analyze requirements\n2. Design architecture\n3. Implement core features\n4. Test thoroughly\n5. Deploy and monitor",
			"translate":                "Bonjour, comment allez-vous?",
			"summarize":                "This article discusses the importance of renewable energy in combating climate change.",
			"step by step":             "Let me think through this step by step:\n1. First, I need to understand the problem\n2. Then, I'll analyze the key factors\n3. Finally, I'll provide a solution\n\nThe answer is 42.",
			"json":                     `{"analysis": "The data shows a clear upward trend", "confidence": 0.85, "recommendations": ["increase investment", "monitor closely"]}`,
			"expert":                   "From a technical perspective, this requires deep analysis of the underlying algorithms and data structures.",
			"beginner":                 "Let me explain this in simple terms that anyone can understand.",
			"creative":                 "Imagine if we could approach this problem like an artist painting on a canvas...",
			"default":                  "I understand your request. This is a mock response for testing the async middleware architecture.",
		},
		capabilities: ClientCapabilities{
			MaxTokens:         4096,
			SupportedModels:   []string{"mock-gpt-4", "mock-gpt-3.5"},
			SupportsStreaming: true,
			SupportsTools:     true,
		},
		latency: 50 * time.Millisecond, // Simulate realistic latency
	}
}

// InferAsync simulates async LLM inference
func (m *MockAsyncLLMClient) InferAsync(ctx context.Context, msgs []Message) <-chan LLMResult {
	resultChan := make(chan LLMResult, 1)
	
	go func() {
		defer close(resultChan)
		
		// Increment call count
		atomic.AddInt64(&m.callCount, 1)
		atomic.AddInt64(&m.metrics.TotalRequests, 1)
		
		// Simulate processing latency
		select {
		case <-time.After(m.latency):
			// Continue processing
		case <-ctx.Done():
			atomic.AddInt64(&m.metrics.FailedRequests, 1)
			resultChan <- LLMResult{Error: ctx.Err()}
			return
		}
		
		if len(msgs) == 0 {
			err := fmt.Errorf("no messages provided")
			atomic.AddInt64(&m.metrics.FailedRequests, 1)
			resultChan <- LLMResult{Error: err}
			return
		}
		
		// Find the last user message to determine response
		var lastUserMessage string
		for i := len(msgs) - 1; i >= 0; i-- {
			if msgs[i].Role == RoleUser {
				lastUserMessage = strings.ToLower(msgs[i].Content)
				break
			}
		}
		
		// Find matching response
		response := m.responses["default"]
		for keyword, resp := range m.responses {
			if strings.Contains(lastUserMessage, keyword) {
				response = resp
				break
			}
		}
		
		// Calculate token counts (mock calculation)
		inputTokens := m.calculateTokens(msgs)
		outputTokens := len(strings.Fields(response))
		
		// Update metrics
		atomic.AddInt64(&m.metrics.SuccessfulRequests, 1)
		atomic.AddInt64(&m.metrics.TotalTokens, int64(inputTokens+outputTokens))
		
		// Update average latency
		totalRequests := atomic.LoadInt64(&m.metrics.TotalRequests)
		if totalRequests > 0 {
			currentAvg := m.metrics.AverageLatency
			newAvg := (currentAvg*time.Duration(totalRequests-1) + m.latency) / time.Duration(totalRequests)
			m.metrics.AverageLatency = newAvg
		}
		
		result := LLMResult{
			Response: response,
			TokenCount: &TokenCount{
				Input:  inputTokens,
				Output: outputTokens,
				Total:  inputTokens + outputTokens,
			},
			Metadata: map[string]interface{}{
				"model":     "mock-gpt-4",
				"timestamp": time.Now(),
				"latency":   m.latency,
			},
		}
		
		resultChan <- result
	}()
	
	return resultChan
}

// GetCapabilities returns the client's capabilities
func (m *MockAsyncLLMClient) GetCapabilities() ClientCapabilities {
	return m.capabilities
}

// GetMetrics returns current client metrics
func (m *MockAsyncLLMClient) GetMetrics() ClientMetrics {
	return ClientMetrics{
		TotalRequests:      atomic.LoadInt64(&m.metrics.TotalRequests),
		SuccessfulRequests: atomic.LoadInt64(&m.metrics.SuccessfulRequests),
		FailedRequests:     atomic.LoadInt64(&m.metrics.FailedRequests),
		AverageLatency:     m.metrics.AverageLatency,
		TotalTokens:        atomic.LoadInt64(&m.metrics.TotalTokens),
	}
}

// GetCallCount returns the number of times InferAsync was called
func (m *MockAsyncLLMClient) GetCallCount() int64 {
	return atomic.LoadInt64(&m.callCount)
}

// AddResponse adds a new keyword-response pair
func (m *MockAsyncLLMClient) AddResponse(keyword, response string) {
	m.responses[keyword] = response
}

// SetJSONResponse sets a response that will be formatted as JSON
func (m *MockAsyncLLMClient) SetJSONResponse(keyword string, data interface{}) error {
	jsonBytes, err := json.Marshal(data)
	if err != nil {
		return err
	}
	m.responses[keyword] = string(jsonBytes)
	return nil
}

// SetLatency configures the simulated processing latency
func (m *MockAsyncLLMClient) SetLatency(latency time.Duration) {
	m.latency = latency
}

// SimulateError configures the client to return errors for specific keywords
func (m *MockAsyncLLMClient) SimulateError(keyword string, err error) {
	// Store error in a special format that InferAsync can recognize
	m.responses[keyword] = fmt.Sprintf("ERROR:%s", err.Error())
}

// calculateTokens provides a mock token calculation
func (m *MockAsyncLLMClient) calculateTokens(msgs []Message) int {
	totalTokens := 0
	for _, msg := range msgs {
		// Simple approximation: 1 token per 4 characters
		totalTokens += len(msg.Content) / 4
		if totalTokens == 0 && len(msg.Content) > 0 {
			totalTokens = 1 // Minimum 1 token for non-empty content
		}
	}
	return totalTokens
}

// Reset clears all metrics and call counts
func (m *MockAsyncLLMClient) Reset() {
	atomic.StoreInt64(&m.callCount, 0)
	atomic.StoreInt64(&m.metrics.TotalRequests, 0)
	atomic.StoreInt64(&m.metrics.SuccessfulRequests, 0)
	atomic.StoreInt64(&m.metrics.FailedRequests, 0)
	atomic.StoreInt64(&m.metrics.TotalTokens, 0)
	m.metrics.AverageLatency = 0
}

