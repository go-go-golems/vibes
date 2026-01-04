package async

import (
	"context"
	"log"
	"sync"
	"time"
)

// Engine manages async middleware execution with strong typing
type Engine struct {
	client           LLMClient
	middlewares      []AsyncMiddleware
	conversationHistory []Message
	contextStore     *Context
	turnCounter      int
	metrics          *EngineMetrics
	mu               sync.RWMutex
	config           *EngineConfig
}

// EngineConfig provides configuration for the async engine
type EngineConfig struct {
	MaxConcurrentTurns int           `json:"max_concurrent_turns"`
	TurnTimeout        time.Duration `json:"turn_timeout"`
	EnableMetrics      bool          `json:"enable_metrics"`
	EnableTracing      bool          `json:"enable_tracing"`
	BufferSize         int           `json:"buffer_size"`
}

// EngineMetrics tracks engine-level performance metrics
type EngineMetrics struct {
	TotalTurns       int64         `json:"total_turns"`
	CompletedTurns   int64         `json:"completed_turns"`
	FailedTurns      int64         `json:"failed_turns"`
	AverageLatency   time.Duration `json:"average_latency"`
	ActiveTurns      int64         `json:"active_turns"`
	PeakConcurrency  int64         `json:"peak_concurrency"`
	mu               sync.RWMutex
}

// NewEngine creates a new async engine with configuration
func NewEngine(client LLMClient, config *EngineConfig, middlewares ...AsyncMiddleware) *Engine {
	if config == nil {
		config = &EngineConfig{
			MaxConcurrentTurns: 10,
			TurnTimeout:        30 * time.Second,
			EnableMetrics:      true,
			EnableTracing:      false,
			BufferSize:         100,
		}
	}

	return &Engine{
		client:              client,
		middlewares:         middlewares,
		conversationHistory: make([]Message, 0),
		contextStore:        NewContext(),
		turnCounter:         0,
		metrics:             &EngineMetrics{},
		config:              config,
	}
}

// NextTurnAsync processes a turn asynchronously and returns a channel
func (e *Engine) NextTurnAsync(ctx context.Context, userText string) <-chan AsyncResult {
	resultChan := make(chan AsyncResult, 1)
	
	go func() {
		defer close(resultChan)
		
		// Create turn with proper initialization
		e.mu.Lock()
		turn := NewTurn(e.turnCounter, userText)
		e.turnCounter++
		
		// Copy conversation history to turn
		turn.Messages = append([]Message{}, e.conversationHistory...)
		turn.Messages = append(turn.Messages, Message{
			Role:      RoleUser,
			Content:   userText,
			Timestamp: time.Now(),
		})
		
		// Copy context store
		turn.Context = e.copyContext()
		e.mu.Unlock()
		
		// Update metrics
		if e.config.EnableMetrics {
			e.updateMetrics(func(m *EngineMetrics) {
				m.TotalTurns++
				m.ActiveTurns++
				if m.ActiveTurns > m.PeakConcurrency {
					m.PeakConcurrency = m.ActiveTurns
				}
			})
		}
		
		// Set turn status to processing
		turn.Status = TurnStatusProcessing
		
		// Create timeout context
		turnCtx, cancel := context.WithTimeout(ctx, e.config.TurnTimeout)
		defer cancel()
		
		// Build and execute middleware chain
		result := e.executeMiddlewareChain(turnCtx, turn)
		
		// Update metrics on completion
		if e.config.EnableMetrics {
			e.updateMetrics(func(m *EngineMetrics) {
				m.ActiveTurns--
				if result.Error == nil {
					m.CompletedTurns++
				} else {
					m.FailedTurns++
				}
				
				// Update average latency
				if turn.Duration != nil {
					totalLatency := time.Duration(m.CompletedTurns+m.FailedTurns-1) * m.AverageLatency + *turn.Duration
					m.AverageLatency = totalLatency / time.Duration(m.CompletedTurns+m.FailedTurns)
				}
			})
		}
		
		// Update conversation history and context store
		if result.Error == nil {
			e.mu.Lock()
			e.conversationHistory = append(e.conversationHistory, turn.Messages...)
			e.mergeContext(turn.Context)
			e.mu.Unlock()
		}
		
		resultChan <- result
	}()
	
	return resultChan
}

// executeMiddlewareChain builds and executes the async middleware chain
func (e *Engine) executeMiddlewareChain(ctx context.Context, turn *Turn) AsyncResult {
	// Build terminal handler
	terminalHandler := func(ctx context.Context, t *Turn) <-chan AsyncResult {
		resultChan := make(chan AsyncResult, 1)
		
		go func() {
			defer close(resultChan)
			
			// Call LLM client asynchronously
			llmResultChan := e.client.InferAsync(ctx, t.Messages)
			
			select {
			case llmResult := <-llmResultChan:
				if llmResult.Error != nil {
					t.Fail(llmResult.Error, "llm_client")
					resultChan <- AsyncResult{Turn: t, Error: llmResult.Error}
					return
				}
				
				// Store LLM response
				t.Output.Raw = llmResult.Response
				if llmResult.TokenCount != nil {
					if t.Output.Metrics == nil {
						t.Output.Metrics = &Metrics{}
					}
					t.Output.Metrics.TokenCount = llmResult.TokenCount
				}
				
				// Add assistant message to conversation
				assistantMsg := Message{
					Role:      RoleAssistant,
					Content:   llmResult.Response,
					Timestamp: time.Now(),
				}
				t.Messages = append(t.Messages, assistantMsg)
				
				t.Complete()
				resultChan <- AsyncResult{Turn: t, Error: nil}
				
			case <-ctx.Done():
				err := ctx.Err()
				t.Fail(err, "context_timeout")
				resultChan <- AsyncResult{Turn: t, Error: err}
			}
		}()
		
		return resultChan
	}
	
	// Compose middleware chain
	handler := ComposeAsync(e.middlewares...)(terminalHandler)
	
	// Execute chain
	resultChan := handler(ctx, turn)
	
	// Wait for result with timeout
	select {
	case result := <-resultChan:
		return result
	case <-ctx.Done():
		err := ctx.Err()
		turn.Fail(err, "middleware_timeout")
		return AsyncResult{Turn: turn, Error: err}
	}
}

// ComposeAsync builds a single async handler from multiple middleware
func ComposeAsync(middlewares ...AsyncMiddleware) func(AsyncHandler) AsyncHandler {
	return func(terminal AsyncHandler) AsyncHandler {
		// Build chain from right to left
		handler := terminal
		for i := len(middlewares) - 1; i >= 0; i-- {
			handler = middlewares[i](handler)
		}
		return handler
	}
}

// SyncToAsync converts a synchronous middleware to async
func SyncToAsync(syncMW func(func(context.Context, *Turn) error) func(context.Context, *Turn) error) AsyncMiddleware {
	return func(next AsyncHandler) AsyncHandler {
		return func(ctx context.Context, turn *Turn) <-chan AsyncResult {
			resultChan := make(chan AsyncResult, 1)
			
			go func() {
				defer close(resultChan)
				
				// Convert async handler to sync
				syncHandler := func(ctx context.Context, t *Turn) error {
					asyncResult := <-next(ctx, t)
					return asyncResult.Error
				}
				
				// Apply sync middleware
				wrappedHandler := syncMW(syncHandler)
				
				// Execute and convert back to async
				err := wrappedHandler(ctx, turn)
				resultChan <- AsyncResult{Turn: turn, Error: err}
			}()
			
			return resultChan
		}
	}
}

// copyContext creates a deep copy of the context store
func (e *Engine) copyContext() *Context {
	newContext := NewContext()
	
	// Copy artifacts
	for id, artifact := range e.contextStore.Artifacts {
		newArtifact := *artifact // Shallow copy - consider deep copy for complex data
		newContext.Artifacts[id] = &newArtifact
	}
	
	// Copy variables
	for key, value := range e.contextStore.Variables {
		newContext.Variables[key] = value
	}
	
	// Copy flags
	for key, value := range e.contextStore.Flags {
		newContext.Flags[key] = value
	}
	
	return newContext
}

// mergeContext merges turn context back into the engine context store
func (e *Engine) mergeContext(turnContext *Context) {
	// Merge artifacts
	for id, artifact := range turnContext.Artifacts {
		e.contextStore.Artifacts[id] = artifact
	}
	
	// Merge variables (turn context takes precedence)
	for key, value := range turnContext.Variables {
		e.contextStore.Variables[key] = value
	}
	
	// Merge flags
	for key, value := range turnContext.Flags {
		e.contextStore.Flags[key] = value
	}
	
	// Append warnings
	e.contextStore.Warnings = append(e.contextStore.Warnings, turnContext.Warnings...)
}

// updateMetrics safely updates engine metrics
func (e *Engine) updateMetrics(updateFn func(*EngineMetrics)) {
	e.metrics.mu.Lock()
	defer e.metrics.mu.Unlock()
	updateFn(e.metrics)
}

// GetMetrics returns a copy of current engine metrics
func (e *Engine) GetMetrics() EngineMetrics {
	e.metrics.mu.RLock()
	defer e.metrics.mu.RUnlock()
	return *e.metrics
}

// GetConversationHistory returns a copy of the conversation history
func (e *Engine) GetConversationHistory() []Message {
	e.mu.RLock()
	defer e.mu.RUnlock()
	
	history := make([]Message, len(e.conversationHistory))
	copy(history, e.conversationHistory)
	return history
}

// GetContextStore returns a copy of the context store
func (e *Engine) GetContextStore() *Context {
	e.mu.RLock()
	defer e.mu.RUnlock()
	return e.copyContext()
}

// Shutdown gracefully shuts down the engine
func (e *Engine) Shutdown(ctx context.Context) error {
	log.Println("Shutting down async engine...")
	
	// Wait for active turns to complete or timeout
	ticker := time.NewTicker(100 * time.Millisecond)
	defer ticker.Stop()
	
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-ticker.C:
			metrics := e.GetMetrics()
			if metrics.ActiveTurns == 0 {
				log.Println("All turns completed, engine shutdown complete")
				return nil
			}
			log.Printf("Waiting for %d active turns to complete...", metrics.ActiveTurns)
		}
	}
}

