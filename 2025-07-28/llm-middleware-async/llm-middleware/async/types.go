package async

import (
	"context"
	"time"
)

// Message represents a single role/content pair for the LLM with stronger typing
type Message struct {
	Role      MessageRole `json:"role"`
	Content   string      `json:"content"`
	Timestamp time.Time   `json:"timestamp"`
	Metadata  Metadata    `json:"metadata,omitempty"`
}

// MessageRole defines valid message roles with type safety
type MessageRole string

const (
	RoleUser      MessageRole = "user"
	RoleAssistant MessageRole = "assistant"
	RoleSystem    MessageRole = "system"
	RoleTool      MessageRole = "tool"
)

// Metadata provides extensible message metadata
type Metadata map[string]interface{}

// Artifact represents a typed piece of data with versioning
type Artifact struct {
	ID          string                 `json:"id"`
	Type        ArtifactType          `json:"type"`
	Version     int                   `json:"version"`
	Data        interface{}           `json:"data"`
	Schema      string                `json:"schema,omitempty"`
	CreatedAt   time.Time             `json:"created_at"`
	UpdatedAt   time.Time             `json:"updated_at"`
	Metadata    map[string]interface{} `json:"metadata,omitempty"`
}

// ArtifactType defines the type of artifact with strong typing
type ArtifactType string

const (
	ArtifactTypeJSON     ArtifactType = "json"
	ArtifactTypeText     ArtifactType = "text"
	ArtifactTypeTemplate ArtifactType = "template"
	ArtifactTypeSchema   ArtifactType = "schema"
	ArtifactTypePersona  ArtifactType = "persona"
	ArtifactTypeExample  ArtifactType = "example"
	ArtifactTypeMetrics  ArtifactType = "metrics"
)

// Turn represents a complete interaction cycle with stronger typing
type Turn struct {
	ID           string               `json:"id"`
	Index        int                  `json:"index"`
	Messages     []Message            `json:"messages"`
	Context      *Context             `json:"context"`
	Output       *Output              `json:"output"`
	StartTime    time.Time            `json:"start_time"`
	EndTime      *time.Time           `json:"end_time,omitempty"`
	Duration     *time.Duration       `json:"duration,omitempty"`
	Status       TurnStatus           `json:"status"`
	Error        *TurnError           `json:"error,omitempty"`
}

// TurnStatus represents the current status of a turn
type TurnStatus string

const (
	TurnStatusPending    TurnStatus = "pending"
	TurnStatusProcessing TurnStatus = "processing"
	TurnStatusCompleted  TurnStatus = "completed"
	TurnStatusFailed     TurnStatus = "failed"
	TurnStatusCancelled  TurnStatus = "cancelled"
)

// Context holds the contextual state with strong typing
type Context struct {
	Artifacts map[string]*Artifact   `json:"artifacts"`
	Variables map[string]interface{} `json:"variables"`
	Flags     map[string]bool        `json:"flags"`
	Metrics   *Metrics               `json:"metrics,omitempty"`
	Warnings  []Warning              `json:"warnings,omitempty"`
}

// Output holds the results of turn processing
type Output struct {
	Raw        string                 `json:"raw"`
	Artifacts  map[string]*Artifact   `json:"artifacts"`
	Metrics    *Metrics               `json:"metrics,omitempty"`
	Metadata   map[string]interface{} `json:"metadata,omitempty"`
}

// Metrics tracks performance and usage statistics
type Metrics struct {
	ExecutionTime    time.Duration            `json:"execution_time"`
	MiddlewareTimings map[string]time.Duration `json:"middleware_timings"`
	TokenCount       *TokenCount              `json:"token_count,omitempty"`
	CacheHits        int                      `json:"cache_hits"`
	CacheMisses      int                      `json:"cache_misses"`
	ErrorCount       int                      `json:"error_count"`
}

// TokenCount tracks token usage
type TokenCount struct {
	Input  int `json:"input"`
	Output int `json:"output"`
	Total  int `json:"total"`
}

// Warning represents a non-fatal issue during processing
type Warning struct {
	Code      string    `json:"code"`
	Message   string    `json:"message"`
	Source    string    `json:"source"`
	Timestamp time.Time `json:"timestamp"`
}

// TurnError represents a detailed error with context
type TurnError struct {
	Code      string                 `json:"code"`
	Message   string                 `json:"message"`
	Source    string                 `json:"source"`
	Details   map[string]interface{} `json:"details,omitempty"`
	Timestamp time.Time              `json:"timestamp"`
	Stack     string                 `json:"stack,omitempty"`
}

// AsyncResult represents the result of an async operation
type AsyncResult struct {
	Turn  *Turn
	Error error
}

// AsyncHandler processes a turn asynchronously and returns a channel
type AsyncHandler func(ctx context.Context, turn *Turn) <-chan AsyncResult

// AsyncMiddleware wraps an AsyncHandler with additional functionality
type AsyncMiddleware func(next AsyncHandler) AsyncHandler

// LLMClient interface for async LLM operations
type LLMClient interface {
	InferAsync(ctx context.Context, msgs []Message) <-chan LLMResult
	GetCapabilities() ClientCapabilities
	GetMetrics() ClientMetrics
}

// LLMResult represents the result of an LLM inference
type LLMResult struct {
	Response   string
	TokenCount *TokenCount
	Metadata   map[string]interface{}
	Error      error
}

// ClientCapabilities describes what the LLM client supports
type ClientCapabilities struct {
	MaxTokens        int      `json:"max_tokens"`
	SupportedModels  []string `json:"supported_models"`
	SupportsStreaming bool     `json:"supports_streaming"`
	SupportsTools    bool     `json:"supports_tools"`
}

// ClientMetrics tracks client-level metrics
type ClientMetrics struct {
	TotalRequests    int64         `json:"total_requests"`
	SuccessfulRequests int64       `json:"successful_requests"`
	FailedRequests   int64         `json:"failed_requests"`
	AverageLatency   time.Duration `json:"average_latency"`
	TotalTokens      int64         `json:"total_tokens"`
}

// NewContext creates a new context with initialized maps
func NewContext() *Context {
	return &Context{
		Artifacts: make(map[string]*Artifact),
		Variables: make(map[string]interface{}),
		Flags:     make(map[string]bool),
		Warnings:  make([]Warning, 0),
	}
}

// NewOutput creates a new output with initialized maps
func NewOutput() *Output {
	return &Output{
		Artifacts: make(map[string]*Artifact),
		Metadata:  make(map[string]interface{}),
	}
}

// NewTurn creates a new turn with proper initialization
func NewTurn(index int, userMessage string) *Turn {
	return &Turn{
		ID:        generateTurnID(),
		Index:     index,
		Messages:  []Message{{Role: RoleUser, Content: userMessage, Timestamp: time.Now()}},
		Context:   NewContext(),
		Output:    NewOutput(),
		StartTime: time.Now(),
		Status:    TurnStatusPending,
	}
}

// AddWarning adds a warning to the context
func (c *Context) AddWarning(code, message, source string) {
	c.Warnings = append(c.Warnings, Warning{
		Code:      code,
		Message:   message,
		Source:    source,
		Timestamp: time.Now(),
	})
}

// SetFlag sets a boolean flag in the context
func (c *Context) SetFlag(key string, value bool) {
	c.Flags[key] = value
}

// GetFlag gets a boolean flag from the context
func (c *Context) GetFlag(key string) bool {
	return c.Flags[key]
}

// SetVariable sets a variable in the context
func (c *Context) SetVariable(key string, value interface{}) {
	c.Variables[key] = value
}

// GetVariable gets a variable from the context
func (c *Context) GetVariable(key string) (interface{}, bool) {
	value, exists := c.Variables[key]
	return value, exists
}

// SetArtifact stores an artifact in the context
func (c *Context) SetArtifact(artifact *Artifact) {
	c.Artifacts[artifact.ID] = artifact
}

// GetArtifact retrieves an artifact from the context
func (c *Context) GetArtifact(id string) (*Artifact, bool) {
	artifact, exists := c.Artifacts[id]
	return artifact, exists
}

// Complete marks the turn as completed and calculates duration
func (t *Turn) Complete() {
	now := time.Now()
	t.EndTime = &now
	duration := now.Sub(t.StartTime)
	t.Duration = &duration
	t.Status = TurnStatusCompleted
}

// Fail marks the turn as failed with an error
func (t *Turn) Fail(err error, source string) {
	now := time.Now()
	t.EndTime = &now
	duration := now.Sub(t.StartTime)
	t.Duration = &duration
	t.Status = TurnStatusFailed
	t.Error = &TurnError{
		Code:      "TURN_FAILED",
		Message:   err.Error(),
		Source:    source,
		Timestamp: now,
	}
}

// generateTurnID generates a unique ID for a turn
func generateTurnID() string {
	// Simple implementation - in production, use UUID or similar
	return time.Now().Format("20060102150405.000000")
}

