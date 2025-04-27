// Package types defines common types used across the framework
package types

// AgentResponse represents a response from an agent
type AgentResponse struct {
	// Type is the type of response (e.g., "thinking", "tool_result", "final")
	Type string `json:"type"`

	// Content is the content of the response
	Content string `json:"content"`

	// ToolCall contains information about a tool call, if applicable
	ToolCall *ToolCall `json:"tool_call,omitempty"`

	// ToolResult contains the result of a tool call, if applicable
	ToolResult *ToolResult `json:"tool_result,omitempty"`
}

// ToolCall represents a call to a tool
type ToolCall struct {
	// Name is the name of the tool
	Name string `json:"name"`

	// Input is the input to the tool
	Input string `json:"input"`
}

// ToolResult represents the result of a tool call
type ToolResult struct {
	// Name is the name of the tool
	Name string `json:"name"`

	// Output is the output from the tool
	Output string `json:"output"`

	// Error is the error message, if any
	Error string `json:"error,omitempty"`
}

// MemoryEntry represents an entry in the agent's memory
type MemoryEntry struct {
	// ID is the unique identifier of the memory entry
	ID string `json:"id"`

	// Content is the content of the memory entry
	Content string `json:"content"`

	// Metadata contains additional information about the memory entry
	Metadata map[string]string `json:"metadata,omitempty"`

	// Embedding is the vector embedding of the content
	Embedding []float32 `json:"embedding,omitempty"`
}

// ParameterSchema defines the schema for a tool parameter
type ParameterSchema struct {
	// Type is the data type of the parameter
	Type string `json:"type"`

	// Description is a description of the parameter
	Description string `json:"description"`

	// Required indicates whether the parameter is required
	Required bool `json:"required"`

	// Enum is a list of allowed values for the parameter
	Enum []string `json:"enum,omitempty"`
}

// Event represents a traceable event in the agent's execution
type Event struct {
	// Type is the type of event
	Type string `json:"type"`

	// Data contains the event data
	Data interface{} `json:"data"`

	// Timestamp is the time the event occurred
	Timestamp int64 `json:"timestamp"`
}

// Span represents a traceable span in the agent's execution
type Span interface {
	// End ends the span
	End()
}

// Trace represents a trace of the agent's execution
type Trace struct {
	// ID is the unique identifier of the trace
	ID string `json:"id"`

	// Events is a list of events in the trace
	Events []Event `json:"events"`

	// Spans is a list of spans in the trace
	Spans []Span `json:"spans"`
}
