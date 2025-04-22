// Package tools defines interfaces and implementations for agent tools
package tools

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/goagent/framework/goagent/types"
)

// Tool interface defines the functionality of a tool
type Tool interface {
	// Name returns the name of the tool
	Name() string
	
	// Description returns the description of the tool
	Description() string
	
	// Execute executes the tool with the given input
	Execute(ctx context.Context, input string) (string, error)
	
	// Parameters returns the parameters schema of the tool
	Parameters() map[string]types.ParameterSchema
}

// ToolExecutor executes tools in parallel
type ToolExecutor struct {
	tools map[string]Tool
}

// NewToolExecutor creates a new ToolExecutor
func NewToolExecutor() *ToolExecutor {
	return &ToolExecutor{
		tools: make(map[string]Tool),
	}
}

// AddTool adds a tool to the executor
func (e *ToolExecutor) AddTool(tool Tool) {
	e.tools[tool.Name()] = tool
}

// GetTool returns a tool by name
func (e *ToolExecutor) GetTool(name string) Tool {
	return e.tools[name]
}

// GetAllTools returns all tools
func (e *ToolExecutor) GetAllTools() []Tool {
	var tools []Tool
	for _, tool := range e.tools {
		tools = append(tools, tool)
	}
	return tools
}

// GetToolNames returns the names of all tools
func (e *ToolExecutor) GetToolNames() []string {
	var names []string
	for name := range e.tools {
		names = append(names, name)
	}
	return names
}

// ExecuteTool executes a tool with the given name and input
func (e *ToolExecutor) ExecuteTool(ctx context.Context, name, input string) (string, error) {
	tool := e.tools[name]
	if tool == nil {
		return "", fmt.Errorf("tool not found: %s", name)
	}
	return tool.Execute(ctx, input)
}

// ToolRequest represents a request to execute a tool
type ToolRequest struct {
	ToolName string
	Input    string
}

// ToolResponse represents the response from executing a tool
type ToolResponse struct {
	ToolName string
	Result   string
	Error    error
}

// ExecuteParallel executes multiple tools in parallel
func (e *ToolExecutor) ExecuteParallel(ctx context.Context, requests []ToolRequest) []ToolResponse {
	responses := make([]ToolResponse, len(requests))
	
	// Create a channel for results
	resultChan := make(chan struct {
		index    int
		response ToolResponse
	}, len(requests))
	
	// Execute each tool in a goroutine
	for i, req := range requests {
		go func(i int, req ToolRequest) {
			var response ToolResponse
			response.ToolName = req.ToolName
			
			tool := e.tools[req.ToolName]
			if tool == nil {
				response.Error = fmt.Errorf("tool not found: %s", req.ToolName)
			} else {
				result, err := tool.Execute(ctx, req.Input)
				response.Result = result
				response.Error = err
			}
			
			resultChan <- struct {
				index    int
				response ToolResponse
			}{i, response}
		}(i, req)
	}
	
	// Collect results
	for i := 0; i < len(requests); i++ {
		result := <-resultChan
		responses[result.index] = result.response
	}
	
	return responses
}

// MockTool implements the Tool interface for testing
type MockTool struct {
	name        string
	description string
	responses   map[string]string
	parameters  map[string]types.ParameterSchema
}

// NewMockTool creates a new MockTool
func NewMockTool(name, description string) *MockTool {
	return &MockTool{
		name:        name,
		description: description,
		responses:   make(map[string]string),
		parameters:  make(map[string]types.ParameterSchema),
	}
}

// AddResponse adds a response for a given input
func (t *MockTool) AddResponse(input, response string) {
	t.responses[input] = response
}

// AddParameter adds a parameter schema
func (t *MockTool) AddParameter(name string, schema types.ParameterSchema) {
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
func (t *MockTool) Parameters() map[string]types.ParameterSchema {
	return t.parameters
}

// WebSearchTool is a mock implementation of a web search tool
type WebSearchTool struct {
	results map[string][]SearchResult
}

// SearchResult represents a search result
type SearchResult struct {
	Title   string `json:"title"`
	URL     string `json:"url"`
	Snippet string `json:"snippet"`
}

// NewWebSearchTool creates a new WebSearchTool
func NewWebSearchTool() *WebSearchTool {
	return &WebSearchTool{
		results: make(map[string][]SearchResult),
	}
}

// AddSearchResults adds search results for a query
func (t *WebSearchTool) AddSearchResults(query string, results []SearchResult) {
	t.results[query] = results
}

// Name returns the name of the tool
func (t *WebSearchTool) Name() string {
	return "web_search"
}

// Description returns the description of the tool
func (t *WebSearchTool) Description() string {
	return "Search the web for information"
}

// Execute executes the tool with the given input
func (t *WebSearchTool) Execute(ctx context.Context, input string) (string, error) {
	var query struct {
		Query string `json:"query"`
	}
	
	if err := json.Unmarshal([]byte(input), &query); err != nil {
		return "", fmt.Errorf("invalid input: %w", err)
	}
	
	results, ok := t.results[query.Query]
	if !ok {
		return "No results found", nil
	}
	
	resultJSON, err := json.MarshalIndent(results, "", "  ")
	if err != nil {
		return "", err
	}
	
	return string(resultJSON), nil
}

// Parameters returns the parameters schema of the tool
func (t *WebSearchTool) Parameters() map[string]types.ParameterSchema {
	return map[string]types.ParameterSchema{
		"query": {
			Type:        "string",
			Description: "The search query",
			Required:    true,
		},
	}
}
