// Package tools defines interfaces and implementations for agent tools
package tools

import (
	"context"
	"fmt"

	claudeapi "github.com/go-go-golems/geppetto/pkg/steps/ai/claude/api"
	go_openai "github.com/sashabaranov/go-openai"

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
	ID       string                 `json:"id,omitempty"`
	ToolName string                 `json:"tool_name"`
	Input    string                 `json:"input"`
	Metadata map[string]interface{} `json:"metadata,omitempty"`
}

// ToolResponse represents the response from executing a tool
type ToolResponse struct {
	ID       string                 `json:"id,omitempty"`
	ToolName string                 `json:"tool_name"`
	Result   string                 `json:"result,omitempty"`
	Error    error                  `json:"error,omitempty"`
	Metadata map[string]interface{} `json:"metadata,omitempty"`
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
			response.ID = req.ID // Propagate ID
			response.ToolName = req.ToolName
			response.Metadata = req.Metadata // Propagate Metadata

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

// JSONSchema represents the structure expected by OpenAI and Claude for parameters.
type JSONSchema struct {
	Type       string                    `json:"type"`
	Properties map[string]ParameterField `json:"properties"`
	Required   []string                  `json:"required,omitempty"`
}

type ParameterField struct {
	Type        string `json:"type"`
	Description string `json:"description,omitempty"`
	// Add other fields as needed for more complex schemas (e.g., Enum, Items for arrays)
}

// toolParametersToJSONSchema converts the tool's parameters into a JSON schema object.
func toolParametersToJSONSchema(params map[string]types.ParameterSchema) JSONSchema {
	properties := make(map[string]ParameterField)
	required := []string{}

	for name, schema := range params {
		properties[name] = ParameterField{
			Type:        schema.Type,
			Description: schema.Description,
		}
		if schema.Required {
			required = append(required, name)
		}
	}

	return JSONSchema{
		Type:       "object",
		Properties: properties,
		Required:   required,
	}
}

// ToolToOpenAITool converts a goagent Tool into an OpenAI Tool definition.
func ToolToOpenAITool(tool Tool) go_openai.Tool {
	return go_openai.Tool{
		Type: go_openai.ToolTypeFunction,
		Function: &go_openai.FunctionDefinition{
			Name:        tool.Name(),
			Description: tool.Description(),
			Parameters:  toolParametersToJSONSchema(tool.Parameters()),
		},
	}
}

// ToolToClaudeTool converts a goagent Tool into a Claude Tool definition.
func ToolToClaudeTool(tool Tool) claudeapi.Tool {
	return claudeapi.Tool{
		Name:        tool.Name(),
		Description: tool.Description(),
		InputSchema: toolParametersToJSONSchema(tool.Parameters()),
	}
}

// OpenAIToolCallToToolRequest converts an OpenAI ToolCall into a ToolRequest.
func OpenAIToolCallToToolRequest(toolCall go_openai.ToolCall) ToolRequest {
	return ToolRequest{
		ID:       toolCall.ID, // Use OpenAI ToolCall ID
		ToolName: toolCall.Function.Name,
		Input:    toolCall.Function.Arguments,
	}
}

// ClaudeToolUseToToolRequest converts a Claude ToolUseContent into a ToolRequest.
func ClaudeToolUseToToolRequest(toolUse claudeapi.ToolUseContent) ToolRequest {
	return ToolRequest{
		ID:       toolUse.ID, // Use Claude ToolUse ID
		ToolName: toolUse.Name,
		Input:    toolUse.Input,
	}
}
