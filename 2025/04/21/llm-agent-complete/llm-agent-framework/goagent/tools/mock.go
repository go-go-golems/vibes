package tools

import (
	"context"
	"github.com/goagent/framework/goagent/types"
)

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
