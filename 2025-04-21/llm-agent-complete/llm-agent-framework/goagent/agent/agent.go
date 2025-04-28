package agent

import (
	"context"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/memory"
	"github.com/goagent/framework/goagent/tools"
	"github.com/goagent/framework/goagent/tracing"
)

// Agent interface defines the core functionality of an agent
type Agent interface {
	// Run executes the agent with the given input and returns the result
	Run(ctx context.Context, input string) (string, error)

	// AddTool adds a tool to the agent
	AddTool(tool tools.Tool) error

	// SetMemory sets the memory system for the agent
	SetMemory(mem memory.Memory) error
}

// BaseAgent provides common functionality for all agents
type BaseAgent struct {
	llm     llm.LLM
	tools   *tools.ToolExecutor
	memory  memory.Memory
	tracer  tracing.Tracer
	maxIter int
}

// NewBaseAgent creates a new BaseAgent
func NewBaseAgent(llmModel llm.LLM, maxIterations int) *BaseAgent {
	return &BaseAgent{
		llm:     llmModel,
		tools:   tools.NewToolExecutor(),
		tracer:  tracing.NewSimpleTracer(),
		maxIter: maxIterations,
	}
}

// AddTool adds a tool to the agent
func (a *BaseAgent) AddTool(tool tools.Tool) error {
	a.tools.AddTool(tool)
	return nil
}

// SetMemory sets the memory system for the agent
func (a *BaseAgent) SetMemory(mem memory.Memory) error {
	a.memory = mem
	return nil
}

// GetTracer returns the tracer
func (a *BaseAgent) GetTracer() tracing.Tracer {
	return a.tracer
}
