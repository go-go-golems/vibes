// Package agent defines interfaces and implementations for LLM agents
package agent

import (
	"context"
	"fmt"
	"strings"

	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/memory"
	"github.com/goagent/framework/goagent/tools"
	"github.com/goagent/framework/goagent/tracing"
	"github.com/goagent/framework/goagent/types"
)

// Agent interface defines the core functionality of an agent
type Agent interface {
	// Run executes the agent with the given input and returns the result
	Run(ctx context.Context, input string) (string, error)
	
	// RunWithStream executes the agent with streaming response
	RunWithStream(ctx context.Context, input string) (<-chan types.AgentResponse, error)
	
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

// ReActAgent implements the ReAct pattern
type ReActAgent struct {
	*BaseAgent
}

// NewReActAgent creates a new ReActAgent
func NewReActAgent(llmModel llm.LLM, maxIterations int) *ReActAgent {
	return &ReActAgent{
		BaseAgent: NewBaseAgent(llmModel, maxIterations),
	}
}

// buildSystemPrompt builds the system prompt for the ReAct agent
func (a *ReActAgent) buildSystemPrompt() string {
	var toolDescriptions []string
	
	// Get all tools from the executor
	for _, tool := range a.tools.GetAllTools() {
		params := tool.Parameters()
		var paramDescs []string
		for name, schema := range params {
			paramDescs = append(paramDescs, fmt.Sprintf("  - %s: %s", name, schema.Description))
		}
		
		toolDesc := fmt.Sprintf("Tool: %s\nDescription: %s\nParameters:\n%s", 
			tool.Name(), 
			tool.Description(),
			strings.Join(paramDescs, "\n"))
		
		toolDescriptions = append(toolDescriptions, toolDesc)
	}
	
	return fmt.Sprintf(`You are an AI assistant that can use tools to help answer questions.

You have access to the following tools:

%s

Use the following format:

Thought: you should always think about what to do
Action: the action to take, should be one of [%s]
Action Input: the input to the action (in JSON format)
Observation: the result of the action
... (this Thought/Action/Action Input/Observation can repeat N times)
Thought: I now know the final answer
Final Answer: the final answer to the original input question`,
		strings.Join(toolDescriptions, "\n\n"),
		strings.Join(a.tools.GetToolNames(), ", "))
}

// parseResponse parses the LLM response into thought, action, and action input
func (a *ReActAgent) parseResponse(response string) (thought, action, actionInput string) {
	lines := strings.Split(response, "\n")
	
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		
		if strings.HasPrefix(line, "Thought:") {
			thought = strings.TrimSpace(strings.TrimPrefix(line, "Thought:"))
			// Look for multi-line thought
			for i+1 < len(lines) && !strings.Contains(lines[i+1], ":") {
				i++
				thought += " " + strings.TrimSpace(lines[i])
			}
		} else if strings.HasPrefix(line, "Action:") {
			action = strings.TrimSpace(strings.TrimPrefix(line, "Action:"))
		} else if strings.HasPrefix(line, "Action Input:") {
			actionInput = strings.TrimSpace(strings.TrimPrefix(line, "Action Input:"))
			// Look for multi-line action input
			for i+1 < len(lines) && !strings.Contains(lines[i+1], ":") {
				i++
				actionInput += " " + strings.TrimSpace(lines[i])
			}
		} else if strings.HasPrefix(line, "Final Answer:") {
			action = "final_answer"
			actionInput = strings.TrimSpace(strings.TrimPrefix(line, "Final Answer:"))
			// Look for multi-line final answer
			for i+1 < len(lines) {
				i++
				actionInput += " " + strings.TrimSpace(lines[i])
			}
		}
	}
	
	return thought, action, actionInput
}

// Run executes the agent with the ReAct pattern
func (a *ReActAgent) Run(ctx context.Context, input string) (string, error) {
	ctx, span := a.tracer.StartSpan(ctx, "ReActAgent.Run")
	defer span.End()
	
	messages := []types.Message{
		{Role: "system", Content: a.buildSystemPrompt()},
		{Role: "user", Content: input},
	}
	
	for i := 0; i < a.maxIter; i++ {
		// Get next step from LLM
		response, err := a.llm.Generate(ctx, messages)
		if err != nil {
			return "", err
		}
		
		// Extract thought, action, and action input
		thought, action, actionInput := a.parseResponse(response)
		
		// Log thought
		a.tracer.LogEvent(ctx, types.Event{
			Type: "thought",
			Data: thought,
			Timestamp: 0, // Will be set by the tracer
		})
		
		// Check if final answer
		if action == "final_answer" {
			return actionInput, nil
		}
		
		// Execute tool
		result, err := a.tools.ExecuteTool(ctx, action, actionInput)
		if err != nil {
			result = "Error: " + err.Error()
		}
		
		// Log tool execution
		a.tracer.LogEvent(ctx, types.Event{
			Type: "tool_execution",
			Data: map[string]string{
				"tool":   action,
				"input":  actionInput,
				"result": result,
			},
			Timestamp: 0, // Will be set by the tracer
		})
		
		// Add to messages
		messages = append(messages, types.Message{
			Role: "assistant",
			Content: response,
		})
		messages = append(messages, types.Message{
			Role: "user",
			Content: "Observation: " + result,
		})
	}
	
	return "Agent exceeded maximum iterations", nil
}

// RunWithStream executes the agent with streaming response
func (a *ReActAgent) RunWithStream(ctx context.Context, input string) (<-chan types.AgentResponse, error) {
	ctx, span := a.tracer.StartSpan(ctx, "ReActAgent.RunWithStream")
	
	responseChan := make(chan types.AgentResponse)
	
	go func() {
		defer close(responseChan)
		defer span.End()
		
		messages := []types.Message{
			{Role: "system", Content: a.buildSystemPrompt()},
			{Role: "user", Content: input},
		}
		
		for i := 0; i < a.maxIter; i++ {
			// Get next step from LLM with streaming
			stepChan, err := a.llm.GenerateWithStream(ctx, messages)
			if err != nil {
				responseChan <- types.AgentResponse{
					Type:    "error",
					Content: err.Error(),
				}
				return
			}
			
			// Collect the full response
			var fullResponse strings.Builder
			for chunk := range stepChan {
				fullResponse.WriteString(chunk)
				responseChan <- types.AgentResponse{
					Type:    "thinking",
					Content: chunk,
				}
			}
			
			response := fullResponse.String()
			
			// Extract thought, action, and action input
			thought, action, actionInput := a.parseResponse(response)
			
			// Log thought
			a.tracer.LogEvent(ctx, types.Event{
				Type: "thought",
				Data: thought,
				Timestamp: 0, // Will be set by the tracer
			})
			
			// Check if final answer
			if action == "final_answer" {
				responseChan <- types.AgentResponse{
					Type:    "final",
					Content: actionInput,
				}
				return
			}
			
			// Send tool call notification
			responseChan <- types.AgentResponse{
				Type:    "tool_call",
				Content: "",
				ToolCall: &types.ToolCall{
					Name:  action,
					Input: actionInput,
				},
			}
			
			// Execute tool
			result, err := a.tools.ExecuteTool(ctx, action, actionInput)
			if err != nil {
				result = "Error: " + err.Error()
			}
			
			// Log tool execution
			a.tracer.LogEvent(ctx, types.Event{
				Type: "tool_execution",
				Data: map[string]string{
					"tool":   action,
					"input":  actionInput,
					"result": result,
				},
				Timestamp: 0, // Will be set by the tracer
			})
			
			// Send tool result
			responseChan <- types.AgentResponse{
				Type:    "tool_result",
				Content: result,
				ToolResult: &types.ToolResult{
					Name:   action,
					Output: result,
				},
			}
			
			// Add to messages
			messages = append(messages, types.Message{
				Role: "assistant",
				Content: response,
			})
			messages = append(messages, types.Message{
				Role: "user",
				Content: "Observation: " + result,
			})
		}
		
		responseChan <- types.AgentResponse{
			Type:    "error",
			Content: "Agent exceeded maximum iterations",
		}
	}()
	
	return responseChan, nil
}

// PlanAndExecuteAgent implements the Plan-and-Execute pattern
type PlanAndExecuteAgent struct {
	*BaseAgent
	planner  llm.LLM
	executor llm.LLM
}

// NewPlanAndExecuteAgent creates a new PlanAndExecuteAgent
func NewPlanAndExecuteAgent(planner, executor llm.LLM, maxIterations int) *PlanAndExecuteAgent {
	return &PlanAndExecuteAgent{
		BaseAgent: NewBaseAgent(executor, maxIterations),
		planner:   planner,
		executor:  executor,
	}
}

// buildPlannerPrompt builds the system prompt for the planner
func (a *PlanAndExecuteAgent) buildPlannerPrompt() string {
	return `You are a task planning assistant. Given a task, create a detailed plan.
Break down the task into a sequence of steps that can be executed one by one.
Each step should be clear and actionable.

Create a plan with the following format:
1. First step
2. Second step
...`
}

// buildExecutorPrompt builds the system prompt for the executor
func (a *PlanAndExecuteAgent) buildExecutorPrompt() string {
	var toolDescriptions []string
	
	// Get all tools from the executor
	for _, tool := range a.tools.GetAllTools() {
		params := tool.Parameters()
		var paramDescs []string
		for name, schema := range params {
			paramDescs = append(paramDescs, fmt.Sprintf("  - %s: %s", name, schema.Description))
		}
		
		toolDesc := fmt.Sprintf("Tool: %s\nDescription: %s\nParameters:\n%s", 
			tool.Name(), 
			tool.Description(),
			strings.Join(paramDescs, "\n"))
		
		toolDescriptions = append(toolDescriptions, toolDesc)
	}
	
	return fmt.Sprintf(`You are a task executor. Follow the plan and execute each step using available tools.
You have access to the following tools:

%s

Use the following format:

Thought: think about the current step
Action: the action to take, should be one of [%s]
Action Input: the input for the action (in JSON format)`,
		strings.Join(toolDescriptions, "\n\n"),
		strings.Join(a.tools.GetToolNames(), ", "))
}

// buildFinalizerPrompt builds the system prompt for the finalizer
func (a *PlanAndExecuteAgent) buildFinalizerPrompt() string {
	return `You are a task finalizer. Given the original input, the plan, and the results of each step,
provide a comprehensive final answer.

Be concise but thorough in your response.`
}

// parsePlan parses the plan into steps
func (a *PlanAndExecuteAgent) parsePlan(plan string) []string {
	var steps []string
	lines := strings.Split(plan, "\n")
	
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		
		// Look for numbered steps like "1. Step one"
		for i := 1; i <= 20; i++ {
			prefix := fmt.Sprintf("%d.", i)
			if strings.HasPrefix(line, prefix) {
				step := strings.TrimSpace(strings.TrimPrefix(line, prefix))
				if step != "" {
					steps = append(steps, step)
				}
				break
			}
		}
	}
	
	return steps
}

// parseExecutorResponse parses the executor response into action and action input
func (a *PlanAndExecuteAgent) parseExecutorResponse(response string) (action, actionInput string) {
	lines := strings.Split(response, "\n")
	
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		
		if strings.HasPrefix(line, "Action:") {
			action = strings.TrimSpace(strings.TrimPrefix(line, "Action:"))
		} else if strings.HasPrefix(line, "Action Input:") {
			actionInput = strings.TrimSpace(strings.TrimPrefix(line, "Action Input:"))
			// Look for multi-line action input
			for i+1 < len(lines) && !strings.Contains(lines[i+1], ":") {
				i++
				actionInput += " " + strings.TrimSpace(lines[i])
			}
		}
	}
	
	return action, actionInput
}

// Run executes the agent with the Plan-and-Execute pattern
func (a *PlanAndExecuteAgent) Run(ctx context.Context, input string) (string, error) {
	ctx, span := a.tracer.StartSpan(ctx, "PlanAndExecuteAgent.Run")
	defer span.End()
	
	// Generate plan
	planMessages := []types.Message{
		{Role: "system", Content: a.buildPlannerPrompt()},
		{Role: "user", Content: input},
	}
	
	planResponse, err := a.planner.Generate(ctx, planMessages)
	if err != nil {
		return "", err
	}
	
	// Parse plan into steps
	steps := a.parsePlan(planResponse)
	
	// Log plan
	a.tracer.LogEvent(ctx, types.Event{
		Type: "plan",
		Data: steps,
		Timestamp: 0, // Will be set by the tracer
	})
	
	// Execute each step
	results := make([]string, len(steps))
	for i, step := range steps {
		// Build executor prompt
		executorMessages := []types.Message{
			{Role: "system", Content: a.buildExecutorPrompt()},
			{Role: "user", Content: fmt.Sprintf("Plan: %s\nCurrent step: %s\nPrevious results: %s", 
				planResponse, step, strings.Join(results[:i], "\n"))},
		}
		
		// Get executor response
		executorResponse, err := a.executor.Generate(ctx, executorMessages)
		if err != nil {
			return "", err
		}
		
		// Extract action and action input
		action, actionInput := a.parseExecutorResponse(executorResponse)
		
		// Execute tool
		result, err := a.tools.ExecuteTool(ctx, action, actionInput)
		if err != nil {
			result = "Error: " + err.Error()
		}
		
		// Store result
		results[i] = fmt.Sprintf("Step %d: %s\nResult: %s", i+1, step, result)
		
		// Log step execution
		a.tracer.LogEvent(ctx, types.Event{
			Type: "step_execution",
			Data: map[string]string{
				"step":   step,
				"tool":   action,
				"input":  actionInput,
				"result": result,
			},
			Timestamp: 0, // Will be set by the tracer
		})
	}
	
	// Generate final answer
	finalMessages := []types.Message{
		{Role: "system", Content: a.buildFinalizerPrompt()},
		{Role: "user", Content: fmt.Sprintf("Input: %s\nPlan: %s\nResults: %s", 
			input, planResponse, strings.Join(results, "\n"))},
	}
	
	finalResponse, err := a.planner.Generate(ctx, finalMessages)
	if err != nil {
		return "", err
	}
	
	return finalResponse, nil
}

// RunWithStream executes the agent with streaming response
func (a *PlanAndExecuteAgent) RunWithStream(ctx context.Context, input string) (<-chan types.AgentResponse, error) {
	ctx, span := a.tracer.StartSpan(ctx, "PlanAndExecuteAgent.RunWithStream")
	
	responseChan := make(chan types.AgentResponse)
	
	go func() {
		defer close(responseChan)
		defer span.End()
		
		// Generate plan
		responseChan <- types.AgentResponse{
			Type:    "status",
			Content: "Generating plan...",
		}
		
		planMessages := []types.Message{
			{Role: "system", Content: a.buildPlannerPrompt()},
			{Role: "user", Content: input},
		}
		
		planChan, err := a.planner.GenerateWithStream(ctx, planMessages)
		if err != nil {
			responseChan <- types.AgentResponse{
				Type:    "error",
				Content: err.Error(),
			}
			return
		}
		
		// Collect the full plan
		var fullPlan strings.Builder
		for chunk := range planChan {
			fullPlan.WriteString(chunk)
			responseChan <- types.AgentResponse{
				Type:    "planning",
				Content: chunk,
			}
		}
		
		planResponse := fullPlan.String()
		
		// Parse plan into steps
		steps := a.parsePlan(planResponse)
		
		// Log plan
		a.tracer.LogEvent(ctx, types.Event{
			Type: "plan",
			Data: steps,
			Timestamp: 0, // Will be set by the tracer
		})
		
		// Send complete plan
		responseChan <- types.AgentResponse{
			Type:    "plan",
			Content: planResponse,
		}
		
		// Execute each step
		results := make([]string, len(steps))
		for i, step := range steps {
			// Send step status
			responseChan <- types.AgentResponse{
				Type:    "status",
				Content: fmt.Sprintf("Executing step %d: %s", i+1, step),
			}
			
			// Build executor prompt
			executorMessages := []types.Message{
				{Role: "system", Content: a.buildExecutorPrompt()},
				{Role: "user", Content: fmt.Sprintf("Plan: %s\nCurrent step: %s\nPrevious results: %s", 
					planResponse, step, strings.Join(results[:i], "\n"))},
			}
			
			// Get executor response with streaming
			executorChan, err := a.executor.GenerateWithStream(ctx, executorMessages)
			if err != nil {
				responseChan <- types.AgentResponse{
					Type:    "error",
					Content: err.Error(),
				}
				return
			}
			
			// Collect the full executor response
			var fullExecutor strings.Builder
			for chunk := range executorChan {
				fullExecutor.WriteString(chunk)
				responseChan <- types.AgentResponse{
					Type:    "thinking",
					Content: chunk,
				}
			}
			
			executorResponse := fullExecutor.String()
			
			// Extract action and action input
			action, actionInput := a.parseExecutorResponse(executorResponse)
			
			// Send tool call notification
			responseChan <- types.AgentResponse{
				Type:    "tool_call",
				Content: "",
				ToolCall: &types.ToolCall{
					Name:  action,
					Input: actionInput,
				},
			}
			
			// Execute tool
			result, err := a.tools.ExecuteTool(ctx, action, actionInput)
			if err != nil {
				result = "Error: " + err.Error()
			}
			
			// Send tool result
			responseChan <- types.AgentResponse{
				Type:    "tool_result",
				Content: result,
				ToolResult: &types.ToolResult{
					Name:   action,
					Output: result,
				},
			}
			
			// Store result
			results[i] = fmt.Sprintf("Step %d: %s\nResult: %s", i+1, step, result)
			
			// Log step execution
			a.tracer.LogEvent(ctx, types.Event{
				Type: "step_execution",
				Data: map[string]string{
					"step":   step,
					"tool":   action,
					"input":  actionInput,
					"result": result,
				},
				Timestamp: 0, // Will be set by the tracer
			})
		}
		
		// Generate final answer
		responseChan <- types.AgentResponse{
			Type:    "status",
			Content: "Generating final answer...",
		}
		
		finalMessages := []types.Message{
			{Role: "system", Content: a.buildFinalizerPrompt()},
			{Role: "user", Content: fmt.Sprintf("Input: %s\nPlan: %s\nResults: %s", 
				input, planResponse, strings.Join(results, "\n"))},
		}
		
		finalChan, err := a.planner.GenerateWithStream(ctx, finalMessages)
		if err != nil {
			responseChan <- types.AgentResponse{
				Type:    "error",
				Content: err.Error(),
			}
			return
		}
		
		// Send final answer chunks
		for chunk := range finalChan {
			responseChan <- types.AgentResponse{
				Type:    "final_stream",
				Content: chunk,
			}
		}
	}()
	
	return responseChan, nil
}
