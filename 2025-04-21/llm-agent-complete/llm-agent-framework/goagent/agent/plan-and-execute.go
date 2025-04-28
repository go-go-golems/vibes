package agent

import (
	"context"
	"fmt"
	"strings"

	"github.com/go-go-golems/geppetto/pkg/conversation"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/types"
)

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
		for pair := params.Oldest(); pair != nil; pair = pair.Next() {
			name := pair.Key
			schema := pair.Value
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
	planMessages := []*conversation.Message{
		conversation.NewChatMessage(conversation.RoleSystem, a.buildPlannerPrompt()),
		conversation.NewChatMessage(conversation.RoleUser, input),
	}

	planResponse, err := a.planner.Generate(ctx, planMessages)
	if err != nil {
		return "", err
	}

	// Parse plan into steps
	steps := a.parsePlan(planResponse.Content.String())

	// Log plan
	a.tracer.LogEvent(ctx, types.Event{
		Type:      "plan",
		Data:      steps,
		Timestamp: 0, // Will be set by the tracer
	})

	// Execute each step
	results := make([]string, len(steps))
	for i, step := range steps {
		// Build executor prompt
		executorMessages := []*conversation.Message{
			conversation.NewChatMessage(conversation.RoleSystem, a.buildExecutorPrompt()),
			conversation.NewChatMessage(conversation.RoleUser, fmt.Sprintf("Plan: %s\nCurrent step: %s\nPrevious results: %s",
				planResponse, step, strings.Join(results[:i], "\n"))),
		}

		// Get executor response
		executorResponse, err := a.executor.Generate(ctx, executorMessages)
		if err != nil {
			return "", err
		}

		// Extract action and action input
		action, actionInput := a.parseExecutorResponse(executorResponse.Content.String())

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
	finalMessages := []*conversation.Message{
		conversation.NewChatMessage(conversation.RoleSystem, a.buildFinalizerPrompt()),
		conversation.NewChatMessage(conversation.RoleUser, fmt.Sprintf("Input: %s\nPlan: %s\nResults: %s",
			input, planResponse, strings.Join(results, "\n"))),
	}

	finalResponse, err := a.planner.Generate(ctx, finalMessages)
	if err != nil {
		return "", err
	}

	return finalResponse.Content.String(), nil
}
