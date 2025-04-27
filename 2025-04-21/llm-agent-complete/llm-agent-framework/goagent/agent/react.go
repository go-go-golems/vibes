package agent

import (
	"context"
	"fmt"
	"strings"

	"github.com/go-go-golems/geppetto/pkg/conversation"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/types"
)

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

	messages := []*conversation.Message{
		conversation.NewChatMessage(conversation.RoleSystem, a.buildSystemPrompt()),
		conversation.NewChatMessage(conversation.RoleUser, input),
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
			Type:      "thought",
			Data:      thought,
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
		messages = append(messages, conversation.NewChatMessage(conversation.RoleAssistant, response))
		messages = append(messages, conversation.NewChatMessage(conversation.RoleUser, "Observation: "+result))
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

		messages := []*conversation.Message{
			conversation.NewChatMessage(conversation.RoleSystem, a.buildSystemPrompt()),
			conversation.NewChatMessage(conversation.RoleUser, input),
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
				Type:      "thought",
				Data:      thought,
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
			messages = append(messages, conversation.NewChatMessage(conversation.RoleAssistant, response))
			messages = append(messages, conversation.NewChatMessage(conversation.RoleUser, "Observation: "+result))
		}

		responseChan <- types.AgentResponse{
			Type:    "error",
			Content: "Agent exceeded maximum iterations",
		}
	}()

	return responseChan, nil
}

// ReActAgent implements the ReAct pattern
type ReActAgent struct {
	*BaseAgent
}
