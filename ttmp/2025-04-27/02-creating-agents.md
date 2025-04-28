# Building LLM Agents with the GoAgent Framework

This tutorial provides a detailed step-by-step guide on how to build LLM agents using the GoAgent framework. We'll explore the framework's architecture, examine existing agent patterns, and build a custom agent that can generate multiple files from a single prompt.

## Table of Contents

1. [Introduction to the GoAgent Framework](#introduction-to-the-goagent-framework)
2. [Framework Architecture](#framework-architecture)
3. [Example Agent Patterns](#example-agent-patterns)
   - [ReAct Pattern](#react-pattern)
   - [Plan-and-Execute Pattern](#plan-and-execute-pattern)
4. [Building a File Collection Agent](#building-a-file-collection-agent)
   - [Agent Design](#agent-design)
   - [Implementation](#implementation)
   - [XML File Delimiter Prompt](#xml-file-delimiter-prompt)
5. [Usage Examples](#usage-examples)
6. [Advanced Techniques](#advanced-techniques)

## Introduction to the GoAgent Framework

The GoAgent framework is a Go-based library for building LLM-powered agents. It provides a structured approach to creating agents that can:

- Interact with LLMs (Large Language Models)
- Use tools to access external systems
- Follow different reasoning patterns
- Maintain memory across interactions
- Trace and log their execution

This framework abstracts the complexity of working with LLMs directly, allowing developers to focus on agent logic and behavior.

## Framework Architecture

The GoAgent framework consists of several core components, primarily located within the `goagent` directory of the `llm-agent-framework`:

1. **Agent Interface** (`goagent/agent/agent.go`): Defines the standard behavior for all agents, with methods like `Run()`, `AddTool()`, and `SetMemory()`.

2. **BaseAgent** (`goagent/agent/agent.go`): Provides common functionality that all agents can inherit, including:
   - LLM integration
   - Tool management
   - Memory system
   - Tracing capabilities

3. **LLM Interface** (`goagent/llm/llm.go`): Abstracts the underlying language model (e.g., `GeppettoLLM` in `goagent/llm/geppetto.go`), providing a consistent API for different LLM providers.

4. **Tools** (`goagent/tools/tools.go`): Defines the interface (`Tool`) and implementation (`ToolExecutor`) for tools that agents can use to interact with external systems.

5. **Memory** (`goagent/memory/...`): Allows agents to store and retrieve information across multiple interactions (implementation details not covered in this tutorial).

6. **Tracing** (`goagent/tracing/...`): Provides mechanisms to track and log agent execution for debugging and analysis (implementation details not covered in this tutorial).

Let's take a closer look at how these components fit together:

```go
// Defined in goagent/agent/agent.go
type Agent interface {
    Run(ctx context.Context, input string) (string, error)
    AddTool(tool tools.Tool) error // tools defined in goagent/tools/tools.go
    SetMemory(mem memory.Memory) error // memory defined in goagent/memory/memory.go
}

// Defined in goagent/agent/agent.go
type BaseAgent struct {
    llm     llm.LLM // llm defined in goagent/llm/llm.go
    tools   *tools.ToolExecutor
    memory  memory.Memory
    tracer  tracing.Tracer // tracer defined in goagent/tracing/tracing.go
    maxIter int
}
```

## Example Agent Patterns

The framework comes with implementations of popular agent patterns in the `goagent/agent` package. Let's examine two of them: ReAct (`react.go`) and Plan-and-Execute (`plan-and-execute.go`).

### ReAct Pattern

The ReAct pattern (Reasoning + Acting) combines reasoning and action in an iterative process. The agent:

1. Thinks about what to do
2. Chooses an action
3. Observes the result
4. Repeats until it reaches a conclusion

Here's how the ReAct agent is implemented in `goagent/agent/react.go`:

```go
// ReActAgent structure
type ReActAgent struct {
    *BaseAgent
}

// NewReActAgent creates a new ReActAgent
func NewReActAgent(llmModel llm.LLM, maxIterations int) *ReActAgent {
    return &ReActAgent{
        BaseAgent: NewBaseAgent(llmModel, maxIterations),
    }
}
```

The core of the ReAct agent is its `Run` method:

```go
// Run executes the agent with the ReAct pattern
func (a *ReActAgent) Run(ctx context.Context, input string) (*conversation.Message, error) {
    ctx, span := a.tracer.StartSpan(ctx, "ReActAgent.Run")
    defer span.End()

    // Initialize conversation with system prompt and user input
    messages := []*conversation.Message{
        conversation.NewChatMessage(conversation.RoleSystem, a.buildSystemPrompt()),
        conversation.NewChatMessage(conversation.RoleUser, input),
    }

    for i := 0; i < a.maxIter; i++ {
        // Get next step from LLM
        responseMsg, err := a.llm.Generate(ctx, messages)
        if err != nil {
            return nil, err
        }

        // Extract response
        response := responseMsg.Content.String()

        // Parse thought, action, and action input
        thought, action, actionInput := a.parseResponse(response)

        // Log thought for tracing
        a.tracer.LogEvent(ctx, types.Event{
            Type: "thought",
            Data: thought,
        })

        // Check if final answer
        if action == "final_answer" {
            return conversation.NewChatMessage(conversation.RoleAssistant, actionInput), nil
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
        })

        // Add to messages for context
        messages = append(messages, responseMsg)
        messages = append(messages, 
            conversation.NewChatMessage(conversation.RoleUser, "Observation: "+result))
    }

    return conversation.NewChatMessage(
        conversation.RoleAssistant, 
        "Agent exceeded maximum iterations"), nil
}
```

### Plan-and-Execute Pattern

The Plan-and-Execute pattern splits the agent's work into two phases:

1. **Planning**: Creates a detailed plan with steps to solve the problem
2. **Execution**: Executes each step of the plan sequentially

This pattern is implemented in `goagent/agent/plan-and-execute.go` as follows:

```go
// PlanAndExecuteAgent structure
type PlanAndExecuteAgent struct {
    *BaseAgent
    planner  llm.LLM
    executor llm.LLM
}

// NewPlanAndExecuteAgent creates a new agent
func NewPlanAndExecuteAgent(planner, executor llm.LLM, maxIterations int) *PlanAndExecuteAgent {
    return &PlanAndExecuteAgent{
        BaseAgent: NewBaseAgent(executor, maxIterations),
        planner:   planner,
        executor:  executor,
    }
}
```

The `Run` method implements the pattern:

```go
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
        Type: "plan",
        Data: steps,
    })

    // Execute each step
    results := make([]string, len(steps))
    for i, step := range steps {
        // Build executor prompt
        executorMessages := []*conversation.Message{
            conversation.NewChatMessage(conversation.RoleSystem, a.buildExecutorPrompt()),
            conversation.NewChatMessage(conversation.RoleUser, fmt.Sprintf(
                "Plan: %s\nCurrent step: %s\nPrevious results: %s",
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
    }

    // Generate final answer
    finalMessages := []*conversation.Message{
        conversation.NewChatMessage(conversation.RoleSystem, a.buildFinalizerPrompt()),
        conversation.NewChatMessage(conversation.RoleUser, fmt.Sprintf(
            "Input: %s\nPlan: %s\nResults: %s",
            input, planResponse, strings.Join(results, "\n"))),
    }

    finalResponse, err := a.planner.Generate(ctx, finalMessages)
    if err != nil {
        return "", err
    }

    return finalResponse.Content.String(), nil
}
```

## Building a File Collection Agent

Now let's create a custom agent that can generate multiple files from a single prompt, using XML tags to delimit the files.

### Agent Design

Our FileCollectionAgent will:

1. Receive a prompt requesting the generation of one or more files
2. Make LLM calls to generate the content of each file
3. Parse the output to extract file contents using XML delimiters
4. Continue making LLM calls until all requested files are generated
5. Return the collected files through a custom method

### Implementation

First, let's define the `FileCollectionAgent` structure. This would typically reside in a custom package or within `goagent/agent` if intended as a core pattern:

```go
// Package: e.g., customagent or agent
package agent

import (
	"context"
	"fmt"
	"regexp"
	"strings"

	"github.com/go-go-golems/geppetto/pkg/conversation"
	"github.com/goagent/framework/goagent/agent" // Import base agent
	"github.com/goagent/framework/goagent/llm"    // Import LLM interface
	"github.com/goagent/framework/goagent/types"   // Import types for tracing
	"golang.org/x/exp/maps"                       // For maps.Keys
)

// FileCollectionAgent implements an agent that can generate multiple files
type FileCollectionAgent struct {
	*agent.BaseAgent        // Embed BaseAgent from goagent/agent
	files map[string]string // Maps filenames to their content
}

// NewFileCollectionAgent creates a new FileCollectionAgent
func NewFileCollectionAgent(llmModel llm.LLM, maxIterations int) *FileCollectionAgent {
	return &FileCollectionAgent{
		BaseAgent: agent.NewBaseAgent(llmModel, maxIterations), // Use constructor from goagent/agent
		files:     make(map[string]string),
	}
}

// GetFiles returns the generated files
func (a *FileCollectionAgent) GetFiles() map[string]string {
	return a.files
}
```

Next, let's implement the system prompt that instructs the LLM how to format file output and signal completion:

```go
// buildSystemPrompt builds the system prompt for the FileCollectionAgent
func (a *FileCollectionAgent) buildSystemPrompt() string {
	return `You are an AI assistant that generates complete, ready-to-use code files based on a request.

When asked to create files, you MUST follow these instructions:
1. Generate each file's complete content sequentially.
2. Wrap EACH file's content within <file name="filename.ext">...</file> XML tags. The filename MUST be included in the 'name' attribute.
3. Ensure each file is complete and ready to use (including imports, comments, etc.).
4. After generating ALL the files requested and ensuring the implementation is complete, you MUST include the comment <!-- all files emitted --> on a new line AFTER the last </file> tag.
5. If the initial request can be fulfilled with a single response containing all files, include <!-- all files emitted --> at the end of that single response.
6. If no files are generated in a response (e.g., asking a question or providing an explanation), do NOT include the <!-- all files emitted --> comment.

Example of generating two files and finishing:
<file name="main.go">
package main

import "fmt"

func main() {
    fmt.Println("Hello, world!")
}
</file>
<file name="helper.go">
package main

// Helper function
func Helper() string {
	return "I'm helping!"
}
</file>
<!-- all files emitted -->

If you need to generate more files in a subsequent response, simply output the next set of <file>...</file> tags without the completion comment. Only add <!-- all files emitted --> when everything is truly finished.
`
}
```

Now, let's implement the `Run` method:

```go
// Run executes the FileCollectionAgent
func (a *FileCollectionAgent) Run(ctx context.Context, input string) (string, error) {
    ctx, span := a.tracer.StartSpan(ctx, "FileCollectionAgent.Run")
    defer span.End()

    // Reset files map for this run
    a.files = make(map[string]string)

    // Initialize conversation
    messages := []*conversation.Message{
        conversation.NewChatMessage(conversation.RoleSystem, a.buildSystemPrompt()),
        conversation.NewChatMessage(conversation.RoleUser, input),
    }

    // Iterate until max iterations or completion
    for i := 0; i < a.maxIter; i++ {
        responseMsg, err := a.llm.Generate(ctx, messages)
        if err != nil {
            return "", err
        }

        response := responseMsg.Content.String()
        
        // Extract files from response
        newFiles := a.extractFiles(response)
        
        // Add files to our collection
        for name, content := range newFiles {
            a.files[name] = content
        }
        
        // Log the files we've extracted
        a.tracer.LogEvent(ctx, types.Event{
            Type: "files_extracted",
            Data: map[string]interface{}{
                "iteration": i,
                "files":     maps.Keys(newFiles),
            },
        })

        // Check if we should ask for more files
        if a.shouldContinue(response) {
            // Add the response to messages
            messages = append(messages, responseMsg)
            
            // Ask for more files
            messages = append(messages, conversation.NewChatMessage(
                conversation.RoleUser,
                "Continue generating any additional files needed to complete the implementation."))
        } else {
            // We're done
            break
        }
    }

    // Build summary of generated files
    var summary strings.Builder
    summary.WriteString(fmt.Sprintf("Generated %d files:\n\n", len(a.files)))
    
    for name := range a.files {
        summary.WriteString(fmt.Sprintf("- %s\n", name))
    }
    
    return summary.String(), nil
}
```

We need to implement the file extraction logic:

```go
// extractFiles parses the LLM response and extracts files using XML delimiters
func (a *FileCollectionAgent) extractFiles(response string) map[string]string {
	files := make(map[string]string)

	// Find all matches of <file name="...">...</file>
	// Use [\s\S]*? for non-greedy matching of any character including newlines
	fileRegex := regexp.MustCompile(`<file\s+name=["']([^"']+)["']>([\s\S]*?)</file>`)
	matches := fileRegex.FindAllStringSubmatch(response, -1)

	for _, match := range matches {
		if len(match) >= 3 {
			filename := match[1]
			// Trim leading/trailing whitespace/newlines which might be captured
			content := strings.TrimSpace(match[2])
			files[filename] = content
		}
	}

	return files
}

// shouldContinue checks if the LLM signaled completion or if no files were generated.
func (a *FileCollectionAgent) shouldContinue(response string) bool {
	// Check if the completion marker is present
	finished := strings.Contains(response, "<!-- all files emitted -->")

	// Check if any <file> tags are present in the response
	hasFiles := strings.Contains(response, "<file name=")

	// Continue if the finished marker is NOT present AND there were files generated.
	// If the finished marker is present, stop.
	// If no files were generated in this response, also stop (assume LLM finished or gave non-file response).
	return !finished && hasFiles
}
```

### XML File Delimiter Prompt

The key to making this agent work correctly is the system prompt that instructs the LLM how to format file output and **how to signal completion**. Let's examine the important parts of the updated prompt:

1. **Clear instructions**: The prompt explains that the LLM should generate complete files.
2. **Specific format**: It defines the exact XML format: `<file name="filename.ext">...</file>`.
3. **Explicit Completion Marker**: It **mandates** the use of `<!-- all files emitted -->` on a new line *after* the last file tag when *all* files are generated.
4. **Example**: It provides a concrete example demonstrating the format and the completion marker.
5. **Completeness guidance**: It emphasizes that files should be complete and ready to use.
6. **Continuation Logic**: It explains *not* to use the marker if more files are needed in subsequent turns.

This explicit instruction about the completion marker is crucial for the updated `shouldContinue` logic, which now relies on this signal rather than heuristics about cut-off responses.

## Usage Examples

Here's an example of how to use the `FileCollectionAgent`:

```go
// main.go (example usage)
package main

import (
	"context"
	"fmt"
	"log"
	"os"

	// Assume FileCollectionAgent is defined in a local 'customagent' package
	"your_project_path/customagent" // Replace with actual path
	"github.com/goagent/framework/goagent/llm"
	// Import necessary geppetto settings
	"github.com/go-go-golems/geppetto/pkg/steps/ai/settings"
	"github.com/go-go-golems/geppetto/pkg/steps/ai/types" // For ApiType
)

func main() {
	// Create settings for the LLM
	stepSettings, err := settings.NewStepSettings()
	if err != nil {
		log.Fatalf("Error creating settings: %v", err)
	}

    // Configure LLM (e.g., setting API type, engine, etc.)
    // ... configuration code ...

	// Create the LLM instance using GeppettoLLM from goagent/llm
	geppettoLLM, err := llm.NewGeppettoLLM(stepSettings)
	if err != nil {
		log.Fatalf("Error creating LLM: %v", err)
	}

	// Create the FileCollectionAgent
	// Use the constructor from your custom agent package
	fileAgent := customagent.NewFileCollectionAgent(geppettoLLM, 5) // Allow up to 5 LLM calls

	// Run the agent with a prompt
	ctx := context.Background()
	prompt := "Create a simple Go web server using the standard library. It should have two routes: '/' which displays 'Hello, World!', and '/ping' which displays 'pong'. Put the server code in 'server.go' and the main function in 'main.go'."
	resultSummary, err := fileAgent.Run(ctx, prompt)
	if err != nil {
		log.Fatalf("Agent execution failed: %v", err)
	}

	// Print the summary returned by Run
	fmt.Println("Agent Run Summary:")
	fmt.Println(resultSummary)

	// Get and save the generated files
	files := fileAgent.GetFiles() // Use the GetFiles method
	fmt.Printf("\nGenerated %d files:\n", len(files))
	outputDir := "generated_files"
	err = os.MkdirAll(outputDir, 0755)
	if err != nil {
		log.Fatalf("Failed to create output directory: %v", err)
	}

	for name, content := range files {
		filePath := fmt.Sprintf("%s/%s", outputDir, name)
		err := os.WriteFile(filePath, []byte(content), 0644)
		if err != nil {
			log.Printf("Error writing file %s: %v", filePath, err)
		} else {
			log.Printf("Wrote file: %s", filePath)
		}
	}
}
```

## Advanced Techniques

Here are some advanced techniques for improving the `FileCollectionAgent`:

1. **Progress Tracking**: Add a progress tracking mechanism that estimates how many files are needed and how many have been generated.

2. **File Dependency Detection**: Implement logic to detect dependencies between files and ensure they're all generated.

3. **Validation and Linting**: Add validation for generated files to ensure they are syntactically correct.

4. **Interactive Refinement**: Allow users to request modifications to specific files without regenerating everything.

5. **Template Support**: Add support for using existing templates as a starting point for file generation.

These advanced techniques can significantly enhance the agent's usability and reliability.

## Conclusion

The GoAgent framework provides a flexible and powerful foundation for building LLM-powered agents. By understanding the framework's architecture and building on existing patterns, you can create custom agents tailored to specific use cases, like our FileCollectionAgent.

The XML file delimiter approach, combined with an explicit completion marker (`<!-- all files emitted -->`), offers a robust way to extract structured content from LLM responses, enabling reliable multi-file generation.

By following the patterns and practices outlined in this tutorial, you can develop sophisticated agents that leverage the power of LLMs while maintaining clean, modular code using the GoAgent framework (found in `github.com/goagent/framework`). 