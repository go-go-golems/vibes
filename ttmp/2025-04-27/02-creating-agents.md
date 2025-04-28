# Building LLM Agents with the GoAgent Framework

This tutorial provides a detailed step-by-step guide on how to build LLM agents using the GoAgent framework. We'll explore the framework's architecture, examine existing agent patterns, and build a custom agent that can generate multiple files from a single prompt and output them as structured data.

## Table of Contents

1. [Introduction to the GoAgent Framework](#introduction-to-the-goagent-framework)
2. [Framework Architecture](#framework-architecture)
   - [Agent Interfaces](#agent-interfaces)
   - [Core Components](#core-components)
3. [Example Agent Patterns](#example-agent-patterns)
   - [ReAct Pattern (WriterAgent)](#react-pattern-writeragent)
   - [Plan-and-Execute Pattern (WriterAgent)](#plan-and-execute-pattern-writeragent)
4. [Building a File Collection Agent (GlazedAgent)](#building-a-file-collection-agent-glazedagent)
   - [Agent Design](#agent-design)
   - [Implementation](#implementation)
       - [`Run` Method (Summary Output)](#run-method-summary-output)
       - [`RunIntoGlazeProcessor` Method (Structured Output)](#runintoglazeprocessor-method-structured-output)
   - [XML File Delimiter Prompt](#xml-file-delimiter-prompt)
5. [Usage Examples](#usage-examples)
   - [Using a WriterAgent](#using-a-writeragent)
   - [Using a GlazedAgent](#using-a-glazedagent)
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

The GoAgent framework consists of several core components, primarily located within the `goagent` directory.

### Agent Interfaces

The framework defines a hierarchy of interfaces for agents in `goagent/agent/agent.go`:

1. **`Agent` (Base Interface)**
   - Defines the fundamental agent capabilities.
   - Key Method: `Run(ctx context.Context, input string) (string, error)`
      - Executes the agent's core logic.
      - Returns a single string summarizing the result or the final answer. Suitable for simple text-based output.
   - Other Methods: `AddTool(tool tools.Tool)`, `SetMemory(mem memory.Memory)`.

2. **`WriterAgent`**
   - Embeds `Agent`.
   - A marker interface for agents whose primary output mechanism is the standard `Run` method returning a string.

3. **`GlazedAgent`**
   - Embeds `Agent`.
   - Defines agents capable of producing structured output directly.
   - Key Method: `RunIntoGlazeProcessor(ctx context.Context, input string, gp middlewares.Processor) error`
      - Executes the agent's logic.
      - Responsible for adding structured data (as `types.Row`) directly to the provided Glazed processor (`gp`).

### Core Components

- **BaseAgent** (`goagent/agent/agent.go`): Provides common functionality (LLM integration via `llm.LLM`, tool execution via `tools.ToolExecutor`, memory, tracing). Implements the base `Agent` interface methods `AddTool` and `SetMemory`. Agents typically embed `BaseAgent`.
- **LLM Interface** (`goagent/llm/llm.go`): Abstracts language models (e.g., `GeppettoLLM` backed by `settings.StepSettings`).
- **Tools** (`goagent/tools/tools.go`): Defines the `Tool` interface and `ToolExecutor`.
- **Memory** (`goagent/memory/...`): For state persistence.
- **Tracing** (`goagent/tracing/...`): For execution logging.
- **Commands** (`goagent/cmds/...`): Provide the command-line interface.
   - `AgentCommand`: Base configuration struct.
   - `WriterAgentCommand`: Implements `cmds.WriterCommand`, interacts with `Agent.Run`.
   - `GlazedAgentCommand`: Implements `cmds.GlazeCommand`, interacts with `GlazedAgent.RunIntoGlazeProcessor`.
   - `AgentCommandLoader`: Loads commands from YAML.
   - `AgentLayer`: Glazed parameter layer for agent settings.

## Example Agent Patterns

Let's examine standard agent patterns, assuming they primarily return text summaries.

### ReAct Pattern (WriterAgent)

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

- **Implementation**: The `ReActAgent` in `react.go` would embed `BaseAgent` and implement the `Agent` interface. Its `Run` method performs the think-act-observe loop and returns a final string answer.
- **Interface Compliance**: It implicitly satisfies `WriterAgent`.
- **Command Usage**: Would typically be loaded and run using a `WriterAgentCommand`.

```go
// Simplified ReAct Run Method Signature
func (a *ReActAgent) Run(ctx context.Context, input string) (string, error) {
    // ... ReAct loop ...
    finalAnswer := a.extractFinalAnswer(lastResponse) // Assume this extracts the string answer
    return finalAnswer, nil
}

// Check interface compliance (implicitly satisfies WriterAgent)
var _ Agent = (*ReActAgent)(nil)
```

### Plan-and-Execute Pattern (WriterAgent)

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

- **Implementation**: The `PlanAndExecuteAgent` in `plan-and-execute.go` embeds `BaseAgent`. Its `Run` method generates a plan, executes steps, and returns a final string summary or result.
- **Interface Compliance**: Satisfies `WriterAgent`.
- **Command Usage**: Would typically be loaded and run using a `WriterAgentCommand`.

```go
// Simplified Plan-and-Execute Run Method Signature
func (a *PlanAndExecuteAgent) Run(ctx context.Context, input string) (string, error) {
    // ... Plan generation ...
    // ... Step execution ...
    finalSummary := a.generateFinalSummary(results) // Assume this creates the string summary
    return finalSummary, nil
}

// Check interface compliance (implicitly satisfies WriterAgent)
var _ Agent = (*PlanAndExecuteAgent)(nil)
```

## Building a File Collection Agent (GlazedAgent)

Now let's adapt our custom `FileCollectionAgent` to fit the `GlazedAgent` pattern, enabling structured output.

### Agent Design

Our `FileCollectionAgent` will:

1. Implement the `GlazedAgent` interface.
2. Use its `Run` method for the core LLM interaction loop (generating file content delimited by XML tags) and return a simple summary string.
3. Implement the `RunIntoGlazeProcessor` method to:
   - Call the internal `Run` method to populate the `a.files` map.
   - Iterate through the collected `a.files`.
   - Create a `types.Row` for each file (e.g., with "filename" and "content" fields).
   - Add each row to the provided `middlewares.Processor`.

### Implementation

```go
// file-collection.go
package agent

import (
	"context"
	"fmt"
	"regexp"
	"strings"

	"github.com/go-go-golems/geppetto/pkg/conversation"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/goagent/framework/goagent/llm"
	goagent_types "github.com/goagent/framework/goagent/types"
	"github.com/pkg/errors"
	"golang.org/x/exp/maps"
)

// FileCollectionAgent implements the GlazedAgent interface.
type FileCollectionAgent struct {
	*BaseAgent
	extractor *FileExtractor
	files     map[string]string
}

// NewFileCollectionAgent creates a new FileCollectionAgent.
func NewFileCollectionAgent(llmModel llm.LLM, maxIterations int) *FileCollectionAgent {
	return &FileCollectionAgent{
		BaseAgent: NewBaseAgent(llmModel, maxIterations),
		extractor: NewFileExtractor(),
		files:     make(map[string]string),
	}
}

// GetFiles returns the generated files collected during the last Run.
func (a *FileCollectionAgent) GetFiles() map[string]string {
	return a.files
}

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

// buildSummary builds the summary string for the FileCollectionAgent
func (a *FileCollectionAgent) buildSummary() string {
	var summary strings.Builder
	summary.WriteString(fmt.Sprintf("Generated %d files:\n\n", len(a.files)))
	
	for name := range a.files {
		summary.WriteString(fmt.Sprintf("- %s\n", name))
	}
	
	return summary.String()
}

// --- Agent Interface Methods ---

// Run executes the core file generation logic and returns a summary string.
// This satisfies the base Agent interface.
func (a *FileCollectionAgent) Run(ctx context.Context, input string) (string, error) {
	ctx, span := a.tracer.StartSpan(ctx, "FileCollectionAgent.Run")
	defer span.End()

	// Reset files map and extractor state for this run
	a.files = make(map[string]string)
	a.extractor = NewFileExtractor()

	messages := []*conversation.Message{
		conversation.NewChatMessage(conversation.RoleSystem, a.buildSystemPrompt()),
		conversation.NewChatMessage(conversation.RoleUser, input),
	}

	// LLM iteration loop (same core logic as before)
	for i := 0; i < a.maxIter; i++ {
		responseMsg, err := a.llm.Generate(ctx, messages)
		if err != nil {
			summary := a.buildSummary()
			return summary, fmt.Errorf("LLM generation failed: %w", err)
		}
		response := responseMsg.Content.String()
		newCompleteFiles := a.extractor.Extract(response)
		for name, content := range newCompleteFiles {
			a.files[name] = content
		}

		// Logging, completion check, continuation prompt logic (same as before)
		a.tracer.LogEvent(ctx, goagent_types.Event{
			Type: "files_extracted",
			Data: map[string]interface{}{
				"iteration": i,
				"files":     maps.Keys(newCompleteFiles),
			},
		})

		if strings.Contains(response, "<!-- all files emitted -->") {
			break
		}

		// Add response and continuation prompt to messages
		messages = append(messages, responseMsg)
		messages = append(messages, conversation.NewChatMessage(
			conversation.RoleUser,
			"Continue generating any additional files needed to complete the implementation."))
	}

	// Return the summary string
	return a.buildSummary(), nil
}

// RunIntoGlazeProcessor executes the agent and streams file data as rows.
// This satisfies the GlazedAgent interface.
func (a *FileCollectionAgent) RunIntoGlazeProcessor(ctx context.Context, input string, gp middlewares.Processor) error {
	// Step 1: Execute the core logic via Run to populate a.files.
	// We ignore the summary string and handle the error below.
	_, runErr := a.Run(ctx, input)

	// Log if Run failed, but proceed to output any files collected before the error.
	if runErr != nil {
		a.tracer.LogEvent(ctx, goagent_types.Event{
			Type: "run_error_in_glazed_processor",
			Data: runErr.Error(),
		})
		// Decide if the error should halt processing entirely:
		// return errors.Wrap(runErr, "core agent run failed within GlazedProcessor")
	}

	// Step 2: Process the collected files into the Glaze processor.
	// a.files is populated by the call to a.Run above.
	for filename, content := range a.GetFiles() { // Use GetFiles() for consistency
		row := types.NewRow(
			types.MRP("filename", filename),
			types.MRP("content", content),
		)
		if err := gp.AddRow(ctx, row); err != nil {
			// If adding a row fails, wrap and return the error immediately.
			return errors.Wrapf(err, "failed to add row for file %s", filename)
		}
	}

	// Step 3: Return the original error from Run if it occurred, otherwise nil.
	// If AddRow failed, that error was already returned.
	if runErr != nil {
		return errors.Wrap(runErr, "core agent run failed but partial results might have been processed")
	}
	return nil
}

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

// FileExtractor struct and methods
type FileExtractor struct {
	// ... (existing fields and methods)
}
func NewFileExtractor() *FileExtractor {
	// ... (existing constructor)
}
func (fe *FileExtractor) Extract(chunk string) map[string]string {
	// ... (existing method)
}

// Ensure FileCollectionAgent implements the GlazedAgent interface.
var _ GlazedAgent = (*FileCollectionAgent)(nil)

```

#### Key Changes:

- **`Run` Method (Summary Output)**: Retains the core LLM interaction logic but returns only the summary string, fulfilling the base `Agent` interface.
- **`RunIntoGlazeProcessor` Method (Structured Output)**: This new method satisfies `GlazedAgent`. It calls `Run` to do the actual work and populate `a.files`, then iterates over `a.files` to add structured rows (`filename`, `content`) to the Glazed processor.
- **Interface Compliance**: The `var _ GlazedAgent = (*FileCollectionAgent)(nil)` line confirms it implements the necessary methods.

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

How you use an agent depends on the command type it's loaded with.

### Using a WriterAgent

If an agent (like `ReActAgent` or `PlanAndExecuteAgent`) is loaded via a `WriterAgentCommand` (either explicitly via `command-type: writer` in YAML or by default):

```go
// main_writer.go
package main

import (
	"context"
	"fmt"
	"os"

	// Assume commands are registered with Cobra/Glazed
)

func main() {
    // Cobra/Glazed executes the command...
    // Internally, WriterAgentCommand.RunIntoWriter is called.
    // It calls reactAgent.Run(ctx, prompt)
    // It gets back a string result.
    // It prints the string result to os.Stdout.

    // Example command line:
    // ./mytool react-agent --prompt "What is the capital of France?"

    // Output:
    // The capital of France is Paris.
}
```

### Using a GlazedAgent

If an agent (like `FileCollectionAgent`) is loaded via a `GlazedAgentCommand` (`command-type: glazed` in YAML):

```go
// main_glazed.go
package main

import (
	"context"
	"fmt"
	"os"

	// Assume commands are registered with Cobra/Glazed
)

func main() {
    // Cobra/Glazed executes the command...
    // Glazed middleware (e.g., JSON output) is set up.
    // Internally, GlazedAgentCommand.RunIntoGlazeProcessor is called.
    // It type-asserts the agent to agent.GlazedAgent.
    // It calls fileAgent.RunIntoGlazeProcessor(ctx, prompt, gp).
    // The agent adds rows {filename, content} to the processor gp.
    // The Glazed middleware formats and prints the rows.

    // Example command line:
    // ./mytool file-agent --prompt "Create main.go and helper.go..." --output=json

    // Output (JSON):
    // [
    //   {
    //     "filename": "main.go",
    //     "content": "package main\n..."
    //   },
    //   {
    //     "filename": "helper.go",
    //     "content": "package main\n..."
    //   }
    // ]
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

The GoAgent framework provides a flexible foundation. By separating agent logic interfaces (`Agent`, `GlazedAgent`) from command execution strategies (`WriterAgentCommand`, `GlazedAgentCommand`), we can build agents tailored to specific output needs – simple text summaries via `Run`, or complex structured data via `RunIntoGlazeProcessor` – while maintaining a consistent command-line configuration and execution experience.

The XML file delimiter approach, combined with an explicit completion marker (`<!-- all files emitted -->`), offers a robust way to extract structured content from LLM responses, enabling reliable multi-file generation.

By following the patterns and practices outlined in this tutorial, you can develop sophisticated agents that leverage the power of LLMs while maintaining clean, modular code using the GoAgent framework (found in `github.com/goagent/framework`). 