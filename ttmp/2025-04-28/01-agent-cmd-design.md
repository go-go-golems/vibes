# Agent Command Design: Creating Custom Command Types for Agent Execution

## Introduction

This document outlines a design plan for creating custom Command types specifically tailored for agent execution in the GoAgent framework. The design centers around a base `AgentCommand` for configuration and two concrete types: `WriterAgentCommand` for text output and `GlazedAgentCommand` for structured output. This approach builds upon patterns from Pinocchio (`pinocchio/pkg/cmds/cmd.go`) while focusing on LLM agent needs, simplifying the execution of different agent types (ReAct, Plan-and-Execute, FileCollectionAgent, etc.) within a unified command structure.

## Background and Motivation

Executing agents previously required significant boilerplate. A dedicated command structure abstracts common steps like LLM setup, prompt rendering, agent instantiation, and output handling, making agent execution more straightforward and consistent. The split into `WriterAgentCommand` and `GlazedAgentCommand` further clarifies the intended output format.

## Design Goals

-   **Consistency**: Provide a unified configuration structure (`AgentCommand`).
-   **Clarity**: Clearly distinguish commands based on output type (`WriterAgentCommand`, `GlazedAgentCommand`).
-   **Configurability**: Allow comprehensive configuration via flags, layers, and config files (LLM settings, agent params, prompts).
-   **Extensibility**: Support custom agent logic types and tools.
-   **Usability**: Reduce boilerplate for common agent execution patterns.
-   **Integration**: Seamlessly integrate with Cobra and Glazed frameworks.

## Command Type Structure

The design involves a base configuration command and two executable command types:

1.  **`AgentCommand` (Base Configuration)** (`goagent/cmds/command.go`)
    -   Holds the `*cmds.CommandDescription` (name, short, long, flags, arguments, layers).
    -   Contains agent-specific configuration fields:
        -   `AgentType`: string (e.g., "react", "plan-execute", "file-collection") - Identifies the *agent logic* to load.
        -   `SystemPrompt`: string
        -   `Prompt`: string (Go template for the initial user prompt)
        -   `Tools`: []string
        -   `AgentOptions`: map[string]interface{} (Passed to the agent constructor)
    -   Includes standard parameter layers (Geppetto for LLM, AgentLayer).
    -   Provides shared methods like `prepareAgent` and `renderInitialPrompt`.
    -   Does **not** implement `cmds.WriterCommand` or `cmds.GlazeCommand` itself.

2.  **`WriterAgentCommand`** (`goagent/cmds/command.go`)
    -   Embeds `*AgentCommand`.
    -   Implements `cmds.WriterCommand`.
    -   Its `RunIntoWriter` method:
        -   Calls `prepareAgent`.
        -   Calls `renderInitialPrompt`.
        -   Calls the agent's standard `Run(ctx, prompt) (string, error)` method.
        -   Writes the returned string to the `io.Writer`.

3.  **`GlazedAgentCommand`** (`goagent/cmds/command.go`)
    -   Embeds `*AgentCommand`.
    -   Implements `cmds.GlazeCommand`.
    -   Its `RunIntoGlazeProcessor` method:
        -   Calls `prepareAgent`.
        -   Calls `renderInitialPrompt`.
        -   Type-asserts the prepared agent to `agent.GlazedAgent`.
        -   If successful, calls the agent's `RunIntoGlazeProcessor(ctx, prompt, gp)` method.
        -   If assertion fails, returns an error.

### YAML Configuration (`AgentCommandDescription`)

The YAML structure used by the loader mirrors the base `AgentCommand` fields and adds a way to specify the command type:

```yaml
# Used by AgentCommandLoader to create either Writer- or GlazedAgentCommand
name: code-assistant
short: "AI code assistant"
command-type: glazed # "writer" (default) or "glazed"
agent-type: file-collection # Identifies agent logic (react, plan-execute, etc.)
tools:
  - code-search
flags:
  - name: language
    type: string
    default: "python"
  # ... other flags
arguments:
  - name: query
    type: stringList
    required: true
system-prompt: |
  You are an AI coding assistant for {{.language}}.
prompt: |
  {{ .query | join " " }}
  {{- /* other template logic using flags/args */ -}}
```

-   `command-type`: Specifies whether to load a `WriterAgentCommand` or `GlazedAgentCommand`. Defaults to `writer` if omitted.
-   `agent-type`: Specifies the *logic* of the agent (e.g., 'react', 'file-collection'). Used by `prepareAgent` to find the correct agent constructor.

## Templated Prompt System

The `Prompt` field in `AgentCommand` uses Go's `text/template` syntax.

### Template Variables

Accessible variables within the `Prompt` template include all parameters defined in the command's layers (especially the default layer populated by flags and arguments). Standard Go template functions can be used.

### Prompt Rendering Process

1.  The `WriterAgentCommand` or `GlazedAgentCommand` parses flags, arguments, and layers.
2.  It calls `AgentCommand.renderInitialPrompt`, passing the `parsedLayers`.
3.  `renderInitialPrompt` extracts parameters from the default layer (`parsedLayers.GetDefaultParameterLayer().Parameters.ToMap()`).
4.  It executes the `AgentCommand.Prompt` template using these parameters as the data context.
5.  The resulting string is returned and passed as the `input` argument to the agent's `Run` or `RunIntoGlazeProcessor` method.

## Core Components

### 1. Agent Command Loader (`AgentCommandLoader` in `loader.go`)

-   Loads agent command definitions from YAML files.
-   Parses the `AgentCommandDescription` structure from YAML.
-   Reads the `command-type` field (defaulting to "writer").
-   Calls `NewWriterAgentCommand` or `NewGlazedAgentCommand` based on `command-type`.
-   Passes YAML fields (agent-type, prompt, tools, etc.) as options to the chosen constructor.

```go
// Simplified Loader Logic Pseudocode
func (loader *AgentCommandLoader) loadAgentCommandFromReader(...) ([]cmds.Command, error) {
    // Decode YAML into description (AgentCommandDescription)
    // ...

    // Create base cmdDescription from description fields (name, flags, etc.)
    cmdDescription := cmds.NewCommandDescription(...)

    // Collect agent options from description
    agentOptions := []AgentCommandOption{
        WithAgentType(description.AgentType),
        WithSystemPrompt(description.SystemPrompt),
        // ... other options ...
    }

    // Determine command type
    cmdType := description.CommandType // Assume CommandType field exists
    if cmdType == "" {
        cmdType = "writer" // Default
    }

    var command cmds.Command
    var err error

    // Instantiate the correct command type
    if cmdType == "glazed" {
        command, err = NewGlazedAgentCommand(cmdDescription, agentOptions...)
    } else { // Default to writer
        command, err = NewWriterAgentCommand(cmdDescription, agentOptions...)
    }

    if err != nil {
        return nil, err
    }

    return []cmds.Command{command}, nil
}
```

### 2. Command Options (`AgentCommandOption`)

Functional options (`WithAgentType`, `WithPrompt`, etc.) configure the base `AgentCommand` during construction.

### 3. Parameter Layers (`layers.go`)

-   **Agent Layer** (`AgentLayerSlug`): Defined in `layers.go`, configures agent execution parameters like `agent-type`, `max-iterations`, `memory-type`. These are parsed into `AgentSettings`.
-   **Geppetto Layers**: Added automatically by `NewAgentCommand` (via `pinocchio_cmds.CreateGeppettoLayers`) to handle LLM configuration (`model`, `temperature`, etc.), prompt variables, and other AI step settings. Parsed into `settings.StepSettings`.
-   **Helpers Layer**: Added by `NewPinocchioCommand` (if used as a base), handles meta-options like autosave, interactive mode.

## Command Execution Flow

### `WriterAgentCommand` Flow

1.  Cobra/Glazed framework parses flags/args into `parsedLayers`.
2.  `WriterAgentCommand.RunIntoWriter` is called.
3.  Calls `AgentCommand.prepareAgent(ctx, parsedLayers)`:
    -   Gets `AgentSettings` from the agent layer.
    -   Gets `StepSettings` from Geppetto layers.
    -   Creates `GeppettoLLM` using `StepSettings`.
    -   Looks up agent constructor using `AgentSettings.AgentType` from a registry.
    -   Instantiates the agent logic (e.g., `ReActAgent`, `PlanAndExecuteAgent`).
    -   Adds tools, sets memory based on `AgentSettings`.
    -   Returns the `agent.Agent` instance.
4.  Calls `AgentCommand.renderInitialPrompt(parsedLayers)` to get the initial prompt string.
5.  Calls `agentInstance.Run(ctx, initialPrompt)`.
6.  Receives the result string.
7.  Writes the string to the `io.Writer`.

### `GlazedAgentCommand` Flow

1.  Cobra/Glazed framework parses flags/args into `parsedLayers`.
2.  `GlazedAgentCommand.RunIntoGlazeProcessor` is called with the `middlewares.Processor`.
3.  Calls `AgentCommand.prepareAgent(ctx, parsedLayers)` (same as step 3 above).
4.  Calls `AgentCommand.renderInitialPrompt(parsedLayers)` to get the initial prompt string.
5.  Type-asserts the returned `agentInstance` to `agent.GlazedAgent`.
    -   If fails: return error.
6.  Calls `glazedAgent.RunIntoGlazeProcessor(ctx, initialPrompt, gp)`.
    -   The agent implementation is responsible for running its logic and adding rows to the `gp`. (e.g., `FileCollectionAgent` calls its own `Run` then iterates `a.files` to add rows).
7.  Returns `nil` on success (or error from the agent/processor).

### Diagram: Execution Flow Comparison

```
+-----------------------+        +------------------------+
| WriterAgentCommand    |        | GlazedAgentCommand     |
| RunIntoWriter(w)      |        | RunIntoGlazeProcessor(gp)|
+-----------+-----------+        +-----------+------------+
            |                                |
            | prepareAgent()                 | prepareAgent()
            | renderPrompt()                 | renderPrompt()
            |                                |
            ▼                                ▼
+-----------+-----------+        +-----------+------------+
|      agent.Agent      |        |      agent.Agent       |
| instance              |        | instance               |
+-----------+-----------+        +-----------+------------+
            |                                |
            | Run(ctx, prompt)               | Type-assert to
            |   -> returns string            | agent.GlazedAgent
            |                                +----------->+ Yes: Call RunIntoGlazeProcessor(ctx, prompt, gp)
            ▼                                           |    (Agent adds rows to gp)
+-----------+-----------+                               + No: Return Error
| Write string to w     |
+-----------------------+

```

## Agent Type Registry

An agent *logic* type registry remains essential:

```go
type AgentConstructor func(ctx context.Context, llmModel llm.LLM, options map[string]interface{}) (agent.Agent, error)

// Registry maps agent-type string (from AgentSettings) to its constructor
var agentRegistry = map[string]AgentConstructor{
    "react":           NewReActAgent,
    "plan-execute":    NewPlanAndExecuteAgent,
    "file-collection": NewFileCollectionAgent,
    // ... register custom agents here
}

// Used within prepareAgent:
constructor := agentRegistry[agentSettings.AgentType]
agentInstance, err := constructor(ctx, llmModel, agentOptions)
```

This registry is used within `prepareAgent` based on the `agent-type` parameter from the `AgentLayer`. The *command* type (`Writer`/`Glazed`) is determined by the loader based on the `command-type` field in the YAML.

## Integration with Existing Framework

(Diagrams showing YAML -> Loader -> {Writer/Glazed}Command -> Cobra are conceptually similar, emphasizing the loader's role in choosing the command type).

## Command Types

-   **`WriterAgentCommand`**: Implements `cmds.WriterCommand`. Ideal for agents whose primary output is text. Executes the standard `agent.Run` method.
-   **`GlazedAgentCommand`**: Implements `cmds.GlazeCommand`. Ideal for agents producing structured data. Requires the underlying agent logic to implement the `agent.GlazedAgent` interface and its `RunIntoGlazeProcessor` method.

## Example Usage Scenarios

-   **Simple ReAct Agent (Writer):**
    ```bash
    # Assuming react-agent loaded as WriterAgentCommand (default)
    agent-cmd react --prompt "Weather in Paris?" --tools web-search
    # Output: Plain text weather report
    ```
-   **File Collection Agent (Glazed):**
    ```bash
    # Assuming file-agent loaded as GlazedAgentCommand (command-type: glazed)
    agent-cmd file-collection --prompt "Create Go REST API..." --output=json
    # Output: JSON array of {filename, content} objects
    ```
-   **Plan-and-Execute (Writer):**
    ```bash
    # Assuming plan-agent loaded as WriterAgentCommand
    agent-cmd plan-execute --prompt "Refactor codebase..." --tools code-search
    # Output: Plain text summary of refactoring actions or final status
    ```

## Implementation Strategy

### Phase 1: Core Command Structure (Done)

1.  Define base `AgentCommand` structure.
2.  Define `WriterAgentCommand` embedding `AgentCommand`, implement `RunIntoWriter`.
3.  Define `GlazedAgentCommand` embedding `AgentCommand`, implement `RunIntoGlazeProcessor`.
4.  Implement `prepareAgent` and `renderInitialPrompt` in base `AgentCommand`.
5.  Create constructors `NewWriterAgentCommand`, `NewGlazedAgentCommand`.

### Phase 2: Agent Interface & Loader (Done)

1.  Define `Agent`, `WriterAgent`, `GlazedAgent` interfaces in `agent/agent.go`.
2.  Update `AgentCommandLoader` to read `command-type` from YAML and call the appropriate constructor (`NewWriterAgentCommand` or `NewGlazedAgentCommand`). Add `command-type` to `AgentCommandDescription`.

### Phase 3: Agent Implementation (Partially Done - FileCollectionAgent)

1.  Ensure standard agents (ReAct, PlanExecute) implement `WriterAgent`.
2.  Update `FileCollectionAgent` to implement `GlazedAgent`:
    -   Keep `Run` returning `(string, error)` (summary).
    -   Implement `RunIntoGlazeProcessor` to call `Run` and process `a.files` into the processor.

### Phase 4: Parameter Layers & LLM Integration (Done)

1.  Define `AgentLayer` in `layers.go`.
2.  Ensure `NewAgentCommand` adds `AgentLayer` and Geppetto layers.
3.  Ensure `prepareAgent` correctly uses `AgentSettings` and `StepSettings` to configure the agent and LLM.

### Phase 5: Tool Integration & Documentation (Ongoing)

1.  Refine tool registration and usage within agents.
2.  Update documentation and examples thoroughly.
3.  Develop integration tests.

## Conclusion

This revised design introduces a clearer separation between agent configuration (`AgentCommand`), output intention (`WriterAgentCommand` vs. `GlazedAgentCommand`), and agent capabilities (`Agent` vs. `GlazedAgent`). It leverages Go interfaces and embedding effectively, integrates prompt templating and LLM configuration via layers, and provides a robust foundation for building and executing diverse LLM agents through a command-line interface. 