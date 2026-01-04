# GoAgent Example YAML Configurations

This directory contains YAML configurations for various agent types that can be loaded directly by the GoAgent framework. These YAML files define the behavior, capabilities, and user interface for different AI agents without requiring Go code.

## Available Agent Configurations

This directory includes:

- **research-agent.yaml**: A research assistant that can search the web and synthesize information.
- **travel-planning-agent.yaml**: A travel planner using the Plan-and-Execute pattern.
- **code-exploration-agent.yaml**: A code analysis tool that explores and explains codebases.
- **file-extraction-agent.yaml**: A file generation system that creates multiple code files.
- **multi-agent.yaml**: Contains multiple agent definitions in a single file.

## Using These Configurations

These configurations can be loaded into a GoAgent application using the `LoadFromYAML` or `LoadFromFile` functions:

```go
// Load from a single file
agentCommands, err := goagentcmds.LoadFromFile("goagent/examples/commands/research-agent.yaml")

// Or load from a directory
agentCommands, err := goagentcmds.LoadFromDirectory("goagent/examples/commands")

// Register with Cobra
for _, cmd := range agentCommands {
    cobraCmd, err := pinocchio_cmds.BuildCobraCommandWithGeppettoMiddlewares(cmd)
    if err != nil {
        log.Error().Err(err).Msg("Error building command")
        continue
    }
    rootCmd.AddCommand(cobraCmd)
}
```

## Agent Types

The configurations use different agent types:

- **react**: Reasoning and acting in steps (research, code-exploration agents)
- **plan-execute**: First planning then executing each step (travel-planning agent)
- **file-collection**: Specialized for generating multiple files (file-extraction agent)

## Command Types

Each agent uses one of two command types:

- **writer**: Produces plain text output (research, travel-planning, code-exploration)
- **glazed**: Produces structured data output (file-extraction)

## Customizing Agents

To customize these agents:

1. **Modify prompts**: Adjust the `system-prompt` and `prompt` sections
2. **Add tools**: Include additional tool names in the `tools` list
3. **Extend parameters**: Add new flags or arguments
4. **Change agent behavior**: Switch between agent types and command types

## Example Usage

```bash
# Research agent
goagent research "climate change impacts" --format report --length long

# Travel planning agent
goagent travel-planning --destination "Paris" --dates "June 15-22, 2025" "museums, local cuisine"

# Code exploration agent
goagent code-exploration "/path/to/project" --focus "architecture" --output_format "summary"

# File generation agent
goagent file-extraction --prompt "Create a REST API in Go with user authentication" --output-directory "./my-api" --language "go"
``` 