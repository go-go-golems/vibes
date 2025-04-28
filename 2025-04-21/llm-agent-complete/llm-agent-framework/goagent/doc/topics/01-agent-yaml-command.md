
# Creating Agent Command YAML Files in GoAgent Framework

## Introduction

The GoAgent framework allows you to define AI agent commands using YAML configuration files. These YAML definitions provide a declarative way to create agent commands without writing Go code, enabling rapid development and easy modification of agent behavior. This approach separates agent configuration from implementation, making it easier to create, share, and modify agent behaviors.

YAML-based agent commands can be used to:

- Define prompts and system instructions
- Configure agent behavior and reasoning patterns
- Specify available tools
- Define command parameters (flags and arguments)
- Control output format (plain text or structured data)

## YAML Structure Fundamentals

Agent command YAML files follow a specific structure that maps to the `AgentCommand` implementation in the framework. The basic structure includes:

```yaml
name: command-name
short: "Short description"
long: |
  Longer description over
  multiple lines
flags:
  - name: parameter
    type: string
    help: "Description of parameter"
    default: "default value"
arguments: # (when an input query is needed)
  - name: query
    type: stringList
    help: "The main query"
    required: true
command-type: writer # or glazed
agent-type: react # or other agent types
system-prompt: "Initial system instructions to the AI model"
prompt: "Template for the user's query: {{ .parameter }}"
tools:
  - tool-name-1
  - tool-name-2
```

Each of these fields maps directly to properties in the `AgentCommand` structure and influences how the command behaves.

### Key Fields:

- **`name`**: Command identifier, used in CLI invocation.
- **`short`/`long`**: Command descriptions for help text.
- **`command-type`**: Determines output format (`writer` for text, `glazed` for structured data).
- **`agent-type`**: Specifies the reasoning logic pattern (e.g., `react`, `plan-execute`).
- **`system-prompt`**: Instructions to guide the agent's behavior.
- **`prompt`**: Template for the initial query, supporting Go template syntax.
- **`tools`**: List of tools the agent can use.
- **`flags`/`arguments`**: Command parameters.

## Command Types

The GoAgent framework supports two primary command types, specified by the `command-type` field:

### Writer Commands

Writer commands (specified with `command-type: writer` or by default if omitted) output plain text. This type is suitable for:

- Question answering
- Text generation
- Simple summaries
- Conversational agents

Example:
```yaml
name: weather-agent
command-type: writer # Could also be omitted as 'writer' is the default
agent-type: react
# Rest of configuration...
```

### Glazed Commands

Glazed commands (specified with `command-type: glazed`) output structured data that can be formatted as JSON, YAML, etc. This type is suitable for:

- Generating multiple files
- Returning structured data
- Creating tables or lists
- Any output that benefits from formal structure

Example:
```yaml
name: file-generator
command-type: glazed
agent-type: file-collection
# Rest of configuration...
```

## Agent Types

The `agent-type` field specifies the reasoning pattern the agent will follow. Common types include:

- **`react`**: Reasoning + Acting pattern, thinks step by step with tool use.
- **`plan-execute`**: First creates a plan, then executes each step.
- **`file-collection`**: Specialized for generating multiple files.

The agent type determines how the agent processes input and generates output. For example:

```yaml
# ReAct agent that uses step-by-step reasoning
name: assistant
agent-type: react
system-prompt: "You are a helpful assistant that thinks step by step."
```

## Prompt Templating

The `prompt` field supports Go template syntax, allowing you to incorporate parameters from flags and arguments:

```yaml
prompt: "Answer this {{.language}} coding question: {{ .query | join \" \" }}"
```

### Template Variables

Any flag or argument is available as a variable in the template:
- `.flag_name` accesses a flag's value
- `.argument_name` accesses an argument's value

### Template Functions

You can use Go template functions:
- `{{ .stringList | join " " }}` joins list items with spaces
- `{{- if .condition -}}...{{- end -}}` for conditional content

Example with conditional logic:
```yaml
prompt: |
  Help with the following {{.language}} task: 
  {{ .query | join " " }}
  {{- if .detailed -}}
  Please provide detailed explanations and examples.
  {{- end -}}
```

## Tools Configuration

The `tools` section lists which tools the agent can use during execution:

```yaml
tools:
  - web-search
  - code-search
  - file-read
```

Available tools depend on the framework implementation, but common ones include:
- `web-search`: Search the web for information
- `code-search`: Search code repositories
- `file-read`: Read file contents
- `file-write`: Write to files

## Parameter Configuration

### Flags

Flags define optional parameters with potential default values:

```yaml
flags:
  - name: language
    type: string
    help: "Programming language to work with"
    default: "go"
  - name: detailed
    type: bool
    help: "Whether to provide detailed output"
    default: false
```

#### Flag Types:
- `string`: Text values
- `int`: Integer values
- `float`: Floating-point values
- `bool`: Boolean values
- `stringList`: List of strings

### Arguments

Arguments define required or optional positional parameters:

```yaml
arguments:
  - name: query
    type: stringList
    help: "The main query"
    required: true
```

Arguments work similarly to flags but are positional rather than named.

## Complete Examples

### Simple Writer Command (Question Answering)

```yaml
name: qa-agent
short: "Question answering agent"
long: "An agent that answers questions using web search when needed."
# command-type: writer is implied (default)
agent-type: react
system-prompt: "You are a helpful AI assistant that answers questions accurately."
prompt: "Answer this question: {{ .query | join \" \" }}{{if .detailed}} Please be detailed.{{end}}"
tools:
  - web-search
flags:
  - name: detailed
    type: bool
    help: "Whether to provide a detailed answer"
    default: false
arguments:
  - name: query
    type: stringList
    help: "The question to answer"
    required: true
```

### Glazed Command (File Generation)

```yaml
name: api-generator
short: "Generate REST API code"
long: "An agent that generates REST API code in various languages."
command-type: glazed
agent-type: file-collection
system-prompt: |
  You are an AI assistant that generates complete, ready-to-use code files.
  When asked to create files, follow these instructions:
  1. Generate each file's complete content sequentially.
  2. Wrap EACH file's content within <file name="filename.ext">...</file> XML tags.
  3. Include <!-- all files emitted --> on a new line AFTER the last </file> tag.
prompt: |
  Generate a complete REST API for {{.resource}} in {{.language}}.
  {{- if .authentication -}}
  Include authentication using {{.authentication}}.
  {{- end -}}
tools:
  - web-search
flags:
  - name: language
    type: string
    help: "Programming language to use"
    default: "go"
  - name: authentication
    type: string
    help: "Authentication method (jwt, oauth, basic)"
    default: ""
arguments:
  - name: resource
    type: string
    help: "Resource name for the API (e.g., users, products)"
    required: true
```

### Multi-Document YAML

You can define multiple commands in a single YAML file using `---` as a separator:

```yaml
name: first-command
# First command definition
---
name: second-command
# Second command definition
```

## Loading and Running YAML Commands

The framework loads YAML commands using the `LoadFromYAML` function:

```go
// Loading from byte slice (as shown in main.go)
yamlContent := []byte(`...YAML content...`)
agentCommands, err := goagentcmds.LoadFromYAML(yamlContent)
if err != nil {
    log.Error().Err(err).Msg("Error loading commands from YAML")
    os.Exit(1)
}

// Adding to Cobra commands
for _, cmd := range agentCommands {
    cobraCmd, err := pinocchio_cmds.BuildCobraCommandWithGeppettoMiddlewares(cmd)
    if err != nil {
        log.Error().Err(err).Str("command", cmd.Description().Name).Msg("Error building cobra command")
        continue
    }
    rootCmd.AddCommand(cobraCmd)
}
```

You can also load from files:

```go
// Loading from a file
agentCommands, err := goagentcmds.LoadFromFile("path/to/commands.yaml")
```

## Advanced Configuration

### LLM Settings Configuration

The agent commands automatically integrate with Geppetto LLM settings, allowing you to control:

- Model selection
- Temperature
- Max tokens
- Other LLM parameters

These are controlled via standard Geppetto flags (e.g., `--model`, `--temperature`) and don't need to be defined in the YAML.

### Agent-Specific Options

For specialized agent behavior, you can include an `agent-options` map:

```yaml
agent-options:
  max_iterations: 10
  memory_type: "vector"
  # Other agent-specific options
```

## Best Practices

When creating YAML agent commands:

- **Be specific in system prompts**: Clearly define the agent's role and behavior.
- **Use template variables thoughtfully**: Make your prompts adaptable but not overly complex.
- **Include only necessary tools**: Limit to what the agent actually needs.
- **Provide helpful parameter descriptions**: Write clear help text for users.
- **Test with various inputs**: Ensure your templates handle all cases.
- **Use appropriate command types**: Choose writer vs. glazed based on output needs.

## Conclusion

YAML agent commands provide a powerful, declarative way to create AI agents with the GoAgent framework. By understanding the structure and options available, you can create sophisticated agents without writing Go code, accelerating development and experimentation.

The separation of agent configuration (YAML) from implementation (Go framework) makes it easier to iterate on agent behavior and share agent definitions, while still leveraging the full power of the underlying framework.
