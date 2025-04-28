package cmds

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"text/template"

	"github.com/go-go-golems/geppetto/pkg/steps/ai/settings"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	pinocchio_cmds "github.com/go-go-golems/pinocchio/pkg/cmds"
	"github.com/goagent/framework/goagent/agent"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/memory"
	"github.com/goagent/framework/goagent/tools"
	goagentTypes "github.com/goagent/framework/goagent/types"
	"github.com/pkg/errors"
)

// AgentCommand is a command that encapsulates agent execution configuration.
// It serves as the base for WriterAgentCommand and GlazedAgentCommand.
// It doesn't implement the Run methods directly.
type AgentCommand struct {
	*cmds.CommandDescription

	// Agent-specific fields
	AgentType    string
	SystemPrompt string
	Prompt       string // Template string for the initial prompt
	Tools        []string
	AgentOptions map[string]interface{}
}

// WriterAgentCommand is an AgentCommand designed to output plain text results.
type WriterAgentCommand struct {
	*AgentCommand
}

// Ensure WriterAgentCommand implements the WriterCommand interface
var _ cmds.WriterCommand = &WriterAgentCommand{}

// GlazedAgentCommand is an AgentCommand designed to output structured data.
type GlazedAgentCommand struct {
	*AgentCommand
}

// Ensure GlazedAgentCommand implements the GlazeCommand interface
var _ cmds.GlazeCommand = &GlazedAgentCommand{}

// AgentCommandOption is a functional option for configuring an AgentCommand
type AgentCommandOption func(*AgentCommand)

// WithAgentType sets the agent type
func WithAgentType(agentType string) AgentCommandOption {
	return func(a *AgentCommand) {
		a.AgentType = agentType
	}
}

// WithSystemPrompt sets the system prompt
func WithSystemPrompt(systemPrompt string) AgentCommandOption {
	return func(a *AgentCommand) {
		a.SystemPrompt = systemPrompt
	}
}

// WithPrompt sets the prompt template
func WithPrompt(prompt string) AgentCommandOption {
	return func(a *AgentCommand) {
		a.Prompt = prompt
	}
}

// WithTools sets the tools to use
func WithTools(tools []string) AgentCommandOption {
	return func(a *AgentCommand) {
		a.Tools = tools
	}
}

// WithAgentOptions sets additional agent options
func WithAgentOptions(options map[string]interface{}) AgentCommandOption {
	return func(a *AgentCommand) {
		a.AgentOptions = options
	}
}

// NewAgentCommand creates a new base AgentCommand configuration.
// It's typically used internally by NewWriterAgentCommand and NewGlazedAgentCommand.
func NewAgentCommand(
	description *cmds.CommandDescription,
	options ...AgentCommandOption,
) (*WriterAgentCommand, error) {
	// Create agent parameter layer
	agentParameterLayer, err := NewAgentParameterLayer()
	if err != nil {
		return nil, errors.Wrap(err, "failed to create agent parameter layer")
	}

	// Add the agent layer to the command description
	description.Layers.PrependLayers(agentParameterLayer)

	// Add Geppetto layers for LLM configuration
	tempSettings, err := settings.NewStepSettings()
	if err != nil {
		return nil, errors.Wrap(err, "failed to create temporary step settings")
	}
	geppettoLayers, err := pinocchio_cmds.CreateGeppettoLayers(tempSettings)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create Geppetto layers")
	}
	description.Layers.AppendLayers(geppettoLayers...)

	ret := &WriterAgentCommand{
		AgentCommand: &AgentCommand{
			CommandDescription: description,
			AgentOptions:       make(map[string]interface{}),
		},
	}

	for _, option := range options {
		option(ret.AgentCommand)
	}

	return ret, nil
}

// NewWriterAgentCommand creates a new AgentCommand configured for text output.
func NewWriterAgentCommand(
	description *cmds.CommandDescription,
	options ...AgentCommandOption,
) (*WriterAgentCommand, error) {
	agentCmd, err := NewAgentCommand(description, options...)
	if err != nil {
		return nil, err
	}
	return agentCmd, nil
}

// NewGlazedAgentCommand creates a new AgentCommand configured for structured output.
func NewGlazedAgentCommand(
	description *cmds.CommandDescription,
	options ...AgentCommandOption,
) (*GlazedAgentCommand, error) {
	agentCmd, err := NewAgentCommand(description, options...)
	if err != nil {
		return nil, err
	}
	return &GlazedAgentCommand{AgentCommand: agentCmd.AgentCommand}, nil
}

// prepareAgent prepares an agent based on the given settings
func (a *AgentCommand) prepareAgent(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
) (agent.Agent, error) {
	// Get agent settings from parsed layers
	agentSettings, err := GetAgentSettingsFromParsedLayers(parsedLayers)
	if err != nil {
		return nil, errors.Wrap(err, "failed to get agent settings")
	}

	// Use agent type from settings or from command
	agentType := agentSettings.AgentType
	if a.AgentType != "" {
		agentType = a.AgentType
	}

	// Create StepSettings from parsed layers for LLM creation
	stepSettings, err := settings.NewStepSettingsFromParsedLayers(parsedLayers)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create step settings from parsed layers")
	}

	// Prepare LLM model using Geppetto LLM
	llmModel, err := llm.NewGeppettoLLM(stepSettings)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create Geppetto LLM")
	}

	// Prepare tools
	toolList := a.prepareTools(agentSettings)

	// Prepare agent options
	options := a.prepareAgentOptions(agentSettings)
	cliOptions := parsedLayers.GetDefaultParameterLayer().Parameters.ToMap()
	for k, v := range cliOptions {
		options[k] = v
	}

	// Get agent constructor from registry
	constructor, err := GetAgentConstructor(agentType)
	if err != nil {
		return nil, errors.Wrap(err, "failed to get agent constructor")
	}

	// Create the agent
	agent, err := constructor(ctx, llmModel, options)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create agent")
	}

	// Set up memory if specified
	if agentSettings.MemoryType != "none" {
		mem, err := a.createMemory(ctx, agentSettings.MemoryType)
		if err != nil {
			return nil, errors.Wrap(err, "failed to create memory")
		}
		if err := agent.SetMemory(mem); err != nil {
			return nil, errors.Wrap(err, "failed to set memory")
		}
	}

	// Register tools with the agent
	for _, tool := range toolList {
		if err := agent.AddTool(tool); err != nil {
			return nil, errors.Wrap(err, "failed to add tool")
		}
	}

	return agent, nil
}

// prepareTools creates tool instances based on the settings
func (a *AgentCommand) prepareTools(settings *AgentSettings) []tools.Tool {
	// In a real implementation, this would create actual tool instances
	// For now, return an empty list for illustration
	return []tools.Tool{}
}

// prepareAgentOptions prepares options for the agent
func (a *AgentCommand) prepareAgentOptions(settings *AgentSettings) map[string]interface{} {
	options := make(map[string]interface{})

	// Copy options from command
	for k, v := range a.AgentOptions {
		options[k] = v
	}

	// Add settings
	options["max_iterations"] = settings.MaxIterations
	options["system_prompt"] = settings.SystemPrompt
	if a.SystemPrompt != "" {
		options["system_prompt"] = a.SystemPrompt
	}

	return options
}

// createMemory creates a memory system based on the specified type
func (a *AgentCommand) createMemory(ctx context.Context, memoryType string) (memory.Memory, error) {
	// In a real implementation, this would create an actual memory system
	// For now, return a mock memory for illustration
	return &mockMemory{}, nil
}

// mockMemory is a simple mock implementation of the Memory interface
type mockMemory struct{}

// Add implements the Memory interface's Add method
func (m *mockMemory) Add(ctx context.Context, memory goagentTypes.MemoryEntry) error {
	return nil
}

// Get implements the Memory interface's Get method
func (m *mockMemory) Get(ctx context.Context, ids []string) ([]goagentTypes.MemoryEntry, error) {
	return []goagentTypes.MemoryEntry{}, nil
}

// Search implements the Memory interface's Search method
func (m *mockMemory) Search(ctx context.Context, query string, limit int) ([]goagentTypes.MemoryEntry, error) {
	return []goagentTypes.MemoryEntry{}, nil
}

// Clear implements the Memory interface's Clear method
func (m *mockMemory) Clear(ctx context.Context) error {
	return nil
}

// RunIntoGlazeProcessor implements the GlazeCommand interface for GlazedAgentCommand
func (gac *GlazedAgentCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Prepare the agent using the embedded AgentCommand logic
	agentInstance, err := gac.AgentCommand.prepareAgent(ctx, parsedLayers)
	if err != nil {
		return errors.Wrap(err, "failed to prepare agent")
	}

	// Render the initial prompt using parameters
	initialPrompt, err := gac.AgentCommand.renderInitialPrompt(parsedLayers)
	if err != nil {
		return errors.Wrap(err, "failed to render initial prompt")
	}

	// Type assert the agent to GlazedAgent
	glazedAgent, ok := agentInstance.(agent.GlazedAgent)
	if !ok {
		return errors.Errorf("agent type %T does not support Glazed output (does not implement GlazedAgent interface)", agentInstance)
	}

	// Run the agent's specific Glazed processor method
	err = glazedAgent.RunIntoGlazeProcessor(ctx, initialPrompt, gp)
	if err != nil {
		return errors.Wrap(err, "failed to run agent into glaze processor")
	}

	return nil
}

// RunIntoWriter implements the WriterCommand interface for WriterAgentCommand
func (wac *WriterAgentCommand) RunIntoWriter(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	w io.Writer,
) error {
	// Prepare the agent using the embedded AgentCommand logic
	agentInstance, err := wac.AgentCommand.prepareAgent(ctx, parsedLayers)
	if err != nil {
		return errors.Wrap(err, "failed to prepare agent")
	}

	// Render the initial prompt using parameters
	initialPrompt, err := wac.AgentCommand.renderInitialPrompt(parsedLayers)
	if err != nil {
		return errors.Wrap(err, "failed to render initial prompt")
	}

	// Run the agent's standard Run method
	resultStr, err := agentInstance.Run(ctx, initialPrompt)
	if err != nil {
		return errors.Wrap(err, "failed to run agent")
	}

	// Write the result string to the writer
	_, err = fmt.Fprintln(w, resultStr)
	return err
}

// renderInitialPrompt renders the command's Prompt template string
// using the parameters from the default layer.
func (a *AgentCommand) renderInitialPrompt(parsedLayers *layers.ParsedLayers) (string, error) {
	if a.Prompt == "" {
		// Or handle this case differently, maybe require a prompt?
		return "", errors.New("cannot run agent without a prompt template defined in the command")
	}

	params := parsedLayers.GetDefaultParameterLayer().Parameters.ToMap()

	tmpl, err := template.New("prompt").Parse(a.Prompt)
	if err != nil {
		return "", errors.Wrap(err, "failed to parse prompt template")
	}

	var buf bytes.Buffer
	err = tmpl.Execute(&buf, params)
	if err != nil {
		return "", errors.Wrap(err, "failed to execute prompt template")
	}

	return buf.String(), nil
}
