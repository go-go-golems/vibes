package cmds

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"os"

	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/go-go-golems/geppetto/pkg/events"
	"github.com/go-go-golems/geppetto/pkg/steps/ai/settings"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/helpers/templating"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	pinocchio_cmds "github.com/go-go-golems/pinocchio/pkg/cmds"
	"github.com/goagent/framework/goagent/agent"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/memory"
	"github.com/goagent/framework/goagent/tools"
	goagentTypes "github.com/goagent/framework/goagent/types"
	"github.com/google/uuid"
	"github.com/pkg/errors"
	"github.com/rs/zerolog/log"
	"golang.org/x/sync/errgroup"
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
	publisher message.Publisher,
	topicID string,
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

	llmOptions := []llm.GeppettoLLMOption{}
	// Only configure publisher if it's provided
	if publisher != nil && topicID != "" {
		stepSettings.Chat.Stream = true
		llmOptions = append(llmOptions, llm.WithPublisherAndTopic(publisher, topicID))
	}

	// Prepare LLM model using Geppetto LLM
	llmModel, err := llm.NewGeppettoLLM(
		stepSettings,
		llmOptions...,
	)
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
	// 5. Prepare the agent, passing nil publisher and empty topic
	agentInstance, err := gac.AgentCommand.prepareAgent(ctx, parsedLayers, nil, "") // Pass nil publisher

	if err != nil {
		return errors.Wrap(err, "failed to prepare agent")
	}

	// 6. Render the initial prompt using parameters
	initialPrompt, err := gac.AgentCommand.renderInitialPrompt(parsedLayers)
	if err != nil {
		return errors.Wrap(err, "failed to render initial prompt")
	}

	// 7. Type assert the agent to GlazedAgent
	glazedAgent, ok := agentInstance.(agent.GlazedAgent)
	if !ok {
		return errors.Errorf("agent type %T does not support Glazed output (does not implement GlazedAgent interface)", agentInstance)
	}

	// 8. Run the agent's specific Glazed processor method (sequentially for now)
	log.Info().Msg("Running GlazedAgent logic")
	err = glazedAgent.RunIntoGlazeProcessor(ctx, initialPrompt, gp)
	if err != nil {
		log.Error().Err(err).Msg("GlazedAgent RunIntoGlazeProcessor failed")
		return errors.Wrap(err, "failed to run agent into glaze processor")
	} else {
		log.Info().Msg("GlazedAgent logic finished")
	}

	// 9. Wait for the event router goroutine to shut down cleanly.
	// Removed router wait

	// If agent execution succeeded, return nil
	return nil // Return nil on success, error was handled above
}

// RunIntoWriter implements the WriterCommand interface for WriterAgentCommand
func (wac *WriterAgentCommand) RunIntoWriter(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	w io.Writer,
) error {
	// 1. Setup context and errgroup for managing goroutines
	ctx, cancel := context.WithCancel(ctx)
	eg, ctx := errgroup.WithContext(ctx) // Use derived context for cancellation
	defer func() {
		// Ensure context cancellation propagates if not already done
		cancel() // If using context.WithCancel instead of errgroup.WithContext directly
	}()

	// 2. Create and start an event router
	router, err := events.NewEventRouter()
	if err != nil {
		log.Error().Err(err).Msg("Failed to create event router")
		return errors.Wrap(err, "failed to create event router")
	}

	// 3. Generate a unique topic ID
	topicID := fmt.Sprintf("%s-llm-%s", wac.AgentCommand.Name, uuid.New().String())

	// 4. Register the StepPrinterFunc handler to print events to stdout
	handlerName := "stdout-printer-" + topicID
	log.Info().Str("handler", handlerName).Str("topic", topicID).Msg("Registering stdout handler for WriterAgent")
	// NOTE: This still prints to os.Stdout, even though RunIntoWriter takes an io.Writer.
	//       If events should go to `w`, a custom handler would be needed.
	//       For now, keeping consistency with simple-agent example (prints to stdout).
	router.AddHandler(
		handlerName,                              // Unique handler name
		topicID,                                  // Topic to subscribe to
		events.StepPrinterFunc("LLM", os.Stdout), // Print events to stdout
	)

	// 5. Prepare the agent, passing the publisher and topic
	agentInstance, err := wac.AgentCommand.prepareAgent(ctx, parsedLayers, router.Publisher, topicID)
	if err != nil {
		return errors.Wrap(err, "failed to prepare agent")
	}

	// 6. Render the initial prompt using parameters
	initialPrompt, err := wac.AgentCommand.renderInitialPrompt(parsedLayers)
	if err != nil {
		return errors.Wrap(err, "failed to render initial prompt")
	}

	// Start the router in a background goroutine
	eg.Go(func() error {
		defer cancel()
		log.Info().Msg("Starting event router for WriterAgent")
		// Ensure router is closed when this goroutine exits
		defer func() {
			log.Info().Msg("Closing event router for WriterAgent")
			_ = router.Close()
			log.Info().Msg("Event router closed for WriterAgent")
		}()
		runErr := router.Run(ctx) // Use errgroup's context
		log.Info().Err(runErr).Msg("Event router stopped for WriterAgent")
		// Don't return context.Canceled as a fatal error
		if runErr != nil && !errors.Is(runErr, context.Canceled) {
			return runErr // Return other errors
		}
		return nil
	})

	// Run the agent logic in a separate goroutine to allow router shutdown
	eg.Go(func() error {
		defer cancel()
		// 7. Run the agent's standard Run method (sequentially for now)

		// Wait for the router to be running before running the agent
		<-router.Running()
		log.Info().Msg("Event router is running, proceeding with agent run (WriterAgent)")

		log.Info().Msg("Running WriterAgent logic")
		resultStr, agentErr := agentInstance.Run(ctx, initialPrompt)
		if agentErr != nil {
			log.Error().Err(agentErr).Msg("WriterAgent Run failed")
			// Return the agent error immediately
			return errors.Wrap(agentErr, "failed to run agent")
		} else {
			log.Info().Msg("WriterAgent logic finished")
		}

		// 8. Write the final result string to the provided writer
		_, writeErr := fmt.Fprintln(w, resultStr)
		if writeErr != nil {
			log.Error().Err(writeErr).Msg("Failed to write agent result")
			// Return write error immediately
			return errors.Wrap(writeErr, "failed to write agent result")
		}

		return nil
	})

	// 9. Wait for the event router goroutine to shut down cleanly.
	log.Info().Msg("Waiting for event router shutdown (WriterAgent)")
	waitErr := eg.Wait()
	if waitErr != nil {
		log.Error().Err(waitErr).Msg("Event router shutdown failed (WriterAgent)")
		// Prioritize returning agent or write errors if they occurred, otherwise return router error
		// Since we return early on agent/write errors, we only need to return waitErr here if it's non-nil.
		return errors.Wrap(waitErr, "event router shutdown failed")

	} else {
		log.Info().Msg("Event router shut down successfully (WriterAgent)")
	}

	// If agent, write, and router shutdown succeeded, return nil
	return nil
}

// renderInitialPrompt renders the command's Prompt template string
// using the parameters from the default layer.
func (a *AgentCommand) renderInitialPrompt(parsedLayers *layers.ParsedLayers) (string, error) {
	if a.Prompt == "" {
		// Or handle this case differently, maybe require a prompt?
		return "", errors.New("cannot run agent without a prompt template defined in the command")
	}

	params := parsedLayers.GetDefaultParameterLayer().Parameters.ToMap()

	// Use the Glazed templating helper which includes Sprig functions
	tmpl, err := templating.CreateTemplate("prompt").Parse(a.Prompt)
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
