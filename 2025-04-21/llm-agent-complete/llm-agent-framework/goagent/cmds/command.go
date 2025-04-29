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
) (*AgentCommand, error) {
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

	ret := &AgentCommand{
		CommandDescription: description,
		AgentOptions:       make(map[string]interface{}),
	}

	for _, option := range options {
		option(ret)
	}

	// Ensure AgentType is set before proceeding
	if ret.AgentType == "" {
		return nil, errors.New("agent type must be specified using WithAgentType option")
	}

	// Get the factory for the specified agent type
	factory, err := agent.GetAgentFactory(ret.AgentType)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to get agent factory for type '%s'", ret.AgentType)
	}

	// Get agent-specific layers from the factory
	agentLayers, err := factory.CreateLayers()
	if err != nil {
		return nil, errors.Wrapf(err, "failed to create layers for agent type '%s'", ret.AgentType)
	}

	// Append agent-specific layers to the command description
	description.Layers.AppendLayers(agentLayers...)

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
	return &WriterAgentCommand{AgentCommand: agentCmd}, nil
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
	return &GlazedAgentCommand{AgentCommand: agentCmd}, nil
}

// prepareLlm prepares the LLM model and related settings.
// It no longer creates the agent itself.
func (a *AgentCommand) prepareLlm(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	publisher message.Publisher,
	topicID string,
) (llm.LLM, *settings.StepSettings, error) {
	// Create StepSettings from parsed layers for LLM creation
	stepSettings, err := settings.NewStepSettingsFromParsedLayers(parsedLayers)
	if err != nil {
		return nil, nil, errors.Wrap(err, "failed to create step settings from parsed layers")
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
		return nil, nil, errors.Wrap(err, "failed to create Geppetto LLM")
	}

	return llmModel, stepSettings, nil
}

// RunIntoGlazeProcessor implements the GlazeCommand interface for GlazedAgentCommand
func (gac *GlazedAgentCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// 1. Prepare LLM (publisher is nil for Glazed)
	llmModel, _, err := gac.AgentCommand.prepareLlm(ctx, parsedLayers, nil, "")
	if err != nil {
		return errors.Wrap(err, "failed to prepare LLM")
	}

	// 2. Get Agent Factory
	factory, err := agent.GetAgentFactory(gac.AgentCommand.AgentType)
	if err != nil {
		return errors.Wrapf(err, "failed to get agent factory for type '%s'", gac.AgentCommand.AgentType)
	}

	// 3. Create Agent Instance using Factory
	agentInstance, err := factory.NewAgent(ctx, parsedLayers, llmModel)
	if err != nil {
		return errors.Wrap(err, "failed to create agent instance")
	}

	// 4. Render the initial prompt using parameters
	initialPrompt, err := gac.AgentCommand.renderInitialPrompt(parsedLayers)
	if err != nil {
		return errors.Wrap(err, "failed to render initial prompt")
	}

	// 5. Type assert the agent to GlazedAgent
	glazedAgent, ok := agentInstance.(agent.GlazedAgent)
	if !ok {
		return errors.Errorf("agent type '%s' (%T) does not support Glazed output (does not implement GlazedAgent interface)", gac.AgentCommand.AgentType, agentInstance)
	}

	// 6. Run the agent's specific Glazed processor method
	log.Info().Str("agentType", gac.AgentCommand.AgentType).Msg("Running GlazedAgent logic")
	err = glazedAgent.RunIntoGlazeProcessor(ctx, initialPrompt, gp)
	if err != nil {
		log.Error().Err(err).Str("agentType", gac.AgentCommand.AgentType).Msg("GlazedAgent RunIntoGlazeProcessor failed")
		return errors.Wrap(err, "failed to run agent into glaze processor")
	} else {
		log.Info().Str("agentType", gac.AgentCommand.AgentType).Msg("GlazedAgent logic finished")
	}

	return nil
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
	defer cancel()

	// 2. Create and start an event router
	router, err := events.NewEventRouter()
	if err != nil {
		log.Error().Err(err).Msg("Failed to create event router")
		return errors.Wrap(err, "failed to create event router")
	}

	// 3. Generate a unique topic ID
	topicID := fmt.Sprintf("%s-llm-%s", wac.AgentCommand.Name, uuid.New().String())

	// 4. Register the StepPrinterFunc handler
	handlerName := "stdout-printer-" + topicID
	log.Info().Str("handler", handlerName).Str("topic", topicID).Msg("Registering stdout handler for WriterAgent")
	router.AddHandler(
		handlerName,
		topicID,
		events.StepPrinterFunc("LLM", os.Stdout), // Events still go to stdout here
	)

	// 5. Prepare LLM, passing the publisher and topic
	llmModel, _, err := wac.AgentCommand.prepareLlm(ctx, parsedLayers, router.Publisher, topicID)
	if err != nil {
		_ = router.Close() // Attempt to close router on error
		return errors.Wrap(err, "failed to prepare LLM")
	}

	// 6. Get Agent Factory
	factory, err := agent.GetAgentFactory(wac.AgentCommand.AgentType)
	if err != nil {
		_ = router.Close()
		return errors.Wrapf(err, "failed to get agent factory for type '%s'", wac.AgentCommand.AgentType)
	}

	// 7. Create Agent Instance using Factory
	agentInstance, err := factory.NewAgent(ctx, parsedLayers, llmModel)
	if err != nil {
		_ = router.Close()
		return errors.Wrap(err, "failed to create agent instance")
	}

	// 8. Render the initial prompt using parameters
	initialPrompt, err := wac.AgentCommand.renderInitialPrompt(parsedLayers)
	if err != nil {
		_ = router.Close()
		return errors.Wrap(err, "failed to render initial prompt")
	}

	// Start the router in a background goroutine
	eg.Go(func() error {
		// No need for defer cancel() here as eg manages context cancellation
		log.Info().Msg("Starting event router for WriterAgent")
		defer func() {
			log.Info().Msg("Closing event router for WriterAgent")
			_ = router.Close()
			log.Info().Msg("Event router closed for WriterAgent")
		}()
		runErr := router.Run(ctx)
		log.Info().Err(runErr).Msg("Event router stopped for WriterAgent")
		if runErr != nil && !errors.Is(runErr, context.Canceled) {
			return runErr
		}
		return nil
	})

	// Run the agent logic in a separate goroutine
	eg.Go(func() error {
		defer cancel() // Ensure cancellation propagates if agent finishes/errors first
		// Wait for the router to be running
		select {
		case <-router.Running():
			log.Info().Str("agentType", wac.AgentCommand.AgentType).Msg("Event router is running, proceeding with agent run")
		case <-ctx.Done():
			log.Warn().Str("agentType", wac.AgentCommand.AgentType).Msg("Context cancelled before router started")
			return ctx.Err()
		}

		// Run the agent's standard Run method
		log.Info().Str("agentType", wac.AgentCommand.AgentType).Msg("Running WriterAgent logic")
		resultStr, agentErr := agentInstance.Run(ctx, initialPrompt)
		if agentErr != nil {
			log.Error().Err(agentErr).Str("agentType", wac.AgentCommand.AgentType).Msg("WriterAgent Run failed")
			return errors.Wrap(agentErr, "failed to run agent")
		} else {
			log.Info().Str("agentType", wac.AgentCommand.AgentType).Msg("WriterAgent logic finished")
		}

		// Write the final result string to the provided writer
		_, writeErr := fmt.Fprintln(w, resultStr)
		if writeErr != nil {
			log.Error().Err(writeErr).Str("agentType", wac.AgentCommand.AgentType).Msg("Failed to write agent result")
			return errors.Wrap(writeErr, "failed to write agent result")
		}

		return nil
	})

	// Wait for all goroutines (router and agent runner)
	log.Info().Msg("Waiting for agent and router shutdown (WriterAgent)")
	waitErr := eg.Wait()
	if waitErr != nil {
		// Don't wrap context.Canceled errors
		if errors.Is(waitErr, context.Canceled) {
			log.Info().Msg("Agent/Router execution cancelled (WriterAgent)")
			return nil // Or return context.Canceled if preferred upstream
		}
		log.Error().Err(waitErr).Msg("Agent execution or router shutdown failed (WriterAgent)")
		return errors.Wrap(waitErr, "agent execution or router shutdown failed")
	} else {
		log.Info().Msg("Agent and router shut down successfully (WriterAgent)")
	}

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
