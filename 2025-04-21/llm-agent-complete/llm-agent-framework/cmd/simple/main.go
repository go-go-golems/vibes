package main

import (
	"context"
	"fmt"
	"os"

	clay "github.com/go-go-golems/clay/pkg"
	"github.com/go-go-golems/clay/pkg/doc"
	"github.com/go-go-golems/geppetto/pkg/conversation"
	"github.com/go-go-golems/geppetto/pkg/events"
	"github.com/go-go-golems/geppetto/pkg/steps/ai/settings"
	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/logging"
	"github.com/go-go-golems/glazed/pkg/help"
	"github.com/goagent/framework/goagent/llm"
	"github.com/google/uuid"
	"github.com/pkg/errors"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
	"golang.org/x/sync/errgroup"

	pinocchio_cmds "github.com/go-go-golems/pinocchio/pkg/cmds"
)

// SimpleAgentCmd represents the command for our simple agent
type SimpleAgentCmd struct {
	*cmds.CommandDescription
}

// NewSimpleAgentCmd creates a new instance of the command
func NewSimpleAgentCmd() (*SimpleAgentCmd, error) {
	// Use the pinocchio helper to create all Geppetto layers
	tempSettings, err := settings.NewStepSettings()
	if err != nil {
		return nil, errors.Wrap(err, "failed to create temporary step settings")
	}
	geppettoLayers, err := pinocchio_cmds.CreateGeppettoLayers(tempSettings)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create Geppetto layers")
	}

	return &SimpleAgentCmd{
		CommandDescription: cmds.NewCommandDescription(
			"simple-agent",
			cmds.WithShort("Runs a simple agent to ask the size of Earth"),
			cmds.WithLayersList(geppettoLayers...),
		),
	}, nil
}

// Run runs the command logic
func (sac *SimpleAgentCmd) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	log.Debug().Interface("parsedLayers", parsedLayers).Msg("Parsed layers")
	// 1. Create StepSettings from parsed layers
	stepSettings, err := settings.NewStepSettingsFromParsedLayers(parsedLayers)
	if err != nil {
		return errors.Wrap(err, "failed to create step settings from parsed layers")
	}

	// Log the loaded settings for debugging
	log.Debug().Interface("settings", stepSettings).Msg("Loaded AI step settings")

	// 2. Create and start an event router
	ctx, cancel := context.WithCancel(ctx) // Use the passed-in context
	defer cancel()                         // Ensure context is cancelled on exit

	router, err := events.NewEventRouter()
	if err != nil {
		log.Error().Err(err).Msg("Failed to create event router")
		return errors.Wrap(err, "failed to create event router")
	}

	// Start the router in a background goroutine
	eg, routerCtx := errgroup.WithContext(ctx)
	eg.Go(func() error {
		defer cancel()
		log.Info().Msg("Starting event router")
		runErr := router.Run(routerCtx) // Use derived context for cancellation
		log.Info().Err(runErr).Msg("Event router stopped")
		// Don't return context.Canceled as a fatal error
		if runErr != nil && !errors.Is(runErr, context.Canceled) {
			return runErr // Return other errors
		}
		return nil
	})

	// 3. Generate a unique topic ID
	topicID := fmt.Sprintf("simple-agent-llm-%s", uuid.New().String())

	// 4. Create the LLM with publisher and topic configuration
	geppettoLLM, err := llm.NewGeppettoLLM(
		stepSettings,
		llm.WithPublisherAndTopic(router.Publisher, topicID), // Configure publisher
	)
	if err != nil {
		return errors.Wrap(err, "failed to create Geppetto LLM")
	}

	// 5. Register the StepPrinterFunc handler
	handlerName := "stdout-printer-" + topicID
	log.Info().Str("handler", handlerName).Str("topic", topicID).Msg("Registering stdout handler")
	router.AddHandler(
		handlerName,                              // Unique handler name
		topicID,                                  // Topic to subscribe to
		events.StepPrinterFunc("LLM", os.Stdout), // Use StepPrinterFunc directly
	)

	// 6. Prepare the prompt
	prompt := "What is the size of the planet Earth? Provide standard metrics like diameter, circumference, mass."
	messages := []*conversation.Message{
		conversation.NewChatMessage(conversation.RoleUser, prompt),
	}

	// Remove the manual print statement
	// fmt.Println("Asking LLM:", prompt)

	// 7. Call the LLM
	eg.Go(func() error {
		defer cancel()
		log.Info().Msg("Calling LLM Generate")
		v, err := geppettoLLM.Generate(ctx, messages) // No topicID argument needed here
		if err != nil {
			log.Error().Err(err).Msg("LLM Generate failed")
			// We might want to cancel and wait even if LLM fails, to ensure clean router shutdown
			// cancel()
			// eg.Wait() // Consider error handling for wait here too
			return errors.Wrap(err, "failed to generate response from LLM")
		} else {
			log.Info().Msg("LLM Generate completed")
			fmt.Println(v.Content.String())
		}
		return nil
	})

	// 8. Signal router to stop and wait for shutdown

	log.Info().Msg("Waiting for event router to shut down")
	if err := eg.Wait(); err != nil {
		// Log error from router.Run (if it wasn't context.Canceled)
		log.Error().Err(err).Msg("Event router shutdown failed")
		// Return this error as the Run function's result
		return errors.Wrap(err, "event router shutdown failed")
	} else {
		log.Info().Msg("Event router shut down successfully")
	}

	// Remove manual printing of the final response
	// fmt.Println("\nLLM Response:")
	// fmt.Println(responseMsg.Content.String())

	log.Info().Msg("Simple agent finished successfully.")
	return nil
}

func main() {
	helpSystem := help.NewHelpSystem()
	err := doc.AddDocToHelpSystem(helpSystem)
	cobra.CheckErr(err)

	// Create the command
	simpleCmd, err := NewSimpleAgentCmd()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating command: %v\n", err)
		os.Exit(1)
	}

	// Build the Cobra command using the Pinocchio helper which includes middlewares
	cobraCmd, err := pinocchio_cmds.BuildCobraCommandWithGeppettoMiddlewares(
		simpleCmd,
		cli.WithProfileSettingsLayer(),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building cobra command: %v\n", err)
		os.Exit(1)
	}
	cobraCmd.PersistentPreRunE = func(cmd *cobra.Command, args []string) error {
		return logging.InitLoggerFromViper()
	}

	helpSystem.SetupCobraRootCommand(cobraCmd)

	// Initialize Viper to load the pinocchio config
	err = clay.InitViper("pinocchio", cobraCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing viper: %v\n", err)
		os.Exit(1)
	}

	// Setup zerolog
	err = logging.InitLoggerFromViper()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing logger: %v\n", err)
		os.Exit(1)
	}

	log.Info().Msg("Executing simple agent")
	log.Debug().Interface("cobraCmd", cobraCmd).Msg("Cobra command")

	// Execute the command
	if err := cobraCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error executing command: %v\n", err)
		os.Exit(1)
	}
}
