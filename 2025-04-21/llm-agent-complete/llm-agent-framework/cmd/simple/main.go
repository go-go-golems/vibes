package main

import (
	"context"
	"fmt"
	"os"

	clay "github.com/go-go-golems/clay/pkg"
	"github.com/go-go-golems/clay/pkg/doc"
	"github.com/go-go-golems/geppetto/pkg/conversation"
	"github.com/go-go-golems/geppetto/pkg/steps/ai/settings"
	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/logging"
	"github.com/go-go-golems/glazed/pkg/help"
	"github.com/goagent/framework/goagent/llm"
	"github.com/pkg/errors"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

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

	// Pass only stepSettings as per the updated NewGeppettoLLM signature
	geppettoLLM, err := llm.NewGeppettoLLM(stepSettings)
	if err != nil {
		return errors.Wrap(err, "failed to create Geppetto LLM")
	}

	// 4. Prepare the prompt
	prompt := "What is the size of the planet Earth? Provide standard metrics like diameter, circumference, mass."
	messages := []*conversation.Message{
		conversation.NewChatMessage(conversation.RoleUser, prompt),
	}

	fmt.Println("Asking LLM:", prompt)

	// 5. Call the LLM
	responseMsg, err := geppettoLLM.Generate(ctx, messages)
	if err != nil {
		log.Error().Err(err).Msg("LLM Generate failed")
		return errors.Wrap(err, "failed to generate response from LLM")
	}

	// 6. Print the response
	fmt.Println("\nLLM Response:")
	fmt.Println(responseMsg.Content.String())

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
