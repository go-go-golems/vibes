package main

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	clay "github.com/go-go-golems/clay/pkg"
	"github.com/go-go-golems/clay/pkg/doc"
	"github.com/go-go-golems/geppetto/pkg/steps/ai/settings"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/logging"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/help"
	pinocchio_cmds "github.com/go-go-golems/pinocchio/pkg/cmds"
	"github.com/goagent/framework/goagent/agent" // Import our agent implementation
	"github.com/goagent/framework/goagent/llm"
	"github.com/pkg/errors"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

// FileExtractionCmd represents the command for the file extraction agent
type FileExtractionCmd struct {
	*cmds.CommandDescription
	Agent *agent.FileCollectionAgent
}

// FileExtractionSettings holds flag values for the command
type FileExtractionSettings struct {
	Prompt          string `glazed.parameter:"prompt"`
	OutputDirectory string `glazed.parameter:"output-directory"`
	MaxIterations   int    `glazed.parameter:"max-iterations"`
}

// NewFileExtractionCmd creates a new instance of the command
func NewFileExtractionCmd() (*FileExtractionCmd, error) {
	// Use the pinocchio helper to create all Geppetto layers
	// These layers handle AI settings, API keys, etc.
	tempSettings, err := settings.NewStepSettings()
	if err != nil {
		return nil, errors.Wrap(err, "failed to create temporary step settings")
	}
	geppettoLayers, err := pinocchio_cmds.CreateGeppettoLayers(tempSettings)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create Geppetto layers")
	}

	// Add application-specific flags
	fileExtractionLayer, err := layers.NewParameterLayer("file-extraction", "File Extraction Settings",
		layers.WithParameterDefinitions(
			parameters.NewParameterDefinition(
				"prompt",
				parameters.ParameterTypeString,
				parameters.WithHelp("The prompt describing the files to generate"),
				parameters.WithRequired(true),
			),
			parameters.NewParameterDefinition(
				"output-directory",
				parameters.ParameterTypeString,
				parameters.WithHelp("Directory to save the extracted files"),
				parameters.WithDefault("generated_files"),
			),
			parameters.NewParameterDefinition(
				"max-iterations",
				parameters.ParameterTypeInteger,
				parameters.WithHelp("Maximum number of LLM calls"),
				parameters.WithDefault(5),
			),
		),
	)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create file extraction layer")
	}

	// Combine Geppetto layers and our application layer
	allLayers := append(geppettoLayers, fileExtractionLayer)

	return &FileExtractionCmd{
		CommandDescription: cmds.NewCommandDescription(
			"file-extract",
			cmds.WithShort("Runs an agent to extract multiple files based on a prompt using XML tags"),
			cmds.WithLayersList(allLayers...),
		),
	}, nil
}

// Run runs the command logic
func (fec *FileExtractionCmd) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	// 1. Parse application-specific settings
	feSettings := &FileExtractionSettings{}
	err := parsedLayers.InitializeStruct("file-extraction", feSettings) // Use layer slug
	if err != nil {
		// Try DefaultSlug as fallback if layer slug fails (might happen with merged layers)
		err = parsedLayers.InitializeStruct(layers.DefaultSlug, feSettings)
		if err != nil {
			return errors.Wrap(err, "failed to initialize file extraction settings from any slug")
		}
	}

	// 2. Create AI StepSettings from parsed layers (handles Geppetto layers)
	stepSettings, err := settings.NewStepSettingsFromParsedLayers(parsedLayers)
	if err != nil {
		return errors.Wrap(err, "failed to create step settings from parsed layers")
	}

	// IMPORTANT: This agent relies on parsing the full response, so disable streaming.
	stepSettings.Chat.Stream = false

	log.Debug().Interface("settings", stepSettings).Msg("Loaded AI step settings")
	log.Debug().Interface("fileExtractionSettings", feSettings).Msg("Loaded file extraction settings")

	// 3. Create the LLM instance
	// We use the GeppettoLLM wrapper which integrates with Geppetto's step system.
	geppettoLLM, err := llm.NewGeppettoLLM(stepSettings)
	if err != nil {
		return errors.Wrap(err, "failed to create Geppetto LLM")
	}

	// 4. Create the FileCollectionAgent
	fec.Agent = agent.NewFileCollectionAgent(geppettoLLM, feSettings.MaxIterations)

	// 5. Run the agent
	log.Info().Str("prompt", feSettings.Prompt).Msg("Starting File Collection Agent")
	summary, err := fec.Agent.Run(ctx, feSettings.Prompt)
	if err != nil {
		log.Error().Err(err).Msg("Agent execution failed")
		return errors.Wrap(err, "agent run failed")
	}

	// 6. Print the summary returned by the agent
	fmt.Println("\n--- Agent Run Summary ---")
	fmt.Println(summary)
	fmt.Println("-------------------------")

	// 7. Get the collected files
	files := fec.Agent.GetFiles()
	if len(files) == 0 {
		log.Info().Msg("Agent did not collect any files.")
		return nil
	}

	// 8. Save the files
	log.Info().Str("outputDir", feSettings.OutputDirectory).Int("fileCount", len(files)).Msg("Saving collected files")
	err = os.MkdirAll(feSettings.OutputDirectory, 0755)
	if err != nil {
		return errors.Wrapf(err, "failed to create output directory: %s", feSettings.OutputDirectory)
	}

	for name, content := range files {
		// Sanitize filename slightly (replace potential path separators)
		safeName := strings.ReplaceAll(name, "/", "_")
		safeName = strings.ReplaceAll(safeName, "\\", "_")
		filePath := filepath.Join(feSettings.OutputDirectory, safeName)

		log.Debug().Str("filePath", filePath).Int("size", len(content)).Msg("Writing file")
		err := os.WriteFile(filePath, []byte(content), 0644)
		if err != nil {
			// Log error but continue trying to write other files
			log.Error().Err(err).Str("filePath", filePath).Msg("Failed to write file")
		} else {
			fmt.Printf("Saved file: %s\n", filePath)
		}
	}

	log.Info().Msg("File extraction agent finished successfully.")
	return nil
}

func main() {
	helpSystem := help.NewHelpSystem()
	err := doc.AddDocToHelpSystem(helpSystem)
	cobra.CheckErr(err)

	// Create the command
	fileExtractCmd, err := NewFileExtractionCmd()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating command: %v\n", err)
		os.Exit(1)
	}

	// Build the Cobra command using the Pinocchio helper which includes middlewares
	// for loading settings, handling profiles, etc.
	cobraCmd, err := pinocchio_cmds.BuildCobraCommandWithGeppettoMiddlewares(
		fileExtractCmd,
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building cobra command: %v\n", err)
		os.Exit(1)
	}

	// Setup logging initialization
	cobraCmd.PersistentPreRunE = func(cmd *cobra.Command, args []string) error {
		// Reinitialize Viper before logging to ensure flags are bound.
		// It might be initialized by BuildCobraCommandWithGeppettoMiddlewares already,
		// but re-initializing here ensures command-line flags override config files.
		err = clay.InitViper("pinocchio", cmd)
		if err != nil {
			return errors.Wrap(err, "failed to reinitialize viper")
		}
		// Initialize logger without extra options
		return logging.InitLoggerFromViper()
	}

	helpSystem.SetupCobraRootCommand(cobraCmd)

	// Initialize Viper once initially for config loading
	err = clay.InitViper("pinocchio", cobraCmd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing viper: %v\n", err)
		os.Exit(1)
	}

	// Initial logger setup (might be overridden in PersistentPreRunE)
	// Initialize logger without extra options
	err = logging.InitLoggerFromViper()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing logger: %v\n", err)
		os.Exit(1)
	}

	log.Info().Msg("Executing file extraction agent command")

	// Execute the command
	if err := cobraCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error executing command: %v\n", err)
		os.Exit(1)
	}
}
