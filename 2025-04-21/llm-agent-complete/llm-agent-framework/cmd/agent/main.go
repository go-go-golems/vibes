package main

import (
	"fmt"
	"os"

	clay "github.com/go-go-golems/clay/pkg"
	"github.com/go-go-golems/clay/pkg/doc"
	glazedcmds "github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/logging"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/help"
	pinocchio_cmds "github.com/go-go-golems/pinocchio/pkg/cmds"
	goagentcmds "github.com/goagent/framework/goagent/cmds"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

func main() {
	helpSystem := help.NewHelpSystem()
	err := doc.AddDocToHelpSystem(helpSystem)
	cobra.CheckErr(err)

	rootCmd := &cobra.Command{
		Use:   "goagent",
		Short: "GoAgent CLI - Execute AI agents from the command line",
		Long: `GoAgent CLI is a tool for executing AI agents from the command line.
It supports various agent types like ReAct, Plan-and-Execute, and File Collection.`,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			// Setup debug logging
			return logging.InitLoggerFromViper()
		},
	}

	// Initialize Viper right after creating the rootCmd
	err = clay.InitViper("pinocchio", rootCmd) // Use "goagent" or a suitable config name
	cobra.CheckErr(err)

	// Setup zerolog
	err = logging.InitLoggerFromViper()
	cobra.CheckErr(err)

	// Setup help system
	helpSystem.SetupCobraRootCommand(rootCmd)

	// Create a simple agent command (example)
	weatherAgentCmd, err := createWeatherAgentCommand()
	if err != nil {
		fmt.Printf("Error creating weather agent command: %v\n", err)
		os.Exit(1)
	}

	// Convert agent command to cobra command using Pinocchio helper
	weatherCobraCmd, err := pinocchio_cmds.BuildCobraCommandWithGeppettoMiddlewares(weatherAgentCmd)
	if err != nil {
		fmt.Printf("Error building weather cobra command: %v\n", err)
		os.Exit(1)
	}
	rootCmd.AddCommand(weatherCobraCmd)

	// --- Load agent commands from YAML files (example) ---
	yamlContent := []byte(`
name: code-assistant
short: "AI code assistant for various programming tasks"
long: "An agent that can help with coding tasks like generating code, explaining code, or fixing bugs"
command-type: writer # Explicitly setting writer (default)
agent-type: react
system-prompt: "You are an AI coding assistant specializing in helping with programming tasks."
prompt: "Help with the following {{.language}} task: {{ .query | join \" \" }}"
tools:
  - web-search
# Example tools, adjust as needed
# - code-search
# - file-read
flags:
  - name: language
    type: string
    help: "Programming language to work with"
    default: "go"
arguments:
  - name: query
    type: stringList
    help: "The coding task to perform"
    required: true
---
name: file-generator
short: "Generate multiple files based on a description"
long: "An agent that takes a description and generates multiple code/text files."
command-type: glazed # Specify glazed for structured output
agent-type: file-collection # Use the file collection agent logic
system-prompt: "You are a helpful AI assistant that generates complete files based on user requests. Follow the file generation instructions precisely."
prompt: "Generate the files described here: {{ .description | join \" \" }}"
flags: []
arguments:
  - name: description
    type: stringList
    help: "Description of the files to generate"
    required: true
`)

	// Load commands directly from the YAML byte slice
	agentCommands, err := goagentcmds.LoadFromYAML(yamlContent)
	if err != nil {
		log.Error().Err(err).Msg("Error loading commands from YAML")
		os.Exit(1)
	}

	// Add all loaded commands to the root command
	for _, cmd := range agentCommands {
		cobraCmd, err := pinocchio_cmds.BuildCobraCommandWithGeppettoMiddlewares(cmd)
		if err != nil {
			log.Error().Err(err).Str("command", cmd.Description().Name).Msg("Error building cobra command from loaded agent command")
			continue
		}
		rootCmd.AddCommand(cobraCmd)
	}

	log.Info().Msg("Starting GoAgent CLI")
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

// createWeatherAgentCommand creates a simple weather agent command (as WriterAgentCommand)
func createWeatherAgentCommand() (glazedcmds.Command, error) {
	// Create a command description
	cmdDescription := glazedcmds.NewCommandDescription(
		"weather",
		glazedcmds.WithShort("Get weather information (Writer example)"),
		glazedcmds.WithLong("An agent that can search for and report on current weather conditions, outputting plain text."),
		glazedcmds.WithFlags(
			parameters.NewParameterDefinition(
				"location",
				parameters.ParameterTypeString,
				parameters.WithHelp("Location to check weather for"),
				parameters.WithRequired(true),
			),
			parameters.NewParameterDefinition(
				"detailed",
				parameters.ParameterTypeBool,
				parameters.WithHelp("Whether to provide detailed weather information"),
				parameters.WithDefault(false),
			),
		),
	)

	// Create the agent command - NewAgentCommand now correctly returns a WriterAgentCommand
	agentCmd, err := goagentcmds.NewAgentCommand(
		cmdDescription,
		goagentcmds.WithAgentType("react"), // Assuming 'react' agent outputs text
		goagentcmds.WithSystemPrompt("You are a helpful weather assistant that can provide current weather information for any location."),
		goagentcmds.WithPrompt("What is the weather like in {{.location}}?{{if .detailed}} Provide details.{{end}}"),
		goagentcmds.WithTools([]string{"web-search"}), // Example tool
	)
	if err != nil {
		return nil, err
	}

	return agentCmd, nil
}
