package cmds

import (
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/pkg/errors"
)

// AgentLayerSlug is the unique identifier for the agent parameter layer
const AgentLayerSlug = "agent"

// AgentSettings holds the settings for an agent command
type AgentSettings struct {
	AgentType     string   `glazed.parameter:"agent-type"`
	MaxIterations int      `glazed.parameter:"max-iterations"`
	SystemPrompt  string   `glazed.parameter:"system-prompt"`
	MemoryType    string   `glazed.parameter:"memory-type"`
	Tools         []string `glazed.parameter:"tools"`
}

// NewAgentParameterLayer creates a new parameter layer for agent configuration
func NewAgentParameterLayer() (layers.ParameterLayer, error) {
	return layers.NewParameterLayer(
		AgentLayerSlug,
		"Agent configuration options",
		layers.WithParameterDefinitions(
			parameters.NewParameterDefinition(
				"agent-type",
				parameters.ParameterTypeChoice,
				parameters.WithHelp("Type of agent to create (react, plan-execute, file-collection)"),
				parameters.WithDefault("react"),
				parameters.WithChoices("react", "plan-execute", "file-collection"),
			),
			parameters.NewParameterDefinition(
				"max-iterations",
				parameters.ParameterTypeInteger,
				parameters.WithHelp("Maximum number of iterations for agent execution"),
				parameters.WithDefault(10),
			),
			parameters.NewParameterDefinition(
				"system-prompt",
				parameters.ParameterTypeString,
				parameters.WithHelp("System prompt to use for the agent"),
				parameters.WithDefault(""),
			),
			parameters.NewParameterDefinition(
				"memory-type",
				parameters.ParameterTypeChoice,
				parameters.WithHelp("Type of memory system to use"),
				parameters.WithDefault("none"),
				parameters.WithChoices("none", "simple", "vectorstore"),
			),
			parameters.NewParameterDefinition(
				"tools",
				parameters.ParameterTypeStringList,
				parameters.WithHelp("List of tools to enable for the agent"),
				parameters.WithDefault([]string{}),
			),
		),
	)
}

// GetAgentSettingsFromParsedLayers extracts agent settings from parsed layers
func GetAgentSettingsFromParsedLayers(parsedLayers *layers.ParsedLayers) (*AgentSettings, error) {
	s := &AgentSettings{}
	if err := parsedLayers.InitializeStruct(AgentLayerSlug, s); err != nil {
		return nil, errors.Wrap(err, "failed to initialize agent settings from parsed layers")
	}
	return s, nil
}
