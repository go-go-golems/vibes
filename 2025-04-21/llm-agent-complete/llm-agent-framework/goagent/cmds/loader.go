package cmds

import (
	"io"
	"io/fs"
	"strings"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/pkg/errors"
	"gopkg.in/yaml.v3"
)

// AgentCommandLoader loads agent commands from YAML files
type AgentCommandLoader struct{}

// IsFileSupported checks if the file is supported by this loader
func (a *AgentCommandLoader) IsFileSupported(f fs.FS, fileName string) bool {
	return strings.HasSuffix(fileName, ".yaml") || strings.HasSuffix(fileName, ".yml")
}

// LoadFromYAML loads Agent commands from YAML content with additional options
func LoadFromYAML(b []byte, options ...cmds.CommandDescriptionOption) ([]cmds.Command, error) {
	loader := &AgentCommandLoader{}
	buf := strings.NewReader(string(b))
	return loader.loadAgentCommandFromReader(buf, options, nil)
}

// loadAgentCommandFromReader loads agent commands from a reader
func (a *AgentCommandLoader) loadAgentCommandFromReader(
	r io.Reader,
	options []cmds.CommandDescriptionOption,
	parent *cmds.CommandDescription,
) ([]cmds.Command, error) {
	var description AgentCommandDescription

	decoder := yaml.NewDecoder(r)
	if err := decoder.Decode(&description); err != nil {
		return nil, errors.Wrap(err, "failed to decode YAML")
	}

	// Create command description from YAML
	cmdDescription := cmds.NewCommandDescription(
		description.Name,
		cmds.WithShort(description.Short),
		cmds.WithLong(description.Long),
		cmds.WithFlags(description.Flags...),
		cmds.WithArguments(description.Arguments...),
	)

	// Apply additional options
	for _, option := range options {
		option(cmdDescription)
	}

	// Create agent command with options from YAML
	agentCmd, err := NewAgentCommand(
		cmdDescription,
		WithAgentType(description.AgentType),
		WithSystemPrompt(description.SystemPrompt),
		WithPrompt(description.Prompt),
		WithTools(description.Tools),
		WithAgentOptions(description.AgentOptions),
	)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create agent command")
	}

	return []cmds.Command{agentCmd}, nil
}

// LoadFileFromFS loads an agent command from a file in the file system
func (a *AgentCommandLoader) LoadFileFromFS(
	f fs.FS,
	filePath string,
	options []cmds.CommandDescriptionOption,
	parent *cmds.CommandDescription,
) ([]cmds.Command, error) {
	file, err := f.Open(filePath)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to open file %s", filePath)
	}
	defer file.Close()

	return a.loadAgentCommandFromReader(file, options, parent)
}
