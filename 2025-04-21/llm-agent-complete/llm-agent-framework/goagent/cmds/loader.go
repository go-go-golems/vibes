package cmds

import (
	"io"
	"io/fs"
	"strings"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/alias"
	"github.com/go-go-golems/glazed/pkg/cmds/loaders"
	"github.com/pkg/errors"
	"gopkg.in/yaml.v3"
)

// AgentCommandLoader loads agent commands from YAML files
type AgentCommandLoader struct{}

const (
	AgentCommandLoaderName = "agent"
)

var _ loaders.CommandLoader = (*AgentCommandLoader)(nil)

// IsFileSupported checks if the file is supported by this loader
func (a *AgentCommandLoader) IsFileSupported(f fs.FS, fileName string) bool {
	return strings.HasSuffix(fileName, ".yaml") || strings.HasSuffix(fileName, ".yml")
}

// LoadCommands implements the CommandLoader interface
func (a *AgentCommandLoader) LoadCommands(
	f fs.FS,
	entryName string,
	options []cmds.CommandDescriptionOption,
	aliasOptions []alias.Option,
) ([]cmds.Command, error) {
	r, err := f.Open(entryName)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to open file %s", entryName)
	}
	defer func(r fs.File) {
		_ = r.Close()
	}(r)

	// Add source tracking option
	sourceOption := cmds.WithSource("file:" + entryName)
	allOptions := append(options, sourceOption)

	return loaders.LoadCommandOrAliasFromReader(
		r,
		a.loadAgentCommandFromReader,
		allOptions,
		aliasOptions)
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
	_ []alias.Option,
) ([]cmds.Command, error) {
	var description AgentCommandDescription

	yamlContent, err := io.ReadAll(r)
	if err != nil {
		return nil, errors.Wrap(err, "failed to read YAML content")
	}

	buf := strings.NewReader(string(yamlContent))
	decoder := yaml.NewDecoder(buf)
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
		cmds.WithType(AgentCommandLoaderName), // Set a type for multi-loader support
	)

	// Apply additional options
	for _, option := range options {
		option(cmdDescription)
	}

	// Create agent command with options from YAML
	agentCmd, err := NewWriterAgentCommand(
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
