// Package cmds provides command types for executing agents in a CLI environment
package cmds

import (
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
)

// AgentCommandDescription defines the structure for agent commands
// It extends the basic command description with agent-specific fields
type AgentCommandDescription struct {
	Name      string                            `yaml:"name"`
	Short     string                            `yaml:"short"`
	Long      string                            `yaml:"long,omitempty"`
	Flags     []*parameters.ParameterDefinition `yaml:"flags,omitempty"`
	Arguments []*parameters.ParameterDefinition `yaml:"arguments,omitempty"`
	Layers    []layers.ParameterLayer           `yaml:"layers,omitempty"`
	Type      string                            `yaml:"type,omitempty"`
	Tags      []string                          `yaml:"tags,omitempty"`
	Metadata  map[string]interface{}            `yaml:"metadata,omitempty"`

	// Agent-specific fields
	AgentType    string                 `yaml:"agent-type"`    // "react", "plan-execute", "file-collection", etc.
	SystemPrompt string                 `yaml:"system-prompt"` // System prompt template
	Prompt       string                 `yaml:"prompt"`        // Template string for prompt generation
	Tools        []string               `yaml:"tools"`         // List of tool names to enable
	AgentOptions map[string]interface{} `yaml:"agent-options"` // Additional agent-specific options
}
