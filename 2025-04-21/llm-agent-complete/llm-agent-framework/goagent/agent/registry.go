package agent

import (
	"context"
	"fmt"
	"sync"

	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/goagent/framework/goagent/llm"
)

type AgentFactory interface {
	NewAgent(ctx context.Context, parsedLayers *layers.ParsedLayers, llmModel llm.LLM) (Agent, error)
	CreateLayers() ([]layers.ParameterLayer, error)
}

// AgentTypeRegistry manages the registration and retrieval of agent types
type AgentTypeRegistry interface {
	// Register registers a new agent constructor with the given name
	Register(name string, factory AgentFactory)

	// Get retrieves an agent constructor by name
	Get(name string) (AgentFactory, bool)

	// List returns a list of all registered agent types
	List() []string
}

// DefaultAgentTypeRegistry is the default implementation of AgentTypeRegistry
type DefaultAgentTypeRegistry struct {
	factories map[string]AgentFactory
	mu        sync.RWMutex
}

// NewAgentTypeRegistry creates a new DefaultAgentTypeRegistry
func NewAgentTypeRegistry() *DefaultAgentTypeRegistry {
	return &DefaultAgentTypeRegistry{
		factories: make(map[string]AgentFactory),
	}
}

// Register registers a new agent constructor with the given name
func (r *DefaultAgentTypeRegistry) Register(name string, factory AgentFactory) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.factories[name] = factory
}

// Get retrieves an agent constructor by name
func (r *DefaultAgentTypeRegistry) Get(name string) (AgentFactory, bool) {
	r.mu.RLock()
	defer r.mu.RUnlock()
	factory, ok := r.factories[name]
	return factory, ok
}

// List returns a list of all registered agent types
func (r *DefaultAgentTypeRegistry) List() []string {
	r.mu.RLock()
	defer r.mu.RUnlock()

	agentTypes := make([]string, 0, len(r.factories))
	for name := range r.factories {
		agentTypes = append(agentTypes, name)
	}

	return agentTypes
}

// Global agent type registry
var globalAgentTypeRegistry = NewAgentTypeRegistry()

// GetGlobalAgentTypeRegistry returns the global agent type registry
func GetGlobalAgentTypeRegistry() AgentTypeRegistry {
	return globalAgentTypeRegistry
}

// RegisterAgentType registers an agent type with the global registry
func RegisterAgentType(name string, factory AgentFactory) {
	globalAgentTypeRegistry.Register(name, factory)
}

// GetAgentConstructor retrieves an agent constructor from the global registry
func GetAgentFactory(agentType string) (AgentFactory, error) {
	factory, ok := globalAgentTypeRegistry.Get(agentType)
	if !ok {
		return nil, fmt.Errorf("agent type not found: %s", agentType)
	}
	return factory, nil
}

// init registers the default agent types
func init() {
	// Register the built-in agent types
	RegisterAgentType(ReactAgentType, &ReactAgentFactory{})

	RegisterAgentType(PlanAndExecuteAgentType, &PlanAndExecuteAgentFactory{})

	RegisterAgentType(FileCollectionAgentType, &FileCollectionAgentFactory{})
}
