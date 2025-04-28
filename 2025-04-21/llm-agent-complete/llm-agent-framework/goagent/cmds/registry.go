package cmds

import (
	"context"
	"fmt"
	"sync"

	"github.com/goagent/framework/goagent/agent"
	"github.com/goagent/framework/goagent/llm"
)

// AgentConstructor is a function that creates a new agent
type AgentConstructor func(ctx context.Context, llmModel llm.LLM, options map[string]interface{}) (agent.Agent, error)

// AgentTypeRegistry manages the registration and retrieval of agent types
type AgentTypeRegistry interface {
	// Register registers a new agent constructor with the given name
	Register(name string, constructor AgentConstructor)

	// Get retrieves an agent constructor by name
	Get(name string) (AgentConstructor, bool)

	// List returns a list of all registered agent types
	List() []string
}

// DefaultAgentTypeRegistry is the default implementation of AgentTypeRegistry
type DefaultAgentTypeRegistry struct {
	constructors map[string]AgentConstructor
	mu           sync.RWMutex
}

// NewAgentTypeRegistry creates a new DefaultAgentTypeRegistry
func NewAgentTypeRegistry() *DefaultAgentTypeRegistry {
	return &DefaultAgentTypeRegistry{
		constructors: make(map[string]AgentConstructor),
	}
}

// Register registers a new agent constructor with the given name
func (r *DefaultAgentTypeRegistry) Register(name string, constructor AgentConstructor) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.constructors[name] = constructor
}

// Get retrieves an agent constructor by name
func (r *DefaultAgentTypeRegistry) Get(name string) (AgentConstructor, bool) {
	r.mu.RLock()
	defer r.mu.RUnlock()
	constructor, ok := r.constructors[name]
	return constructor, ok
}

// List returns a list of all registered agent types
func (r *DefaultAgentTypeRegistry) List() []string {
	r.mu.RLock()
	defer r.mu.RUnlock()

	agentTypes := make([]string, 0, len(r.constructors))
	for name := range r.constructors {
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
func RegisterAgentType(name string, constructor AgentConstructor) {
	globalAgentTypeRegistry.Register(name, constructor)
}

// GetAgentConstructor retrieves an agent constructor from the global registry
func GetAgentConstructor(agentType string) (AgentConstructor, error) {
	constructor, ok := globalAgentTypeRegistry.Get(agentType)
	if !ok {
		return nil, fmt.Errorf("agent type not found: %s", agentType)
	}
	return constructor, nil
}

// init registers the default agent types
func init() {
	// Register the built-in agent types
	RegisterAgentType("react", func(ctx context.Context, llmModel llm.LLM, options map[string]interface{}) (agent.Agent, error) {
		maxIterations := 10
		if maxIter, ok := options["max_iterations"].(int); ok {
			maxIterations = maxIter
		}
		return agent.NewReActAgent(llmModel, maxIterations), nil
	})

	RegisterAgentType("plan-execute", func(ctx context.Context, llmModel llm.LLM, options map[string]interface{}) (agent.Agent, error) {
		maxIterations := 10
		if maxIter, ok := options["max_iterations"].(int); ok {
			maxIterations = maxIter
		}
		return agent.NewPlanAndExecuteAgent(llmModel, llmModel, maxIterations), nil
	})

	RegisterAgentType("file-collection", func(ctx context.Context, llmModel llm.LLM, options map[string]interface{}) (agent.Agent, error) {
		maxIterations := 10
		if maxIter, ok := options["max_iterations"].(int); ok {
			maxIterations = maxIter
		}

		return agent.NewFileCollectionAgent(llmModel, maxIterations), nil
	})
}
