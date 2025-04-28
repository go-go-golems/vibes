package llm

import (
	"context"

	"github.com/go-go-golems/geppetto/pkg/conversation"
	"github.com/go-go-golems/geppetto/pkg/embeddings"
	"github.com/go-go-golems/geppetto/pkg/steps"
	"github.com/go-go-golems/geppetto/pkg/steps/ai"
	"github.com/go-go-golems/geppetto/pkg/steps/ai/settings"
	"github.com/pkg/errors"
)

// GeppettoLLM is an implementation of the LLM interface that uses Geppetto for inference and embeddings
type GeppettoLLM struct {
	// stepSettings contains configuration for the LLM
	stepSettings *settings.StepSettings
	// embeddingProvider is used to generate embeddings
	embeddingProvider embeddings.Provider
}

// NewGeppettoLLM creates a new GeppettoLLM with the provided settings.
// It derives the embedding provider from the stepSettings.
func NewGeppettoLLM(
	stepSettings *settings.StepSettings,
) (*GeppettoLLM, error) {
	if stepSettings == nil {
		return nil, errors.New("stepSettings cannot be nil")
	}

	// Create embedding provider from stepSettings
	embeddingFactory := embeddings.NewSettingsFactoryFromStepSettings(stepSettings)
	embeddingProvider, err := embeddingFactory.NewProvider()
	if err != nil {
		// Provide more context in the error message
		return nil, errors.Wrapf(err, "failed to create embedding provider from step settings (type: %s, engine: %s)",
			stepSettings.Embeddings.Type, stepSettings.Embeddings.Engine)
	}

	return &GeppettoLLM{
		stepSettings:      stepSettings,
		embeddingProvider: embeddingProvider,
	}, nil
}

// Generate implements the LLM interface's Generate method
func (g *GeppettoLLM) Generate(ctx context.Context, messages []*conversation.Message) (*conversation.Message, error) {
	// Create a chat step based on the configured settings
	chatStep, err := createChatStep(g.stepSettings)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create chat step")
	}

	// Convert messages slice to Conversation type
	conv := conversation.Conversation(messages)

	// Start the step with the provided messages
	result, err := chatStep.Start(ctx, conv)
	if err != nil {
		return nil, errors.Wrap(err, "failed to start chat step")
	}

	// Get the results and return the last message
	results := result.Return()
	if len(results) == 0 {
		return nil, errors.New("no response generated")
	}

	lastResult := results[len(results)-1]
	message, err := lastResult.Value()
	if err != nil {
		return nil, errors.Wrap(err, "failed to get message value")
	}

	return message, nil
}

// GenerateEmbedding implements the LLM interface's GenerateEmbedding method
func (g *GeppettoLLM) GenerateEmbedding(ctx context.Context, text string) ([]float32, error) {
	if g.embeddingProvider == nil {
		return nil, errors.New("embedding provider is not initialized")
	}
	// Use the embedding provider to generate the embedding
	embedding, err := g.embeddingProvider.GenerateEmbedding(ctx, text)
	if err != nil {
		return nil, errors.Wrap(err, "failed to generate embedding")
	}

	return embedding, nil
}

// Helper functions

// createChatStep creates a chat step based on the step settings
func createChatStep(stepSettings *settings.StepSettings) (steps.Step[conversation.Conversation, *conversation.Message], error) {
	// Use StandardStepFactory to create the appropriate chat step based on settings
	factory := &ai.StandardStepFactory{
		Settings: stepSettings,
	}

	// The factory will automatically create the appropriate step based on API type
	return factory.NewStep()
}

// Ensure GeppettoLLM implements the LLM interface
var _ LLM = (*GeppettoLLM)(nil)
