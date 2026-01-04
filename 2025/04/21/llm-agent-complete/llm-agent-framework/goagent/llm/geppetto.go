package llm

import (
	"context"

	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/go-go-golems/geppetto/pkg/conversation"
	"github.com/go-go-golems/geppetto/pkg/embeddings"
	"github.com/go-go-golems/geppetto/pkg/steps/ai"
	"github.com/go-go-golems/geppetto/pkg/steps/ai/chat"
	"github.com/go-go-golems/geppetto/pkg/steps/ai/settings"
	"github.com/pkg/errors"
	"github.com/rs/zerolog/log"
)

// GeppettoLLM is an implementation of the LLM interface that uses Geppetto for inference and embeddings
type GeppettoLLM struct {
	// stepSettings contains configuration for the LLM
	stepSettings *settings.StepSettings
	// embeddingProvider is used to generate embeddings
	embeddingProvider embeddings.Provider
	// publisher is used to publish events if configured
	publisher message.Publisher
	// topicID is the topic to publish events to if configured
	topicID string
}

// GeppettoLLMOption defines a function type for configuring GeppettoLLM.
type GeppettoLLMOption func(*GeppettoLLM) error

// WithPublisherAndTopic adds a publisher and topic for event emission
func WithPublisherAndTopic(publisher message.Publisher, topicID string) GeppettoLLMOption {
	return func(llm *GeppettoLLM) error {
		if publisher == nil {
			return errors.New("publisher cannot be nil")
		}
		if topicID == "" {
			return errors.New("topicID cannot be empty")
		}
		llm.publisher = publisher
		llm.topicID = topicID
		return nil
	}
}

// NewGeppettoLLM creates a new GeppettoLLM with the provided settings and options.
// It derives the embedding provider from the stepSettings.
func NewGeppettoLLM(
	stepSettings *settings.StepSettings,
	opts ...GeppettoLLMOption,
) (*GeppettoLLM, error) {
	if stepSettings == nil {
		return nil, errors.New("stepSettings cannot be nil")
	}

	stepSettings = stepSettings.Clone()
	stepSettings.Chat.Stream = true

	embeddingFactory := embeddings.NewSettingsFactoryFromStepSettings(stepSettings)
	embeddingProvider, err := embeddingFactory.NewProvider()
	if err != nil {
		return nil, errors.Wrapf(err, "failed to create embedding provider from step settings (type: %s, engine: %s)",
			stepSettings.Embeddings.Type, stepSettings.Embeddings.Engine)
	}

	llm := &GeppettoLLM{
		stepSettings:      stepSettings,
		embeddingProvider: embeddingProvider,
	}

	// Apply options
	for _, opt := range opts {
		if err := opt(llm); err != nil {
			return nil, errors.Wrap(err, "failed to apply GeppettoLLM option")
		}
	}

	return llm, nil
}

// createChatStep creates a new chat step, potentially configured to publish events.
func createChatStep(
	stepSettings *settings.StepSettings,
	publisher message.Publisher,
	topicID string,
) (chat.Step, error) {
	factory := &ai.StandardStepFactory{
		Settings: stepSettings,
	}
	step, err := factory.NewStep()
	if err != nil {
		return nil, errors.Wrap(err, "failed to create chat step in factory")
	}

	// Add publisher if provided
	if publisher != nil && topicID != "" {
		log.Debug().Str("topic", topicID).Msg("Adding published topic to step")
		err = step.AddPublishedTopic(publisher, topicID)
		if err != nil {
			return nil, errors.Wrapf(err, "failed to add published topic %s", topicID)
		}
	} else {
		log.Debug().Msg("No publisher or topic ID provided, skipping event publishing setup for step")
	}

	// Type assertion to the specific expected step type
	return step, nil
}

// Generate implements the LLM interface's Generate method.
// If a publisher and topicID are configured, it uses the createChatStep helper
// to ensure the underlying step publishes events.
func (g *GeppettoLLM) Generate(
	ctx context.Context,
	messages []*conversation.Message,
) (*conversation.Message, error) {
	// Use the helper function to create the step, passing publisher/topic
	chatStep, err := createChatStep(g.stepSettings, g.publisher, g.topicID)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create chat step")
	}

	conv := conversation.Conversation(messages)
	result, err := chatStep.Start(ctx, conv)
	if err != nil {
		return nil, errors.Wrap(err, "failed to start chat step")
	}

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
	embedding, err := g.embeddingProvider.GenerateEmbedding(ctx, text)
	if err != nil {
		return nil, errors.Wrap(err, "failed to generate embedding")
	}

	return embedding, nil
}

var _ LLM = (*GeppettoLLM)(nil)
