// Package llm defines interfaces and implementations for LLM providers
package llm

import (
	"context"
	"github.com/goagent/framework/goagent/types"
)

// LLM interface defines the functionality of a language model
type LLM interface {
	// Generate generates a response to the given messages
	Generate(ctx context.Context, messages []types.Message) (string, error)

	// GenerateWithStream generates a response to the given messages with streaming
	GenerateWithStream(ctx context.Context, messages []types.Message) (<-chan string, error)

	// GenerateEmbedding generates an embedding for the given text
	GenerateEmbedding(ctx context.Context, text string) ([]float32, error)
}
