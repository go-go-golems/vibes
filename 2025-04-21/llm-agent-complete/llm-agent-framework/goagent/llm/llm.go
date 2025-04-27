// Package llm defines interfaces and implementations for LLM providers
package llm

import (
	"context"

	"github.com/go-go-golems/geppetto/pkg/conversation"
)

// LLM interface defines the functionality of a language model
type LLM interface {
	// Generate generates a response to the given messages
	Generate(ctx context.Context, messages []*conversation.Message) (string, error)

	// GenerateWithStream generates a response to the given messages with streaming
	GenerateWithStream(ctx context.Context, messages []*conversation.Message) (<-chan string, error)

	// GenerateEmbedding generates an embedding for the given text
	GenerateEmbedding(ctx context.Context, text string) ([]float32, error)
}
