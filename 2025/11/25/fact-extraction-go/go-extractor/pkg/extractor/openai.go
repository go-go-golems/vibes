package extractor

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/fact-extraction/go-extractor/pkg/types"
	"github.com/rs/zerolog/log"
	"github.com/sashabaranov/go-openai"
)

// OpenAIExtractor extracts facts using the OpenAI client directly
type OpenAIExtractor struct {
	client        *openai.Client
	model         string
	promptBuilder *PromptBuilder
	parser        *ResultParser
}

// NewOpenAIExtractor creates a new OpenAI-based extractor
func NewOpenAIExtractor(model string) (*OpenAIExtractor, error) {
	// Get API configuration from environment
	apiKey := os.Getenv("OPENAI_API_KEY")
	if apiKey == "" {
		return nil, fmt.Errorf("OPENAI_API_KEY environment variable not set")
	}

	baseURL := os.Getenv("OPENAI_BASE_URL")
	if baseURL == "" {
		baseURL = "https://api.openai.com/v1"
	}

	// Create OpenAI client
	config := openai.DefaultConfig(apiKey)
	config.BaseURL = baseURL

	client := openai.NewClientWithConfig(config)

	return &OpenAIExtractor{
		client:        client,
		model:         model,
		promptBuilder: NewPromptBuilder(),
		parser:        NewResultParser(),
	}, nil
}

// Extract extracts facts from a single document
func (oe *OpenAIExtractor) Extract(ctx context.Context, doc types.Document) (*types.ExtractionResult, error) {
	log.Debug().Str("doc_id", doc.ID).Msg("Starting extraction")

	// Build prompt
	systemPrompt := oe.promptBuilder.systemPrompt
	userPrompt := "Extract facts from the following document:\n\n" + doc.Content

	// Create chat completion request
	req := openai.ChatCompletionRequest{
		Model: oe.model,
		Messages: []openai.ChatCompletionMessage{
			{
				Role:    openai.ChatMessageRoleSystem,
				Content: systemPrompt,
			},
			{
				Role:    openai.ChatMessageRoleUser,
				Content: userPrompt,
			},
		},
		Stream: false, // Explicitly disable streaming for Manus proxy compatibility
	}

	// Run inference
	startTime := time.Now()
	resp, err := oe.client.CreateChatCompletion(ctx, req)
	if err != nil {
		return nil, fmt.Errorf("inference failed: %w", err)
	}
	duration := time.Since(startTime)

	log.Debug().
		Str("doc_id", doc.ID).
		Dur("duration", duration).
		Msg("Inference completed")

	// Extract assistant response
	if len(resp.Choices) == 0 {
		return nil, fmt.Errorf("no response choices returned")
	}

	assistantText := resp.Choices[0].Message.Content

	// Parse response
	response, err := oe.parser.Parse(assistantText)
	if err != nil {
		return nil, fmt.Errorf("failed to parse response: %w", err)
	}

	// Calculate cost based on gpt-4.1-mini pricing
	// Input: $0.15 per 1M tokens, Output: $0.60 per 1M tokens
	cost := (float64(resp.Usage.PromptTokens) * 0.15 / 1_000_000) +
		(float64(resp.Usage.CompletionTokens) * 0.60 / 1_000_000)

	result := &types.ExtractionResult{
		DocumentID:  doc.ID,
		Triples:     response.Triples,
		CostUSD:     cost,
		TokensIn:    resp.Usage.PromptTokens,
		TokensOut:   resp.Usage.CompletionTokens,
		ProcessedAt: time.Now(),
	}

	log.Info().
		Str("doc_id", doc.ID).
		Int("triples", len(result.Triples)).
		Float64("cost", cost).
		Int("tokens_in", result.TokensIn).
		Int("tokens_out", result.TokensOut).
		Msg("Extraction completed")

	return result, nil
}
