package extractor

import (
	"context"
	"fmt"
	"time"

	"github.com/fact-extraction/go-extractor/pkg/types"
	"github.com/go-go-golems/geppetto/pkg/steps/ai/openai"
	"github.com/go-go-golems/geppetto/pkg/steps/ai/settings"
	openaisettings "github.com/go-go-golems/geppetto/pkg/steps/ai/settings/openai"
	aitypes "github.com/go-go-golems/geppetto/pkg/steps/ai/types"
	"github.com/go-go-golems/geppetto/pkg/turns"
	"github.com/rs/zerolog/log"
)

// GeppettoExtractor extracts facts using the geppetto framework
type GeppettoExtractor struct {
	engine        *openai.OpenAIEngine
	promptBuilder *PromptBuilder
	parser        *ResultParser
}

// NewGeppettoExtractor creates a new geppetto-based extractor
func NewGeppettoExtractor(apiKey string, model string) (*GeppettoExtractor, error) {
	// Create step settings
	apiType := aitypes.ApiTypeOpenAI
	stepSettings := &settings.StepSettings{
		API: &settings.APISettings{
			APIKeys: map[string]string{
				"openai-api-key": apiKey,
			},
			BaseUrls: map[string]string{
				"openai-base-url": "https://api.openai.com/v1",
			},
		},
		Chat: &settings.ChatSettings{
			Engine:  &model,
			ApiType: &apiType,
			Stream:  false, // Disable streaming for simpler parsing
		},
		OpenAI: &openaisettings.Settings{},
	}

	// Create OpenAI engine
	engine, err := openai.NewOpenAIEngine(stepSettings)
	if err != nil {
		return nil, fmt.Errorf("failed to create OpenAI engine: %w", err)
	}

	return &GeppettoExtractor{
		engine:        engine,
		promptBuilder: NewPromptBuilder(),
		parser:        NewResultParser(),
	}, nil
}

// Extract extracts facts from a single document
func (ge *GeppettoExtractor) Extract(ctx context.Context, doc types.Document) (*types.ExtractionResult, error) {
	log.Debug().Str("doc_id", doc.ID).Msg("Starting extraction")

	// Build turn from document
	turn := ge.promptBuilder.BuildTurn(doc)

	// Run inference
	startTime := time.Now()
	resultTurn, err := ge.engine.RunInference(ctx, turn)
	if err != nil {
		return nil, fmt.Errorf("inference failed: %w", err)
	}
	duration := time.Since(startTime)

	log.Debug().
		Str("doc_id", doc.ID).
		Dur("duration", duration).
		Msg("Inference completed")

	// Extract assistant response
	assistantText := extractAssistantText(resultTurn)
	if assistantText == "" {
		return nil, fmt.Errorf("no assistant response found")
	}

	// Parse response
	response, err := ge.parser.Parse(assistantText)
	if err != nil {
		return nil, fmt.Errorf("failed to parse response: %w", err)
	}

	// Extract usage information from turn metadata
	tokensIn, tokensOut, cost := extractUsageInfo(resultTurn)

	result := &types.ExtractionResult{
		DocumentID:  doc.ID,
		Triples:     response.Triples,
		CostUSD:     cost,
		TokensIn:    tokensIn,
		TokensOut:   tokensOut,
		ProcessedAt: time.Now(),
	}

	log.Info().
		Str("doc_id", doc.ID).
		Int("triples", len(result.Triples)).
		Float64("cost", cost).
		Msg("Extraction completed")

	return result, nil
}

// extractAssistantText extracts the text from assistant blocks in a turn
func extractAssistantText(turn *turns.Turn) string {
	var text string
	for _, block := range turn.Blocks {
		if block.Role == "assistant" || block.Kind == turns.BlockKindLLMText {
			if textContent, ok := block.Payload["text"].(string); ok {
				text += textContent
			}
		}
	}
	return text
}

// extractUsageInfo extracts token usage and cost information from turn metadata
func extractUsageInfo(turn *turns.Turn) (tokensIn, tokensOut int, cost float64) {
	// Try to extract from turn.Data
	if turn.Data != nil {
		if usage, ok := turn.Data["usage"].(map[string]any); ok {
			if promptTokens, ok := usage["prompt_tokens"].(int); ok {
				tokensIn = promptTokens
			}
			if completionTokens, ok := usage["completion_tokens"].(int); ok {
				tokensOut = completionTokens
			}
		}
	}

	// Estimate cost based on gpt-4.1-mini pricing
	// Input: $0.15 per 1M tokens, Output: $0.60 per 1M tokens
	cost = (float64(tokensIn) * 0.15 / 1_000_000) + (float64(tokensOut) * 0.60 / 1_000_000)

	return
}
