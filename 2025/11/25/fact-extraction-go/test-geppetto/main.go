package main

import (
	"context"
	"fmt"
	"os"

	"github.com/go-go-golems/geppetto/pkg/steps/ai/openai"
	"github.com/go-go-golems/geppetto/pkg/steps/ai/settings"
	openaisettings "github.com/go-go-golems/geppetto/pkg/steps/ai/settings/openai"
	aitypes "github.com/go-go-golems/geppetto/pkg/steps/ai/types"
	"github.com/go-go-golems/geppetto/pkg/turns"
)

func main() {
	// Get API key and base URL
	apiKey := os.Getenv("OPENAI_API_KEY")
	if apiKey == "" {
		fmt.Println("Error: OPENAI_API_KEY not set")
		os.Exit(1)
	}

	baseURL := os.Getenv("OPENAI_BASE_URL")
	if baseURL == "" {
		baseURL = "https://api.openai.com/v1"
	}

	// Create step settings using NewStepSettings
	stepSettings, err := settings.NewStepSettings()
	if err != nil {
		fmt.Printf("Error creating step settings: %v\n", err)
		os.Exit(1)
	}

	// Configure API settings
	stepSettings.API = &settings.APISettings{
		APIKeys: map[string]string{
			"openai-api-key": apiKey,
		},
		BaseUrls: map[string]string{
			"openai-base-url": baseURL,
		},
	}

	// Configure Chat settings
	model := "gpt-4.1-mini"
	apiType := aitypes.ApiTypeOpenAI
	stepSettings.Chat = &settings.ChatSettings{
		Engine:  &model,
		ApiType: &apiType,
		Stream:  false,
	}

	// Configure OpenAI settings
	stepSettings.OpenAI = &openaisettings.Settings{}

	// Create OpenAI engine
	engine, err := openai.NewOpenAIEngine(stepSettings)
	if err != nil {
		fmt.Printf("Error creating OpenAI engine: %v\n", err)
		os.Exit(1)
	}

	// Create a simple turn
	turn := &turns.Turn{}
	turns.AppendBlock(turn, turns.NewSystemTextBlock("You are a helpful assistant."))
	turns.AppendBlock(turn, turns.NewUserTextBlock("Say hello in one sentence."))

	// Run inference
	fmt.Println("Running inference...")
	ctx := context.Background()
	resultTurn, err := engine.RunInference(ctx, turn)
	if err != nil {
		fmt.Printf("Error running inference: %v\n", err)
		os.Exit(1)
	}

	// Extract response
	fmt.Println("\n=== Response ===")
	for _, block := range resultTurn.Blocks {
		if block.Kind == turns.BlockKindLLMText {
			if text, ok := block.Payload["text"].(string); ok {
				fmt.Println(text)
			}
		}
	}

	fmt.Println("\n=== Success! ===")
}
