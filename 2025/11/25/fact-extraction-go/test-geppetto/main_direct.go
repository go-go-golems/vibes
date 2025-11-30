package main

import (
	"context"
	"fmt"
	"os"

	"github.com/sashabaranov/go-openai"
)

func main() {
	// Get API configuration from environment
	apiKey := os.Getenv("OPENAI_API_KEY")
	if apiKey == "" {
		fmt.Println("Error: OPENAI_API_KEY not set")
		os.Exit(1)
	}

	baseURL := os.Getenv("OPENAI_BASE_URL")
	if baseURL == "" {
		baseURL = "https://api.openai.com/v1"
	}

	// Create OpenAI client configuration
	config := openai.DefaultConfig(apiKey)
	config.BaseURL = baseURL

	// Create client
	client := openai.NewClientWithConfig(config)

	// Create chat completion request
	req := openai.ChatCompletionRequest{
		Model: "gpt-4.1-mini",
		Messages: []openai.ChatCompletionMessage{
			{
				Role:    openai.ChatMessageRoleSystem,
				Content: "You are a helpful assistant.",
			},
			{
				Role:    openai.ChatMessageRoleUser,
				Content: "Say hello in one sentence.",
			},
		},
		Stream: false, // Explicitly disable streaming
	}

	// Make request
	fmt.Println("Running inference...")
	ctx := context.Background()
	resp, err := client.CreateChatCompletion(ctx, req)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		os.Exit(1)
	}

	// Print response
	fmt.Println("\n=== Response ===")
	if len(resp.Choices) > 0 {
		fmt.Println(resp.Choices[0].Message.Content)
	}

	// Print usage
	fmt.Printf("\n=== Usage ===\n")
	fmt.Printf("Prompt tokens: %d\n", resp.Usage.PromptTokens)
	fmt.Printf("Completion tokens: %d\n", resp.Usage.CompletionTokens)
	fmt.Printf("Total tokens: %d\n", resp.Usage.TotalTokens)

	fmt.Println("\n=== Success! ===")
}
