package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"strings"
	"time"

	"llm-middleware/llmflow"
)

// Example scenarios to demonstrate the middleware architecture
func main() {
	log.SetOutput(os.Stdout)
	log.SetFlags(log.LstdFlags | log.Lshortfile)

	fmt.Println("=== LLM Middleware Architecture Demo ===\n")

	// Run different example scenarios
	runBasicExample()
	runCachingExample()
	runSchemaExample()
	runThinkingModeExample()
	runComplexPipelineExample()
}

// runBasicExample demonstrates basic middleware chain with logging
func runBasicExample() {
	fmt.Println("--- Basic Example: Logging Middleware ---")
	
	client := llmflow.NewMockLLMClient()
	engine := llmflow.NewEngine(
		client,
		llmflow.Logging("BASIC"),
	)

	ctx := context.Background()
	turn, err := engine.NextTurn(ctx, "Hello, how are you?")
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	printTurnResults("basic_example", turn, engine)
	fmt.Println()
}

// runCachingExample demonstrates caching middleware
func runCachingExample() {
	fmt.Println("--- Caching Example: Cache Hit/Miss ---")
	
	client := llmflow.NewMockLLMClient()
	cache := llmflow.NewInMemoryCache()
	
	engine := llmflow.NewEngine(
		client,
		llmflow.Logging("CACHE"),
		llmflow.Cache(cache),
	)

	ctx := context.Background()
	
	// First call - cache miss
	fmt.Println("First call (cache miss):")
	turn1, err := engine.NextTurn(ctx, "What is the weather like?")
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}
	printTurnResults("cache_miss", turn1, engine)

	// Second call with same input - cache hit
	fmt.Println("\nSecond call with same input (cache hit):")
	turn2, err := engine.NextTurn(ctx, "What is the weather like?")
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}
	printTurnResults("cache_hit", turn2, engine)
	fmt.Println()
}

// runSchemaExample demonstrates schema enforcement middleware
func runSchemaExample() {
	fmt.Println("--- Schema Example: JSON Schema Enforcement ---")
	
	client := llmflow.NewMockLLMClient()
	schema := `{"title": "string", "content": "string", "author": "string", "publishDate": "date", "tags": ["string"]}`
	
	engine := llmflow.NewEngine(
		client,
		llmflow.Logging("SCHEMA"),
		llmflow.Schema(schema, "parsed_blog"),
	)

	ctx := context.Background()
	turn, err := engine.NextTurn(ctx, "Create a blog schema")
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	printTurnResults("schema_example", turn, engine)
	fmt.Println()
}

// runThinkingModeExample demonstrates thinking mode middleware
func runThinkingModeExample() {
	fmt.Println("--- Thinking Mode Example: Mode-based Prompting ---")
	
	client := llmflow.NewMockLLMClient()
	
	modePrompts := map[string]string{
		"draft":  "Draft mode: free-form reasoning and exploration.",
		"review": "Review mode: critical analysis and validation.",
		"final":  "Final mode: concise and definitive answers.",
	}
	
	modeCtrl, modeMW := llmflow.ThinkingMode("draft", modePrompts)
	
	engine := llmflow.NewEngine(
		client,
		llmflow.Logging("MODE"),
		modeMW,
	)

	ctx := context.Background()
	
	// First turn in draft mode
	fmt.Println("Turn 1 (draft mode):")
	turn1, err := engine.NextTurn(ctx, "Explain middleware patterns")
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}
	printTurnResults("mode_draft", turn1, engine)

	// Switch to final mode
	modeCtrl.SetMode("final")
	
	// Second turn in final mode
	fmt.Println("\nTurn 2 (final mode):")
	turn2, err := engine.NextTurn(ctx, "Generate a plan for implementation")
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}
	printTurnResults("mode_final", turn2, engine)
	fmt.Println()
}

// runComplexPipelineExample demonstrates a complex middleware pipeline
func runComplexPipelineExample() {
	fmt.Println("--- Complex Pipeline: All Middleware Combined ---")
	
	client := llmflow.NewMockLLMClient()
	cache := llmflow.NewInMemoryCache()
	schema := `{"answer": "string", "confidence": "number", "reasoning": "string"}`
	
	modePrompts := map[string]string{
		"analytical": "Analytical mode: provide detailed reasoning and high confidence scores.",
		"creative":   "Creative mode: think outside the box with moderate confidence.",
	}
	
	modeCtrl, modeMW := llmflow.ThinkingMode("analytical", modePrompts)
	
	engine := llmflow.NewEngine(
		client,
		llmflow.Logging("COMPLEX"),
		llmflow.Retry(3, 100*time.Millisecond),
		llmflow.Cache(cache),
		modeMW,
		llmflow.Schema(schema, "structured_response"),
	)

	ctx := context.Background()
	
	// First complex turn
	turn1, err := engine.NextTurn(ctx, "Create a blog schema")
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}
	printTurnResults("complex_turn1", turn1, engine)

	// Switch mode and make another turn
	modeCtrl.SetMode("creative")
	turn2, err := engine.NextTurn(ctx, "Generate a plan")
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}
	printTurnResults("complex_turn2", turn2, engine)
	fmt.Println()
}

// printTurnResults outputs the turn results in both JSON and markdown formats
func printTurnResults(scenarioName string, turn *llmflow.Turn, engine *llmflow.Engine) {
	// Create output directory if it doesn't exist
	os.MkdirAll("output", 0755)

	// Save JSON output (context and conversation)
	jsonOutput := map[string]interface{}{
		"turn":                turn,
		"conversation_history": engine.GetConversationHistory(),
		"context_store":       engine.GetContextStore(),
		"scenario":            scenarioName,
		"timestamp":           time.Now().Format(time.RFC3339),
	}

	jsonBytes, err := json.MarshalIndent(jsonOutput, "", "  ")
	if err != nil {
		log.Printf("Error marshaling JSON: %v", err)
		return
	}

	jsonFile := fmt.Sprintf("output/%s.json", scenarioName)
	err = os.WriteFile(jsonFile, jsonBytes, 0644)
	if err != nil {
		log.Printf("Error writing JSON file: %v", err)
	} else {
		fmt.Printf("JSON output saved to: %s\n", jsonFile)
	}

	// Save Markdown output (just conversation)
	mdContent := generateMarkdownConversation(engine.GetConversationHistory(), turn)
	mdFile := fmt.Sprintf("output/%s.md", scenarioName)
	err = os.WriteFile(mdFile, []byte(mdContent), 0644)
	if err != nil {
		log.Printf("Error writing Markdown file: %v", err)
	} else {
		fmt.Printf("Markdown output saved to: %s\n", mdFile)
	}

	// Print summary to console
	fmt.Printf("Turn %d completed. Messages: %d, Context keys: %d, Output keys: %d\n",
		turn.Index,
		len(turn.Messages),
		len(turn.Context),
		len(turn.Output))
}

// generateMarkdownConversation creates a markdown representation of the conversation
func generateMarkdownConversation(history []llmflow.Message, currentTurn *llmflow.Turn) string {
	var md strings.Builder
	
	md.WriteString("# Conversation Log\n\n")
	md.WriteString(fmt.Sprintf("**Generated:** %s\n\n", time.Now().Format(time.RFC3339)))
	
	// Write conversation history
	for i, msg := range history {
		switch msg.Role {
		case "user":
			md.WriteString(fmt.Sprintf("## User Message %d\n\n", i+1))
		case "assistant":
			md.WriteString(fmt.Sprintf("## Assistant Response %d\n\n", i+1))
		case "system":
			md.WriteString(fmt.Sprintf("## System Message %d\n\n", i+1))
		}
		
		md.WriteString(msg.Content)
		md.WriteString("\n\n")
	}
	
	// Add current turn information
	md.WriteString("## Turn Information\n\n")
	md.WriteString(fmt.Sprintf("- **Turn Index:** %d\n", currentTurn.Index))
	md.WriteString(fmt.Sprintf("- **Messages in Turn:** %d\n", len(currentTurn.Messages)))
	md.WriteString(fmt.Sprintf("- **Context Keys:** %d\n", len(currentTurn.Context)))
	md.WriteString(fmt.Sprintf("- **Output Keys:** %d\n", len(currentTurn.Output)))
	
	// Add output information if available
	if len(currentTurn.Output) > 0 {
		md.WriteString("\n### Turn Output\n\n")
		for key, value := range currentTurn.Output {
			md.WriteString(fmt.Sprintf("- **%s:** %v\n", key, value))
		}
	}
	
	return md.String()
}

