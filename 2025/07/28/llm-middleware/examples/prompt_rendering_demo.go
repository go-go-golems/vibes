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

// Enhanced mock client with more sophisticated responses for prompt rendering demos
func setupEnhancedMockClient() *llmflow.MockLLMClient {
	client := llmflow.NewMockLLMClient()
	
	// Add responses for templating examples
	client.AddResponse("translate", "Bonjour, comment allez-vous?")
	client.AddResponse("summarize", "This article discusses the importance of renewable energy in combating climate change.")
	
	// Add responses for chain of thought examples
	client.AddResponse("step by step", "Let me think through this step by step:\n1. First, I need to understand the problem\n2. Then, I'll analyze the key factors\n3. Finally, I'll provide a solution\n\nThe answer is 42.")
	
	// Add JSON responses for structured schema
	err := client.SetJSONResponse("json", map[string]interface{}{
		"analysis": "The data shows a clear upward trend",
		"confidence": 0.85,
		"recommendations": []string{"increase investment", "monitor closely"},
	})
	if err != nil {
		log.Printf("Error setting JSON response: %v", err)
	}
	
	// Add persona-specific responses
	client.AddResponse("expert", "From a technical perspective, this requires deep analysis of the underlying algorithms and data structures.")
	client.AddResponse("beginner", "Let me explain this in simple terms that anyone can understand.")
	client.AddResponse("creative", "Imagine if we could approach this problem like an artist painting on a canvas...")
	
	return client
}

func main() {
	log.SetOutput(os.Stdout)
	log.SetFlags(log.LstdFlags | log.Lshortfile)

	fmt.Println("=== Advanced Prompt Rendering Middleware Demo ===\n")

	// Run different prompt rendering examples
	runPromptTemplatingExample()
	runChainOfThoughtExample()
	runExampleInjectionExample()
	runStructuredSchemaExample()
	runPersonaSwitchExample()
	runComplexPromptPipelineExample()
}

// runPromptTemplatingExample demonstrates template-based prompt generation
func runPromptTemplatingExample() {
	fmt.Println("--- Prompt Templating Example ---")
	
	client := setupEnhancedMockClient()
	
	// Template for translation tasks
	template := "Translate the following {{.Language}} text to {{.TargetLanguage}}: '{{.Text}}'"

	ctx := context.Background()
	
	// Set up template variables in context
	turn := &llmflow.Turn{
		Index:    0,
		Messages: []llmflow.Message{{Role: "user", Content: "Hello, how are you?"}},
		Context: map[string]interface{}{
			"template_vars": map[string]interface{}{
				"Language":       "English",
				"TargetLanguage": "French",
				"Text":           "Hello, how are you?",
			},
		},
		Output: make(map[string]interface{}),
	}

	// Manually execute the middleware chain for demonstration
	handler := llmflow.Compose(
		llmflow.Logging("TEMPLATE"),
		llmflow.PromptTemplating(template, "template_vars"),
	)(func(c context.Context, t *llmflow.Turn) error {
		raw, err := client.Infer(c, t.Messages)
		if err != nil {
			return err
		}
		t.Output["raw"] = raw
		return nil
	})

	err := handler(ctx, turn)
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	printTurnResults("prompt_templating", turn, nil)
	fmt.Println()
}

// runChainOfThoughtExample demonstrates CoT prompting
func runChainOfThoughtExample() {
	fmt.Println("--- Chain of Thought Example ---")
	
	client := setupEnhancedMockClient()

	ctx := context.Background()
	
	// First turn with CoT enabled
	turn1 := &llmflow.Turn{
		Index:    0,
		Messages: []llmflow.Message{{Role: "user", Content: "What is 2+2?"}},
		Context: map[string]interface{}{
			"enable_cot": true,
		},
		Output: make(map[string]interface{}),
	}

	handler := llmflow.Compose(
		llmflow.Logging("COT"),
		llmflow.ChainOfThoughtInjector("Let's think step by step.", "enable_cot", "cot_used"),
	)(func(c context.Context, t *llmflow.Turn) error {
		raw, err := client.Infer(c, t.Messages)
		if err != nil {
			return err
		}
		t.Output["raw"] = raw
		return nil
	})

	err := handler(ctx, turn1)
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	printTurnResults("chain_of_thought_enabled", turn1, nil)

	// Second turn with CoT disabled
	turn2 := &llmflow.Turn{
		Index:    1,
		Messages: []llmflow.Message{{Role: "user", Content: "What is 3+3?"}},
		Context: map[string]interface{}{
			"enable_cot": false,
		},
		Output: make(map[string]interface{}),
	}

	err = handler(ctx, turn2)
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	printTurnResults("chain_of_thought_disabled", turn2, nil)
	fmt.Println()
}

// runExampleInjectionExample demonstrates few-shot learning
func runExampleInjectionExample() {
	fmt.Println("--- Example Injection Example ---")
	
	client := setupEnhancedMockClient()
	
	// Create examples for sentiment analysis
	examples := []llmflow.Example{
		{User: "I love this product!", Assistant: "Positive sentiment"},
		{User: "This is terrible.", Assistant: "Negative sentiment"},
		{User: "It's okay, nothing special.", Assistant: "Neutral sentiment"},
		{User: "Amazing quality and great service!", Assistant: "Positive sentiment"},
	}

	ctx := context.Background()
	
	turn := &llmflow.Turn{
		Index:    0,
		Messages: []llmflow.Message{{Role: "user", Content: "Analyze sentiment: The weather is nice today."}},
		Context:  make(map[string]interface{}),
		Output:   make(map[string]interface{}),
	}

	handler := llmflow.Compose(
		llmflow.Logging("EXAMPLES"),
		llmflow.ExampleInjection(examples, 2, llmflow.ModeRandom),
	)(func(c context.Context, t *llmflow.Turn) error {
		raw, err := client.Infer(c, t.Messages)
		if err != nil {
			return err
		}
		t.Output["raw"] = raw
		return nil
	})

	err := handler(ctx, turn)
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	printTurnResults("example_injection", turn, nil)
	fmt.Println()
}

// runStructuredSchemaExample demonstrates structured output parsing
func runStructuredSchemaExample() {
	fmt.Println("--- Structured Schema Example ---")
	
	client := setupEnhancedMockClient()
	
	schema := `{
		"analysis": "string",
		"confidence": "number",
		"recommendations": ["string"]
	}`

	ctx := context.Background()
	
	turn := &llmflow.Turn{
		Index:    0,
		Messages: []llmflow.Message{{Role: "user", Content: "Analyze this data and provide structured output"}},
		Context:  make(map[string]interface{}),
		Output:   make(map[string]interface{}),
	}

	handler := llmflow.Compose(
		llmflow.Logging("SCHEMA"),
		llmflow.StructuredSchema(schema, "structured_output"),
	)(func(c context.Context, t *llmflow.Turn) error {
		raw, err := client.Infer(c, t.Messages)
		if err != nil {
			return err
		}
		t.Output["raw"] = raw
		return nil
	})

	err := handler(ctx, turn)
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	printTurnResults("structured_schema", turn, nil)
	fmt.Println()
}

// runPersonaSwitchExample demonstrates persona-based prompting
func runPersonaSwitchExample() {
	fmt.Println("--- Persona Switch Example ---")
	
	client := setupEnhancedMockClient()
	
	personas := map[string]string{
		"expert":    "You are a technical expert. Provide detailed, precise analysis.",
		"beginner":  "You are a friendly teacher. Explain things simply and clearly.",
		"creative":  "You are a creative thinker. Approach problems with imagination.",
	}
	
	controller, middleware := llmflow.PersonaSwitch("expert", personas)

	ctx := context.Background()
	
	// Turn 0: Initial with expert persona
	turn1 := &llmflow.Turn{
		Index:    0,
		Messages: []llmflow.Message{{Role: "user", Content: "Explain machine learning"}},
		Context:  make(map[string]interface{}),
		Output:   make(map[string]interface{}),
	}

	handler := llmflow.Compose(
		llmflow.Logging("PERSONA"),
		middleware,
	)(func(c context.Context, t *llmflow.Turn) error {
		raw, err := client.Infer(c, t.Messages)
		if err != nil {
			return err
		}
		t.Output["raw"] = raw
		return nil
	})

	err := handler(ctx, turn1)
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	printTurnResults("persona_expert", turn1, nil)

	// Switch to beginner persona
	controller.SetPersona("beginner")
	
	// Turn 1: With beginner persona
	turn2 := &llmflow.Turn{
		Index:    1,
		Messages: []llmflow.Message{{Role: "user", Content: "Explain machine learning"}},
		Context:  make(map[string]interface{}),
		Output:   make(map[string]interface{}),
	}

	err = handler(ctx, turn2)
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	printTurnResults("persona_beginner", turn2, nil)

	// Test persona switch command
	turn3 := &llmflow.Turn{
		Index:    2,
		Messages: []llmflow.Message{{Role: "user", Content: "/persona creative"}},
		Context:  make(map[string]interface{}),
		Output:   make(map[string]interface{}),
	}

	err = handler(ctx, turn3)
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	printTurnResults("persona_switch_command", turn3, nil)
	fmt.Println()
}

// runComplexPromptPipelineExample demonstrates all middleware working together
func runComplexPromptPipelineExample() {
	fmt.Println("--- Complex Prompt Pipeline: All Middleware Combined ---")
	
	client := setupEnhancedMockClient()
	
	// Set up all middleware components
	template := "Task: {{.Task}} | Context: {{.Context}}"
	
	examples := []llmflow.Example{
		{User: "Analyze: Sales increased 20%", Assistant: "Positive trend analysis"},
		{User: "Analyze: Costs rose 15%", Assistant: "Concerning cost analysis"},
	}
	
	schema := `{"summary": "string", "sentiment": "string", "confidence": "number"}`
	
	personas := map[string]string{
		"analyst": "You are a data analyst. Provide objective, data-driven insights.",
		"advisor": "You are a business advisor. Focus on actionable recommendations.",
	}
	
	controller, personaMiddleware := llmflow.PersonaSwitch("analyst", personas)
	_ = controller // Use controller to avoid unused variable error

	ctx := context.Background()
	
	turn := &llmflow.Turn{
		Index: 0,
		Messages: []llmflow.Message{{Role: "user", Content: "Analyze the quarterly performance data"}},
		Context: map[string]interface{}{
			"template_vars": map[string]interface{}{
				"Task":    "Quarterly Analysis",
				"Context": "Q3 2024 Performance Review",
			},
			"enable_cot": true,
		},
		Output: make(map[string]interface{}),
	}

	handler := llmflow.Compose(
		llmflow.Logging("COMPLEX"),
		llmflow.PromptTemplating(template, "template_vars"),
		llmflow.ChainOfThoughtInjector("Let's analyze this systematically.", "enable_cot", "cot_used"),
		llmflow.ExampleInjection(examples, 1, llmflow.ModeSequential),
		personaMiddleware,
		llmflow.StructuredSchema(schema, "analysis_result"),
	)(func(c context.Context, t *llmflow.Turn) error {
		raw, err := client.Infer(c, t.Messages)
		if err != nil {
			return err
		}
		t.Output["raw"] = raw
		return nil
	})

	err := handler(ctx, turn)
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	printTurnResults("complex_prompt_pipeline", turn, nil)
	fmt.Println()
}

// printTurnResults outputs the turn results in both JSON and markdown formats
func printTurnResults(scenarioName string, turn *llmflow.Turn, engine *llmflow.Engine) {
	// Create output directory if it doesn't exist
	os.MkdirAll("output", 0755)

	// Save JSON output
	jsonOutput := map[string]interface{}{
		"turn":      turn,
		"scenario":  scenarioName,
		"timestamp": time.Now().Format(time.RFC3339),
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

	// Save Markdown output
	mdContent := generateMarkdownConversation(turn)
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

// generateMarkdownConversation creates a markdown representation of the turn
func generateMarkdownConversation(turn *llmflow.Turn) string {
	var md strings.Builder
	
	md.WriteString("# Prompt Rendering Demo - Turn Analysis\n\n")
	md.WriteString(fmt.Sprintf("**Generated:** %s\n\n", time.Now().Format(time.RFC3339)))
	
	// Write messages
	md.WriteString("## Messages\n\n")
	for i, msg := range turn.Messages {
		switch msg.Role {
		case "user":
			md.WriteString(fmt.Sprintf("### User Message %d\n\n", i+1))
		case "assistant":
			md.WriteString(fmt.Sprintf("### Assistant Response %d\n\n", i+1))
		case "system":
			md.WriteString(fmt.Sprintf("### System Message %d\n\n", i+1))
		}
		
		md.WriteString(msg.Content)
		md.WriteString("\n\n")
	}
	
	// Add turn information
	md.WriteString("## Turn Information\n\n")
	md.WriteString(fmt.Sprintf("- **Turn Index:** %d\n", turn.Index))
	md.WriteString(fmt.Sprintf("- **Total Messages:** %d\n", len(turn.Messages)))
	md.WriteString(fmt.Sprintf("- **Context Keys:** %d\n", len(turn.Context)))
	md.WriteString(fmt.Sprintf("- **Output Keys:** %d\n", len(turn.Output)))
	
	// Add context information
	if len(turn.Context) > 0 {
		md.WriteString("\n### Context\n\n")
		for key, value := range turn.Context {
			md.WriteString(fmt.Sprintf("- **%s:** %v\n", key, value))
		}
	}
	
	// Add output information
	if len(turn.Output) > 0 {
		md.WriteString("\n### Output\n\n")
		for key, value := range turn.Output {
			md.WriteString(fmt.Sprintf("- **%s:** %v\n", key, value))
		}
	}
	
	return md.String()
}

