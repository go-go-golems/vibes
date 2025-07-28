package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	async "github.com/manus/llm-middleware-async"
)

func main() {
	fmt.Println("=== Async LLM Middleware Architecture Demo ===")
	
	// Create output directory
	if err := os.MkdirAll("async_output", 0755); err != nil {
		log.Fatalf("Failed to create output directory: %v", err)
	}
	
	// Run all async examples
	runAsyncBasicExample()
	runAsyncConcurrencyExample()
	runAsyncPromptRenderingExample()
	runAsyncErrorHandlingExample()
	runAsyncPerformanceExample()
	runAsyncComplexPipelineExample()
	
	fmt.Println("\n=== All Async Examples Completed ===")
}

func runAsyncBasicExample() {
	fmt.Println("\n--- Async Basic Example ---")
	
	client := async.NewMockAsyncLLMClient()
	config := &async.EngineConfig{
		MaxConcurrentTurns: 5,
		TurnTimeout:        10 * time.Second,
		EnableMetrics:      true,
		BufferSize:         10,
	}
	
	engine := async.NewEngine(
		client,
		config,
		async.AsyncLogging("BASIC"),
	)
	
	ctx := context.Background()
	
	// Process a simple turn
	resultChan := engine.NextTurnAsync(ctx, "Hello, how are you?")
	
	select {
	case result := <-resultChan:
		if result.Error != nil {
			log.Printf("Error: %v", result.Error)
			return
		}
		
		saveAsyncOutput("basic_example", result.Turn)
		fmt.Printf("Turn %d completed. Messages: %d, Status: %s, Duration: %v\n", 
			result.Turn.Index, len(result.Turn.Messages), result.Turn.Status, result.Turn.Duration)
		
	case <-time.After(15 * time.Second):
		fmt.Println("Timeout waiting for result")
	}
	
	// Print engine metrics
	metrics := engine.GetMetrics()
	fmt.Printf("Engine metrics: Total: %d, Completed: %d, Failed: %d, Avg Latency: %v\n",
		metrics.TotalTurns, metrics.CompletedTurns, metrics.FailedTurns, metrics.AverageLatency)
}

func runAsyncConcurrencyExample() {
	fmt.Println("\n--- Async Concurrency Example ---")
	
	client := async.NewMockAsyncLLMClient()
	client.SetLatency(100 * time.Millisecond) // Simulate realistic latency
	
	config := &async.EngineConfig{
		MaxConcurrentTurns: 10,
		TurnTimeout:        5 * time.Second,
		EnableMetrics:      true,
	}
	
	engine := async.NewEngine(
		client,
		config,
		async.AsyncLogging("CONCURRENT"),
		async.AsyncCache(async.NewInMemoryAsyncCache()),
	)
	
	ctx := context.Background()
	
	// Launch multiple concurrent turns
	queries := []string{
		"What is the weather like?",
		"Explain machine learning",
		"Create a simple plan",
		"What is the weather like?", // Duplicate for cache test
		"Translate hello to French",
	}
	
	var wg sync.WaitGroup
	results := make([]async.AsyncResult, len(queries))
	
	start := time.Now()
	
	for i, query := range queries {
		wg.Add(1)
		go func(index int, q string) {
			defer wg.Done()
			
			resultChan := engine.NextTurnAsync(ctx, q)
			select {
			case result := <-resultChan:
				results[index] = result
				if result.Error != nil {
					log.Printf("Turn %d error: %v", index, result.Error)
				} else {
					fmt.Printf("Turn %d completed in %v\n", index, result.Turn.Duration)
				}
			case <-time.After(10 * time.Second):
				log.Printf("Turn %d timed out", index)
			}
		}(i, query)
	}
	
	wg.Wait()
	totalDuration := time.Since(start)
	
	// Save results
	for i, result := range results {
		if result.Error == nil {
			saveAsyncOutput(fmt.Sprintf("concurrent_turn_%d", i), result.Turn)
		}
	}
	
	fmt.Printf("Concurrent processing completed in %v\n", totalDuration)
	
	// Print final metrics
	metrics := engine.GetMetrics()
	fmt.Printf("Final metrics: Total: %d, Completed: %d, Peak Concurrency: %d\n",
		metrics.TotalTurns, metrics.CompletedTurns, metrics.PeakConcurrency)
}

func runAsyncPromptRenderingExample() {
	fmt.Println("\n--- Async Prompt Rendering Example ---")
	
	client := async.NewMockAsyncLLMClient()
	
	// Template example
	template := "Task: {{.Task}} | Context: {{.Context}} | Priority: {{.Priority}}"
	
	// Examples for few-shot learning
	examples := []async.Example{
		{User: "Analyze sales data", Assistant: "I'll analyze the sales trends and patterns", Weight: 1.0},
		{User: "Create a report", Assistant: "I'll generate a comprehensive report", Weight: 1.5},
		{User: "Summarize findings", Assistant: "Here's a concise summary of key findings", Weight: 1.2},
	}
	
	// Personas
	personas := map[string]string{
		"analyst": "You are a data analyst. Provide objective, data-driven insights.",
		"advisor": "You are a business advisor. Focus on actionable recommendations.",
		"expert":  "You are a technical expert. Provide detailed technical analysis.",
	}
	
	controller, personaMiddleware := async.AsyncPersonaSwitch("analyst", personas)
	
	config := &async.EngineConfig{
		MaxConcurrentTurns: 3,
		TurnTimeout:        10 * time.Second,
		EnableMetrics:      true,
	}
	
	engine := async.NewEngine(
		client,
		config,
		async.AsyncLogging("PROMPT"),
		async.AsyncPromptTemplating(template, "template_vars"),
		async.AsyncChainOfThoughtInjector("Let's analyze this systematically.", "enable_cot", "cot_used"),
		async.AsyncExampleInjection(examples, 2, async.ModeWeighted),
		personaMiddleware,
		async.AsyncStructuredSchema(`{"analysis": "string", "confidence": "number", "recommendations": ["string"]}`, "analysis_result"),
	)
	
	ctx := context.Background()
	
	// Create turn with template variables
	turn := async.NewTurn(0, "Analyze the quarterly performance data")
	turn.Context.SetVariable("template_vars", map[string]interface{}{
		"Task":     "Quarterly Analysis",
		"Context":  "Q4 2024 Performance Review",
		"Priority": "High",
	})
	turn.Context.SetFlag("enable_cot", true)
	
	// Process turn
	resultChan := engine.NextTurnAsync(ctx, "Analyze the quarterly performance data")
	
	select {
	case result := <-resultChan:
		if result.Error != nil {
			log.Printf("Error: %v", result.Error)
			return
		}
		
		saveAsyncOutput("prompt_rendering", result.Turn)
		fmt.Printf("Prompt rendering completed. Messages: %d, Artifacts: %d\n", 
			len(result.Turn.Messages), len(result.Turn.Context.Artifacts))
		
		// Test persona switching
		fmt.Println("Testing persona switch...")
		controller.SwitchPersona("advisor", result.Turn.ID)
		
		resultChan2 := engine.NextTurnAsync(ctx, "What are the next steps?")
		select {
		case result2 := <-resultChan2:
			if result2.Error == nil {
				saveAsyncOutput("persona_switched", result2.Turn)
				fmt.Printf("Persona switch completed. Current persona: %s\n", controller.GetCurrentPersona())
			}
		case <-time.After(10 * time.Second):
			fmt.Println("Persona switch timed out")
		}
		
	case <-time.After(15 * time.Second):
		fmt.Println("Prompt rendering timed out")
	}
}

func runAsyncErrorHandlingExample() {
	fmt.Println("\n--- Async Error Handling Example ---")
	
	client := async.NewMockAsyncLLMClient()
	
	config := &async.EngineConfig{
		MaxConcurrentTurns: 2,
		TurnTimeout:        2 * time.Second, // Short timeout for testing
		EnableMetrics:      true,
	}
	
	engine := async.NewEngine(
		client,
		config,
		async.AsyncLogging("ERROR"),
		async.AsyncRetry(3, 100*time.Millisecond),
	)
	
	ctx := context.Background()
	
	// Test timeout scenario
	fmt.Println("Testing timeout scenario...")
	client.SetLatency(5 * time.Second) // Longer than timeout
	
	resultChan := engine.NextTurnAsync(ctx, "This will timeout")
	
	select {
	case result := <-resultChan:
		if result.Error != nil {
			fmt.Printf("Expected timeout error: %v\n", result.Error)
			saveAsyncOutput("timeout_error", result.Turn)
		}
	case <-time.After(10 * time.Second):
		fmt.Println("Test itself timed out")
	}
	
	// Reset latency for next test
	client.SetLatency(50 * time.Millisecond)
	
	// Test context cancellation
	fmt.Println("Testing context cancellation...")
	cancelCtx, cancel := context.WithCancel(ctx)
	
	resultChan2 := engine.NextTurnAsync(cancelCtx, "This will be cancelled")
	
	// Cancel after a short delay
	go func() {
		time.Sleep(10 * time.Millisecond)
		cancel()
	}()
	
	select {
	case result := <-resultChan2:
		if result.Error != nil {
			fmt.Printf("Expected cancellation error: %v\n", result.Error)
			saveAsyncOutput("cancellation_error", result.Turn)
		}
	case <-time.After(5 * time.Second):
		fmt.Println("Cancellation test timed out")
	}
}

func runAsyncPerformanceExample() {
	fmt.Println("\n--- Async Performance Example ---")
	
	client := async.NewMockAsyncLLMClient()
	client.SetLatency(10 * time.Millisecond) // Fast responses
	
	config := &async.EngineConfig{
		MaxConcurrentTurns: 20,
		TurnTimeout:        5 * time.Second,
		EnableMetrics:      true,
	}
	
	engine := async.NewEngine(
		client,
		config,
		async.AsyncLogging("PERF"),
		async.AsyncCache(async.NewInMemoryAsyncCache()),
	)
	
	ctx := context.Background()
	
	// Benchmark sequential vs concurrent processing
	queries := make([]string, 10)
	for i := range queries {
		queries[i] = fmt.Sprintf("Query number %d", i)
	}
	
	// Sequential processing
	fmt.Println("Sequential processing...")
	start := time.Now()
	for i, query := range queries {
		resultChan := engine.NextTurnAsync(ctx, query)
		select {
		case result := <-resultChan:
			if result.Error != nil {
				log.Printf("Sequential turn %d error: %v", i, result.Error)
			}
		case <-time.After(5 * time.Second):
			log.Printf("Sequential turn %d timed out", i)
		}
	}
	sequentialDuration := time.Since(start)
	
	// Reset client for fair comparison
	client.Reset()
	
	// Concurrent processing
	fmt.Println("Concurrent processing...")
	start = time.Now()
	var wg sync.WaitGroup
	for i, query := range queries {
		wg.Add(1)
		go func(index int, q string) {
			defer wg.Done()
			resultChan := engine.NextTurnAsync(ctx, q)
			select {
			case result := <-resultChan:
				if result.Error != nil {
					log.Printf("Concurrent turn %d error: %v", index, result.Error)
				}
			case <-time.After(5 * time.Second):
				log.Printf("Concurrent turn %d timed out", index)
			}
		}(i, query)
	}
	wg.Wait()
	concurrentDuration := time.Since(start)
	
	fmt.Printf("Performance comparison:\n")
	fmt.Printf("  Sequential: %v\n", sequentialDuration)
	fmt.Printf("  Concurrent: %v\n", concurrentDuration)
	fmt.Printf("  Speedup: %.2fx\n", float64(sequentialDuration)/float64(concurrentDuration))
	
	// Save performance metrics
	metrics := engine.GetMetrics()
	perfData := map[string]interface{}{
		"sequential_duration": sequentialDuration,
		"concurrent_duration": concurrentDuration,
		"speedup":            float64(sequentialDuration) / float64(concurrentDuration),
		"engine_metrics":     metrics,
		"client_metrics":     client.GetMetrics(),
	}
	
	saveAsyncData("performance_metrics", perfData)
}

func runAsyncComplexPipelineExample() {
	fmt.Println("\n--- Async Complex Pipeline Example ---")
	
	client := async.NewMockAsyncLLMClient()
	
	// Complex middleware stack
	template := "Analysis Type: {{.AnalysisType}} | Data Source: {{.DataSource}} | Urgency: {{.Urgency}}"
	
	examples := []async.Example{
		{User: "Analyze market trends", Assistant: "Market analysis shows upward trend", Weight: 2.0},
		{User: "Review financial data", Assistant: "Financial review indicates strong performance", Weight: 1.5},
		{User: "Assess risk factors", Assistant: "Risk assessment reveals moderate exposure", Weight: 1.8},
	}
	
	personas := map[string]string{
		"analyst":    "You are a senior analyst. Provide comprehensive analysis.",
		"strategist": "You are a strategic advisor. Focus on long-term implications.",
		"specialist": "You are a domain specialist. Provide expert insights.",
	}
	
	controller, personaMiddleware := async.AsyncPersonaSwitch("analyst", personas)
	cache := async.NewInMemoryAsyncCache()
	
	config := &async.EngineConfig{
		MaxConcurrentTurns: 5,
		TurnTimeout:        15 * time.Second,
		EnableMetrics:      true,
	}
	
	engine := async.NewEngine(
		client,
		config,
		async.AsyncLogging("COMPLEX"),
		async.AsyncCache(cache),
		async.AsyncRetry(2, 50*time.Millisecond),
		async.AsyncPromptTemplating(template, "analysis_vars"),
		async.AsyncChainOfThoughtInjector("Let's approach this systematically.", "enable_analysis", "analysis_used"),
		async.AsyncExampleInjection(examples, 2, async.ModeWeighted),
		personaMiddleware,
		async.AsyncStructuredSchema(`{"summary": "string", "insights": ["string"], "confidence": "number", "next_steps": ["string"]}`, "structured_analysis"),
	)
	
	ctx := context.Background()
	
	// Create complex turn
	turn := async.NewTurn(0, "Perform comprehensive market analysis")
	turn.Context.SetVariable("analysis_vars", map[string]interface{}{
		"AnalysisType": "Market Research",
		"DataSource":   "Q4 2024 Market Data",
		"Urgency":      "High Priority",
	})
	turn.Context.SetFlag("enable_analysis", true)
	
	resultChan := engine.NextTurnAsync(ctx, "Perform comprehensive market analysis")
	
	select {
	case result := <-resultChan:
		if result.Error != nil {
			log.Printf("Error: %v", result.Error)
			return
		}
		
		saveAsyncOutput("complex_pipeline", result.Turn)
		
		fmt.Printf("Complex pipeline completed:\n")
		fmt.Printf("  Messages: %d\n", len(result.Turn.Messages))
		fmt.Printf("  Context artifacts: %d\n", len(result.Turn.Context.Artifacts))
		fmt.Printf("  Output artifacts: %d\n", len(result.Turn.Output.Artifacts))
		fmt.Printf("  Warnings: %d\n", len(result.Turn.Context.Warnings))
		fmt.Printf("  Duration: %v\n", result.Turn.Duration)
		
		// Print middleware timings
		if result.Turn.Output.Metrics != nil && result.Turn.Output.Metrics.MiddlewareTimings != nil {
			fmt.Println("  Middleware timings:")
			for name, duration := range result.Turn.Output.Metrics.MiddlewareTimings {
				fmt.Printf("    %s: %v\n", name, duration)
			}
		}
		
	case <-time.After(20 * time.Second):
		fmt.Println("Complex pipeline timed out")
	}
	
	// Test pipeline with persona switching
	fmt.Println("Testing pipeline with persona switch...")
	controller.SwitchPersona("strategist", "test-turn")
	
	resultChan2 := engine.NextTurnAsync(ctx, "What are the strategic implications?")
	select {
	case result2 := <-resultChan2:
		if result2.Error == nil {
			saveAsyncOutput("complex_pipeline_switched", result2.Turn)
			fmt.Printf("Pipeline with persona switch completed. Current persona: %s\n", controller.GetCurrentPersona())
		}
	case <-time.After(15 * time.Second):
		fmt.Println("Pipeline with persona switch timed out")
	}
}

func saveAsyncOutput(scenario string, turn *async.Turn) {
	// Save JSON output
	jsonFile := filepath.Join("async_output", scenario+".json")
	jsonData := map[string]interface{}{
		"scenario":  scenario,
		"timestamp": time.Now(),
		"turn":      turn,
	}
	
	jsonBytes, err := json.MarshalIndent(jsonData, "", "  ")
	if err != nil {
		log.Printf("Failed to marshal JSON for %s: %v", scenario, err)
		return
	}
	
	if err := os.WriteFile(jsonFile, jsonBytes, 0644); err != nil {
		log.Printf("Failed to write JSON file for %s: %v", scenario, err)
		return
	}
	
	// Save Markdown output
	mdFile := filepath.Join("async_output", scenario+".md")
	mdContent := generateAsyncMarkdown(scenario, turn)
	
	if err := os.WriteFile(mdFile, []byte(mdContent), 0644); err != nil {
		log.Printf("Failed to write Markdown file for %s: %v", scenario, err)
		return
	}
	
	fmt.Printf("JSON output saved to: %s\n", jsonFile)
	fmt.Printf("Markdown output saved to: %s\n", mdFile)
}

func saveAsyncData(filename string, data interface{}) {
	jsonFile := filepath.Join("async_output", filename+".json")
	jsonBytes, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		log.Printf("Failed to marshal data for %s: %v", filename, err)
		return
	}
	
	if err := os.WriteFile(jsonFile, jsonBytes, 0644); err != nil {
		log.Printf("Failed to write data file for %s: %v", filename, err)
		return
	}
	
	fmt.Printf("Data saved to: %s\n", jsonFile)
}

func generateAsyncMarkdown(scenario string, turn *async.Turn) string {
	var md strings.Builder
	
	md.WriteString(fmt.Sprintf("# %s\n\n", strings.Title(strings.ReplaceAll(scenario, "_", " "))))
	md.WriteString(fmt.Sprintf("**Turn ID:** %s  \n", turn.ID))
	md.WriteString(fmt.Sprintf("**Index:** %d  \n", turn.Index))
	md.WriteString(fmt.Sprintf("**Status:** %s  \n", turn.Status))
	if turn.Duration != nil {
		md.WriteString(fmt.Sprintf("**Duration:** %v  \n", *turn.Duration))
	}
	md.WriteString("\n")
	
	md.WriteString("## Conversation\n\n")
	for _, msg := range turn.Messages {
		role := strings.Title(string(msg.Role))
		md.WriteString(fmt.Sprintf("**%s:** %s\n\n", role, msg.Content))
	}
	
	if turn.Output != nil && turn.Output.Raw != "" {
		md.WriteString("## Output\n\n")
		md.WriteString(fmt.Sprintf("%s\n\n", turn.Output.Raw))
	}
	
	return md.String()
}

