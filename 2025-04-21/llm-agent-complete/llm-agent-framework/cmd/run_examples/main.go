package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"

	"github.com/goagent/framework/goagent/examples"
)

func main() {
	// Create traces directory if it doesn't exist
	tracesDir := filepath.Join("..", "traces")
	if _, err := os.Stat(tracesDir); os.IsNotExist(err) {
		os.MkdirAll(tracesDir, 0755)
	}

	// Run research example
	fmt.Println("Running research example...")
	runResearchExample()

	// Run travel planning example
	fmt.Println("\nRunning travel planning example...")
	runTravelPlanningExample()

	// Run code exploration example
	fmt.Println("\nRunning code exploration example...")
	runCodeExplorationExample()

	fmt.Println("\nAll examples completed. Traces saved to the 'traces' directory.")
}

func runResearchExample() {
	// Set up the research agent
	agent, err := examples.SetupResearchAgent()
	if err != nil {
		fmt.Printf("Error setting up research agent: %v\n", err)
		return
	}

	// Run the agent
	ctx := context.Background()
	result, err := agent.Run(ctx, "Research the latest information on climate change impacts and write a comprehensive article about it.")
	if err != nil {
		fmt.Printf("Error running research agent: %v\n", err)
		return
	}

	// Save the result and trace
	saveResult("research_result.txt", result.Content.String())
	saveTrace("research_trace.json", agent.BaseAgent.GetTracer().GetEvents())
}

func runTravelPlanningExample() {
	// Set up the travel planning agent
	agent, err := examples.SetupTravelPlanningAgent()
	if err != nil {
		fmt.Printf("Error setting up travel planning agent: %v\n", err)
		return
	}

	// Run the agent
	ctx := context.Background()
	result, err := agent.Run(ctx, "Plan a 7-day trip to Europe, visiting Paris and Rome. Include flights, hotels, and must-see attractions.")
	if err != nil {
		fmt.Printf("Error running travel planning agent: %v\n", err)
		return
	}

	// Save the result and trace
	saveResult("travel_planning_result.txt", result.Content.String())
	saveTrace("travel_planning_trace.json", agent.BaseAgent.GetTracer().GetEvents())
}

func runCodeExplorationExample() {
	// Set up the code exploration agent
	agent, err := examples.SetupCodeExplorationAgent()
	if err != nil {
		fmt.Printf("Error setting up code exploration agent: %v\n", err)
		return
	}

	// Run the agent
	ctx := context.Background()
	result, err := agent.Run(ctx, "Explore the codebase in the /project directory and explain how the main.go file interacts with other components.")
	if err != nil {
		fmt.Printf("Error running code exploration agent: %v\n", err)
		return
	}

	// Save the result and trace
	saveResult("code_exploration_result.txt", result.Content.String())
	saveTrace("code_exploration_trace.json", agent.BaseAgent.GetTracer().GetEvents())
}

func saveResult(filename string, content string) {
	path := filepath.Join("..", "traces", filename)
	err := os.WriteFile(path, []byte(content), 0644)
	if err != nil {
		fmt.Printf("Error saving result to %s: %v\n", path, err)
	} else {
		fmt.Printf("Result saved to %s\n", path)
	}
}

func saveTrace(filename string, events []interface{}) {
	path := filepath.Join("..", "traces", filename)
	
	// Convert events to JSON
	data, err := json.MarshalIndent(events, "", "  ")
	if err != nil {
		fmt.Printf("Error marshaling trace data: %v\n", err)
		return
	}
	
	err = os.WriteFile(path, data, 0644)
	if err != nil {
		fmt.Printf("Error saving trace to %s: %v\n", path, err)
	} else {
		fmt.Printf("Trace saved to %s\n", path)
	}
}
