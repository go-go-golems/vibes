// Package examples provides example implementations using the GoAgent framework
package examples

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"github.com/goagent/framework/goagent/agent"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/memory"
	"github.com/goagent/framework/goagent/tools"
	"github.com/goagent/framework/goagent/types"
)

// SetupResearchAgent creates an agent for researching information and writing articles
func SetupResearchAgent() (*agent.ReActAgent, error) {
	// Create a mock LLM
	mockLLM := llm.NewMockLLM()
	
	// Create a ReAct agent
	reactAgent := agent.NewReActAgent(mockLLM, 10)
	
	// Create and add tools
	webSearch := tools.NewWebSearchTool()
	
	// Add some mock search results
	webSearch.AddSearchResults("climate change impacts", []tools.SearchResult{
		{
			Title:   "Climate Change Impacts - IPCC Report 2025",
			URL:     "https://example.com/ipcc-report-2025",
			Snippet: "The latest IPCC report shows accelerating impacts of climate change across all continents.",
		},
		{
			Title:   "How Climate Change Affects Biodiversity",
			URL:     "https://example.com/climate-biodiversity",
			Snippet: "Research indicates that climate change is causing significant biodiversity loss worldwide.",
		},
	})
	
	webSearch.AddSearchResults("renewable energy solutions", []tools.SearchResult{
		{
			Title:   "Advances in Solar Technology 2025",
			URL:     "https://example.com/solar-tech-2025",
			Snippet: "New solar panel designs have achieved 35% efficiency, making them more viable than ever.",
		},
		{
			Title:   "Wind Energy Growth Worldwide",
			URL:     "https://example.com/wind-energy-growth",
			Snippet: "Wind energy capacity has grown by 25% in the past year, with significant installations in developing countries.",
		},
	})
	
	// Create a file tool for writing content
	fileTool := &MockFileTool{
		name:        "write_file",
		description: "Write content to a file",
		files:       make(map[string]string),
	}
	
	// Add tools to the agent
	reactAgent.AddTool(webSearch)
	reactAgent.AddTool(fileTool)
	
	// Create and set up vector memory
	vectorMem, err := memory.NewSimpleVectorMemory(mockLLM)
	if err != nil {
		return nil, err
	}
	
	// Add some initial memories
	err = vectorMem.Add(context.Background(), types.MemoryEntry{
		Content: "Climate change is causing rising sea levels and more extreme weather events.",
		Metadata: map[string]string{
			"source": "previous_research",
			"topic":  "climate_change",
		},
	})
	if err != nil {
		return nil, err
	}
	
	// Set the memory
	reactAgent.SetMemory(vectorMem)
	
	// Add responses to the mock LLM for the research scenario
	addResearchResponses(mockLLM)
	
	return reactAgent, nil
}

// SetupTravelPlanningAgent creates an agent for planning travel trips
func SetupTravelPlanningAgent() (*agent.PlanAndExecuteAgent, error) {
	// Create mock LLMs for planner and executor
	plannerLLM := llm.NewMockLLM()
	executorLLM := llm.NewMockLLM()
	
	// Create a Plan-and-Execute agent
	planExecAgent := agent.NewPlanAndExecuteAgent(plannerLLM, executorLLM, 10)
	
	// Create and add tools
	flightSearchTool := &MockFlightSearchTool{
		name:        "search_flights",
		description: "Search for flights between cities",
		flights:     make(map[string][]FlightResult),
	}
	
	hotelSearchTool := &MockHotelSearchTool{
		name:        "search_hotels",
		description: "Search for hotels in a city",
		hotels:      make(map[string][]HotelResult),
	}
	
	attractionsTool := &MockAttractionsTool{
		name:        "find_attractions",
		description: "Find attractions in a city",
		attractions: make(map[string][]AttractionResult),
	}
	
	// Add mock data
	addMockTravelData(flightSearchTool, hotelSearchTool, attractionsTool)
	
	// Add tools to the agent
	planExecAgent.AddTool(flightSearchTool)
	planExecAgent.AddTool(hotelSearchTool)
	planExecAgent.AddTool(attractionsTool)
	
	// Add responses to the mock LLMs for the travel planning scenario
	addTravelPlanningResponses(plannerLLM, executorLLM)
	
	return planExecAgent, nil
}

// SetupCodeExplorationAgent creates an agent for exploring and analyzing codebases
func SetupCodeExplorationAgent() (*agent.ReActAgent, error) {
	// Create a mock LLM
	mockLLM := llm.NewMockLLM()
	
	// Create a ReAct agent
	reactAgent := agent.NewReActAgent(mockLLM, 15)
	
	// Create and add tools
	fileReaderTool := &MockFileReaderTool{
		name:        "read_file",
		description: "Read the content of a file",
		files:       make(map[string]string),
	}
	
	fileSearchTool := &MockFileSearchTool{
		name:        "search_files",
		description: "Search for files in a directory",
		fileSystem:  make(map[string][]string),
	}
	
	codeAnalysisTool := &MockCodeAnalysisTool{
		name:        "analyze_code",
		description: "Analyze code structure and dependencies",
		analyses:    make(map[string]CodeAnalysisResult),
	}
	
	// Add mock code data
	addMockCodeData(fileReaderTool, fileSearchTool, codeAnalysisTool)
	
	// Add tools to the agent
	reactAgent.AddTool(fileReaderTool)
	reactAgent.AddTool(fileSearchTool)
	reactAgent.AddTool(codeAnalysisTool)
	
	// Add responses to the mock LLM for the code exploration scenario
	addCodeExplorationResponses(mockLLM)
	
	return reactAgent, nil
}

// Mock tools and data structures for the examples

// MockFileTool is a mock implementation of a file writing tool
type MockFileTool struct {
	name        string
	description string
	files       map[string]string
}

func (t *MockFileTool) Name() string {
	return t.name
}

func (t *MockFileTool) Description() string {
	return t.description
}

func (t *MockFileTool) Execute(ctx context.Context, input string) (string, error) {
	var params struct {
		Filename string `json:"filename"`
		Content  string `json:"content"`
	}
	
	if err := json.Unmarshal([]byte(input), &params); err != nil {
		return "", fmt.Errorf("invalid input: %w", err)
	}
	
	t.files[params.Filename] = params.Content
	return fmt.Sprintf("File '%s' written successfully", params.Filename), nil
}

func (t *MockFileTool) Parameters() map[string]types.ParameterSchema {
	return map[string]types.ParameterSchema{
		"filename": {
			Type:        "string",
			Description: "The name of the file to write",
			Required:    true,
		},
		"content": {
			Type:        "string",
			Description: "The content to write to the file",
			Required:    true,
		},
	}
}

// FlightResult represents a flight search result
type FlightResult struct {
	Airline     string    `json:"airline"`
	FlightNo    string    `json:"flight_no"`
	Departure   string    `json:"departure"`
	Arrival     string    `json:"arrival"`
	DepartureAt time.Time `json:"departure_at"`
	ArrivalAt   time.Time `json:"arrival_at"`
	Price       float64   `json:"price"`
}

// MockFlightSearchTool is a mock implementation of a flight search tool
type MockFlightSearchTool struct {
	name        string
	description string
	flights     map[string][]FlightResult
}

func (t *MockFlightSearchTool) Name() string {
	return t.name
}

func (t *MockFlightSearchTool) Description() string {
	return t.description
}

func (t *MockFlightSearchTool) Execute(ctx context.Context, input string) (string, error) {
	var params struct {
		From string `json:"from"`
		To   string `json:"to"`
		Date string `json:"date"`
	}
	
	if err := json.Unmarshal([]byte(input), &params); err != nil {
		return "", fmt.Errorf("invalid input: %w", err)
	}
	
	key := fmt.Sprintf("%s-%s-%s", params.From, params.To, params.Date)
	results, ok := t.flights[key]
	if !ok {
		return "No flights found", nil
	}
	
	resultJSON, err := json.MarshalIndent(results, "", "  ")
	if err != nil {
		return "", err
	}
	
	return string(resultJSON), nil
}

func (t *MockFlightSearchTool) Parameters() map[string]types.ParameterSchema {
	return map[string]types.ParameterSchema{
		"from": {
			Type:        "string",
			Description: "Departure city",
			Required:    true,
		},
		"to": {
			Type:        "string",
			Description: "Arrival city",
			Required:    true,
		},
		"date": {
			Type:        "string",
			Description: "Departure date (YYYY-MM-DD)",
			Required:    true,
		},
	}
}

// HotelResult represents a hotel search result
type HotelResult struct {
	Name     string  `json:"name"`
	Address  string  `json:"address"`
	Stars    int     `json:"stars"`
	Price    float64 `json:"price"`
	Features string  `json:"features"`
}

// MockHotelSearchTool is a mock implementation of a hotel search tool
type MockHotelSearchTool struct {
	name        string
	description string
	hotels      map[string][]HotelResult
}

func (t *MockHotelSearchTool) Name() string {
	return t.name
}

func (t *MockHotelSearchTool) Description() string {
	return t.description
}

func (t *MockHotelSearchTool) Execute(ctx context.Context, input string) (string, error) {
	var params struct {
		City  string `json:"city"`
		Stars int    `json:"stars,omitempty"`
	}
	
	if err := json.Unmarshal([]byte(input), &params); err != nil {
		return "", fmt.Errorf("invalid input: %w", err)
	}
	
	results, ok := t.hotels[params.City]
	if !ok {
		return "No hotels found", nil
	}
	
	if params.Stars > 0 {
		var filtered []HotelResult
		for _, hotel := range results {
			if hotel.Stars >= params.Stars {
				filtered = append(filtered, hotel)
			}
		}
		results = filtered
	}
	
	resultJSON, err := json.MarshalIndent(results, "", "  ")
	if err != nil {
		return "", err
	}
	
	return string(resultJSON), nil
}

func (t *MockHotelSearchTool) Parameters() map[string]types.ParameterSchema {
	return map[string]types.ParameterSchema{
		"city": {
			Type:        "string",
			Description: "City to search for hotels",
			Required:    true,
		},
		"stars": {
			Type:        "integer",
			Description: "Minimum star rating (1-5)",
			Required:    false,
		},
	}
}

// AttractionResult represents an attraction search result
type AttractionResult struct {
	Name        string  `json:"name"`
	Description string  `json:"description"`
	Category    string  `json:"category"`
	Rating      float64 `json:"rating"`
}

// MockAttractionsTool is a mock implementation of an attractions search tool
type MockAttractionsTool struct {
	name        string
	description string
	attractions map[string][]AttractionResult
}

func (t *MockAttractionsTool) Name() string {
	return t.name
}

func (t *MockAttractionsTool) Description() string {
	return t.description
}

func (t *MockAttractionsTool) Execute(ctx context.Context, input string) (string, error) {
	var params struct {
		City     string `json:"city"`
		Category string `json:"category,omitempty"`
	}
	
	if err := json.Unmarshal([]byte(input), &params); err != nil {
		return "", fmt.Errorf("invalid input: %w", err)
	}
	
	results, ok := t.attractions[params.City]
	if !ok {
		return "No attractions found", nil
	}
	
	if params.Category != "" {
		var filtered []AttractionResult
		for _, attraction := range results {
			if strings.EqualFold(attraction.Category, params.Category) {
				filtered = append(filtered, attraction)
			}
		}
		results = filtered
	}
	
	resultJSON, err := json.MarshalIndent(results, "", "  ")
	if err != nil {
		return "", err
	}
	
	return string(resultJSON), nil
}

func (t *MockAttractionsTool) Parameters() map[string]types.ParameterSchema {
	return map[string]types.ParameterSchema{
		"city": {
			Type:        "string",
			Description: "City to search for attractions",
			Required:    true,
		},
		"category": {
			Type:        "string",
			Description: "Category of attractions (e.g., museum, park, restaurant)",
			Required:    false,
		},
	}
}

// MockFileReaderTool is a mock implementation of a file reader tool
type MockFileReaderTool struct {
	name        string
	description string
	files       map[string]string
}

func (t *MockFileReaderTool) Name() string {
	return t.name
}

func (t *MockFileReaderTool) Description() string {
	return t.description
}

func (t *MockFileReaderTool) Execute(ctx context.Context, input string) (string, error) {
	var params struct {
		Filename string `json:"filename"`
	}
	
	if err := json.Unmarshal([]byte(input), &params); err != nil {
		return "", fmt.Errorf("invalid input: %w", err)
	}
	
	content, ok := t.files[params.Filename]
	if !ok {
		return "", fmt.Errorf("file not found: %s", params.Filename)
	}
	
	return content, nil
}

func (t *MockFileReaderTool) Parameters() map[string]types.ParameterSchema {
	return map[string]types.ParameterSchema{
		"filename": {
			Type:        "string",
			Description: "The name of the file to read",
			Required:    true,
		},
	}
}

// MockFileSearchTool is a mock implementation of a file search tool
type MockFileSearchTool struct {
	name        string
	description string
	fileSystem  map[string][]string
}

func (t *MockFileSearchTool) Name() string {
	return t.name
}

func (t *MockFileSearchTool) Description() string {
	return t.description
}

func (t *MockFileSearchTool) Execute(ctx context.Context, input string) (string, error) {
	var params struct {
		Directory string `json:"directory"`
		Pattern   string `json:"pattern,omitempty"`
	}
	
	if err := json.Unmarshal([]byte(input), &params); err != nil {
		return "", fmt.Errorf("invalid input: %w", err)
	}
	
	files, ok := t.fileSystem[params.Directory]
	if !ok {
		return "", fmt.Errorf("directory not found: %s", params.Directory)
	}
	
	if params.Pattern != "" {
		var filtered []string
		for _, file := range files {
			if strings.Contains(file, params.Pattern) {
				filtered = append(filtered, file)
			}
		}
		files = filtered
	}
	
	resultJSON, err := json.MarshalIndent(files, "", "  ")
	if err != nil {
		return "", err
	}
	
	return string(resultJSON), nil
}

func (t *MockFileSearchTool) Parameters() map[string]types.ParameterSchema {
	return map[string]types.ParameterSchema{
		"directory": {
			Type:        "string",
			Description: "The directory to search in",
			Required:    true,
		},
		"pattern": {
			Type:        "string",
			Description: "Optional pattern to filter files",
			Required:    false,
		},
	}
}

// CodeAnalysisResult represents the result of code analysis
type CodeAnalysisResult struct {
	Functions    []string          `json:"functions"`
	Classes      []string          `json:"classes"`
	Imports      []string          `json:"imports"`
	Dependencies map[string]string `json:"dependencies"`
}

// MockCodeAnalysisTool is a mock implementation of a code analysis tool
type MockCodeAnalysisTool struct {
	name        string
	description string
	analyses    map[string]CodeAnalysisResult
}

func (t *MockCodeAnalysisTool) Name() string {
	return t.name
}

func (t *MockCodeAnalysisTool) Description() string {
	return t.description
}

func (t *MockCodeAnalysisTool) Execute(ctx context.Context, input string) (string, error) {
	var params struct {
		Filename string `json:"filename"`
	}
	
	if err := json.Unmarshal([]byte(input), &params); err != nil {
		return "", fmt.Errorf("invalid input: %w", err)
	}
	
	analysis, ok := t.analyses[params.Filename]
	if !ok {
		return "", fmt.Errorf("no analysis available for file: %s", params.Filename)
	}
	
	resultJSON, err := json.MarshalIndent(analysis, "", "  ")
	if err != nil {
		return "", err
	}
	
	return string(resultJSON), nil
}

func (t *MockCodeAnalysisTool) Parameters() map[string]types.ParameterSchema {
	return map[string]types.ParameterSchema{
		"filename": {
			Type:        "string",
			Description: "The name of the file to analyze",
			Required:    true,
		},
	}
}

// Helper functions to add mock data and responses

func addMockTravelData(flightTool *MockFlightSearchTool, hotelTool *MockHotelSearchTool, attractionsTool *MockAttractionsTool) {
	// Add mock flights
	flightTool.flights["New York-Paris-2025-06-15"] = []FlightResult{
		{
			Airline:     "Air France",
			FlightNo:    "AF123",
			Departure:   "JFK",
			Arrival:     "CDG",
			DepartureAt: time.Date(2025, 6, 15, 18, 30, 0, 0, time.UTC),
			ArrivalAt:   time.Date(2025, 6, 16, 8, 0, 0, 0, time.UTC),
			Price:       850.50,
		},
		{
			Airline:     "Delta",
			FlightNo:    "DL456",
			Departure:   "JFK",
			Arrival:     "CDG",
			DepartureAt: time.Date(2025, 6, 15, 21, 45, 0, 0, time.UTC),
			ArrivalAt:   time.Date(2025, 6, 16, 11, 15, 0, 0, time.UTC),
			Price:       790.75,
		},
	}
	
	flightTool.flights["Paris-Rome-2025-06-20"] = []FlightResult{
		{
			Airline:     "Alitalia",
			FlightNo:    "AZ789",
			Departure:   "CDG",
			Arrival:     "FCO",
			DepartureAt: time.Date(2025, 6, 20, 10, 15, 0, 0, time.UTC),
			ArrivalAt:   time.Date(2025, 6, 20, 12, 30, 0, 0, time.UTC),
			Price:       210.25,
		},
	}
	
	// Add mock hotels
	hotelTool.hotels["Paris"] = []HotelResult{
		{
			Name:     "Hotel de Luxe",
			Address:  "123 Champs-Élysées, Paris",
			Stars:    5,
			Price:    350.00,
			Features: "Spa, Restaurant, City View",
		},
		{
			Name:     "Cozy Parisian Inn",
			Address:  "45 Rue de Rivoli, Paris",
			Stars:    3,
			Price:    150.00,
			Features: "Free Wifi, Breakfast Included",
		},
	}
	
	hotelTool.hotels["Rome"] = []HotelResult{
		{
			Name:     "Roman Retreat",
			Address:  "78 Via Veneto, Rome",
			Stars:    4,
			Price:    220.00,
			Features: "Pool, Restaurant, Historic Building",
		},
	}
	
	// Add mock attractions
	attractionsTool.attractions["Paris"] = []AttractionResult{
		{
			Name:        "Eiffel Tower",
			Description: "Iconic iron tower built in 1889",
			Category:    "landmark",
			Rating:      4.7,
		},
		{
			Name:        "Louvre Museum",
			Description: "World's largest art museum and historic monument",
			Category:    "museum",
			Rating:      4.8,
		},
		{
			Name:        "Le Jules Verne",
			Description: "Upscale French restaurant in the Eiffel Tower",
			Category:    "restaurant",
			Rating:      4.5,
		},
	}
	
	attractionsTool.attractions["Rome"] = []AttractionResult{
		{
			Name:        "Colosseum",
			Description: "Ancient amphitheater built in 70-80 AD",
			Category:    "landmark",
			Rating:      4.8,
		},
		{
			Name:        "Vatican Museums",
			Description: "Museums displaying works from the extensive collection of the Catholic Church",
			Category:    "museum",
			Rating:      4.7,
		},
	}
}

func addMockCodeData(fileReaderTool *MockFileReaderTool, fileSearchTool *MockFileSearchTool, codeAnalysisTool *MockCodeAnalysisTool) {
	// Add mock files
	fileReaderTool.files["/project/main.go"] = `package main

import (
	"fmt"
	"github.com/example/project/utils"
)

func main() {
	fmt.Println("Hello, World!")
	result := utils.Calculate(10, 20)
	fmt.Printf("Result: %d\n", result)
}
`
	
	fileReaderTool.files["/project/utils/math.go"] = `package utils

// Calculate adds two numbers and returns the result
func Calculate(a, b int) int {
	return a + b
}

// Multiply multiplies two numbers and returns the result
func Multiply(a, b int) int {
	return a * b
}
`
	
	fileReaderTool.files["/project/utils/strings.go"] = `package utils

import "strings"

// Concat concatenates two strings with a separator
func Concat(a, b, sep string) string {
	return a + sep + b
}

// ToUpper converts a string to uppercase
func ToUpper(s string) string {
	return strings.ToUpper(s)
}
`
	
	// Add mock file system
	fileSearchTool.fileSystem["/project"] = []string{
		"main.go",
		"go.mod",
		"go.sum",
		"README.md",
	}
	
	fileSearchTool.fileSystem["/project/utils"] = []string{
		"math.go",
		"strings.go",
	}
	
	// Add mock code analyses
	codeAnalysisTool.analyses["/project/main.go"] = CodeAnalysisResult{
		Functions: []string{"main"},
		Classes:   []string{},
		Imports:   []string{"fmt", "github.com/example/project/utils"},
		Dependencies: map[string]string{
			"utils": "github.com/example/project/utils",
		},
	}
	
	codeAnalysisTool.analyses["/project/utils/math.go"] = CodeAnalysisResult{
		Functions: []string{"Calculate", "Multiply"},
		Classes:   []string{},
		Imports:   []string{},
		Dependencies: map[string]string{},
	}
	
	codeAnalysisTool.analyses["/project/utils/strings.go"] = CodeAnalysisResult{
		Functions: []string{"Concat", "ToUpper"},
		Classes:   []string{},
		Imports:   []string{"strings"},
		Dependencies: map[string]string{
			"strings": "strings",
		},
	}
}

func addResearchResponses(mockLLM *llm.MockLLM) {
	// Add responses for the research scenario
	mockLLM.AddResponse(
		"system: You are an AI assistant that can use tools to help answer questions.\n\nYou have access to the following tools:\n\nTool: web_search\nDescription: Search the web for information\nParameters:\n  - query: The search query\n\nTool: write_file\nDescription: Write content to a file\nParameters:\n  - filename: The name of the file to write\n  - content: The content to write to the file\n\nUse the following format:\n\nThought: you should always think about what to do\nAction: the action to take, should be one of [web_search, write_file]\nAction Input: the input to the action (in JSON format)\nObservation: the result of the action\n... (this Thought/Action/Action Input/Observation can repeat N times)\nThought: I now know the final answer\nFinal Answer: the final answer to the original input question\n\nuser: Research the latest information on climate change impacts and write a comprehensive article about it.",
		"Thought: I need to gather information about the latest climate change impacts. Let me search for recent information.\nAction: web_search\nAction Input: {\"query\": \"climate change impacts\"}")
	
	mockLLM.AddResponse(
		"system: You are an AI assistant that can use tools to help answer questions.\n\nYou have access to the following tools:\n\nTool: web_search\nDescription: Search the web for information\nParameters:\n  - query: The search query\n\nTool: write_file\nDescription: Write content to a file\nParameters:\n  - filename: The name of the file to write\n  - content: The content to write to the file\n\nUse the following format:\n\nThought: you should always think about what to do\nAction: the action to take, should be one of [web_search, write_file]\nAction Input: the input to the action (in JSON format)\nObservation: the result of the action\n... (this Thought/Action/Action Input/Observation can repeat N times)\nThought: I now know the final answer\nFinal Answer: the final answer to the original input question\n\nuser: Research the latest information on climate change impacts and write a comprehensive article about it.\nassistant: Thought: I need to gather information about the latest climate change impacts. Let me search for recent information.\nAction: web_search\nAction Input: {\"query\": \"climate change impacts\"}\nuser: Observation: [\n  {\n    \"title\": \"Climate Change Impacts - IPCC Report 2025\",\n    \"url\": \"https://example.com/ipcc-report-2025\",\n    \"snippet\": \"The latest IPCC report shows accelerating impacts of climate change across all continents.\"\n  },\n  {\n    \"title\": \"How Climate Change Affects Biodiversity\",\n    \"url\": \"https://example.com/climate-biodiversity\",\n    \"snippet\": \"Research indicates that climate change is causing significant biodiversity loss worldwide.\"\n  }\n]",
		"Thought: I've found some information about climate change impacts, including a reference to the latest IPCC report and information about biodiversity impacts. Let me search for information about renewable energy solutions as well, since that's an important part of addressing climate change.\nAction: web_search\nAction Input: {\"query\": \"renewable energy solutions\"}")
	
	mockLLM.AddResponse(
		"system: You are an AI assistant that can use tools to help answer questions.\n\nYou have access to the following tools:\n\nTool: web_search\nDescription: Search the web for information\nParameters:\n  - query: The search query\n\nTool: write_file\nDescription: Write content to a file\nParameters:\n  - filename: The name of the file to write\n  - content: The content to write to the file\n\nUse the following format:\n\nThought: you should always think about what to do\nAction: the action to take, should be one of [web_search, write_file]\nAction Input: the input to the action (in JSON format)\nObservation: the result of the action\n... (this Thought/Action/Action Input/Observation can repeat N times)\nThought: I now know the final answer\nFinal Answer: the final answer to the original input question\n\nuser: Research the latest information on climate change impacts and write a comprehensive article about it.\nassistant: Thought: I need to gather information about the latest climate change impacts. Let me search for recent information.\nAction: web_search\nAction Input: {\"query\": \"climate change impacts\"}\nuser: Observation: [\n  {\n    \"title\": \"Climate Change Impacts - IPCC Report 2025\",\n    \"url\": \"https://example.com/ipcc-report-2025\",\n    \"snippet\": \"The latest IPCC report shows accelerating impacts of climate change across all continents.\"\n  },\n  {\n    \"title\": \"How Climate Change Affects Biodiversity\",\n    \"url\": \"https://example.com/climate-biodiversity\",\n    \"snippet\": \"Research indicates that climate change is causing significant biodiversity loss worldwide.\"\n  }\n]\nassistant: Thought: I've found some information about climate change impacts, including a reference to the latest IPCC report and information about biodiversity impacts. Let me search for information about renewable energy solutions as well, since that's an important part of addressing climate change.\nAction: web_search\nAction Input: {\"query\": \"renewable energy solutions\"}\nuser: Observation: [\n  {\n    \"title\": \"Advances in Solar Technology 2025\",\n    \"url\": \"https://example.com/solar-tech-2025\",\n    \"snippet\": \"New solar panel designs have achieved 35% efficiency, making them more viable than ever.\"\n  },\n  {\n    \"title\": \"Wind Energy Growth Worldwide\",\n    \"url\": \"https://example.com/wind-energy-growth\",\n    \"snippet\": \"Wind energy capacity has grown by 25% in the past year, with significant installations in developing countries.\"\n  }\n]",
		"Thought: Now I have information about both climate change impacts and renewable energy solutions. I can write a comprehensive article that covers the latest findings on climate change impacts and discusses potential solutions through renewable energy. Let me write the article.\nAction: write_file\nAction Input: {\"filename\": \"climate_change_article.md\", \"content\": \"# The Accelerating Impact of Climate Change and the Promise of Renewable Energy\\n\\n## Introduction\\n\\nClimate change continues to be one of the most pressing challenges facing our planet. According to the latest IPCC Report 2025, the impacts of climate change are accelerating across all continents, affecting ecosystems, human health, and economic stability. This article explores the latest findings on climate change impacts and examines how renewable energy solutions are evolving to address this global crisis.\\n\\n## Recent Climate Change Impacts\\n\\nThe latest IPCC report provides compelling evidence that climate change impacts are intensifying worldwide. These impacts include:\\n\\n- Rising global temperatures leading to more frequent and severe heatwaves\\n- Increasing sea levels threatening coastal communities\\n- More extreme weather events, including hurricanes, floods, and droughts\\n- Accelerating loss of biodiversity and ecosystem disruption\\n\\nResearch indicates that climate change is causing significant biodiversity loss worldwide. As temperatures rise and habitats change, many species are unable to adapt quickly enough, leading to population declines and potential extinctions. This loss of biodiversity has profound implications for ecosystem stability and resilience.\\n\\n## Renewable Energy Solutions\\n\\nAddressing climate change requires a transition away from fossil fuels toward renewable energy sources. Recent advances in renewable energy technology offer promising solutions:\\n\\n### Solar Energy\\n\\nNew solar panel designs have achieved 35% efficiency, making them more viable than ever. This represents a significant improvement over previous generations of solar technology, which typically operated at 15-20% efficiency. Higher efficiency means more energy can be generated from the same amount of sunlight, reducing the cost per kilowatt-hour and making solar energy increasingly competitive with fossil fuels.\\n\\n### Wind Energy\\n\\nWind energy capacity has grown by 25% in the past year, with significant installations in developing countries. This rapid growth demonstrates the increasing economic viability of wind power as a clean energy solution. Improvements in turbine design, materials, and grid integration have all contributed to making wind energy one of the fastest-growing renewable energy sectors.\\n\\n## Conclusion\\n\\nThe latest research on climate change impacts underscores the urgency of transitioning to a low-carbon economy. While the challenges are significant, advances in renewable energy technology provide hope that we can mitigate the worst effects of climate change. By accelerating the deployment of solar, wind, and other renewable energy sources, we can reduce greenhouse gas emissions while creating new economic opportunities.\\n\\nAddressing climate change will require coordinated action at all levels—from individual choices to international agreements. The latest scientific findings and technological advances give us the knowledge and tools we need to create a more sustainable future.\"}")
	
	mockLLM.AddResponse(
		"system: You are an AI assistant that can use tools to help answer questions.\n\nYou have access to the following tools:\n\nTool: web_search\nDescription: Search the web for information\nParameters:\n  - query: The search query\n\nTool: write_file\nDescription: Write content to a file\nParameters:\n  - filename: The name of the file to write\n  - content: The content to write to the file\n\nUse the following format:\n\nThought: you should always think about what to do\nAction: the action to take, should be one of [web_search, write_file]\nAction Input: the input to the action (in JSON format)\nObservation: the result of the action\n... (this Thought/Action/Action Input/Observation can repeat N times)\nThought: I now know the final answer\nFinal Answer: the final answer to the original input question\n\nuser: Research the latest information on climate change impacts and write a comprehensive article about it.\nassistant: Thought: I need to gather information about the latest climate change impacts. Let me search for recent information.\nAction: web_search\nAction Input: {\"query\": \"climate change impacts\"}\nuser: Observation: [\n  {\n    \"title\": \"Climate Change Impacts - IPCC Report 2025\",\n    \"url\": \"https://example.com/ipcc-report-2025\",\n    \"snippet\": \"The latest IPCC report shows accelerating impacts of climate change across all continents.\"\n  },\n  {\n    \"title\": \"How Climate Change Affects Biodiversity\",\n    \"url\": \"https://example.com/climate-biodiversity\",\n    \"snippet\": \"Research indicates that climate change is causing significant biodiversity loss worldwide.\"\n  }\n]\nassistant: Thought: I've found some information about climate change impacts, including a reference to the latest IPCC report and information about biodiversity impacts. Let me search for information about renewable energy solutions as well, since that's an important part of addressing climate change.\nAction: web_search\nAction Input: {\"query\": \"renewable energy solutions\"}\nuser: Observation: [\n  {\n    \"title\": \"Advances in Solar Technology 2025\",\n    \"url\": \"https://example.com/solar-tech-2025\",\n    \"snippet\": \"New solar panel designs have achieved 35% efficiency, making them more viable than ever.\"\n  },\n  {\n    \"title\": \"Wind Energy Growth Worldwide\",\n    \"url\": \"https://example.com/wind-energy-growth\",\n    \"snippet\": \"Wind energy capacity has grown by 25% in the past year, with significant installations in developing countries.\"\n  }\n]\nassistant: Thought: Now I have information about both climate change impacts and renewable energy solutions. I can write a comprehensive article that covers the latest findings on climate change impacts and discusses potential solutions through renewable energy. Let me write the article.\nAction: write_file\nAction Input: {\"filename\": \"climate_change_article.md\", \"content\": \"# The Accelerating Impact of Climate Change and the Promise of Renewable Energy\\n\\n## Introduction\\n\\nClimate change continues to be one of the most pressing challenges facing our planet. According to the latest IPCC Report 2025, the impacts of climate change are accelerating across all continents, affecting ecosystems, human health, and economic stability. This article explores the latest findings on climate change impacts and examines how renewable energy solutions are evolving to address this global crisis.\\n\\n## Recent Climate Change Impacts\\n\\nThe latest IPCC report provides compelling evidence that climate change impacts are intensifying worldwide. These impacts include:\\n\\n- Rising global temperatures leading to more frequent and severe heatwaves\\n- Increasing sea levels threatening coastal communities\\n- More extreme weather events, including hurricanes, floods, and droughts\\n- Accelerating loss of biodiversity and ecosystem disruption\\n\\nResearch indicates that climate change is causing significant biodiversity loss worldwide. As temperatures rise and habitats change, many species are unable to adapt quickly enough, leading to population declines and potential extinctions. This loss of biodiversity has profound implications for ecosystem stability and resilience.\\n\\n## Renewable Energy Solutions\\n\\nAddressing climate change requires a transition away from fossil fuels toward renewable energy sources. Recent advances in renewable energy technology offer promising solutions:\\n\\n### Solar Energy\\n\\nNew solar panel designs have achieved 35% efficiency, making them more viable than ever. This represents a significant improvement over previous generations of solar technology, which typically operated at 15-20% efficiency. Higher efficiency means more energy can be generated from the same amount of sunlight, reducing the cost per kilowatt-hour and making solar energy increasingly competitive with fossil fuels.\\n\\n### Wind Energy\\n\\nWind energy capacity has grown by 25% in the past year, with significant installations in developing countries. This rapid growth demonstrates the increasing economic viability of wind power as a clean energy solution. Improvements in turbine design, materials, and grid integration have all contributed to making wind energy one of the fastest-growing renewable energy sectors.\\n\\n## Conclusion\\n\\nThe latest research on climate change impacts underscores the urgency of transitioning to a low-carbon economy. While the challenges are significant, advances in renewable energy technology provide hope that we can mitigate the worst effects of climate change. By accelerating the deployment of solar, wind, and other renewable energy sources, we can reduce greenhouse gas emissions while creating new economic opportunities.\\n\\nAddressing climate change will require coordinated action at all levels—from individual choices to international agreements. The latest scientific findings and technological advances give us the knowledge and tools we need to create a more sustainable future.\"}\nuser: Observation: File 'climate_change_article.md' written successfully",
		"Thought: I have successfully researched information about climate change impacts and renewable energy solutions, and written a comprehensive article about these topics. The article covers the latest findings from the IPCC report, discusses biodiversity loss due to climate change, and explores recent advances in solar and wind energy technology.\nFinal Answer: I've researched the latest information on climate change impacts and written a comprehensive article titled \"The Accelerating Impact of Climate Change and the Promise of Renewable Energy.\" The article covers the latest findings from the 2025 IPCC report, which shows accelerating impacts across all continents, and discusses how climate change is causing significant biodiversity loss worldwide. It also explores recent advances in renewable energy solutions, including new solar panel designs achieving 35% efficiency and wind energy capacity growing by 25% in the past year. The article has been saved as 'climate_change_article.md'.")
	
	// Add streaming responses
	mockLLM.AddStreamResponse(
		"system: You are an AI assistant that can use tools to help answer questions.\n\nYou have access to the following tools:\n\nTool: web_search\nDescription: Search the web for information\nParameters:\n  - query: The search query\n\nTool: write_file\nDescription: Write content to a file\nParameters:\n  - filename: The name of the file to write\n  - content: The content to write to the file\n\nUse the following format:\n\nThought: you should always think about what to do\nAction: the action to take, should be one of [web_search, write_file]\nAction Input: the input to the action (in JSON format)\nObservation: the result of the action\n... (this Thought/Action/Action Input/Observation can repeat N times)\nThought: I now know the final answer\nFinal Answer: the final answer to the original input question\n\nuser: Research the latest information on climate change impacts and write a comprehensive article about it.",
		[]string{
			"Thought: I need to gather ",
			"information about the latest ",
			"climate change impacts. Let me ",
			"search for recent information.\n",
			"Action: web_search\n",
			"Action Input: {\"query\": \"climate change impacts\"}",
		})
}

func addTravelPlanningResponses(plannerLLM, executorLLM *llm.MockLLM) {
	// Add responses for the travel planning scenario
	plannerLLM.AddResponse(
		"system: You are a task planning assistant. Given a task, create a detailed plan.\nBreak down the task into a sequence of steps that can be executed one by one.\nEach step should be clear and actionable.\n\nCreate a plan with the following format:\n1. First step\n2. Second step\n...\n\nuser: Plan a 7-day trip to Europe, visiting Paris and Rome. Include flights, hotels, and must-see attractions.",
		"1. Search for flights from New York to Paris for the first day of the trip\n2. Search for hotels in Paris for 3 nights\n3. Find attractions to visit in Paris\n4. Search for flights from Paris to Rome for the fourth day\n5. Search for hotels in Rome for 3 nights\n6. Find attractions to visit in Rome\n7. Create a day-by-day itinerary for the entire trip")
	
	executorLLM.AddResponse(
		"system: You are a task executor. Follow the plan and execute each step using available tools.\nYou have access to the following tools:\n\nTool: search_flights\nDescription: Search for flights between cities\nParameters:\n  - from: Departure city\n  - to: Arrival city\n  - date: Departure date (YYYY-MM-DD)\n\nTool: search_hotels\nDescription: Search for hotels in a city\nParameters:\n  - city: City to search for hotels\n  - stars: Minimum star rating (1-5)\n\nTool: find_attractions\nDescription: Find attractions in a city\nParameters:\n  - city: City to search for attractions\n  - category: Category of attractions (e.g., museum, park, restaurant)\n\nUse the following format:\n\nThought: think about the current step\nAction: the action to take, should be one of [search_flights, search_hotels, find_attractions]\nAction Input: the input for the action (in JSON format)\n\nuser: Plan: 1. Search for flights from New York to Paris for the first day of the trip\n2. Search for hotels in Paris for 3 nights\n3. Find attractions to visit in Paris\n4. Search for flights from Paris to Rome for the fourth day\n5. Search for hotels in Rome for 3 nights\n6. Find attractions to visit in Rome\n7. Create a day-by-day itinerary for the entire trip\nCurrent step: Search for flights from New York to Paris for the first day of the trip\nPrevious results: ",
		"Thought: I need to search for flights from New York to Paris for the first day of the trip. I'll use the search_flights tool for this.\nAction: search_flights\nAction Input: {\"from\": \"New York\", \"to\": \"Paris\", \"date\": \"2025-06-15\"}")
	
	executorLLM.AddResponse(
		"system: You are a task executor. Follow the plan and execute each step using available tools.\nYou have access to the following tools:\n\nTool: search_flights\nDescription: Search for flights between cities\nParameters:\n  - from: Departure city\n  - to: Arrival city\n  - date: Departure date (YYYY-MM-DD)\n\nTool: search_hotels\nDescription: Search for hotels in a city\nParameters:\n  - city: City to search for hotels\n  - stars: Minimum star rating (1-5)\n\nTool: find_attractions\nDescription: Find attractions in a city\nParameters:\n  - city: City to search for attractions\n  - category: Category of attractions (e.g., museum, park, restaurant)\n\nUse the following format:\n\nThought: think about the current step\nAction: the action to take, should be one of [search_flights, search_hotels, find_attractions]\nAction Input: the input for the action (in JSON format)\n\nuser: Plan: 1. Search for flights from New York to Paris for the first day of the trip\n2. Search for hotels in Paris for 3 nights\n3. Find attractions to visit in Paris\n4. Search for flights from Paris to Rome for the fourth day\n5. Search for hotels in Rome for 3 nights\n6. Find attractions to visit in Rome\n7. Create a day-by-day itinerary for the entire trip\nCurrent step: Search for hotels in Paris for 3 nights\nPrevious results: Step 1: Search for flights from New York to Paris for the first day of the trip\nResult: [\n  {\n    \"airline\": \"Air France\",\n    \"flight_no\": \"AF123\",\n    \"departure\": \"JFK\",\n    \"arrival\": \"CDG\",\n    \"departure_at\": \"2025-06-15T18:30:00Z\",\n    \"arrival_at\": \"2025-06-16T08:00:00Z\",\n    \"price\": 850.5\n  },\n  {\n    \"airline\": \"Delta\",\n    \"flight_no\": \"DL456\",\n    \"departure\": \"JFK\",\n    \"arrival\": \"CDG\",\n    \"departure_at\": \"2025-06-15T21:45:00Z\",\n    \"arrival_at\": \"2025-06-16T11:15:00Z\",\n    \"price\": 790.75\n  }\n]",
		"Thought: Now I need to search for hotels in Paris for 3 nights. I'll use the search_hotels tool for this. I'll look for hotels with at least 3 stars to ensure a comfortable stay.\nAction: search_hotels\nAction Input: {\"city\": \"Paris\", \"stars\": 3}")
	
	executorLLM.AddResponse(
		"system: You are a task executor. Follow the plan and execute each step using available tools.\nYou have access to the following tools:\n\nTool: search_flights\nDescription: Search for flights between cities\nParameters:\n  - from: Departure city\n  - to: Arrival city\n  - date: Departure date (YYYY-MM-DD)\n\nTool: search_hotels\nDescription: Search for hotels in a city\nParameters:\n  - city: City to search for hotels\n  - stars: Minimum star rating (1-5)\n\nTool: find_attractions\nDescription: Find attractions in a city\nParameters:\n  - city: City to search for attractions\n  - category: Category of attractions (e.g., museum, park, restaurant)\n\nUse the following format:\n\nThought: think about the current step\nAction: the action to take, should be one of [search_flights, search_hotels, find_attractions]\nAction Input: the input for the action (in JSON format)\n\nuser: Plan: 1. Search for flights from New York to Paris for the first day of the trip\n2. Search for hotels in Paris for 3 nights\n3. Find attractions to visit in Paris\n4. Search for flights from Paris to Rome for the fourth day\n5. Search for hotels in Rome for 3 nights\n6. Find attractions to visit in Rome\n7. Create a day-by-day itinerary for the entire trip\nCurrent step: Find attractions to visit in Paris\nPrevious results: Step 1: Search for flights from New York to Paris for the first day of the trip\nResult: [\n  {\n    \"airline\": \"Air France\",\n    \"flight_no\": \"AF123\",\n    \"departure\": \"JFK\",\n    \"arrival\": \"CDG\",\n    \"departure_at\": \"2025-06-15T18:30:00Z\",\n    \"arrival_at\": \"2025-06-16T08:00:00Z\",\n    \"price\": 850.5\n  },\n  {\n    \"airline\": \"Delta\",\n    \"flight_no\": \"DL456\",\n    \"departure\": \"JFK\",\n    \"arrival\": \"CDG\",\n    \"departure_at\": \"2025-06-15T21:45:00Z\",\n    \"arrival_at\": \"2025-06-16T11:15:00Z\",\n    \"price\": 790.75\n  }\n]\nStep 2: Search for hotels in Paris for 3 nights\nResult: [\n  {\n    \"name\": \"Hotel de Luxe\",\n    \"address\": \"123 Champs-Élysées, Paris\",\n    \"stars\": 5,\n    \"price\": 350,\n    \"features\": \"Spa, Restaurant, City View\"\n  },\n  {\n    \"name\": \"Cozy Parisian Inn\",\n    \"address\": \"45 Rue de Rivoli, Paris\",\n    \"stars\": 3,\n    \"price\": 150,\n    \"features\": \"Free Wifi, Breakfast Included\"\n  }\n]",
		"Thought: Now I need to find attractions to visit in Paris. I'll use the find_attractions tool to get a list of popular attractions. I won't specify a category to get a variety of options.\nAction: find_attractions\nAction Input: {\"city\": \"Paris\"}")
	
	plannerLLM.AddResponse(
		"system: You are a task finalizer. Given the original input, the plan, and the results of each step,\nprovide a comprehensive final answer.\n\nBe concise but thorough in your response.\n\nuser: Input: Plan a 7-day trip to Europe, visiting Paris and Rome. Include flights, hotels, and must-see attractions.\nPlan: 1. Search for flights from New York to Paris for the first day of the trip\n2. Search for hotels in Paris for 3 nights\n3. Find attractions to visit in Paris\n4. Search for flights from Paris to Rome for the fourth day\n5. Search for hotels in Rome for 3 nights\n6. Find attractions to visit in Rome\n7. Create a day-by-day itinerary for the entire trip\nResults: Step 1: Search for flights from New York to Paris for the first day of the trip\nResult: [\n  {\n    \"airline\": \"Air France\",\n    \"flight_no\": \"AF123\",\n    \"departure\": \"JFK\",\n    \"arrival\": \"CDG\",\n    \"departure_at\": \"2025-06-15T18:30:00Z\",\n    \"arrival_at\": \"2025-06-16T08:00:00Z\",\n    \"price\": 850.5\n  },\n  {\n    \"airline\": \"Delta\",\n    \"flight_no\": \"DL456\",\n    \"departure\": \"JFK\",\n    \"arrival\": \"CDG\",\n    \"departure_at\": \"2025-06-15T21:45:00Z\",\n    \"arrival_at\": \"2025-06-16T11:15:00Z\",\n    \"price\": 790.75\n  }\n]\nStep 2: Search for hotels in Paris for 3 nights\nResult: [\n  {\n    \"name\": \"Hotel de Luxe\",\n    \"address\": \"123 Champs-Élysées, Paris\",\n    \"stars\": 5,\n    \"price\": 350,\n    \"features\": \"Spa, Restaurant, City View\"\n  },\n  {\n    \"name\": \"Cozy Parisian Inn\",\n    \"address\": \"45 Rue de Rivoli, Paris\",\n    \"stars\": 3,\n    \"price\": 150,\n    \"features\": \"Free Wifi, Breakfast Included\"\n  }\n]\nStep 3: Find attractions to visit in Paris\nResult: [\n  {\n    \"name\": \"Eiffel Tower\",\n    \"description\": \"Iconic iron tower built in 1889\",\n    \"category\": \"landmark\",\n    \"rating\": 4.7\n  },\n  {\n    \"name\": \"Louvre Museum\",\n    \"description\": \"World's largest art museum and historic monument\",\n    \"category\": \"museum\",\n    \"rating\": 4.8\n  },\n  {\n    \"name\": \"Le Jules Verne\",\n    \"description\": \"Upscale French restaurant in the Eiffel Tower\",\n    \"category\": \"restaurant\",\n    \"rating\": 4.5\n  }\n]\nStep 4: Search for flights from Paris to Rome for the fourth day\nResult: [\n  {\n    \"airline\": \"Alitalia\",\n    \"flight_no\": \"AZ789\",\n    \"departure\": \"CDG\",\n    \"arrival\": \"FCO\",\n    \"departure_at\": \"2025-06-20T10:15:00Z\",\n    \"arrival_at\": \"2025-06-20T12:30:00Z\",\n    \"price\": 210.25\n  }\n]\nStep 5: Search for hotels in Rome for 3 nights\nResult: [\n  {\n    \"name\": \"Roman Retreat\",\n    \"address\": \"78 Via Veneto, Rome\",\n    \"stars\": 4,\n    \"price\": 220,\n    \"features\": \"Pool, Restaurant, Historic Building\"\n  }\n]\nStep 6: Find attractions to visit in Rome\nResult: [\n  {\n    \"name\": \"Colosseum\",\n    \"description\": \"Ancient amphitheater built in 70-80 AD\",\n    \"category\": \"landmark\",\n    \"rating\": 4.8\n  },\n  {\n    \"name\": \"Vatican Museums\",\n    \"description\": \"Museums displaying works from the extensive collection of the Catholic Church\",\n    \"category\": \"museum\",\n    \"rating\": 4.7\n  }\n]",
		"# 7-Day Europe Trip: Paris & Rome Itinerary\n\n## Flight Details\n\n**Outbound: New York to Paris**\n- Delta Airlines DL456\n- June 15, 2025, 9:45 PM - June 16, 2025, 11:15 AM\n- Price: $790.75\n\n**Connection: Paris to Rome**\n- Alitalia AZ789\n- June 20, 2025, 10:15 AM - 12:30 PM\n- Price: $210.25\n\n## Accommodations\n\n**Paris (June 16-19)**\n- Cozy Parisian Inn\n- 45 Rue de Rivoli, Paris\n- 3-star hotel, $150/night\n- Features: Free WiFi, Breakfast Included\n\n**Rome (June 20-23)**\n- Roman Retreat\n- 78 Via Veneto, Rome\n- 4-star hotel, $220/night\n- Features: Pool, Restaurant, Historic Building\n\n## Day-by-Day Itinerary\n\n### Day 1 (June 16) - Arrival in Paris\n- Arrive at Charles de Gaulle Airport (CDG) at 11:15 AM\n- Check-in at Cozy Parisian Inn\n- Afternoon: Rest and explore the neighborhood\n- Evening: Dinner at a local café\n\n### Day 2 (June 17) - Paris Exploration I\n- Morning: Visit the Eiffel Tower (4.7/5 rating)\n- Lunch at Le Jules Verne restaurant in the Eiffel Tower (4.5/5 rating)\n- Afternoon: Explore surrounding areas and Seine River\n\n### Day 3 (June 18) - Paris Exploration II\n- Full day at the Louvre Museum (4.8/5 rating)\n- Evening: Stroll along the Champs-Élysées\n\n### Day 4 (June 19) - Paris to Rome\n- Morning: Check-out from Cozy Parisian Inn\n- 10:15 AM: Flight from Paris (CDG) to Rome (FCO)\n- Arrive in Rome at 12:30 PM\n- Check-in at Roman Retreat\n- Afternoon: Rest and explore the neighborhood\n\n### Day 5 (June 20) - Rome Exploration I\n- Full day at the Colosseum and Roman Forum (4.8/5 rating)\n- Evening: Dinner in Trastevere district\n\n### Day 6 (June 21) - Rome Exploration II\n- Full day at Vatican Museums (4.7/5 rating)\n- Visit St. Peter's Basilica\n- Evening: Dinner near Piazza Navona\n\n### Day 7 (June 22) - Final Day in Rome\n- Morning: Shopping and souvenir hunting\n- Afternoon: Relax at hotel pool\n- Evening: Farewell dinner\n\n## Total Budget Estimate\n- Flights: $1,001.00\n- Hotels: $1,110.00 (3 nights in Paris + 3 nights in Rome)\n- Daily expenses (food, attractions, transportation): ~$150/day = $1,050\n- **Total estimate: $3,161.00**\n\nThis itinerary provides a balanced experience of both cities, allowing you to see the major attractions while also having time to relax and enjoy the local atmosphere.")
}

func addCodeExplorationResponses(mockLLM *llm.MockLLM) {
	// Add responses for the code exploration scenario
	mockLLM.AddResponse(
		"system: You are an AI assistant that can use tools to help answer questions.\n\nYou have access to the following tools:\n\nTool: read_file\nDescription: Read the content of a file\nParameters:\n  - filename: The name of the file to read\n\nTool: search_files\nDescription: Search for files in a directory\nParameters:\n  - directory: The directory to search in\n  - pattern: Optional pattern to filter files\n\nTool: analyze_code\nDescription: Analyze code structure and dependencies\nParameters:\n  - filename: The name of the file to analyze\n\nUse the following format:\n\nThought: you should always think about what to do\nAction: the action to take, should be one of [read_file, search_files, analyze_code]\nAction Input: the input to the action (in JSON format)\nObservation: the result of the action\n... (this Thought/Action/Action Input/Observation can repeat N times)\nThought: I now know the final answer\nFinal Answer: the final answer to the original input question\n\nuser: Explore the codebase in the /project directory and explain how the main.go file interacts with other components.",
		"Thought: I need to explore the codebase to understand how main.go interacts with other components. First, let me see what files are in the /project directory.\nAction: search_files\nAction Input: {\"directory\": \"/project\"}")
	
	mockLLM.AddResponse(
		"system: You are an AI assistant that can use tools to help answer questions.\n\nYou have access to the following tools:\n\nTool: read_file\nDescription: Read the content of a file\nParameters:\n  - filename: The name of the file to read\n\nTool: search_files\nDescription: Search for files in a directory\nParameters:\n  - directory: The directory to search in\n  - pattern: Optional pattern to filter files\n\nTool: analyze_code\nDescription: Analyze code structure and dependencies\nParameters:\n  - filename: The name of the file to analyze\n\nUse the following format:\n\nThought: you should always think about what to do\nAction: the action to take, should be one of [read_file, search_files, analyze_code]\nAction Input: the input to the action (in JSON format)\nObservation: the result of the action\n... (this Thought/Action/Action Input/Observation can repeat N times)\nThought: I now know the final answer\nFinal Answer: the final answer to the original input question\n\nuser: Explore the codebase in the /project directory and explain how the main.go file interacts with other components.\nassistant: Thought: I need to explore the codebase to understand how main.go interacts with other components. First, let me see what files are in the /project directory.\nAction: search_files\nAction Input: {\"directory\": \"/project\"}\nuser: Observation: [\n  \"main.go\",\n  \"go.mod\",\n  \"go.sum\",\n  \"README.md\"\n]",
		"Thought: Now I can see the files in the /project directory. Let me first look at the content of main.go to understand its structure and dependencies.\nAction: read_file\nAction Input: {\"filename\": \"/project/main.go\"}")
	
	mockLLM.AddResponse(
		"system: You are an AI assistant that can use tools to help answer questions.\n\nYou have access to the following tools:\n\nTool: read_file\nDescription: Read the content of a file\nParameters:\n  - filename: The name of the file to read\n\nTool: search_files\nDescription: Search for files in a directory\nParameters:\n  - directory: The directory to search in\n  - pattern: Optional pattern to filter files\n\nTool: analyze_code\nDescription: Analyze code structure and dependencies\nParameters:\n  - filename: The name of the file to analyze\n\nUse the following format:\n\nThought: you should always think about what to do\nAction: the action to take, should be one of [read_file, search_files, analyze_code]\nAction Input: the input to the action (in JSON format)\nObservation: the result of the action\n... (this Thought/Action/Action Input/Observation can repeat N times)\nThought: I now know the final answer\nFinal Answer: the final answer to the original input question\n\nuser: Explore the codebase in the /project directory and explain how the main.go file interacts with other components.\nassistant: Thought: I need to explore the codebase to understand how main.go interacts with other components. First, let me see what files are in the /project directory.\nAction: search_files\nAction Input: {\"directory\": \"/project\"}\nuser: Observation: [\n  \"main.go\",\n  \"go.mod\",\n  \"go.sum\",\n  \"README.md\"\n]\nassistant: Thought: Now I can see the files in the /project directory. Let me first look at the content of main.go to understand its structure and dependencies.\nAction: read_file\nAction Input: {\"filename\": \"/project/main.go\"}\nuser: Observation: package main\n\nimport (\n\t\"fmt\"\n\t\"github.com/example/project/utils\"\n)\n\nfunc main() {\n\tfmt.Println(\"Hello, World!\")\n\tresult := utils.Calculate(10, 20)\n\tfmt.Printf(\"Result: %d\\n\", result)\n}",
		"Thought: I can see that main.go imports a package called \"github.com/example/project/utils\" and uses a function called Calculate from it. Let me check if there's a utils directory in the project.\nAction: search_files\nAction Input: {\"directory\": \"/project/utils\"}")
	
	mockLLM.AddResponse(
		"system: You are an AI assistant that can use tools to help answer questions.\n\nYou have access to the following tools:\n\nTool: read_file\nDescription: Read the content of a file\nParameters:\n  - filename: The name of the file to read\n\nTool: search_files\nDescription: Search for files in a directory\nParameters:\n  - directory: The directory to search in\n  - pattern: Optional pattern to filter files\n\nTool: analyze_code\nDescription: Analyze code structure and dependencies\nParameters:\n  - filename: The name of the file to analyze\n\nUse the following format:\n\nThought: you should always think about what to do\nAction: the action to take, should be one of [read_file, search_files, analyze_code]\nAction Input: the input to the action (in JSON format)\nObservation: the result of the action\n... (this Thought/Action/Action Input/Observation can repeat N times)\nThought: I now know the final answer\nFinal Answer: the final answer to the original input question\n\nuser: Explore the codebase in the /project directory and explain how the main.go file interacts with other components.\nassistant: Thought: I need to explore the codebase to understand how main.go interacts with other components. First, let me see what files are in the /project directory.\nAction: search_files\nAction Input: {\"directory\": \"/project\"}\nuser: Observation: [\n  \"main.go\",\n  \"go.mod\",\n  \"go.sum\",\n  \"README.md\"\n]\nassistant: Thought: Now I can see the files in the /project directory. Let me first look at the content of main.go to understand its structure and dependencies.\nAction: read_file\nAction Input: {\"filename\": \"/project/main.go\"}\nuser: Observation: package main\n\nimport (\n\t\"fmt\"\n\t\"github.com/example/project/utils\"\n)\n\nfunc main() {\n\tfmt.Println(\"Hello, World!\")\n\tresult := utils.Calculate(10, 20)\n\tfmt.Printf(\"Result: %d\\n\", result)\n}\nassistant: Thought: I can see that main.go imports a package called \"github.com/example/project/utils\" and uses a function called Calculate from it. Let me check if there's a utils directory in the project.\nAction: search_files\nAction Input: {\"directory\": \"/project/utils\"}\nuser: Observation: [\n  \"math.go\",\n  \"strings.go\"\n]",
		"Thought: Great, I found the utils directory with two files: math.go and strings.go. Since main.go is using a Calculate function, it's likely in the math.go file. Let me check the content of math.go.\nAction: read_file\nAction Input: {\"filename\": \"/project/utils/math.go\"}")
	
	mockLLM.AddResponse(
		"system: You are an AI assistant that can use tools to help answer questions.\n\nYou have access to the following tools:\n\nTool: read_file\nDescription: Read the content of a file\nParameters:\n  - filename: The name of the file to read\n\nTool: search_files\nDescription: Search for files in a directory\nParameters:\n  - directory: The directory to search in\n  - pattern: Optional pattern to filter files\n\nTool: analyze_code\nDescription: Analyze code structure and dependencies\nParameters:\n  - filename: The name of the file to analyze\n\nUse the following format:\n\nThought: you should always think about what to do\nAction: the action to take, should be one of [read_file, search_files, analyze_code]\nAction Input: the input to the action (in JSON format)\nObservation: the result of the action\n... (this Thought/Action/Action Input/Observation can repeat N times)\nThought: I now know the final answer\nFinal Answer: the final answer to the original input question\n\nuser: Explore the codebase in the /project directory and explain how the main.go file interacts with other components.\nassistant: Thought: I need to explore the codebase to understand how main.go interacts with other components. First, let me see what files are in the /project directory.\nAction: search_files\nAction Input: {\"directory\": \"/project\"}\nuser: Observation: [\n  \"main.go\",\n  \"go.mod\",\n  \"go.sum\",\n  \"README.md\"\n]\nassistant: Thought: Now I can see the files in the /project directory. Let me first look at the content of main.go to understand its structure and dependencies.\nAction: read_file\nAction Input: {\"filename\": \"/project/main.go\"}\nuser: Observation: package main\n\nimport (\n\t\"fmt\"\n\t\"github.com/example/project/utils\"\n)\n\nfunc main() {\n\tfmt.Println(\"Hello, World!\")\n\tresult := utils.Calculate(10, 20)\n\tfmt.Printf(\"Result: %d\\n\", result)\n}\nassistant: Thought: I can see that main.go imports a package called \"github.com/example/project/utils\" and uses a function called Calculate from it. Let me check if there's a utils directory in the project.\nAction: search_files\nAction Input: {\"directory\": \"/project/utils\"}\nuser: Observation: [\n  \"math.go\",\n  \"strings.go\"\n]\nassistant: Thought: Great, I found the utils directory with two files: math.go and strings.go. Since main.go is using a Calculate function, it's likely in the math.go file. Let me check the content of math.go.\nAction: read_file\nAction Input: {\"filename\": \"/project/utils/math.go\"}\nuser: Observation: package utils\n\n// Calculate adds two numbers and returns the result\nfunc Calculate(a, b int) int {\n\treturn a + b\n}\n\n// Multiply multiplies two numbers and returns the result\nfunc Multiply(a, b int) int {\n\treturn a * b\n}",
		"Thought: Now I understand that main.go is using the Calculate function from the utils package, which is defined in math.go. The Calculate function simply adds two numbers and returns the result. Let me also check the content of strings.go to see if there are any other functions that might be relevant.\nAction: read_file\nAction Input: {\"filename\": \"/project/utils/strings.go\"}")
	
	mockLLM.AddResponse(
		"system: You are an AI assistant that can use tools to help answer questions.\n\nYou have access to the following tools:\n\nTool: read_file\nDescription: Read the content of a file\nParameters:\n  - filename: The name of the file to read\n\nTool: search_files\nDescription: Search for files in a directory\nParameters:\n  - directory: The directory to search in\n  - pattern: Optional pattern to filter files\n\nTool: analyze_code\nDescription: Analyze code structure and dependencies\nParameters:\n  - filename: The name of the file to analyze\n\nUse the following format:\n\nThought: you should always think about what to do\nAction: the action to take, should be one of [read_file, search_files, analyze_code]\nAction Input: the input to the action (in JSON format)\nObservation: the result of the action\n... (this Thought/Action/Action Input/Observation can repeat N times)\nThought: I now know the final answer\nFinal Answer: the final answer to the original input question\n\nuser: Explore the codebase in the /project directory and explain how the main.go file interacts with other components.\nassistant: Thought: I need to explore the codebase to understand how main.go interacts with other components. First, let me see what files are in the /project directory.\nAction: search_files\nAction Input: {\"directory\": \"/project\"}\nuser: Observation: [\n  \"main.go\",\n  \"go.mod\",\n  \"go.sum\",\n  \"README.md\"\n]\nassistant: Thought: Now I can see the files in the /project directory. Let me first look at the content of main.go to understand its structure and dependencies.\nAction: read_file\nAction Input: {\"filename\": \"/project/main.go\"}\nuser: Observation: package main\n\nimport (\n\t\"fmt\"\n\t\"github.com/example/project/utils\"\n)\n\nfunc main() {\n\tfmt.Println(\"Hello, World!\")\n\tresult := utils.Calculate(10, 20)\n\tfmt.Printf(\"Result: %d\\n\", result)\n}\nassistant: Thought: I can see that main.go imports a package called \"github.com/example/project/utils\" and uses a function called Calculate from it. Let me check if there's a utils directory in the project.\nAction: search_files\nAction Input: {\"directory\": \"/project/utils\"}\nuser: Observation: [\n  \"math.go\",\n  \"strings.go\"\n]\nassistant: Thought: Great, I found the utils directory with two files: math.go and strings.go. Since main.go is using a Calculate function, it's likely in the math.go file. Let me check the content of math.go.\nAction: read_file\nAction Input: {\"filename\": \"/project/utils/math.go\"}\nuser: Observation: package utils\n\n// Calculate adds two numbers and returns the result\nfunc Calculate(a, b int) int {\n\treturn a + b\n}\n\n// Multiply multiplies two numbers and returns the result\nfunc Multiply(a, b int) int {\n\treturn a * b\n}\nassistant: Thought: Now I understand that main.go is using the Calculate function from the utils package, which is defined in math.go. The Calculate function simply adds two numbers and returns the result. Let me also check the content of strings.go to see if there are any other functions that might be relevant.\nAction: read_file\nAction Input: {\"filename\": \"/project/utils/strings.go\"}\nuser: Observation: package utils\n\nimport \"strings\"\n\n// Concat concatenates two strings with a separator\nfunc Concat(a, b, sep string) string {\n\treturn a + sep + b\n}\n\n// ToUpper converts a string to uppercase\nfunc ToUpper(s string) string {\n\treturn strings.ToUpper(s)\n}",
		"Thought: Now I have a good understanding of the codebase. Let me use the analyze_code tool to get a more structured view of the main.go file and its dependencies.\nAction: analyze_code\nAction Input: {\"filename\": \"/project/main.go\"}")
	
	mockLLM.AddResponse(
		"system: You are an AI assistant that can use tools to help answer questions.\n\nYou have access to the following tools:\n\nTool: read_file\nDescription: Read the content of a file\nParameters:\n  - filename: The name of the file to read\n\nTool: search_files\nDescription: Search for files in a directory\nParameters:\n  - directory: The directory to search in\n  - pattern: Optional pattern to filter files\n\nTool: analyze_code\nDescription: Analyze code structure and dependencies\nParameters:\n  - filename: The name of the file to analyze\n\nUse the following format:\n\nThought: you should always think about what to do\nAction: the action to take, should be one of [read_file, search_files, analyze_code]\nAction Input: the input to the action (in JSON format)\nObservation: the result of the action\n... (this Thought/Action/Action Input/Observation can repeat N times)\nThought: I now know the final answer\nFinal Answer: the final answer to the original input question\n\nuser: Explore the codebase in the /project directory and explain how the main.go file interacts with other components.\nassistant: Thought: I need to explore the codebase to understand how main.go interacts with other components. First, let me see what files are in the /project directory.\nAction: search_files\nAction Input: {\"directory\": \"/project\"}\nuser: Observation: [\n  \"main.go\",\n  \"go.mod\",\n  \"go.sum\",\n  \"README.md\"\n]\nassistant: Thought: Now I can see the files in the /project directory. Let me first look at the content of main.go to understand its structure and dependencies.\nAction: read_file\nAction Input: {\"filename\": \"/project/main.go\"}\nuser: Observation: package main\n\nimport (\n\t\"fmt\"\n\t\"github.com/example/project/utils\"\n)\n\nfunc main() {\n\tfmt.Println(\"Hello, World!\")\n\tresult := utils.Calculate(10, 20)\n\tfmt.Printf(\"Result: %d\\n\", result)\n}\nassistant: Thought: I can see that main.go imports a package called \"github.com/example/project/utils\" and uses a function called Calculate from it. Let me check if there's a utils directory in the project.\nAction: search_files\nAction Input: {\"directory\": \"/project/utils\"}\nuser: Observation: [\n  \"math.go\",\n  \"strings.go\"\n]\nassistant: Thought: Great, I found the utils directory with two files: math.go and strings.go. Since main.go is using a Calculate function, it's likely in the math.go file. Let me check the content of math.go.\nAction: read_file\nAction Input: {\"filename\": \"/project/utils/math.go\"}\nuser: Observation: package utils\n\n// Calculate adds two numbers and returns the result\nfunc Calculate(a, b int) int {\n\treturn a + b\n}\n\n// Multiply multiplies two numbers and returns the result\nfunc Multiply(a, b int) int {\n\treturn a * b\n}\nassistant: Thought: Now I understand that main.go is using the Calculate function from the utils package, which is defined in math.go. The Calculate function simply adds two numbers and returns the result. Let me also check the content of strings.go to see if there are any other functions that might be relevant.\nAction: read_file\nAction Input: {\"filename\": \"/project/utils/strings.go\"}\nuser: Observation: package utils\n\nimport \"strings\"\n\n// Concat concatenates two strings with a separator\nfunc Concat(a, b, sep string) string {\n\treturn a + sep + b\n}\n\n// ToUpper converts a string to uppercase\nfunc ToUpper(s string) string {\n\treturn strings.ToUpper(s)\n}\nassistant: Thought: Now I have a good understanding of the codebase. Let me use the analyze_code tool to get a more structured view of the main.go file and its dependencies.\nAction: analyze_code\nAction Input: {\"filename\": \"/project/main.go\"}\nuser: Observation: {\n  \"functions\": [\n    \"main\"\n  ],\n  \"classes\": [],\n  \"imports\": [\n    \"fmt\",\n    \"github.com/example/project/utils\"\n  ],\n  \"dependencies\": {\n    \"utils\": \"github.com/example/project/utils\"\n  }\n}",
		"Thought: I now have a comprehensive understanding of how main.go interacts with other components in the codebase. Let me summarize my findings.\nFinal Answer: After exploring the codebase, I can explain how main.go interacts with other components:\n\n1. The main.go file is the entry point of the application and contains the main function that gets executed when the program runs.\n\n2. Main.go has two import dependencies:\n   - The standard library package \"fmt\" for printing to the console\n   - A custom package \"github.com/example/project/utils\" which contains utility functions\n\n3. The utils package is organized into two files:\n   - math.go: Contains mathematical utility functions\n   - strings.go: Contains string manipulation utility functions\n\n4. In the main function, main.go specifically interacts with the utils package by calling the Calculate function from math.go:\n   ```go\n   result := utils.Calculate(10, 20)\n   ```\n   This function takes two integers (10 and 20), adds them together, and returns the result.\n\n5. The Calculate function is defined in utils/math.go as:\n   ```go\n   func Calculate(a, b int) int {\n       return a + b\n   }\n   ```\n\n6. While main.go doesn't currently use any functions from utils/strings.go, those functions (Concat and ToUpper) are available for use if needed in the future.\n\nThe codebase follows a simple modular structure where utility functions are separated into their own package and organized by functionality (math vs. strings), promoting code reusability and maintainability. The main.go file serves as the orchestrator that imports and uses these utility functions to perform its tasks.")
}
