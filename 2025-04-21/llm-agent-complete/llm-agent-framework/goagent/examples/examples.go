// Package examples provides example implementations using the GoAgent framework
package examples

import (
	"context"
	"embed"
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"github.com/go-go-golems/geppetto/pkg/conversation"
	"github.com/goagent/framework/goagent/agent"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/memory"
	"github.com/goagent/framework/goagent/tools"
	"github.com/goagent/framework/goagent/types"
	"gopkg.in/yaml.v2"
)

//go:embed mock-conversations/*.yaml
var mockConversationsFS embed.FS

// YAMLConversation represents the structure of conversations in the YAML files
type YAMLConversation struct {
	Interactions []struct {
		Request  []map[string]interface{} `yaml:"request"`
		Response string                   `yaml:"response"`
	} `yaml:"interactions"`
}

// LoadYAMLConversation loads a conversation from a YAML file
func LoadYAMLConversation(filePath string) (*YAMLConversation, error) {
	data, err := mockConversationsFS.ReadFile(filePath)
	if err != nil {
		return nil, err
	}

	var conversation YAMLConversation
	err = yaml.Unmarshal(data, &conversation)
	if err != nil {
		return nil, err
	}

	return &conversation, nil
}

// ConvertToMessages converts YAML conversation to proper conversation.Message format
func ConvertToMessages(yamlConversation *YAMLConversation) ([][]*conversation.Message, []string, []string) {
	var allMessages [][]*conversation.Message
	var allResponses []string
	var allChunks []string

	for _, interaction := range yamlConversation.Interactions {
		// Each interaction contains a request and a response
		var messages []*conversation.Message

		// Process request to create messages
		for _, item := range interaction.Request {
			// Check for system message
			if systemMsg, ok := item["system"]; ok {
				if text, ok := systemMsg.(string); ok {
					msg := conversation.NewChatMessage(conversation.RoleSystem, text)
					messages = append(messages, msg)
				}
			}

			// Check for user message
			if userMsg, ok := item["user"]; ok {
				if text, ok := userMsg.(string); ok {
					msg := conversation.NewChatMessage(conversation.RoleUser, text)
					messages = append(messages, msg)
				}
			}

			// Check for assistant message
			if assistantMsg, ok := item["assistant"]; ok {
				if text, ok := assistantMsg.(string); ok {
					msg := conversation.NewChatMessage(conversation.RoleAssistant, text)
					messages = append(messages, msg)
				}
			}
		}

		// Add to collections
		allMessages = append(allMessages, messages)
		allResponses = append(allResponses, interaction.Response)

		// For chunks, we'll just split the response by lines for simplicity
		lines := strings.Split(interaction.Response, "\n")
		for _, chunk := range lines {
			if chunk != "" {
				allChunks = append(allChunks, chunk)
			}
		}
	}

	return allMessages, allResponses, allChunks
}

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
		Functions:    []string{"Calculate", "Multiply"},
		Classes:      []string{},
		Imports:      []string{},
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
	// Load conversation data from YAML
	conversationPath := "mock-conversations/research.yaml"
	yamlConversation, err := LoadYAMLConversation(conversationPath)
	if err != nil {
		// Fallback with empty responses if file can't be loaded
		systemMsg := conversation.NewChatMessage(conversation.RoleSystem, "You are a helpful research assistant.")
		userMsg := conversation.NewChatMessage(conversation.RoleUser, "Tell me about climate change.")
		messages := []*conversation.Message{systemMsg, userMsg}

		response := "I'll research that for you."
		chunks := []string{"I'll", "research", "that", "for", "you."}

		mockLLM.AddResponse(messages, response)
		mockLLM.AddStreamResponse(messages, chunks)
		return
	}

	// Convert YAML to message format
	allMessages, allResponses, allChunks := ConvertToMessages(yamlConversation)

	// Add responses for the research scenario
	for i, messages := range allMessages {
		if i < len(allResponses) {
			mockLLM.AddResponse(messages, allResponses[i])
		}
	}

	// Add streaming response using the first message
	if len(allMessages) > 0 && len(allChunks) > 0 {
		mockLLM.AddStreamResponse(allMessages[0], allChunks)
	}
}

func addTravelPlanningResponses(plannerLLM, executorLLM *llm.MockLLM) {
	// Load conversation data from YAML
	conversationPath := "mock-conversations/travel-planning.yaml"
	yamlConversation, err := LoadYAMLConversation(conversationPath)
	if err != nil {
		// Fallback with empty responses if file can't be loaded
		systemMsg := conversation.NewChatMessage(conversation.RoleSystem, "You are a helpful travel planner.")
		userMsg := conversation.NewChatMessage(conversation.RoleUser, "Plan a trip to Paris.")
		plannerMessages := []*conversation.Message{systemMsg, userMsg}

		executorUserMsg := conversation.NewChatMessage(conversation.RoleUser, "Book a flight to Paris.")
		executorMessages := []*conversation.Message{systemMsg, executorUserMsg}

		plannerResponse := "Here's a plan for your trip."
		executorResponse := "I've executed part of your travel plan."

		plannerLLM.AddResponse(plannerMessages, plannerResponse)
		executorLLM.AddResponse(executorMessages, executorResponse)
		return
	}

	// Convert YAML to message format
	allMessages, allResponses, _ := ConvertToMessages(yamlConversation)

	// Add responses for the travel planning scenario
	for i, messages := range allMessages {
		if i < len(allResponses) {
			// Even indices go to planner, odd to executor
			if i%2 == 0 {
				plannerLLM.AddResponse(messages, allResponses[i])
			} else {
				executorLLM.AddResponse(messages, allResponses[i])
			}
		}
	}
}

func addCodeExplorationResponses(mockLLM *llm.MockLLM) {
	// Load conversation data from YAML
	conversationPath := "mock-conversations/code-explorations.yaml"
	yamlConversation, err := LoadYAMLConversation(conversationPath)
	if err != nil {
		// Fallback with empty responses if file can't be loaded
		systemMsg := conversation.NewChatMessage(conversation.RoleSystem, "You are a helpful code explorer.")
		userMsg := conversation.NewChatMessage(conversation.RoleUser, "Explore this codebase.")
		messages := []*conversation.Message{systemMsg, userMsg}

		response := "I'll explore the code for you."

		mockLLM.AddResponse(messages, response)
		return
	}

	// Convert YAML to message format
	allMessages, allResponses, _ := ConvertToMessages(yamlConversation)

	// Add responses for the code exploration scenario
	for i, messages := range allMessages {
		if i < len(allResponses) {
			mockLLM.AddResponse(messages, allResponses[i])
		}
	}
}
