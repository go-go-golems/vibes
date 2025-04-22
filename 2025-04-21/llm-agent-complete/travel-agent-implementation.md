# Travel Agent Implementation with Event-Driven Architecture

This document demonstrates how a travel planning agent would be implemented using the event-driven architecture. It includes pseudocode, timing diagrams, control flow graphs, and explanations of how the components interact through events.

## Table of Contents

1. [Overview](#overview)
2. [Component Architecture](#component-architecture)
3. [Event Types](#event-types)
4. [Implementation](#implementation)
   - [Travel Agent](#travel-agent)
   - [Travel Tools](#travel-tools)
   - [LLM Integration](#llm-integration)
   - [Memory System](#memory-system)
5. [Control Flow](#control-flow)
6. [Timing Diagrams](#timing-diagrams)
7. [Example Execution](#example-execution)
8. [Advanced Features](#advanced-features)

## Overview

The travel agent helps users plan trips by:
- Understanding travel requirements
- Searching for flights and accommodations
- Recommending activities and attractions
- Creating itineraries
- Providing cost estimates

In our event-driven implementation, all these functions are orchestrated through events flowing through a central event queue. Components never call each other directly; they only emit and react to events.

## Component Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Event Queue                              │
└─────────────────────────────────────────────────────────────────┘
     ▲                 ▲                 ▲                 ▲
     │                 │                 │                 │
     ▼                 ▼                 ▼                 ▼
┌──────────┐     ┌──────────┐     ┌──────────┐     ┌──────────┐
│  Travel  │     │  Flight  │     │  Hotel   │     │ Activity │
│  Agent   │     │  Tool    │     │  Tool    │     │  Tool    │
└──────────┘     └──────────┘     └──────────┘     └──────────┘
     ▲                 ▲                 ▲                 ▲
     │                 │                 │                 │
     ▼                 ▼                 ▼                 ▼
┌─────────────────────────────────────────────────────────────────┐
│                        Event Queue                              │
└─────────────────────────────────────────────────────────────────┘
     ▲                 ▲                 ▲                 ▲
     │                 │                 │                 │
     ▼                 ▼                 ▼                 ▼
┌──────────┐     ┌──────────┐     ┌──────────┐     ┌──────────┐
│   LLM    │     │  Memory  │     │ Weather  │     │ Currency │
│ Service  │     │  System  │     │  Tool    │     │  Tool    │
└──────────┘     └──────────┘     └──────────┘     └──────────┘
```

## Event Types

The travel agent system uses the following event types:

### Agent Events
- `TravelAgentStartEvent`: Initiates a new travel planning session
- `TravelAgentThinkingEvent`: Represents agent reasoning steps
- `TravelAgentPlanEvent`: Represents a travel plan being created
- `TravelAgentCompleteEvent`: Signals completion of travel planning

### Tool Events
- `FlightSearchEvent`: Requests flight search
- `FlightSearchResultEvent`: Returns flight search results
- `HotelSearchEvent`: Requests hotel search
- `HotelSearchResultEvent`: Returns hotel search results
- `ActivitySearchEvent`: Requests activity search
- `ActivitySearchResultEvent`: Returns activity search results
- `WeatherCheckEvent`: Requests weather information
- `WeatherCheckResultEvent`: Returns weather information
- `CurrencyConversionEvent`: Requests currency conversion
- `CurrencyConversionResultEvent`: Returns currency conversion results

### LLM Events
- `LLMRequestEvent`: Requests LLM processing
- `LLMResponseEvent`: Returns LLM response

### Memory Events
- `MemoryStoreEvent`: Stores information in memory
- `MemoryRetrieveEvent`: Retrieves information from memory
- `MemoryRetrieveResultEvent`: Returns retrieved memory

## Implementation

### Travel Agent

```go
// TravelAgent handles travel planning
type TravelAgent struct {
    id           string
    eventQueue   *EventQueue
    sessions     map[string]*TravelSession
    mutex        sync.RWMutex
}

// TravelSession represents a travel planning session
type TravelSession struct {
    id              string
    userPreferences TravelPreferences
    destinations    []Destination
    flights         []Flight
    hotels          []Hotel
    activities      []Activity
    itinerary       []ItineraryItem
    currentStep     string
    messages        []Message
    state           string // "planning", "searching_flights", "searching_hotels", etc.
}

// TravelPreferences contains user preferences
type TravelPreferences struct {
    StartDate       time.Time
    EndDate         time.Time
    Budget          float64
    Travelers       int
    PreferredAirlines []string
    HotelStars      int
    ActivityTypes   []string
}

// NewTravelAgent creates a new travel agent
func NewTravelAgent(eventQueue *EventQueue) *TravelAgent {
    agent := &TravelAgent{
        id:         uuid.New().String(),
        eventQueue: eventQueue,
        sessions:   make(map[string]*TravelSession),
    }
    
    // Register event handlers
    eventQueue.RegisterHandler(EventTypeTravelAgentStart, agent.handleStart)
    eventQueue.RegisterHandler(EventTypeLLMResponse, agent.handleLLMResponse)
    eventQueue.RegisterHandler(EventTypeFlightSearchResult, agent.handleFlightResult)
    eventQueue.RegisterHandler(EventTypeHotelSearchResult, agent.handleHotelResult)
    eventQueue.RegisterHandler(EventTypeActivitySearchResult, agent.handleActivityResult)
    eventQueue.RegisterHandler(EventTypeWeatherCheckResult, agent.handleWeatherResult)
    eventQueue.RegisterHandler(EventTypeMemoryRetrieveResult, agent.handleMemoryResult)
    
    return agent
}

// StartPlanning begins a new travel planning session
func (a *TravelAgent) StartPlanning(userInput string) string {
    sessionID := uuid.New().String()
    
    // Create new session
    session := &TravelSession{
        id:          sessionID,
        messages:    []Message{{Role: "user", Content: userInput}},
        state:       "planning",
        currentStep: "understand_requirements",
    }
    
    a.mutex.Lock()
    a.sessions[sessionID] = session
    a.mutex.Unlock()
    
    // Emit start event
    a.eventQueue.Enqueue(NewTravelAgentStartEvent(a.id, sessionID, userInput))
    
    return sessionID
}

// handleStart processes a start event
func (a *TravelAgent) handleStart(event Event) error {
    startEvent, ok := event.Payload().(TravelAgentStart)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    // Emit thinking event
    a.eventQueue.Enqueue(NewTravelAgentThinkingEvent(
        a.id, 
        startEvent.SessionID, 
        event.ID(), 
        "Understanding travel requirements",
    ))
    
    // Request LLM to understand requirements
    a.mutex.RLock()
    session := a.sessions[startEvent.SessionID]
    messages := session.messages
    a.mutex.RUnlock()
    
    // Emit LLM request event
    a.eventQueue.Enqueue(NewLLMRequestEvent(
        a.id,
        startEvent.SessionID,
        messages,
        "extract_travel_requirements",
    ))
    
    return nil
}

// handleLLMResponse processes an LLM response event
func (a *TravelAgent) handleLLMResponse(event Event) error {
    responseEvent, ok := event.Payload().(LLMResponse)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    a.mutex.Lock()
    session, ok := a.sessions[responseEvent.SessionID]
    if !ok {
        a.mutex.Unlock()
        return fmt.Errorf("session not found")
    }
    
    // Add response to messages
    session.messages = append(session.messages, Message{
        Role:    "assistant",
        Content: responseEvent.Content,
    })
    
    // Process based on current state
    switch session.state {
    case "planning":
        if session.currentStep == "understand_requirements" {
            // Extract preferences from LLM response
            preferences, err := extractPreferences(responseEvent.Content)
            if err == nil {
                session.userPreferences = preferences
                session.currentStep = "search_flights"
                session.state = "searching_flights"
                
                // Store preferences in memory
                a.eventQueue.Enqueue(NewMemoryStoreEvent(
                    a.id,
                    responseEvent.SessionID,
                    "preferences",
                    preferences,
                ))
                
                // Emit flight search event
                a.eventQueue.Enqueue(NewFlightSearchEvent(
                    a.id,
                    responseEvent.SessionID,
                    preferences.StartDate,
                    preferences.EndDate,
                    preferences.Travelers,
                    preferences.PreferredAirlines,
                ))
            }
        }
    
    case "creating_itinerary":
        // Process itinerary creation
        itinerary, err := extractItinerary(responseEvent.Content)
        if err == nil {
            session.itinerary = itinerary
            session.state = "complete"
            
            // Emit completion event
            a.eventQueue.Enqueue(NewTravelAgentCompleteEvent(
                a.id,
                responseEvent.SessionID,
                event.ID(),
                formatItinerary(itinerary),
            ))
        }
    
    // Handle other states...
    }
    
    a.mutex.Unlock()
    return nil
}

// handleFlightResult processes a flight search result event
func (a *TravelAgent) handleFlightResult(event Event) error {
    resultEvent, ok := event.Payload().(FlightSearchResult)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    a.mutex.Lock()
    session, ok := a.sessions[resultEvent.SessionID]
    if !ok {
        a.mutex.Unlock()
        return fmt.Errorf("session not found")
    }
    
    // Store flight results
    session.flights = resultEvent.Flights
    
    // Move to next step
    session.currentStep = "search_hotels"
    session.state = "searching_hotels"
    
    // Store flights in memory
    a.eventQueue.Enqueue(NewMemoryStoreEvent(
        a.id,
        resultEvent.SessionID,
        "flights",
        resultEvent.Flights,
    ))
    
    // Emit hotel search event
    a.eventQueue.Enqueue(NewHotelSearchEvent(
        a.id,
        resultEvent.SessionID,
        session.userPreferences.StartDate,
        session.userPreferences.EndDate,
        session.userPreferences.Travelers,
        session.userPreferences.HotelStars,
    ))
    
    a.mutex.Unlock()
    return nil
}

// handleHotelResult processes a hotel search result event
func (a *TravelAgent) handleHotelResult(event Event) error {
    resultEvent, ok := event.Payload().(HotelSearchResult)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    a.mutex.Lock()
    session, ok := a.sessions[resultEvent.SessionID]
    if !ok {
        a.mutex.Unlock()
        return fmt.Errorf("session not found")
    }
    
    // Store hotel results
    session.hotels = resultEvent.Hotels
    
    // Move to next step
    session.currentStep = "search_activities"
    session.state = "searching_activities"
    
    // Store hotels in memory
    a.eventQueue.Enqueue(NewMemoryStoreEvent(
        a.id,
        resultEvent.SessionID,
        "hotels",
        resultEvent.Hotels,
    ))
    
    // Emit activity search event
    a.eventQueue.Enqueue(NewActivitySearchEvent(
        a.id,
        resultEvent.SessionID,
        session.userPreferences.StartDate,
        session.userPreferences.EndDate,
        session.userPreferences.ActivityTypes,
    ))
    
    a.mutex.Unlock()
    return nil
}

// handleActivityResult processes an activity search result event
func (a *TravelAgent) handleActivityResult(event Event) error {
    resultEvent, ok := event.Payload().(ActivitySearchResult)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    a.mutex.Lock()
    session, ok := a.sessions[resultEvent.SessionID]
    if !ok {
        a.mutex.Unlock()
        return fmt.Errorf("session not found")
    }
    
    // Store activity results
    session.activities = resultEvent.Activities
    
    // Move to next step
    session.currentStep = "create_itinerary"
    session.state = "creating_itinerary"
    
    // Store activities in memory
    a.eventQueue.Enqueue(NewMemoryStoreEvent(
        a.id,
        resultEvent.SessionID,
        "activities",
        resultEvent.Activities,
    ))
    
    // Retrieve all data from memory
    a.eventQueue.Enqueue(NewMemoryRetrieveEvent(
        a.id,
        resultEvent.SessionID,
        []string{"preferences", "flights", "hotels", "activities"},
    ))
    
    a.mutex.Unlock()
    return nil
}

// handleMemoryResult processes a memory retrieval result event
func (a *TravelAgent) handleMemoryResult(event Event) error {
    resultEvent, ok := event.Payload().(MemoryRetrieveResult)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    a.mutex.RLock()
    session, ok := a.sessions[resultEvent.SessionID]
    if !ok {
        a.mutex.RUnlock()
        return fmt.Errorf("session not found")
    }
    
    // If we're creating an itinerary and have all the data
    if session.state == "creating_itinerary" && 
       resultEvent.Data["preferences"] != nil &&
       resultEvent.Data["flights"] != nil &&
       resultEvent.Data["hotels"] != nil &&
       resultEvent.Data["activities"] != nil {
        
        // Create prompt for LLM to generate itinerary
        prompt := createItineraryPrompt(
            resultEvent.Data["preferences"].(TravelPreferences),
            resultEvent.Data["flights"].([]Flight),
            resultEvent.Data["hotels"].([]Hotel),
            resultEvent.Data["activities"].([]Activity),
        )
        
        messages := append(session.messages, Message{
            Role:    "user",
            Content: prompt,
        })
        
        a.mutex.RUnlock()
        
        // Emit LLM request event
        a.eventQueue.Enqueue(NewLLMRequestEvent(
            a.id,
            resultEvent.SessionID,
            messages,
            "create_itinerary",
        ))
        
        return nil
    }
    
    a.mutex.RUnlock()
    return nil
}

// Helper functions
func extractPreferences(content string) (TravelPreferences, error) {
    // Parse LLM response to extract preferences
    // In a real implementation, this would use regex or structured output parsing
    return TravelPreferences{
        StartDate:        time.Now().AddDate(0, 1, 0),
        EndDate:          time.Now().AddDate(0, 1, 7),
        Budget:           1500.0,
        Travelers:        2,
        PreferredAirlines: []string{"Delta", "United"},
        HotelStars:       4,
        ActivityTypes:    []string{"sightseeing", "food", "adventure"},
    }, nil
}

func extractItinerary(content string) ([]ItineraryItem, error) {
    // Parse LLM response to extract itinerary
    // In a real implementation, this would use regex or structured output parsing
    return []ItineraryItem{
        {Day: 1, Time: "09:00", Activity: "Flight departure", Description: "Delta Airlines DL123"},
        {Day: 1, Time: "14:00", Activity: "Hotel check-in", Description: "Grand Hotel"},
        {Day: 1, Time: "18:00", Activity: "Dinner", Description: "Local restaurant"},
        // More items...
    }, nil
}

func formatItinerary(itinerary []ItineraryItem) string {
    // Format itinerary for presentation
    var sb strings.Builder
    sb.WriteString("Your Travel Itinerary:\n\n")
    
    currentDay := 0
    for _, item := range itinerary {
        if item.Day != currentDay {
            currentDay = item.Day
            sb.WriteString(fmt.Sprintf("Day %d:\n", currentDay))
        }
        
        sb.WriteString(fmt.Sprintf("  %s - %s: %s\n", 
            item.Time, item.Activity, item.Description))
    }
    
    return sb.String()
}

func createItineraryPrompt(preferences TravelPreferences, flights []Flight, 
                          hotels []Hotel, activities []Activity) string {
    // Create a prompt for the LLM to generate an itinerary
    return fmt.Sprintf(`Create a detailed travel itinerary based on the following:
    
Travel dates: %s to %s
Number of travelers: %d
Budget: $%.2f
    
Selected flight:
%s

Selected hotel:
%s

Available activities:
%s

Please create a day-by-day itinerary that includes all transportation, 
accommodation, and activities. Format each item as "Time - Activity: Description".`,
        preferences.StartDate.Format("2006-01-02"),
        preferences.EndDate.Format("2006-01-02"),
        preferences.Travelers,
        preferences.Budget,
        formatFlights(flights),
        formatHotels(hotels),
        formatActivities(activities),
    )
}
```

### Travel Tools

```go
// FlightTool searches for flights
type FlightTool struct {
    id           string
    eventQueue   *EventQueue
    apiClient    *FlightAPIClient
}

// NewFlightTool creates a new flight tool
func NewFlightTool(eventQueue *EventQueue, apiClient *FlightAPIClient) *FlightTool {
    tool := &FlightTool{
        id:        uuid.New().String(),
        eventQueue: eventQueue,
        apiClient: apiClient,
    }
    
    // Register event handler
    eventQueue.RegisterHandler(EventTypeFlightSearch, tool.handleFlightSearch)
    
    return tool
}

// handleFlightSearch processes a flight search event
func (t *FlightTool) handleFlightSearch(event Event) error {
    searchEvent, ok := event.Payload().(FlightSearch)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    // In a real implementation, this would call an actual flight API
    // For this example, we'll simulate a flight search
    flights := t.simulateFlightSearch(
        searchEvent.StartDate,
        searchEvent.EndDate,
        searchEvent.Travelers,
        searchEvent.PreferredAirlines,
    )
    
    // Emit flight search result event
    t.eventQueue.Enqueue(NewFlightSearchResultEvent(
        searchEvent.AgentID,
        searchEvent.SessionID,
        event.ID(),
        flights,
    ))
    
    return nil
}

// simulateFlightSearch simulates a flight search
func (t *FlightTool) simulateFlightSearch(startDate, endDate time.Time, 
                                         travelers int, 
                                         preferredAirlines []string) []Flight {
    // Simulate flight search
    return []Flight{
        {
            Airline:     "Delta",
            FlightNumber: "DL123",
            DepartureTime: startDate.Add(9 * time.Hour),
            ArrivalTime:  startDate.Add(14 * time.Hour),
            Price:        450.0,
            Class:        "Economy",
        },
        {
            Airline:     "United",
            FlightNumber: "UA456",
            DepartureTime: startDate.Add(10 * time.Hour),
            ArrivalTime:  startDate.Add(15 * time.Hour),
            Price:        425.0,
            Class:        "Economy",
        },
        // More flights...
    }
}

// HotelTool searches for hotels
type HotelTool struct {
    id           string
    eventQueue   *EventQueue
    apiClient    *HotelAPIClient
}

// NewHotelTool creates a new hotel tool
func NewHotelTool(eventQueue *EventQueue, apiClient *HotelAPIClient) *HotelTool {
    tool := &HotelTool{
        id:        uuid.New().String(),
        eventQueue: eventQueue,
        apiClient: apiClient,
    }
    
    // Register event handler
    eventQueue.RegisterHandler(EventTypeHotelSearch, tool.handleHotelSearch)
    
    return tool
}

// handleHotelSearch processes a hotel search event
func (t *HotelTool) handleHotelSearch(event Event) error {
    searchEvent, ok := event.Payload().(HotelSearch)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    // In a real implementation, this would call an actual hotel API
    // For this example, we'll simulate a hotel search
    hotels := t.simulateHotelSearch(
        searchEvent.StartDate,
        searchEvent.EndDate,
        searchEvent.Travelers,
        searchEvent.Stars,
    )
    
    // Emit hotel search result event
    t.eventQueue.Enqueue(NewHotelSearchResultEvent(
        searchEvent.AgentID,
        searchEvent.SessionID,
        event.ID(),
        hotels,
    ))
    
    return nil
}

// simulateHotelSearch simulates a hotel search
func (t *HotelTool) simulateHotelSearch(startDate, endDate time.Time, 
                                       travelers, stars int) []Hotel {
    // Simulate hotel search
    return []Hotel{
        {
            Name:     "Grand Hotel",
            Address:  "123 Main St",
            Stars:    4,
            Price:    150.0,
            Amenities: []string{"Pool", "Spa", "Restaurant"},
        },
        {
            Name:     "Luxury Resort",
            Address:  "456 Beach Rd",
            Stars:    5,
            Price:    250.0,
            Amenities: []string{"Pool", "Spa", "Restaurant", "Beach Access"},
        },
        // More hotels...
    }
}

// ActivityTool searches for activities
type ActivityTool struct {
    id           string
    eventQueue   *EventQueue
    apiClient    *ActivityAPIClient
}

// NewActivityTool creates a new activity tool
func NewActivityTool(eventQueue *EventQueue, apiClient *ActivityAPIClient) *ActivityTool {
    tool := &ActivityTool{
        id:        uuid.New().String(),
        eventQueue: eventQueue,
        apiClient: apiClient,
    }
    
    // Register event handler
    eventQueue.RegisterHandler(EventTypeActivitySearch, tool.handleActivitySearch)
    
    return tool
}

// handleActivitySearch processes an activity search event
func (t *ActivityTool) handleActivitySearch(event Event) error {
    searchEvent, ok := event.Payload().(ActivitySearch)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    // In a real implementation, this would call an actual activity API
    // For this example, we'll simulate an activity search
    activities := t.simulateActivitySearch(
        searchEvent.StartDate,
        searchEvent.EndDate,
        searchEvent.Types,
    )
    
    // Emit activity search result event
    t.eventQueue.Enqueue(NewActivitySearchResultEvent(
        searchEvent.AgentID,
        searchEvent.SessionID,
        event.ID(),
        activities,
    ))
    
    return nil
}

// simulateActivitySearch simulates an activity search
func (t *ActivityTool) simulateActivitySearch(startDate, endDate time.Time, 
                                            types []string) []Activity {
    // Simulate activity search
    return []Activity{
        {
            Name:        "City Tour",
            Description: "Guided tour of the city's main attractions",
            Duration:    3.0,
            Price:       45.0,
            Type:        "sightseeing",
        },
        {
            Name:        "Food Tour",
            Description: "Sample local cuisine at multiple restaurants",
            Duration:    4.0,
            Price:       65.0,
            Type:        "food",
        },
        {
            Name:        "Hiking Excursion",
            Description: "Guided hike through scenic trails",
            Duration:    5.0,
            Price:       35.0,
            Type:        "adventure",
        },
        // More activities...
    }
}
```

### LLM Integration

```go
// LLMService handles LLM interactions
type LLMService struct {
    id           string
    eventQueue   *EventQueue
    provider     LLMProvider
}

// LLMProvider represents an LLM API provider
type LLMProvider interface {
    Generate(messages []Message, purpose string) (string, error)
}

// NewLLMService creates a new LLM service
func NewLLMService(eventQueue *EventQueue, provider LLMProvider) *LLMService {
    service := &LLMService{
        id:        uuid.New().String(),
        eventQueue: eventQueue,
        provider:  provider,
    }
    
    // Register event handler
    eventQueue.RegisterHandler(EventTypeLLMRequest, service.handleLLMRequest)
    
    return service
}

// handleLLMRequest processes an LLM request event
func (s *LLMService) handleLLMRequest(event Event) error {
    requestEvent, ok := event.Payload().(LLMRequest)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    // Call LLM provider
    response, err := s.provider.Generate(requestEvent.Messages, requestEvent.Purpose)
    if err != nil {
        // Emit error event
        s.eventQueue.Enqueue(NewErrorEvent(
            "llm_error",
            requestEvent.AgentID,
            requestEvent.SessionID,
            fmt.Sprintf("LLM error: %v", err),
        ))
        return nil
    }
    
    // Emit LLM response event
    s.eventQueue.Enqueue(NewLLMResponseEvent(
        requestEvent.AgentID,
        requestEvent.SessionID,
        event.ID(),
        response,
        requestEvent.Purpose,
    ))
    
    return nil
}

// MockLLMProvider is a mock LLM provider for testing
type MockLLMProvider struct {
    responses map[string]string
}

// NewMockLLMProvider creates a new mock LLM provider
func NewMockLLMProvider() *MockLLMProvider {
    return &MockLLMProvider{
        responses: map[string]string{
            "extract_travel_requirements": `I'll help you plan your trip. Based on your message, here are your preferences:
- Destination: Paris, France
- Travel dates: June 15-22, 2025
- Budget: $3000
- 2 travelers
- Prefer 4-star hotels
- Interested in sightseeing, food tours, and museums`,
            
            "create_itinerary": `Here's your itinerary:

Day 1:
  09:00 - Flight departure: Delta Airlines DL123
  14:00 - Arrival in Paris
  15:30 - Hotel check-in: Grand Hotel
  18:00 - Dinner: Local bistro near hotel

Day 2:
  09:00 - Breakfast at hotel
  10:00 - Eiffel Tower visit
  13:00 - Lunch at Café de Paris
  15:00 - Louvre Museum
  19:00 - Seine River dinner cruise

Day 3:
  09:00 - Breakfast at hotel
  10:00 - Montmartre walking tour
  13:00 - Lunch at Le Consulat
  15:00 - Sacré-Cœur Basilica
  18:00 - Dinner and show at Moulin Rouge`,
        },
    }
}

// Generate generates a response for the given messages and purpose
func (p *MockLLMProvider) Generate(messages []Message, purpose string) (string, error) {
    // Check if we have a predefined response for this purpose
    if response, ok := p.responses[purpose]; ok {
        return response, nil
    }
    
    // Default response
    return "I'm not sure how to respond to that.", nil
}
```

### Memory System

```go
// MemorySystem stores and retrieves information
type MemorySystem struct {
    id           string
    eventQueue   *EventQueue
    storage      map[string]map[string]interface{}
    mutex        sync.RWMutex
}

// NewMemorySystem creates a new memory system
func NewMemorySystem(eventQueue *EventQueue) *MemorySystem {
    system := &MemorySystem{
        id:        uuid.New().String(),
        eventQueue: eventQueue,
        storage:   make(map[string]map[string]interface{}),
    }
    
    // Register event handlers
    eventQueue.RegisterHandler(EventTypeMemoryStore, system.handleMemoryStore)
    eventQueue.RegisterHandler(EventTypeMemoryRetrieve, system.handleMemoryRetrieve)
    
    return system
}

// handleMemoryStore processes a memory store event
func (s *MemorySystem) handleMemoryStore(event Event) error {
    storeEvent, ok := event.Payload().(MemoryStore)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    s.mutex.Lock()
    defer s.mutex.Unlock()
    
    // Initialize session storage if needed
    if _, ok := s.storage[storeEvent.SessionID]; !ok {
        s.storage[storeEvent.SessionID] = make(map[string]interface{})
    }
    
    // Store data
    s.storage[storeEvent.SessionID][storeEvent.Key] = storeEvent.Value
    
    return nil
}

// handleMemoryRetrieve processes a memory retrieve event
func (s *MemorySystem) handleMemoryRetrieve(event Event) error {
    retrieveEvent, ok := event.Payload().(MemoryRetrieve)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    s.mutex.RLock()
    defer s.mutex.RUnlock()
    
    // Check if session exists
    sessionStorage, ok := s.storage[retrieveEvent.SessionID]
    if !ok {
        // Emit empty result
        s.eventQueue.Enqueue(NewMemoryRetrieveResultEvent(
            retrieveEvent.AgentID,
            retrieveEvent.SessionID,
            event.ID(),
            make(map[string]interface{}),
        ))
        return nil
    }
    
    // Retrieve requested keys
    result := make(map[string]interface{})
    for _, key := range retrieveEvent.Keys {
        if value, ok := sessionStorage[key]; ok {
            result[key] = value
        }
    }
    
    // Emit result event
    s.eventQueue.Enqueue(NewMemoryRetrieveResultEvent(
        retrieveEvent.AgentID,
        retrieveEvent.SessionID,
        event.ID(),
        result,
    ))
    
    return nil
}
```

## Control Flow

The control flow of the travel agent is entirely event-driven. Here's a diagram showing the flow of events:

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│ User Input  │────>│ Agent Start │────>│   Agent     │
│             │     │   Event     │     │  Thinking   │
└─────────────┘     └─────────────┘     └─────────────┘
                                              │
                                              ▼
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│    LLM      │<────│    LLM      │<────│    LLM      │
│  Response   │     │   Request   │     │   Request   │
│   Event     │     │    Event    │     │    Event    │
└─────────────┘     └─────────────┘     └─────────────┘
      │
      ▼
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Extract   │────>│   Memory    │────>│   Flight    │
│ Preferences │     │    Store    │     │   Search    │
│             │     │    Event    │     │    Event    │
└─────────────┘     └─────────────┘     └─────────────┘
                                              │
                                              ▼
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Memory    │<────│   Flight    │<────│   Flight    │
│    Store    │     │   Search    │     │    Tool     │
│    Event    │     │   Result    │     │             │
└─────────────┘     └─────────────┘     └─────────────┘
      │
      ▼
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│    Hotel    │────>│    Hotel    │────>│   Memory    │
│   Search    │     │   Search    │     │    Store    │
│    Event    │     │   Result    │     │    Event    │
└─────────────┘     └─────────────┘     └─────────────┘
                                              │
                                              ▼
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│  Activity   │────>│  Activity   │────>│   Memory    │
│   Search    │     │   Search    │     │    Store    │
│    Event    │     │   Result    │     │    Event    │
└─────────────┘     └─────────────┘     └─────────────┘
                                              │
                                              ▼
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Memory    │────>│   Memory    │────>│    LLM      │
│  Retrieve   │     │  Retrieve   │     │   Request   │
│    Event    │     │   Result    │     │    Event    │
└─────────────┘     └─────────────┘     └─────────────┘
                                              │
                                              ▼
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│    LLM      │────>│   Create    │────>│    Agent    │
│  Response   │     │  Itinerary  │     │  Complete   │
│   Event     │     │             │     │    Event    │
└─────────────┘     └─────────────┘     └─────────────┘
```

## Timing Diagrams

The following timing diagram shows the sequence of events and their processing times:

```
Time (ms) ─────────────────────────────────────────────────────────────────────────>
           0        100       200       300       400       500       600       700
Agent      ──┬───────────────────────────────────────────────────────────────────
             │Start
             │        │Thinking
             │        │        │LLM Request
             │        │        │                │Process LLM Response
             │        │        │                │        │Flight Search
             │        │        │                │        │                │Process Flight Result
             │        │        │                │        │                │        │Hotel Search
             └────────┴────────┴────────────────┴────────┴────────────────┴────────┴─────────

LLM         ────────────┬───────────────────────────────────────────────────────────────────
                        │Process Request
                        │        │Generate Response
                        │        │        │Emit Response Event
                        │        │        │                                        │Process Request
                        └────────┴────────┴────────────────────────────────────────┴─────────

Flight Tool ────────────────────────────────────┬───────────────────────────────────────────
                                                │Process Search Event
                                                │        │Search Flights
                                                │        │        │Emit Result Event
                                                └────────┴────────┴─────────────────────────

Hotel Tool  ────────────────────────────────────────────────────────┬───────────────────────
                                                                    │Process Search Event
                                                                    │        │Search Hotels
                                                                    │        │        │Emit Result Event
                                                                    └────────┴────────┴─────

Activity    ────────────────────────────────────────────────────────────────────┬───────────
Tool                                                                            │Process Search Event
                                                                                │        │Search Activities
                                                                                │        │        │Emit Result
                                                                                └────────┴────────┴─────

Memory      ────────────────────────────────────────────┬───────────────────────────────────
System                                                  │Store Preferences
                                                        │                │Store Flights
                                                        │                │        │Store Hotels
                                                        │                │        │        │Store Activities
                                                        │                │        │        │        │Retrieve All
                                                        └────────────────┴────────┴────────┴────────┴─────
```

## Example Execution

Here's a step-by-step example of how the travel agent processes a user request:

1. **User Input**: "I want to plan a trip to Paris for me and my wife from June 15-22, 2025. We have a budget of $3000 and prefer 4-star hotels. We're interested in sightseeing, food tours, and museums."

2. **Agent Start**:
   - The travel agent creates a new session
   - Emits `TravelAgentStartEvent`
   - Handler processes the event and emits `TravelAgentThinkingEvent`
   - Handler emits `LLMRequestEvent` to understand requirements

3. **LLM Processing**:
   - LLM service processes the request
   - Generates a response extracting travel preferences
   - Emits `LLMResponseEvent`

4. **Extract Preferences**:
   - Agent processes the LLM response
   - Extracts travel preferences
   - Stores preferences in memory via `MemoryStoreEvent`
   - Emits `FlightSearchEvent`

5. **Flight Search**:
   - Flight tool processes the search event
   - Searches for flights
   - Emits `FlightSearchResultEvent`

6. **Process Flight Results**:
   - Agent processes the flight results
   - Stores flights in memory via `MemoryStoreEvent`
   - Emits `HotelSearchEvent`

7. **Hotel Search**:
   - Hotel tool processes the search event
   - Searches for hotels
   - Emits `HotelSearchResultEvent`

8. **Process Hotel Results**:
   - Agent processes the hotel results
   - Stores hotels in memory via `MemoryStoreEvent`
   - Emits `ActivitySearchEvent`

9. **Activity Search**:
   - Activity tool processes the search event
   - Searches for activities
   - Emits `ActivitySearchResultEvent`

10. **Process Activity Results**:
    - Agent processes the activity results
    - Stores activities in memory via `MemoryStoreEvent`
    - Emits `MemoryRetrieveEvent` to get all data

11. **Memory Retrieval**:
    - Memory system processes the retrieve event
    - Retrieves all stored data
    - Emits `MemoryRetrieveResultEvent`

12. **Create Itinerary**:
    - Agent processes the memory retrieval result
    - Creates a prompt for the LLM to generate an itinerary
    - Emits `LLMRequestEvent`

13. **LLM Processing**:
    - LLM service processes the request
    - Generates an itinerary
    - Emits `LLMResponseEvent`

14. **Complete Planning**:
    - Agent processes the LLM response
    - Extracts the itinerary
    - Emits `TravelAgentCompleteEvent` with the final itinerary

## Advanced Features

### Parallel Processing

The event-driven architecture allows for parallel processing of events. For example, the agent could emit multiple search events simultaneously:

```go
// Emit search events in parallel
a.eventQueue.Enqueue(NewFlightSearchEvent(...))
a.eventQueue.Enqueue(NewHotelSearchEvent(...))
a.eventQueue.Enqueue(NewActivitySearchEvent(...))
```

### Error Handling

Error handling is managed through error events:

```go
// Error event handler
func (a *TravelAgent) handleError(event Event) error {
    errorEvent, ok := event.Payload().(ErrorEvent)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    a.mutex.Lock()
    session, ok := a.sessions[errorEvent.SessionID]
    if !ok {
        a.mutex.Unlock()
        return fmt.Errorf("session not found")
    }
    
    // Log error
    log.Printf("Error in session %s: %s", errorEvent.SessionID, errorEvent.Message)
    
    // Update session state
    session.state = "error"
    
    // Notify user
    a.eventQueue.Enqueue(NewTravelAgentCompleteEvent(
        a.id,
        errorEvent.SessionID,
        event.ID(),
        fmt.Sprintf("Error: %s", errorEvent.Message),
    ))
    
    a.mutex.Unlock()
    return nil
}
```

### User Interaction

The event-driven architecture can easily accommodate user interaction through additional events:

```go
// UserInputEvent represents user input during planning
type UserInputEvent struct {
    AgentID   string
    SessionID string
    Input     string
}

// Handle user input
func (a *TravelAgent) handleUserInput(event Event) error {
    inputEvent, ok := event.Payload().(UserInputEvent)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    a.mutex.Lock()
    session, ok := a.sessions[inputEvent.SessionID]
    if !ok {
        a.mutex.Unlock()
        return fmt.Errorf("session not found")
    }
    
    // Add user input to messages
    session.messages = append(session.messages, Message{
        Role:    "user",
        Content: inputEvent.Input,
    })
    
    // Process based on current state
    // ...
    
    a.mutex.Unlock()
    return nil
}
```

### Cancellation

The event-driven architecture supports cancellation through cancel events:

```go
// CancelEvent represents a request to cancel planning
type CancelEvent struct {
    AgentID   string
    SessionID string
    Reason    string
}

// Handle cancellation
func (a *TravelAgent) handleCancel(event Event) error {
    cancelEvent, ok := event.Payload().(CancelEvent)
    if !ok {
        return fmt.Errorf("invalid event payload")
    }
    
    a.mutex.Lock()
    session, ok := a.sessions[cancelEvent.SessionID]
    if !ok {
        a.mutex.Unlock()
        return fmt.Errorf("session not found")
    }
    
    // Update session state
    session.state = "cancelled"
    
    // Notify user
    a.eventQueue.Enqueue(NewTravelAgentCompleteEvent(
        a.id,
        cancelEvent.SessionID,
        event.ID(),
        fmt.Sprintf("Planning cancelled: %s", cancelEvent.Reason),
    ))
    
    a.mutex.Unlock()
    return nil
}
```

This implementation demonstrates how a travel agent can be built using an event-driven architecture. All components communicate exclusively through events, creating a highly decoupled system that is easy to extend and maintain.
