package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"time"

	"github.com/gorilla/mux"
	"github.com/ory/fosite"
)

// MockMCPServer implements a simple MCP server with mock tools
type MockMCPServer struct {
	oauth fosite.OAuth2Provider
	store *MemoryStore
}

// MCPTool represents a mock MCP tool
type MCPTool struct {
	Name        string                 `json:"name"`
	Description string                 `json:"description"`
	InputSchema map[string]interface{} `json:"inputSchema"`
}

// MCPToolResult represents the result of executing an MCP tool
type MCPToolResult struct {
	Content []MCPContent `json:"content"`
	IsError bool         `json:"isError,omitempty"`
}

// MCPContent represents MCP content
type MCPContent struct {
	Type string `json:"type"`
	Text string `json:"text"`
}

// MCPResource represents an MCP resource
type MCPResource struct {
	URI         string `json:"uri"`
	Name        string `json:"name"`
	Description string `json:"description"`
	MimeType    string `json:"mimeType,omitempty"`
}

// NewMockMCPServer creates a new mock MCP server
func NewMockMCPServer(oauth fosite.OAuth2Provider, store *MemoryStore) *MockMCPServer {
	return &MockMCPServer{
		oauth: oauth,
		store: store,
	}
}

// GetTools returns the list of available MCP tools
func (m *MockMCPServer) GetTools() []MCPTool {
	return []MCPTool{
		{
			Name:        "calculator",
			Description: "Perform basic arithmetic calculations",
			InputSchema: map[string]interface{}{
				"type": "object",
				"properties": map[string]interface{}{
					"operation": map[string]interface{}{
						"type":        "string",
						"description": "The arithmetic operation to perform",
						"enum":        []string{"add", "subtract", "multiply", "divide"},
					},
					"a": map[string]interface{}{
						"type":        "number",
						"description": "First number",
					},
					"b": map[string]interface{}{
						"type":        "number",
						"description": "Second number",
					},
				},
				"required": []string{"operation", "a", "b"},
			},
		},
		{
			Name:        "weather",
			Description: "Get weather information for a location",
			InputSchema: map[string]interface{}{
				"type": "object",
				"properties": map[string]interface{}{
					"location": map[string]interface{}{
						"type":        "string",
						"description": "The location to get weather for",
					},
					"units": map[string]interface{}{
						"type":        "string",
						"description": "Temperature units",
						"enum":        []string{"celsius", "fahrenheit"},
						"default":     "celsius",
					},
				},
				"required": []string{"location"},
			},
		},
		{
			Name:        "time",
			Description: "Get current time and date information",
			InputSchema: map[string]interface{}{
				"type": "object",
				"properties": map[string]interface{}{
					"timezone": map[string]interface{}{
						"type":        "string",
						"description": "Timezone (e.g., UTC, America/New_York)",
						"default":     "UTC",
					},
					"format": map[string]interface{}{
						"type":        "string",
						"description": "Time format",
						"enum":        []string{"iso", "unix", "human"},
						"default":     "iso",
					},
				},
			},
		},
	}
}

// GetResources returns the list of available MCP resources
func (m *MockMCPServer) GetResources() []MCPResource {
	return []MCPResource{
		{
			URI:         "mcp://server/config",
			Name:        "Server Configuration",
			Description: "Current server configuration and status",
			MimeType:    "application/json",
		},
		{
			URI:         "mcp://server/stats",
			Name:        "Server Statistics",
			Description: "Server usage statistics and metrics",
			MimeType:    "application/json",
		},
		{
			URI:         "mcp://oauth/clients",
			Name:        "OAuth Clients",
			Description: "List of registered OAuth clients",
			MimeType:    "application/json",
		},
	}
}

// ExecuteTool executes a mock MCP tool
func (m *MockMCPServer) ExecuteTool(toolName string, arguments map[string]interface{}) MCPToolResult {
	switch toolName {
	case "calculator":
		return m.executeCalculator(arguments)
	case "weather":
		return m.executeWeather(arguments)
	case "time":
		return m.executeTime(arguments)
	default:
		return MCPToolResult{
			Content: []MCPContent{{
				Type: "text",
				Text: fmt.Sprintf("Unknown tool: %s", toolName),
			}},
			IsError: true,
		}
	}
}

func (m *MockMCPServer) executeCalculator(args map[string]interface{}) MCPToolResult {
	operation, ok := args["operation"].(string)
	if !ok {
		return MCPToolResult{
			Content: []MCPContent{{Type: "text", Text: "Missing or invalid operation"}},
			IsError: true,
		}
	}

	a, aOk := args["a"].(float64)
	b, bOk := args["b"].(float64)
	if !aOk || !bOk {
		return MCPToolResult{
			Content: []MCPContent{{Type: "text", Text: "Missing or invalid numbers"}},
			IsError: true,
		}
	}

	var result float64
	var resultText string

	switch operation {
	case "add":
		result = a + b
		resultText = fmt.Sprintf("%.2f + %.2f = %.2f", a, b, result)
	case "subtract":
		result = a - b
		resultText = fmt.Sprintf("%.2f - %.2f = %.2f", a, b, result)
	case "multiply":
		result = a * b
		resultText = fmt.Sprintf("%.2f × %.2f = %.2f", a, b, result)
	case "divide":
		if b == 0 {
			return MCPToolResult{
				Content: []MCPContent{{Type: "text", Text: "Cannot divide by zero"}},
				IsError: true,
			}
		}
		result = a / b
		resultText = fmt.Sprintf("%.2f ÷ %.2f = %.2f", a, b, result)
	default:
		return MCPToolResult{
			Content: []MCPContent{{Type: "text", Text: "Invalid operation"}},
			IsError: true,
		}
	}

	return MCPToolResult{
		Content: []MCPContent{{
			Type: "text",
			Text: resultText,
		}},
	}
}

func (m *MockMCPServer) executeWeather(args map[string]interface{}) MCPToolResult {
	location, ok := args["location"].(string)
	if !ok {
		return MCPToolResult{
			Content: []MCPContent{{Type: "text", Text: "Missing location"}},
			IsError: true,
		}
	}

	units := "celsius"
	if u, ok := args["units"].(string); ok {
		units = u
	}

	// Mock weather data
	temp := 22.5
	if units == "fahrenheit" {
		temp = temp*9/5 + 32
	}

	tempUnit := "°C"
	if units == "fahrenheit" {
		tempUnit = "°F"
	}

	weatherText := fmt.Sprintf("Weather in %s:\n- Temperature: %.1f%s\n- Condition: Partly cloudy\n- Humidity: 65%%\n- Wind: 12 km/h", 
		location, temp, tempUnit)

	return MCPToolResult{
		Content: []MCPContent{{
			Type: "text",
			Text: weatherText,
		}},
	}
}

func (m *MockMCPServer) executeTime(args map[string]interface{}) MCPToolResult {
	timezone := "UTC"
	if tz, ok := args["timezone"].(string); ok {
		timezone = tz
	}

	format := "iso"
	if f, ok := args["format"].(string); ok {
		format = f
	}

	now := time.Now().UTC()
	var timeText string

	switch format {
	case "iso":
		timeText = fmt.Sprintf("Current time (%s): %s", timezone, now.Format(time.RFC3339))
	case "unix":
		timeText = fmt.Sprintf("Current time (%s): %d", timezone, now.Unix())
	case "human":
		timeText = fmt.Sprintf("Current time (%s): %s", timezone, now.Format("Monday, January 2, 2006 at 3:04 PM"))
	default:
		timeText = fmt.Sprintf("Current time (%s): %s", timezone, now.Format(time.RFC3339))
	}

	return MCPToolResult{
		Content: []MCPContent{{
			Type: "text",
			Text: timeText,
		}},
	}
}

// GetResource retrieves an MCP resource
func (m *MockMCPServer) GetResource(uri string) (interface{}, error) {
	switch uri {
	case "mcp://server/config":
		return map[string]interface{}{
			"server_name":    "OIDC MCP Server",
			"version":        "1.0.0",
			"oauth_enabled":  true,
			"mcp_enabled":    true,
			"transport":      "http",
			"port":           8080,
			"started_at":     time.Now().Format(time.RFC3339),
		}, nil
		case "mcp://server/stats":
			return map[string]interface{}{
				"total_requests":    42,
				"oauth_clients":     len(m.store.Clients),
				"active_sessions":   len(m.store.AuthCodes),
				"uptime_seconds":    3600,
				"tools_available":   len(m.GetTools()),
				"resources_available": len(m.GetResources()),
			}, nil
		case "mcp://oauth/clients":
			clientList := make([]map[string]interface{}, 0)
			for clientID, client := range m.store.Clients {
				clientList = append(clientList, map[string]interface{}{
					"client_id":     clientID,
					"client_name":   client.GetID(), // Use GetID instead of GetClientName
					"redirect_uris": client.GetRedirectURIs(),
					"grant_types":   client.GetGrantTypes(),
				})
			}
			return map[string]interface{}{
				"clients": clientList,
				"total":   len(clientList),
			}, nil
	default:
		return nil, fmt.Errorf("resource not found: %s", uri)
	}
}

// MCP HTTP Handlers

func (m *MockMCPServer) handleMCPTools(w http.ResponseWriter, r *http.Request) {
	// Check OAuth authentication
	_, _, err := m.oauth.IntrospectToken(r.Context(), fosite.AccessTokenFromRequest(r), fosite.AccessToken, nil)
	if err != nil {
		http.Error(w, "Unauthorized", http.StatusUnauthorized)
		return
	}

	switch r.Method {
	case "GET":
		// List tools
		tools := m.GetTools()
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]interface{}{
			"tools": tools,
		})
	case "POST":
		// Execute tool
		var request struct {
			Name      string                 `json:"name"`
			Arguments map[string]interface{} `json:"arguments"`
		}

		if err := json.NewDecoder(r.Body).Decode(&request); err != nil {
			http.Error(w, "Invalid request body", http.StatusBadRequest)
			return
		}

		result := m.ExecuteTool(request.Name, request.Arguments)
		w.Header().Set("Content-Type", "application/json")
		if result.IsError {
			w.WriteHeader(http.StatusBadRequest)
		}
		json.NewEncoder(w).Encode(result)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

func (m *MockMCPServer) handleMCPResources(w http.ResponseWriter, r *http.Request) {
	// Check OAuth authentication
	_, _, err := m.oauth.IntrospectToken(r.Context(), fosite.AccessTokenFromRequest(r), fosite.AccessToken, nil)
	if err != nil {
		http.Error(w, "Unauthorized", http.StatusUnauthorized)
		return
	}

	switch r.Method {
	case "GET":
		uri := r.URL.Query().Get("uri")
		if uri == "" {
			// List resources
			resources := m.GetResources()
			w.Header().Set("Content-Type", "application/json")
			json.NewEncoder(w).Encode(map[string]interface{}{
				"resources": resources,
			})
		} else {
			// Get specific resource
			resource, err := m.GetResource(uri)
			if err != nil {
				http.Error(w, err.Error(), http.StatusNotFound)
				return
			}
			w.Header().Set("Content-Type", "application/json")
			json.NewEncoder(w).Encode(map[string]interface{}{
				"contents": []map[string]interface{}{{
					"uri":      uri,
					"mimeType": "application/json",
					"text":     resource,
				}},
			})
		}
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

func (m *MockMCPServer) handleMCPPrompts(w http.ResponseWriter, r *http.Request) {
	// Check OAuth authentication
	_, _, err := m.oauth.IntrospectToken(r.Context(), fosite.AccessTokenFromRequest(r), fosite.AccessToken, nil)
	if err != nil {
		http.Error(w, "Unauthorized", http.StatusUnauthorized)
		return
	}

	// Mock prompts
	prompts := []map[string]interface{}{
		{
			"name":        "analyze-data",
			"description": "Analyze data and provide insights",
			"arguments": []map[string]interface{}{
				{
					"name":        "data",
					"description": "The data to analyze",
					"required":    true,
				},
				{
					"name":        "format",
					"description": "Output format",
					"required":    false,
				},
			},
		},
		{
			"name":        "generate-report",
			"description": "Generate a comprehensive report",
			"arguments": []map[string]interface{}{
				{
					"name":        "topic",
					"description": "Report topic",
					"required":    true,
				},
				{
					"name":        "length",
					"description": "Report length",
					"required":    false,
				},
			},
		},
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"prompts": prompts,
	})
}

// SetupMCPRoutes adds MCP routes to the router
func SetupMCPRoutes(router *mux.Router, mcpServer *MockMCPServer) {
	// MCP API endpoints
	router.HandleFunc("/mcp/tools", mcpServer.handleMCPTools).Methods("GET", "POST")
	router.HandleFunc("/mcp/resources", mcpServer.handleMCPResources).Methods("GET")
	router.HandleFunc("/mcp/prompts", mcpServer.handleMCPPrompts).Methods("GET")
	
	// MCP Server info endpoint
	router.HandleFunc("/mcp/info", func(w http.ResponseWriter, r *http.Request) {
		info := map[string]interface{}{
			"name":         "OIDC MCP Server",
			"version":      "1.0.0",
			"protocol":     "mcp/1.0",
			"capabilities": map[string]interface{}{
				"tools":     map[string]interface{}{"listChanged": true},
				"resources": map[string]interface{}{"subscribe": false, "listChanged": true},
				"prompts":   map[string]interface{}{"listChanged": true},
			},
			"oauth_protected": true,
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(info)
	}).Methods("GET")
}

