package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"sync"
	"time"

	"github.com/gorilla/mux"
	"github.com/ory/fosite"
	"github.com/rs/zerolog"
)

// SimpleMCPServer provides MCP functionality with OAuth protection and SSE support
type SimpleMCPServer struct {
	oauth  fosite.OAuth2Provider
	store  *MemoryStore
	logger zerolog.Logger
	mu     sync.RWMutex
}

// NewSimpleMCPServer creates a new MCP server with OAuth and SSE support
func NewSimpleMCPServer(oauth fosite.OAuth2Provider, store *MemoryStore, logger zerolog.Logger) *SimpleMCPServer {
	return &SimpleMCPServer{
		oauth:  oauth,
		store:  store,
		logger: logger,
	}
}

// MCPToolDefinition represents an MCP tool definition
type MCPToolDefinition struct {
	Name        string                 `json:"name"`
	Description string                 `json:"description"`
	InputSchema map[string]interface{} `json:"inputSchema"`
}

// MCPResourceDefinition represents an MCP resource definition
type MCPResourceDefinition struct {
	URI         string `json:"uri"`
	Name        string `json:"name"`
	Description string `json:"description"`
	MimeType    string `json:"mimeType,omitempty"`
}

// MCPPromptDefinition represents an MCP prompt definition
type MCPPromptDefinition struct {
	Name        string                   `json:"name"`
	Description string                   `json:"description"`
	Arguments   []map[string]interface{} `json:"arguments,omitempty"`
}

// GetMCPTools returns available MCP tools
func (s *SimpleMCPServer) GetMCPTools() []MCPToolDefinition {
	return []MCPToolDefinition{
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

// GetMCPResources returns available MCP resources
func (s *SimpleMCPServer) GetMCPResources() []MCPResourceDefinition {
	return []MCPResourceDefinition{
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

// GetMCPPrompts returns available MCP prompts
func (s *SimpleMCPServer) GetMCPPrompts() []MCPPromptDefinition {
	return []MCPPromptDefinition{
		{
			Name:        "analyze-data",
			Description: "Analyze data and provide insights",
			Arguments: []map[string]interface{}{
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
			Name:        "generate-report",
			Description: "Generate a comprehensive report",
			Arguments: []map[string]interface{}{
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
}

// ExecuteTool executes an MCP tool
func (s *SimpleMCPServer) ExecuteTool(toolName string, arguments map[string]interface{}) map[string]interface{} {
	switch toolName {
	case "calculator":
		return s.executeCalculator(arguments)
	case "weather":
		return s.executeWeather(arguments)
	case "time":
		return s.executeTime(arguments)
	default:
		return map[string]interface{}{
			"content": []map[string]interface{}{{
				"type": "text",
				"text": fmt.Sprintf("Unknown tool: %s", toolName),
			}},
			"isError": true,
		}
	}
}

func (s *SimpleMCPServer) executeCalculator(args map[string]interface{}) map[string]interface{} {
	operation, ok := args["operation"].(string)
	if !ok {
		return map[string]interface{}{
			"content": []map[string]interface{}{{
				"type": "text",
				"text": "Missing or invalid operation",
			}},
			"isError": true,
		}
	}

	a, aOk := args["a"].(float64)
	b, bOk := args["b"].(float64)
	if !aOk || !bOk {
		return map[string]interface{}{
			"content": []map[string]interface{}{{
				"type": "text",
				"text": "Missing or invalid numbers",
			}},
			"isError": true,
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
			return map[string]interface{}{
				"content": []map[string]interface{}{{
					"type": "text",
					"text": "Cannot divide by zero",
				}},
				"isError": true,
			}
		}
		result = a / b
		resultText = fmt.Sprintf("%.2f ÷ %.2f = %.2f", a, b, result)
	default:
		return map[string]interface{}{
			"content": []map[string]interface{}{{
				"type": "text",
				"text": "Invalid operation",
			}},
			"isError": true,
		}
	}

	return map[string]interface{}{
		"content": []map[string]interface{}{{
			"type": "text",
			"text": resultText,
		}},
	}
}

func (s *SimpleMCPServer) executeWeather(args map[string]interface{}) map[string]interface{} {
	location, ok := args["location"].(string)
	if !ok {
		return map[string]interface{}{
			"content": []map[string]interface{}{{
				"type": "text",
				"text": "Missing location",
			}},
			"isError": true,
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

	return map[string]interface{}{
		"content": []map[string]interface{}{{
			"type": "text",
			"text": weatherText,
		}},
	}
}

func (s *SimpleMCPServer) executeTime(args map[string]interface{}) map[string]interface{} {
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

	return map[string]interface{}{
		"content": []map[string]interface{}{{
			"type": "text",
			"text": timeText,
		}},
	}
}

// GetResource retrieves an MCP resource
func (s *SimpleMCPServer) GetResource(uri string) (interface{}, error) {
	switch uri {
	case "mcp://server/config":
		return map[string]interface{}{
			"server_name":    "OIDC MCP Server",
			"version":        "1.0.0",
			"oauth_enabled":  true,
			"mcp_enabled":    true,
			"transports":     []string{"http", "sse"},
			"port":           8080,
			"started_at":     time.Now().Format(time.RFC3339),
		}, nil
	case "mcp://server/stats":
		return map[string]interface{}{
			"total_requests":      42,
			"oauth_clients":       len(s.store.Clients),
			"active_sessions":     len(s.store.AuthCodes),
			"uptime_seconds":      3600,
			"tools_available":     len(s.GetMCPTools()),
			"resources_available": len(s.GetMCPResources()),
			"prompts_available":   len(s.GetMCPPrompts()),
		}, nil
	case "mcp://oauth/clients":
		clientList := make([]map[string]interface{}, 0)
		for clientID, client := range s.store.Clients {
			clientList = append(clientList, map[string]interface{}{
				"client_id":     clientID,
				"client_name":   client.GetID(),
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

// checkOAuthToken validates OAuth token from request
func (s *SimpleMCPServer) checkOAuthToken(r *http.Request) error {
	_, _, err := s.oauth.IntrospectToken(r.Context(), fosite.AccessTokenFromRequest(r), fosite.AccessToken, nil)
	return err
}

// HTTP Handlers for MCP endpoints

func (s *SimpleMCPServer) handleMCPInfo(w http.ResponseWriter, r *http.Request) {
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
		"transports":      []string{"http", "sse"},
	}
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(info)
}

func (s *SimpleMCPServer) handleMCPTools(w http.ResponseWriter, r *http.Request) {
	// Check OAuth authentication for protected endpoints
	if r.Header.Get("Authorization") != "" {
		if err := s.checkOAuthToken(r); err != nil {
			http.Error(w, "Unauthorized", http.StatusUnauthorized)
			return
		}
	}

	switch r.Method {
	case "GET":
		tools := s.GetMCPTools()
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]interface{}{
			"tools": tools,
		})
	case "POST":
		// Require OAuth for tool execution
		if err := s.checkOAuthToken(r); err != nil {
			http.Error(w, "Unauthorized", http.StatusUnauthorized)
			return
		}

		var request struct {
			Name      string                 `json:"name"`
			Arguments map[string]interface{} `json:"arguments"`
		}

		if err := json.NewDecoder(r.Body).Decode(&request); err != nil {
			http.Error(w, "Invalid request body", http.StatusBadRequest)
			return
		}

		result := s.ExecuteTool(request.Name, request.Arguments)
		w.Header().Set("Content-Type", "application/json")
		if isError, ok := result["isError"].(bool); ok && isError {
			w.WriteHeader(http.StatusBadRequest)
		}
		json.NewEncoder(w).Encode(result)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

func (s *SimpleMCPServer) handleMCPResources(w http.ResponseWriter, r *http.Request) {
	// Check OAuth authentication for protected endpoints
	if r.Header.Get("Authorization") != "" {
		if err := s.checkOAuthToken(r); err != nil {
			http.Error(w, "Unauthorized", http.StatusUnauthorized)
			return
		}
	}

	switch r.Method {
	case "GET":
		uri := r.URL.Query().Get("uri")
		if uri == "" {
			// List resources
			resources := s.GetMCPResources()
			w.Header().Set("Content-Type", "application/json")
			json.NewEncoder(w).Encode(map[string]interface{}{
				"resources": resources,
			})
		} else {
			// Get specific resource - require OAuth
			if err := s.checkOAuthToken(r); err != nil {
				http.Error(w, "Unauthorized", http.StatusUnauthorized)
				return
			}

			resource, err := s.GetResource(uri)
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

func (s *SimpleMCPServer) handleMCPPrompts(w http.ResponseWriter, r *http.Request) {
	// Check OAuth authentication for protected endpoints
	if r.Header.Get("Authorization") != "" {
		if err := s.checkOAuthToken(r); err != nil {
			http.Error(w, "Unauthorized", http.StatusUnauthorized)
			return
		}
	}

	prompts := s.GetMCPPrompts()
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"prompts": prompts,
	})
}

// SSE endpoint for MCP
func (s *SimpleMCPServer) handleMCPSSE(w http.ResponseWriter, r *http.Request) {
	// For SSE, we can optionally check OAuth via query parameter or header
	if token := r.URL.Query().Get("access_token"); token != "" {
		// Create a temporary request with the token in the Authorization header
		tempReq := r.Clone(r.Context())
		tempReq.Header.Set("Authorization", "Bearer "+token)
		if err := s.checkOAuthToken(tempReq); err != nil {
			http.Error(w, "Unauthorized", http.StatusUnauthorized)
			return
		}
	} else if r.Header.Get("Authorization") != "" {
		if err := s.checkOAuthToken(r); err != nil {
			http.Error(w, "Unauthorized", http.StatusUnauthorized)
			return
		}
	}

	// Set SSE headers
	w.Header().Set("Content-Type", "text/event-stream")
	w.Header().Set("Cache-Control", "no-cache")
	w.Header().Set("Connection", "keep-alive")
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Headers", "Cache-Control")

	// Create a simple SSE connection
	flusher, ok := w.(http.Flusher)
	if !ok {
		http.Error(w, "Streaming unsupported", http.StatusInternalServerError)
		return
	}

	// Send initial connection message
	fmt.Fprintf(w, "data: %s\n\n", `{"type":"connection","status":"connected","server":"OIDC MCP Server"}`)
	flusher.Flush()

	// Send server info
	info := map[string]interface{}{
		"type":         "server_info",
		"name":         "OIDC MCP Server",
		"version":      "1.0.0",
		"protocol":     "mcp/1.0",
		"capabilities": map[string]interface{}{
			"tools":     map[string]interface{}{"listChanged": true},
			"resources": map[string]interface{}{"subscribe": false, "listChanged": true},
			"prompts":   map[string]interface{}{"listChanged": true},
		},
		"oauth_protected": true,
		"transports":      []string{"http", "sse"},
	}
	infoData, _ := json.Marshal(info)
	fmt.Fprintf(w, "data: %s\n\n", string(infoData))
	flusher.Flush()

	// Send available tools
	tools := s.GetMCPTools()
	toolsData, _ := json.Marshal(map[string]interface{}{
		"type":  "tools_list",
		"tools": tools,
	})
	fmt.Fprintf(w, "data: %s\n\n", string(toolsData))
	flusher.Flush()

	// Send available resources
	resources := s.GetMCPResources()
	resourcesData, _ := json.Marshal(map[string]interface{}{
		"type":      "resources_list",
		"resources": resources,
	})
	fmt.Fprintf(w, "data: %s\n\n", string(resourcesData))
	flusher.Flush()

	// Send available prompts
	prompts := s.GetMCPPrompts()
	promptsData, _ := json.Marshal(map[string]interface{}{
		"type":    "prompts_list",
		"prompts": prompts,
	})
	fmt.Fprintf(w, "data: %s\n\n", string(promptsData))
	flusher.Flush()

	// Keep connection alive and handle client disconnect
	ctx := r.Context()
	ticker := time.NewTicker(30 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			// Send heartbeat
			fmt.Fprintf(w, "data: %s\n\n", `{"type":"heartbeat","timestamp":"`+time.Now().Format(time.RFC3339)+`"}`)
			flusher.Flush()
		}
	}
}

// SetupMCPRoutes adds MCP routes to the router
func (s *SimpleMCPServer) SetupMCPRoutes(router *mux.Router) {
	// HTTP MCP endpoints
	router.HandleFunc("/mcp/info", s.handleMCPInfo).Methods("GET")
	router.HandleFunc("/mcp/tools", s.handleMCPTools).Methods("GET", "POST")
	router.HandleFunc("/mcp/resources", s.handleMCPResources).Methods("GET")
	router.HandleFunc("/mcp/prompts", s.handleMCPPrompts).Methods("GET")

	// SSE MCP endpoint
	router.HandleFunc("/mcp/sse", s.handleMCPSSE).Methods("GET")
}

