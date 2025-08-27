package main

import (
    "encoding/json"
    "fmt"
    "log"
    "math/rand"
    "net/http"
    "strconv"
    "strings"
    "time"

    "github.com/gorilla/mux"
)

// Responses API structures
type ResponsesCreateRequest struct {
	Model              string                 `json:"model"`
	Input              interface{}            `json:"input"`
	Instructions       string                 `json:"instructions,omitempty"`
	Tools              []Tool                 `json:"tools,omitempty"`
	Temperature        *float64               `json:"temperature,omitempty"`
	MaxOutputTokens    *int                   `json:"max_output_tokens,omitempty"`
	Stream             *bool                  `json:"stream,omitempty"`
	PreviousResponseID string                 `json:"previous_response_id,omitempty"`
	ResponseFormat     map[string]interface{} `json:"response_format,omitempty"`
}

type Tool struct {
	Type string `json:"type"`
}

type ResponsesResponse struct {
	ID      string         `json:"id"`
	Object  string         `json:"object"`
	Created int64          `json:"created"`
	Model   string         `json:"model"`
	Output  []OutputObject `json:"output"`
	Usage   Usage          `json:"usage"`
}

type OutputObject struct {
	ID      string          `json:"id"`
	Type    string          `json:"type"`
	Status  string          `json:"status,omitempty"`
	Content []ContentObject `json:"content,omitempty"`
}

type ContentObject struct {
	Type        string       `json:"type"`
	Text        string       `json:"text,omitempty"`
	Annotations []Annotation `json:"annotations,omitempty"`
}

type Annotation struct {
	Index *int   `json:"index"`
	Title string `json:"title"`
	Type  string `json:"type"`
	URL   string `json:"url,omitempty"`
}

type Usage struct {
	PromptTokens     int `json:"prompt_tokens"`
	CompletionTokens int `json:"completion_tokens"`
	TotalTokens      int `json:"total_tokens"`
}

// Streaming structures
type StreamEvent struct {
	Type  string      `json:"type"`
	Delta string      `json:"delta,omitempty"`
	Error interface{} `json:"error,omitempty"`
}

// In-memory storage for responses
var responseStore = make(map[string]*ResponsesResponse)
var conversationHistory = make(map[string][]string)

// Generate unique response ID
func generateResponseID() string {
	return fmt.Sprintf("resp_%d_%d", time.Now().Unix(), rand.Intn(10000))
}

// Generate unique message ID
func generateMessageID() string {
	return fmt.Sprintf("msg_%d_%d", time.Now().Unix(), rand.Intn(10000))
}

// Generate unique tool call ID
func generateToolCallID() string {
	return fmt.Sprintf("ws_%d_%d", time.Now().Unix(), rand.Intn(10000))
}

// Mock response generation based on input
func generateMockResponse(req *ResponsesCreateRequest) string {
	inputStr := ""
	
	// Handle different input types
	switch v := req.Input.(type) {
	case string:
		inputStr = v
	case []interface{}:
		// Handle array of messages (multimodal)
		for _, msg := range v {
			if msgMap, ok := msg.(map[string]interface{}); ok {
				if content, exists := msgMap["content"]; exists {
					if contentStr, ok := content.(string); ok {
						inputStr += contentStr + " "
					}
				}
			}
		}
	}

	inputLower := strings.ToLower(inputStr)
	
	// Generate contextual responses
	if strings.Contains(inputLower, "joke") {
		jokes := []string{
			"Why don't scientists trust atoms? Because they make up everything!",
			"Why did the scarecrow win an award? Because he was outstanding in his field!",
			"Why don't skeletons fight each other? They don't have the guts!",
			"What do you call a fake noodle? An impasta!",
		}
		return jokes[rand.Intn(len(jokes))]
	}
	
	if strings.Contains(inputLower, "weather") {
		return "I'm a mock API, so I can't provide real weather data. But I can tell you it's always sunny in the world of mock responses!"
	}
	
	if strings.Contains(inputLower, "news") || strings.Contains(inputLower, "latest") {
		return "Here are some mock news headlines: 'AI Continues to Advance', 'Mock APIs Prove Useful for Development', 'Developers Love Testing with Fake Data'."
	}
	
	if strings.Contains(inputLower, "hello") || strings.Contains(inputLower, "hi") {
		return "Hello! I'm a mock OpenAI Responses API. How can I help you today?"
	}
	
	// Default response
	return fmt.Sprintf("This is a mock response to your input: '%s'. The Responses API is working correctly!", inputStr)
}

// Generate mock web search results
func generateWebSearchResults() []OutputObject {
	toolCallID := generateToolCallID()
	messageID := generateMessageID()
	
	return []OutputObject{
		{
			ID:     toolCallID,
			Type:   "web_search_call",
			Status: "completed",
		},
		{
			ID:   messageID,
			Type: "message",
			Content: []ContentObject{
				{
					Type: "text",
					Text: "Based on my web search, here are the latest developments: Mock search results show that AI technology continues to advance rapidly. Recent breakthroughs include improved language models and better integration capabilities.",
					Annotations: []Annotation{
						{
							Index: nil,
							Title: "AI Technology Advances in 2025",
							Type:  "url_citation",
							URL:   "https://example.com/ai-advances-2025",
						},
						{
							Index: nil,
							Title: "Language Model Improvements",
							Type:  "url_citation", 
							URL:   "https://example.com/language-models",
						},
					},
				},
			},
		},
	}
}

// Generate mock file search results
func generateFileSearchResults() []OutputObject {
	toolCallID := generateToolCallID()
	messageID := generateMessageID()
	
	return []OutputObject{
		{
			ID:     toolCallID,
			Type:   "file_search_call",
			Status: "completed",
		},
		{
			ID:   messageID,
			Type: "message",
			Content: []ContentObject{
				{
					Type: "text",
					Text: "Based on the uploaded documents, I found relevant information about your query. The documents contain detailed specifications and examples that match your request.",
					Annotations: []Annotation{
						{
							Index: nil,
							Title: "Document Section 3.2",
							Type:  "file_citation",
						},
						{
							Index: nil,
							Title: "Appendix A - Examples",
							Type:  "file_citation",
						},
					},
				},
			},
		},
	}
}

// Handle responses creation
func handleResponsesCreate(w http.ResponseWriter, r *http.Request) {
    var req ResponsesCreateRequest
    if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
        http.Error(w, "Invalid JSON", http.StatusBadRequest)
        return
    }

    // Check if streaming is requested
    if req.Stream != nil && *req.Stream {
        handleStreamingResponse(w, r, &req)
        return
    }

    // Resolve via configuration first
    resolved, errOut := resolveResponsesContent(&req)
    if errOut != nil {
        w.Header().Set("Content-Type", "application/json")
        w.WriteHeader(errOut.Status)
        json.NewEncoder(w).Encode(map[string]interface{}{
            "error": map[string]interface{}{
                "message": errOut.Message,
                "code":    errOut.Code,
            },
        })
        return
    }

    // Generate response ID
    responseID := generateResponseID()
	
	// Build conversation history
	var fullContext string
	if req.PreviousResponseID != "" {
		if history, exists := conversationHistory[req.PreviousResponseID]; exists {
			fullContext = strings.Join(history, "\n") + "\n"
		}
	}
	
	// Add current input to context
	inputStr := ""
	switch v := req.Input.(type) {
	case string:
		inputStr = v
	case []interface{}:
		for _, msg := range v {
			if msgMap, ok := msg.(map[string]interface{}); ok {
				if content, exists := msgMap["content"]; exists {
					if contentStr, ok := content.(string); ok {
						inputStr += contentStr + " "
					}
				}
			}
		}
	}
	fullContext += inputStr

    // Generate output (config-aware or legacy)
    var output []OutputObject
	
	// Check for tools
    if resolved != nil {
        // Use resolved tools + message
        output = append(output, resolved.PrefixTools...)
        messageID := generateMessageID()
        msg := OutputObject{ID: messageID, Type: "message", Content: []ContentObject{{Type: "text", Text: resolved.Text}}}
        if len(resolved.Annotations) > 0 {
            msg.Content[0].Annotations = resolved.Annotations
        }
        output = append(output, msg)
    } else {
        // Legacy path based on requested tools
        hasWebSearch := false
        hasFileSearch := false
        for _, tool := range req.Tools {
            if tool.Type == "web_search" || tool.Type == "web_search_preview" {
                hasWebSearch = true
            }
            if tool.Type == "file_search" {
                hasFileSearch = true
            }
        }
        if hasWebSearch {
            output = generateWebSearchResults()
        } else if hasFileSearch {
            output = generateFileSearchResults()
        } else {
            responseText := generateMockResponse(&req)
            messageID := generateMessageID()
            output = []OutputObject{{
                ID:   messageID,
                Type: "message",
                Content: []ContentObject{{Type: "text", Text: responseText}},
            }}
        }
    }

	// Create response
	response := &ResponsesResponse{
		ID:      responseID,
		Object:  "response",
		Created: time.Now().Unix(),
		Model:   req.Model,
		Output:  output,
		Usage: Usage{
			PromptTokens:     len(strings.Fields(fullContext)),
			CompletionTokens: 50,
			TotalTokens:      len(strings.Fields(fullContext)) + 50,
		},
	}

	// Store response and update conversation history
	responseStore[responseID] = response
	conversationHistory[responseID] = append(conversationHistory[req.PreviousResponseID], inputStr, response.Output[len(response.Output)-1].Content[0].Text)

    w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(response)
}

// Handle streaming responses
func handleStreamingResponse(w http.ResponseWriter, r *http.Request, req *ResponsesCreateRequest) {
    w.Header().Set("Content-Type", "text/event-stream")
    w.Header().Set("Cache-Control", "no-cache")
    w.Header().Set("Connection", "keep-alive")
    origin := "*"
    if botConfig != nil && botConfig.Server.CORS != "" { origin = botConfig.Server.CORS }
    w.Header().Set("Access-Control-Allow-Origin", origin)

	flusher, ok := w.(http.Flusher)
	if !ok {
		http.Error(w, "Streaming unsupported", http.StatusInternalServerError)
		return
	}

    // Resolve via configuration (fallback to legacy)
    resolved, _ := resolveResponsesContent(req)
    responseText := ""
    if resolved != nil && resolved.Text != "" {
        responseText = resolved.Text
    } else {
        responseText = generateMockResponse(req)
    }
    words := strings.Fields(responseText)

	// Stream response word by word
    // Determine delay
    delayMs := 200
    if botConfig != nil && botConfig.Streaming.ChunkDelayMs != nil {
        delayMs = *botConfig.Streaming.ChunkDelayMs
    }
    for i, word := range words {
        event := StreamEvent{
            Type:  "response.output_text.delta",
            Delta: word,
        }
		
		if i < len(words)-1 {
			event.Delta += " "
		}

		eventData, _ := json.Marshal(event)
		fmt.Fprintf(w, "data: %s\n\n", eventData)
		flusher.Flush()

        // Add delay for demonstration (configurable)
        time.Sleep(time.Duration(delayMs) * time.Millisecond)
    }

	// Send completion event
	completionEvent := StreamEvent{
		Type: "response.done",
	}
	eventData, _ := json.Marshal(completionEvent)
	fmt.Fprintf(w, "data: %s\n\n", eventData)
	flusher.Flush()
}

// Handle response retrieval
func handleResponsesRetrieve(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	responseID := vars["response_id"]

	response, exists := responseStore[responseID]
	if !exists {
		http.Error(w, "Response not found", http.StatusNotFound)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

// Handle responses listing
func handleResponsesList(w http.ResponseWriter, r *http.Request) {
	// Parse query parameters
	limitStr := r.URL.Query().Get("limit")
	limit := 20 // default
	if limitStr != "" {
		if l, err := strconv.Atoi(limitStr); err == nil {
			limit = l
		}
	}

	// Get all responses (in a real implementation, you'd paginate properly)
	var responses []*ResponsesResponse
	count := 0
	for _, response := range responseStore {
		if count >= limit {
			break
		}
		responses = append(responses, response)
		count++
	}

	result := map[string]interface{}{
		"object": "list",
		"data":   responses,
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(result)
}

// Setup Responses API routes
func setupResponsesRoutes(router *mux.Router) {
	// Responses API endpoints
	router.HandleFunc("/v1/responses", handleResponsesCreate).Methods("POST")
	router.HandleFunc("/v1/responses", handleResponsesList).Methods("GET")
	router.HandleFunc("/v1/responses/{response_id}", handleResponsesRetrieve).Methods("GET")
	
	log.Println("Responses API routes configured:")
	log.Println("  POST /v1/responses - Create response")
	log.Println("  GET /v1/responses - List responses")
	log.Println("  GET /v1/responses/{response_id} - Retrieve response")
}

// Configuration-driven resolver for Responses API
type ResolvedResponse struct {
    Text        string
    PrefixTools []OutputObject
    Annotations []Annotation
}

func resolveResponsesContent(req *ResponsesCreateRequest) (*ResolvedResponse, *ErrorOut) {
    if botConfig == nil {
        return nil, nil
    }
    // Build context strings
    inputStr := ""
    switch v := req.Input.(type) {
    case string:
        inputStr = v
    case []interface{}:
        for _, msg := range v {
            if msgMap, ok := msg.(map[string]interface{}); ok {
                if content, exists := msgMap["content"]; exists {
                    switch c := content.(type) {
                    case string:
                        inputStr += c + " "
                    case []interface{}:
                        // ignore non-text for matching
                        _ = c
                    }
                }
            }
        }
    }
    lastUser := strings.TrimSpace(inputStr)
    full := lastUser

    mr := evaluateRules("responses", req.Model, "", lastUser, full)
    if mr == nil {
        // Fallback
        if botConfig.Fallback.Text != "" || botConfig.Fallback.Message.Text != "" {
            txt := pickText(botConfig.Fallback)
            ctx := buildTemplateContext(req.Model, lastUser, full)
            return &ResolvedResponse{Text: renderTemplate(txt, ctx)}, nil
        }
        return nil, nil
    }
    // error injection
    if mr.Rule.Respond.Error != nil {
        return nil, mr.Rule.Respond.Error
    }
    // Build response
    res := &ResolvedResponse{}
    ctx := buildTemplateContext(req.Model, lastUser, full)

    // Build tools from registry
    accumulatedText := ""
    var accumulatedAnn []Annotation
    for _, name := range mr.Rule.Respond.UseTools {
        if !isToolEnabled(name) { continue }
        if def, ok := getToolDef(name); ok {
            // tool call
            res.PrefixTools = append(res.PrefixTools, OutputObject{ID: generateToolCallID(), Type: def.CallType, Status: def.Status})
            // default message from tool
            if def.Message != nil {
                txt := renderTemplate(def.Message.Text, ctx)
                if txt != "" {
                    if accumulatedText != "" { accumulatedText += "\n" }
                    accumulatedText += txt
                }
                for _, a := range def.Message.Annotations {
                    accumulatedAnn = append(accumulatedAnn, Annotation{Index: nil, Title: a.Title, Type: a.Type, URL: a.URL})
                }
            }
        }
    }

    // Explicit tool calls still supported
    for _, t := range mr.Rule.Respond.Tools {
        res.PrefixTools = append(res.PrefixTools, OutputObject{ID: generateToolCallID(), Type: t.Type, Status: t.Status})
    }

    // Message text precedence: rule.message.text > rule.text/choose > accumulated tool text
    ruleChosen := pickText(mr.Rule.Respond)
    if mr.Rule.Respond.Message.Text != "" {
        res.Text = renderTemplate(mr.Rule.Respond.Message.Text, ctx)
    } else if ruleChosen != "" {
        res.Text = renderTemplate(ruleChosen, ctx)
    } else {
        res.Text = accumulatedText
    }

    // Annotations combine tool defaults + rule.message.annotations
    res.Annotations = append(res.Annotations, accumulatedAnn...)
    if len(mr.Rule.Respond.Message.Annotations) > 0 {
        for _, a := range mr.Rule.Respond.Message.Annotations {
            res.Annotations = append(res.Annotations, Annotation{Index: nil, Title: a.Title, Type: a.Type, URL: a.URL})
        }
    }
    return res, nil
}
