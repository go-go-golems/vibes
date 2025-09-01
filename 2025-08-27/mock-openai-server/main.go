package main

import (
	"encoding/json"
	"fmt"
	"log"
	"math/rand"
	"net/http"
	"strings"
	"time"

	"github.com/gorilla/mux"
    docpkg "mock-openai-server/pkg/doc"
)

// Chat Completions API structures (existing)
type ChatCompletionRequest struct {
	Model       string    `json:"model"`
	Messages    []Message `json:"messages"`
	Temperature *float64  `json:"temperature,omitempty"`
	MaxTokens   *int      `json:"max_tokens,omitempty"`
	Stream      *bool     `json:"stream,omitempty"`
}

type Message struct {
	Role    string `json:"role"`
	Content string `json:"content"`
}

type ChatCompletionResponse struct {
	ID      string   `json:"id"`
	Object  string   `json:"object"`
	Created int64    `json:"created"`
	Model   string   `json:"model"`
	Choices []Choice `json:"choices"`
	Usage   Usage    `json:"usage"`
}

type Choice struct {
	Index        int     `json:"index"`
	Message      Message `json:"message"`
	FinishReason string  `json:"finish_reason"`
	Delta        *Delta  `json:"delta,omitempty"`
}

type Delta struct {
	Role    string `json:"role,omitempty"`
	Content string `json:"content,omitempty"`
}

type StreamChunk struct {
	ID      string   `json:"id"`
	Object  string   `json:"object"`
	Created int64    `json:"created"`
	Model   string   `json:"model"`
	Choices []Choice `json:"choices"`
}

// Models API structures
type Model struct {
	ID      string `json:"id"`
	Object  string `json:"object"`
	Created int64  `json:"created"`
	OwnedBy string `json:"owned_by"`
}

type ModelsResponse struct {
	Object string  `json:"object"`
	Data   []Model `json:"data"`
}

// CORS middleware
func corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		origin := "*"
		if botConfig != nil && botConfig.Server.CORS != "" { origin = botConfig.Server.CORS }
		w.Header().Set("Access-Control-Allow-Origin", origin)
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")

		if r.Method == "OPTIONS" {
			w.WriteHeader(http.StatusOK)
			return
		}

		next.ServeHTTP(w, r)
	})
}

// Generate mock response based on conversation context
func generateChatResponse(messages []Message) string {
	if len(messages) == 0 {
		return "Hello! How can I help you today?"
	}

	lastMessage := messages[len(messages)-1].Content
	lastMessageLower := strings.ToLower(lastMessage)

	// Context-aware responses
	if strings.Contains(lastMessageLower, "joke") {
		jokes := []string{
			"Why don't scientists trust atoms? Because they make up everything!",
			"Why did the scarecrow win an award? Because he was outstanding in his field!",
			"What do you call a fake noodle? An impasta!",
			"Why don't skeletons fight each other? They don't have the guts!",
		}
		return jokes[rand.Intn(len(jokes))]
	}

	if strings.Contains(lastMessageLower, "weather") {
		return "I'm a mock API, so I can't provide real weather data. But I can tell you it's always sunny in the world of mock responses!"
	}

	if strings.Contains(lastMessageLower, "hello") || strings.Contains(lastMessageLower, "hi") {
		return "Hello! I'm a mock OpenAI API. How can I assist you today?"
	}

	if strings.Contains(lastMessageLower, "streaming") {
		return "This response is being streamed token by token from your mock OpenAI server. Each word appears with a slight delay to simulate real streaming behavior."
	}

	// Default response
	return fmt.Sprintf("This is a mock response to your message: '%s'. The chat completions API is working correctly!", lastMessage)
}

// Handle chat completions
func handleChatCompletions(w http.ResponseWriter, r *http.Request) {
	var req ChatCompletionRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Handle streaming
	if req.Stream != nil && *req.Stream {
		handleStreamingChat(w, r, &req)
		return
	}

    // Generate response (config-aware)
    responseText, errOut, _ := resolveChatResponse(&req)
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
	
	response := ChatCompletionResponse{
		ID:      fmt.Sprintf("chatcmpl-%d", time.Now().Unix()),
		Object:  "chat.completion",
		Created: time.Now().Unix(),
		Model:   req.Model,
		Choices: []Choice{
			{
				Index: 0,
				Message: Message{
					Role:    "assistant",
					Content: responseText,
				},
				FinishReason: "stop",
			},
		},
		Usage: Usage{
			PromptTokens:     countTokens(req.Messages),
			CompletionTokens: len(strings.Fields(responseText)),
			TotalTokens:      countTokens(req.Messages) + len(strings.Fields(responseText)),
		},
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

// Handle streaming chat completions
func handleStreamingChat(w http.ResponseWriter, r *http.Request, req *ChatCompletionRequest) {
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

	responseText, _, delay := resolveChatResponse(req)
	words := strings.Fields(responseText)
	chatID := fmt.Sprintf("chatcmpl-%d", time.Now().Unix())

	// Send initial chunk with role
	initialChunk := StreamChunk{
		ID:      chatID,
		Object:  "chat.completion.chunk",
		Created: time.Now().Unix(),
		Model:   req.Model,
		Choices: []Choice{
			{
				Index: 0,
				Delta: &Delta{Role: "assistant"},
			},
		},
	}

	chunkData, _ := json.Marshal(initialChunk)
	fmt.Fprintf(w, "data: %s\n\n", chunkData)
	flusher.Flush()

	// Stream each word
	for i, word := range words {
		chunk := StreamChunk{
			ID:      chatID,
			Object:  "chat.completion.chunk",
			Created: time.Now().Unix(),
			Model:   req.Model,
			Choices: []Choice{
				{
					Index: 0,
					Delta: &Delta{Content: word},
				},
			},
		}

		if i < len(words)-1 {
			chunk.Choices[0].Delta.Content += " "
		}

		chunkData, _ = json.Marshal(chunk)
		fmt.Fprintf(w, "data: %s\n\n", chunkData)
		flusher.Flush()

		// Add delay for realistic streaming (configurable)
		time.Sleep(delay)
	}

	// Send final chunk
	finalChunk := StreamChunk{
		ID:      chatID,
		Object:  "chat.completion.chunk",
		Created: time.Now().Unix(),
		Model:   req.Model,
		Choices: []Choice{
			{
				Index:        0,
				Delta:        &Delta{},
				FinishReason: "stop",
			},
		},
	}

	chunkData, _ = json.Marshal(finalChunk)
	fmt.Fprintf(w, "data: %s\n\n", chunkData)
	fmt.Fprintf(w, "data: [DONE]\n\n")
	flusher.Flush()
}

// Count tokens (simple word count approximation)
func countTokens(messages []Message) int {
	total := 0
	for _, msg := range messages {
		total += len(strings.Fields(msg.Content))
	}
	return total
}

// Handle models endpoint
func handleModels(w http.ResponseWriter, r *http.Request) {
	var data []Model
	if botConfig != nil && len(botConfig.Models) > 0 {
		for _, m := range botConfig.Models {
			data = append(data, Model{ID: m.ID, Object: "model", Created: 1677610602, OwnedBy: m.OwnedBy})
		}
	} else {
		data = []Model{
			{ID: "gpt-4o", Object: "model", Created: 1677610602, OwnedBy: "openai"},
			{ID: "gpt-4o-mini", Object: "model", Created: 1677610602, OwnedBy: "openai"},
			{ID: "gpt-3.5-turbo", Object: "model", Created: 1677610602, OwnedBy: "openai"},
		}
	}
	models := ModelsResponse{Object: "list", Data: data}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(models)
}

// Health check endpoint
func handleHealth(w http.ResponseWriter, r *http.Request) {
	health := map[string]interface{}{
		"status":    "healthy",
		"timestamp": time.Now().Unix(),
		"apis": map[string]string{
			"chat_completions": "available",
			"responses":        "available",
			"models":          "available",
		},
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(health)
}

func startHttpServer() error {
    router := mux.NewRouter()
    router.Use(corsMiddleware)

    // Chat Completions API
    router.HandleFunc("/v1/chat/completions", handleChatCompletions).Methods("POST")
    router.HandleFunc("/v1/models", handleModels).Methods("GET")
    router.HandleFunc("/health", handleHealth).Methods("GET")

    // Help endpoints
    docpkg.RegisterHelpRoutes(router)

    // Responses API
    setupResponsesRoutes(router)

    port := "3117"
    if botConfig != nil && botConfig.Server.Port != "" { port = botConfig.Server.Port }
    log.Printf("🚀 Mock OpenAI Server with Responses API starting on :%s", port)
    log.Println("")
    log.Println("Available APIs:")
    log.Println("📝 Chat Completions API:\n  POST /v1/chat/completions")
    log.Println("🔄 Responses API:\n  POST /v1/responses\n  GET /v1/responses\n  GET /v1/responses/{response_id}")
    log.Println("🔧 Utility endpoints:\n  GET /v1/models\n  GET /health\n  GET /help, /help/{slug}")
    log.Println("")
    log.Println("Features:\n✅ Streaming support for both APIs\n✅ Built-in tools (web_search, file_search)\n✅ Stateful conversations\n✅ Conversation forking\n✅ CORS enabled")

    return http.ListenAndServe(":"+port, router)
}

// Resolve chat response using configuration rules; falls back to built-in generator.
func resolveChatResponse(req *ChatCompletionRequest) (string, *ErrorOut, time.Duration) {
    // Build input context
    lastUser := ""
    full := ""
    lastRole := ""
    for _, m := range req.Messages {
        if m.Role == "user" { lastUser = m.Content }
        lastRole = m.Role
        if m.Content != "" { full += m.Content + "\n" }
    }
    delayMs := 150
    if botConfig != nil && botConfig.Streaming.ChunkDelayMs != nil {
        delayMs = *botConfig.Streaming.ChunkDelayMs
    }
    delay := time.Duration(delayMs) * time.Millisecond

    mr := evaluateRules("chat", req.Model, lastRole, lastUser, full)
    if mr != nil {
        delay = mr.Delay
        // error path
        if mr.Rule.Respond.Error != nil {
            return "", mr.Rule.Respond.Error, delay
        }
        // text path with optional tools aggregation
        ctx := buildTemplateContext(req.Model, lastUser, full)

        // Aggregate tool output texts if any are requested via use_tools
        agg := ""
        for _, name := range mr.Rule.Respond.UseTools {
            if !isToolEnabled(name) { continue }
            if def, ok := getToolDef(name); ok && def.Message != nil {
                t := renderTemplate(def.Message.Text, ctx)
                if t != "" {
                    if agg != "" { agg += "\n" }
                    agg += t
                }
            }
        }

        txt := pickText(mr.Rule.Respond)
        if txt != "" {
            rendered := renderTemplate(txt, ctx)
            if agg != "" { rendered = agg + "\n" + rendered }
            return rendered, nil, delay
        }
        if agg != "" {
            return agg, nil, delay
        }
    }

    // fallback to configured fallback text
    if botConfig != nil && (botConfig.Fallback.Text != "" || botConfig.Fallback.Message.Text != "") {
        ctx := buildTemplateContext(req.Model, lastUser, full)
        txt := pickText(botConfig.Fallback)
        if txt != "" {
            return renderTemplate(txt, ctx), nil, delay
        }
    }

    // built-in logic
    return generateChatResponse(req.Messages), nil, delay
}
