package fileeditor

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"strings"

	"github.com/rs/zerolog"
)

var log zerolog.Logger

func init() {
	zerolog.SetGlobalLevel(zerolog.DebugLevel)
	log = zerolog.New(os.Stderr).With().Timestamp().Logger()
}

// GeminiClient handles communication with the Gemini API
type GeminiClient struct {
	apiKey     string
	baseURL    string
	httpClient *http.Client
}

// NewGeminiClient creates a new Gemini API client
func NewGeminiClient(apiKey string) *GeminiClient {
	return &GeminiClient{
		apiKey:     apiKey,
		baseURL:    "https://generativelanguage.googleapis.com/v1beta",
		httpClient: &http.Client{},
	}
}

// Tool represents a function tool that can be called by Gemini
type Tool struct {
	FunctionDeclarations []FunctionDeclaration `json:"function_declarations"`
}

// FunctionDeclaration defines a function that can be called
type FunctionDeclaration struct {
	Name        string                 `json:"name"`
	Description string                 `json:"description"`
	Parameters  map[string]interface{} `json:"parameters"`
}

// Content represents message content
type Content struct {
	Parts []Part `json:"parts"`
	Role  string `json:"role,omitempty"`
}

// Part represents a part of the content
type Part struct {
	Text         string        `json:"text,omitempty"`
	FunctionCall *FunctionCall `json:"functionCall,omitempty"`
	FunctionResponse *FunctionResponse `json:"functionResponse,omitempty"`
}

// FunctionCall represents a function call from the model
type FunctionCall struct {
	Name string                 `json:"name"`
	Args map[string]interface{} `json:"args"`
}

// FunctionResponse represents the response to a function call
type FunctionResponse struct {
	Name     string                 `json:"name"`
	Response map[string]interface{} `json:"response"`
}

// GenerateContentRequest represents a request to generate content
type GenerateContentRequest struct {
	Contents         []Content `json:"contents"`
	Tools            []Tool    `json:"tools,omitempty"`
	SystemInstruction *Content `json:"systemInstruction,omitempty"`
}

// GenerateContentResponse represents the response from Gemini
type GenerateContentResponse struct {
	Candidates []Candidate `json:"candidates"`
}

// Candidate represents a candidate response
type Candidate struct {
	Content      Content `json:"content"`
	FinishReason string  `json:"finishReason"`
}

// GetFileEditingTools returns the tool definitions for file editing
func GetFileEditingTools() []Tool {
	return []Tool{
		{
			FunctionDeclarations: []FunctionDeclaration{
				{
					Name:        "read_file",
					Description: "Read the contents of a file at the specified path",
					Parameters: map[string]interface{}{
						"type": "object",
						"properties": map[string]interface{}{
							"path": map[string]interface{}{
								"type":        "string",
								"description": "The path of the file to read (relative to the current working directory)",
							},
						},
						"required": []string{"path"},
					},
				},
				{
					Name:        "write_to_file",
					Description: "Write content to a file at the specified path. If the file exists, it will be overwritten. If it doesn't exist, it will be created along with any necessary directories.",
					Parameters: map[string]interface{}{
						"type": "object",
						"properties": map[string]interface{}{
							"path": map[string]interface{}{
								"type":        "string",
								"description": "The path of the file to write to (relative to the current working directory)",
							},
							"content": map[string]interface{}{
								"type":        "string",
								"description": "The content to write to the file. ALWAYS provide the COMPLETE intended content of the file.",
							},
						},
						"required": []string{"path", "content"},
					},
				},
				{
					Name:        "replace_in_file",
					Description: "Replace sections of content in an existing file using SEARCH/REPLACE blocks that define exact changes to specific parts of the file.",
					Parameters: map[string]interface{}{
						"type": "object",
						"properties": map[string]interface{}{
							"path": map[string]interface{}{
								"type":        "string",
								"description": "The path of the file to modify (relative to the current working directory)",
							},
							"diff": map[string]interface{}{
								"type":        "string",
								"description": "One or more SEARCH/REPLACE blocks following the exact format: ------- SEARCH\\n[exact content to find]\\n=======\\n[new content to replace with]\\n+++++++ REPLACE",
							},
						},
						"required": []string{"path", "diff"},
					},
				},
				{
					Name:        "list_files",
					Description: "List files and directories within the specified directory",
					Parameters: map[string]interface{}{
						"type": "object",
						"properties": map[string]interface{}{
							"path": map[string]interface{}{
								"type":        "string",
								"description": "The path of the directory to list contents for (relative to the current working directory)",
							},
							"recursive": map[string]interface{}{
								"type":        "boolean",
								"description": "Whether to list files recursively. Use true for recursive listing, false for top-level only.",
							},
						},
						"required": []string{"path"},
					},
				},
			},
		},
	}
}

// GenerateContent sends a request to Gemini to generate content
func (gc *GeminiClient) GenerateContent(request GenerateContentRequest) (*GenerateContentResponse, error) {
	url := fmt.Sprintf("%s/models/gemini-2.0-flash-exp:generateContent?key=%s", gc.baseURL, gc.apiKey)
	
	jsonData, err := json.Marshal(request)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal request: %w", err)
	}
	
	resp, err := gc.httpClient.Post(url, "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		return nil, fmt.Errorf("failed to send request: %w", err)
	}
	defer resp.Body.Close()
	
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read response: %w", err)
	}
	
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("API request failed with status %d: %s", resp.StatusCode, string(body))
	}
	
	var response GenerateContentResponse
	if err := json.Unmarshal(body, &response); err != nil {
		return nil, fmt.Errorf("failed to unmarshal response: %w", err)
	}
	
	return &response, nil
}

// GeminiAgent combines the Gemini client with file editing capabilities
type GeminiAgent struct {
	client     *GeminiClient
	fileEditor *FileEditor
}

// NewGeminiAgent creates a new Gemini agent with file editing capabilities
func NewGeminiAgent(apiKey, workingDir string) *GeminiAgent {
	return &GeminiAgent{
		client:     NewGeminiClient(apiKey),
		fileEditor: NewFileEditor(workingDir),
	}
}

// ExecuteFunction executes a function call from Gemini
func (ga *GeminiAgent) ExecuteFunction(call FunctionCall) (map[string]interface{}, error) {
	log.Info().Str("tool", call.Name).Interface("args", call.Args).Msg("Gemini tool called")
	fmt.Printf("[TOOL] %s called with args: %+v\n", call.Name, call.Args)
	switch call.Name {
	case "read_file":
		path, ok := call.Args["path"].(string)
		if !ok {
			return nil, fmt.Errorf("invalid path argument")
		}
		
		content, err := ga.fileEditor.ReadFile(path)
		if err != nil {
			return map[string]interface{}{
				"error": err.Error(),
			}, nil
		}
		
		return map[string]interface{}{
			"content": content,
		}, nil
		
	case "write_to_file":
		path, ok := call.Args["path"].(string)
		if !ok {
			return nil, fmt.Errorf("invalid path argument")
		}
		
		content, ok := call.Args["content"].(string)
		if !ok {
			return nil, fmt.Errorf("invalid content argument")
		}
		
		err := ga.fileEditor.WriteToFile(path, content)
		if err != nil {
			return map[string]interface{}{
				"error": err.Error(),
			}, nil
		}
		
		return map[string]interface{}{
			"success": true,
			"message": fmt.Sprintf("Successfully wrote to %s", path),
		}, nil
		
	case "replace_in_file":
		path, ok := call.Args["path"].(string)
		if !ok {
			return nil, fmt.Errorf("invalid path argument")
		}
		
		diff, ok := call.Args["diff"].(string)
		if !ok {
			return nil, fmt.Errorf("invalid diff argument")
		}
		
		err := ga.fileEditor.ReplaceInFile(path, diff)
		if err != nil {
			return map[string]interface{}{
				"error": err.Error(),
			}, nil
		}
		
		return map[string]interface{}{
			"success": true,
			"message": fmt.Sprintf("Successfully applied changes to %s", path),
		}, nil
		
	case "list_files":
		path, ok := call.Args["path"].(string)
		if !ok {
			return nil, fmt.Errorf("invalid path argument")
		}
		
		recursive := false
		if r, exists := call.Args["recursive"]; exists {
			if rb, ok := r.(bool); ok {
				recursive = rb
			}
		}
		
		files, err := ga.fileEditor.ListFiles(path, recursive)
		if err != nil {
			return map[string]interface{}{
				"error": err.Error(),
			}, nil
		}
		
		return map[string]interface{}{
			"files": files,
		}, nil
		
	default:
		return nil, fmt.Errorf("unknown function: %s", call.Name)
	}
}

// Chat conducts a conversation with Gemini, handling function calls
func (ga *GeminiAgent) Chat(systemPrompt, userMessage string) (string, error) {
	contents := []Content{
		{
			Parts: []Part{{Text: userMessage}},
			Role:  "user",
		},
	}
	
	var systemInstruction *Content
	if systemPrompt != "" {
		systemInstruction = &Content{
			Parts: []Part{{Text: systemPrompt}},
		}
	}
	
	tools := GetFileEditingTools()
	
	for {
		request := GenerateContentRequest{
			Contents:          contents,
			Tools:             tools,
			SystemInstruction: systemInstruction,
		}
		
		// Log the outgoing request (without API key)
		if reqJson, err := json.MarshalIndent(request, "", "  "); err == nil {
			log.Debug().RawJSON("request", reqJson).Msg("Sending Gemini chat completion request")
		}
		
		response, err := ga.client.GenerateContent(request)
		if err != nil {
			log.Error().Err(err).Msg("Failed to generate content from Gemini")
			return "", err
		}
		
		// Log the full response
		if respJson, err := json.MarshalIndent(response, "", "  "); err == nil {
			log.Debug().RawJSON("response", respJson).Msg("Received Gemini chat completion response")
		}
		
		if len(response.Candidates) == 0 {
			log.Error().Msg("No candidates in Gemini response")
			return "", nil
		}
		
		candidate := response.Candidates[0]
		if candJson, err := json.MarshalIndent(candidate, "", "  "); err == nil {
			log.Debug().RawJSON("candidate", candJson).Msg("Gemini candidate content")
		}
		contents = append(contents, candidate.Content)
		
		// Check if there are function calls to execute
		hasFunctionCalls := false
		for _, part := range candidate.Content.Parts {
			if part.FunctionCall != nil {
				hasFunctionCalls = true
				log.Debug().Interface("function_call", part.FunctionCall).Msg("Executing Gemini function call")
				
				// Execute the function call
				result, err := ga.ExecuteFunction(*part.FunctionCall)
				if err != nil {
					log.Error().Err(err).Str("function", part.FunctionCall.Name).Msg("Failed to execute function call")
					return "", err
				}
				
				log.Debug().Interface("function_result", result).Str("function", part.FunctionCall.Name).Msg("Function call result")
				
				// Add function response to conversation
				contents = append(contents, Content{
					Parts: []Part{{
						FunctionResponse: &FunctionResponse{
							Name:     part.FunctionCall.Name,
							Response: result,
						},
					}},
					Role: "user",
				})
			}
		}
		
		// If no function calls, return the text response
		if !hasFunctionCalls {
			var textParts []string
			for _, part := range candidate.Content.Parts {
				if part.Text != "" {
					log.Debug().Str("text_part", part.Text).Msg("Gemini text part")
					textParts = append(textParts, part.Text)
				}
			}
			final := strings.Join(textParts, "")
			log.Info().Str("final_response", final).Msg("Gemini chat completion final response")
			return final, nil
		}
	}
}

