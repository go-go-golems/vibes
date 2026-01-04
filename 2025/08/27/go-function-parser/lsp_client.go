package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

// LSPClient represents a client for the Language Server Protocol
type LSPClient struct {
	cmd        *exec.Cmd
	stdin      io.WriteCloser
	stdout     *bufio.Reader
	nextID     int
	mutex      sync.Mutex
	workspaces []string
	logger     zerolog.Logger
}

// LSPRequest represents a request to the LSP server
type LSPRequest struct {
	JSONRPC string      `json:"jsonrpc"`
	ID      int         `json:"id"`
	Method  string      `json:"method"`
	Params  interface{} `json:"params,omitempty"`
}

// LSPResponse represents a response from the LSP server
type LSPResponse struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      int             `json:"id"`
	Result  json.RawMessage `json:"result,omitempty"`
	Error   *LSPError       `json:"error,omitempty"`
}

// LSPError represents an error from the LSP server
type LSPError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

// LSPNotification represents a notification from the LSP server
type LSPNotification struct {
	JSONRPC string      `json:"jsonrpc"`
	Method  string      `json:"method"`
	Params  interface{} `json:"params,omitempty"`
}

// Position represents a position in a text document
type Position struct {
	Line      int `json:"line"`
	Character int `json:"character"`
}

// Range represents a range in a text document
type Range struct {
	Start Position `json:"start"`
	End   Position `json:"end"`
}

// Location represents a location in a text document
type Location struct {
	URI   string `json:"uri"`
	Range Range  `json:"range"`
}

// TextDocumentIdentifier identifies a text document
type TextDocumentIdentifier struct {
	URI string `json:"uri"`
}

// ReferenceParams represents parameters for the textDocument/references request
type ReferenceParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Position     Position               `json:"position"`
	Context      ReferenceContext       `json:"context"`
}

// ReferenceContext represents context for the textDocument/references request
type ReferenceContext struct {
	IncludeDeclaration bool `json:"includeDeclaration"`
}

// InitializeParams represents parameters for the initialize request
type InitializeParams struct {
	ProcessID             int                    `json:"processId"`
	RootURI               string                 `json:"rootUri"`
	Capabilities          ClientCapabilities     `json:"capabilities"`
	WorkspaceFolders      []WorkspaceFolder      `json:"workspaceFolders,omitempty"`
	InitializationOptions map[string]interface{} `json:"initializationOptions,omitempty"`
}

// ClientCapabilities represents capabilities of the client
type ClientCapabilities struct {
	Workspace    WorkspaceClientCapabilities    `json:"workspace,omitempty"`
	TextDocument TextDocumentClientCapabilities `json:"textDocument,omitempty"`
}

// WorkspaceClientCapabilities represents workspace capabilities of the client
type WorkspaceClientCapabilities struct {
	WorkspaceFolders bool `json:"workspaceFolders,omitempty"`
}

// TextDocumentClientCapabilities represents text document capabilities of the client
type TextDocumentClientCapabilities struct {
	References TextDocumentReferencesCapabilities `json:"references,omitempty"`
}

// TextDocumentReferencesCapabilities represents references capabilities of the client
type TextDocumentReferencesCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"`
}

// WorkspaceFolder represents a workspace folder
type WorkspaceFolder struct {
	URI  string `json:"uri"`
	Name string `json:"name"`
}

// DidOpenTextDocumentParams represents parameters for the textDocument/didOpen notification
type DidOpenTextDocumentParams struct {
	TextDocument TextDocumentItem `json:"textDocument"`
}

// TextDocumentItem represents a text document item
type TextDocumentItem struct {
	URI        string `json:"uri"`
	LanguageID string `json:"languageId"`
	Version    int    `json:"version"`
	Text       string `json:"text"`
}

// logCaller adds file and line information to the log
func logCaller() zerolog.Logger {
	_, file, line, ok := runtime.Caller(1)
	if !ok {
		file = "unknown"
		line = 0
	}

	// Get just the filename, not the full path
	parts := strings.Split(file, "/")
	file = parts[len(parts)-1]

	return log.With().Str("file", file).Int("line", line).Logger()
}

// NewLSPClient creates a new LSP client
func NewLSPClient() (*LSPClient, error) {
	logger := logCaller()
	logger.Debug().Msg("Creating new LSP client")

	goplsPath := "gopls"
	logger.Debug().Str("goplsPath", goplsPath).Msg("Using gopls path")

	cmd := exec.Command(goplsPath, "-rpc.trace", "-v")
	logger.Debug().Str("command", cmd.String()).Msg("Created command")

	// Set up stderr to capture gopls logs
	stderr, err := cmd.StderrPipe()
	if err != nil {
		logger.Error().Err(err).Msg("Failed to create stderr pipe")
		return nil, fmt.Errorf("failed to create stderr pipe: %v", err)
	}

	// Start a goroutine to log stderr output
	go func() {
		scanner := bufio.NewScanner(stderr)
		stderrLogger := log.With().Str("source", "gopls_stderr").Logger()
		for scanner.Scan() {
			stderrLogger.Debug().Msg(scanner.Text())
		}
	}()

	stdin, err := cmd.StdinPipe()
	if err != nil {
		logger.Error().Err(err).Msg("Failed to create stdin pipe")
		return nil, fmt.Errorf("failed to create stdin pipe: %v", err)
	}

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		logger.Error().Err(err).Msg("Failed to create stdout pipe")
		return nil, fmt.Errorf("failed to create stdout pipe: %v", err)
	}

	logger.Debug().Msg("Starting gopls process")
	err = cmd.Start()
	if err != nil {
		logger.Error().Err(err).Msg("Failed to start gopls")
		return nil, fmt.Errorf("failed to start gopls: %v", err)
	}

	logger.Debug().Int("pid", cmd.Process.Pid).Msg("Gopls process started")

	client := &LSPClient{
		cmd:    cmd,
		stdin:  stdin,
		stdout: bufio.NewReader(stdout),
		nextID: 1,
		logger: log.With().Str("component", "LSPClient").Logger(),
	}

	logger.Debug().Msg("LSP client created successfully")
	return client, nil
}

// Initialize initializes the LSP server
func (c *LSPClient) Initialize(rootURI string) error {
	logger := logCaller()
	logger.Debug().Str("rootURI", rootURI).Msg("Initializing LSP server")

	params := InitializeParams{
		ProcessID: os.Getpid(),
		RootURI:   rootURI,
		Capabilities: ClientCapabilities{
			Workspace: WorkspaceClientCapabilities{
				WorkspaceFolders: true,
			},
			TextDocument: TextDocumentClientCapabilities{
				References: TextDocumentReferencesCapabilities{
					DynamicRegistration: true,
				},
			},
		},
		WorkspaceFolders: []WorkspaceFolder{
			{
				URI:  rootURI,
				Name: "root",
			},
		},
	}

	logger.Debug().Interface("params", params).Msg("Sending initialize request")
	result, err := c.SendRequest("initialize", params)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to initialize")
		return fmt.Errorf("failed to initialize: %v", err)
	}

	logger.Debug().RawJSON("result", result).Msg("Initialize request successful")

	logger.Debug().Msg("Sending initialized notification")
	err = c.SendNotification("initialized", struct{}{})
	if err != nil {
		logger.Error().Err(err).Msg("Failed to send initialized notification")
		return fmt.Errorf("failed to send initialized notification: %v", err)
	}

	logger.Debug().Msg("Initialized notification sent")

	c.workspaces = append(c.workspaces, rootURI)
	logger.Debug().Strs("workspaces", c.workspaces).Msg("Added workspace")
	return nil
}

// DidOpenTextDocument notifies the server that a text document has been opened
func (c *LSPClient) DidOpenTextDocument(uri, languageID, text string) error {
	logger := logCaller()
	logger.Debug().Str("uri", uri).Str("languageID", languageID).Int("textLength", len(text)).Msg("Opening text document")

	params := DidOpenTextDocumentParams{
		TextDocument: TextDocumentItem{
			URI:        uri,
			LanguageID: languageID,
			Version:    1,
			Text:       text,
		},
	}

	logger.Debug().Interface("params", params).Msg("Sending didOpen notification")
	err := c.SendNotification("textDocument/didOpen", params)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to send didOpen notification")
		return fmt.Errorf("failed to send didOpen notification: %v", err)
	}

	logger.Debug().Msg("didOpen notification sent")
	return nil
}

// FindReferences finds all references to a symbol at a position
func (c *LSPClient) FindReferences(uri string, line, character int, includeDeclaration bool) ([]Location, error) {
	logger := logCaller()
	logger.Debug().
		Str("uri", uri).
		Int("line", line).
		Int("character", character).
		Bool("includeDeclaration", includeDeclaration).
		Msg("Finding references")

	params := ReferenceParams{
		TextDocument: TextDocumentIdentifier{
			URI: uri,
		},
		Position: Position{
			Line:      line,
			Character: character,
		},
		Context: ReferenceContext{
			IncludeDeclaration: includeDeclaration,
		},
	}

	logger.Debug().Interface("params", params).Msg("Sending textDocument/references request")
	result, err := c.SendRequest("textDocument/references", params)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to find references")
		return nil, fmt.Errorf("failed to find references: %v", err)
	}

	logger.Debug().RawJSON("result", result).Msg("References request successful")

	// Check if result is null
	if string(result) == "null" {
		logger.Debug().Msg("No references found (null result)")
		return []Location{}, nil
	}

	// Check if result is empty array
	if string(result) == "[]" {
		logger.Debug().Msg("No references found (empty array)")
		return []Location{}, nil
	}

	var locations []Location
	err = json.Unmarshal(result, &locations)
	if err != nil {
		logger.Error().Err(err).RawJSON("rawResult", result).Msg("Failed to unmarshal references result")
		return nil, fmt.Errorf("failed to unmarshal references result: %v", err)
	}

	logger.Debug().Int("count", len(locations)).Msg("Successfully unmarshaled references")
	return locations, nil
}

// SendRequest sends a request to the LSP server
func (c *LSPClient) SendRequest(method string, params interface{}) (json.RawMessage, error) {
	logger := logCaller()

	c.mutex.Lock()
	id := c.nextID
	c.nextID++
	c.mutex.Unlock()

	logger.Debug().Int("id", id).Str("method", method).Msg("Preparing request")

	request := LSPRequest{
		JSONRPC: "2.0",
		ID:      id,
		Method:  method,
		Params:  params,
	}

	requestJSON, err := json.Marshal(request)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to marshal request")
		return nil, fmt.Errorf("failed to marshal request: %v", err)
	}

	logger.Debug().RawJSON("request", requestJSON).Msg("Request marshaled")

	contentLength := len(requestJSON)
	requestWithHeader := fmt.Sprintf("Content-Length: %d\r\n\r\n%s", contentLength, requestJSON)

	logger.Debug().Int("contentLength", contentLength).Msg("Sending request to gopls")

	_, err = c.stdin.Write([]byte(requestWithHeader))
	if err != nil {
		logger.Error().Err(err).Msg("Failed to write request")
		return nil, fmt.Errorf("failed to write request: %v", err)
	}

	logger.Debug().Msg("Request sent, waiting for response")

	// Set a timeout for reading the response
	responseChan := make(chan *LSPResponse, 1)
	errorChan := make(chan error, 1)

	go func() {
		response, err := c.readResponse()
		if err != nil {
			errorChan <- err
			return
		}
		responseChan <- response
	}()

	// Wait for response with timeout
	select {
	case response := <-responseChan:
		if response.Error != nil {
			logger.Error().
				Int("code", response.Error.Code).
				Str("message", response.Error.Message).
				Msg("LSP error in response")
			return nil, fmt.Errorf("LSP error: %s (code %d)", response.Error.Message, response.Error.Code)
		}

		logger.Debug().Int("id", response.ID).Msg("Received successful response")
		return response.Result, nil

	case err := <-errorChan:
		logger.Error().Err(err).Msg("Error reading response")
		return nil, fmt.Errorf("failed to read response: %v", err)

	case <-time.After(10 * time.Second):
		logger.Error().Msg("Timeout waiting for response")
		return nil, fmt.Errorf("timeout waiting for response")
	}
}

// SendNotification sends a notification to the LSP server
func (c *LSPClient) SendNotification(method string, params interface{}) error {
	logger := logCaller()
	logger.Debug().Str("method", method).Msg("Preparing notification")

	notification := LSPNotification{
		JSONRPC: "2.0",
		Method:  method,
		Params:  params,
	}

	notificationJSON, err := json.Marshal(notification)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to marshal notification")
		return fmt.Errorf("failed to marshal notification: %v", err)
	}

	logger.Debug().RawJSON("notification", notificationJSON).Msg("Notification marshaled")

	contentLength := len(notificationJSON)
	notificationWithHeader := fmt.Sprintf("Content-Length: %d\r\n\r\n%s", contentLength, notificationJSON)

	logger.Debug().Int("contentLength", contentLength).Msg("Sending notification to gopls")

	_, err = c.stdin.Write([]byte(notificationWithHeader))
	if err != nil {
		logger.Error().Err(err).Msg("Failed to write notification")
		return fmt.Errorf("failed to write notification: %v", err)
	}

	logger.Debug().Msg("Notification sent")
	return nil
}

// readResponse reads a response from the LSP server
func (c *LSPClient) readResponse() (*LSPResponse, error) {
	logger := logCaller()
	logger.Debug().Msg("Reading response headers")

	// Read headers
	var contentLength int
	for {
		line, err := c.stdout.ReadString('\n')
		if err != nil {
			logger.Error().Err(err).Msg("Failed to read header line")
			return nil, fmt.Errorf("failed to read header: %v", err)
		}

		line = strings.TrimSpace(line)
		logger.Debug().Str("headerLine", line).Msg("Read header line")

		if line == "" {
			logger.Debug().Msg("End of headers")
			break
		}

		if strings.HasPrefix(line, "Content-Length:") {
			lengthStr := strings.TrimSpace(strings.TrimPrefix(line, "Content-Length:"))
			contentLength, err = strconv.Atoi(lengthStr)
			if err != nil {
				logger.Error().Err(err).Str("lengthStr", lengthStr).Msg("Failed to parse content length")
				return nil, fmt.Errorf("failed to parse content length: %v", err)
			}
			logger.Debug().Int("contentLength", contentLength).Msg("Parsed content length")
		}
	}

	if contentLength == 0 {
		logger.Error().Msg("Content length is 0")
		return nil, fmt.Errorf("content length is 0")
	}

	// Read content
	logger.Debug().Int("contentLength", contentLength).Msg("Reading response content")
	content := make([]byte, contentLength)
	_, err := io.ReadFull(c.stdout, content)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to read content")
		return nil, fmt.Errorf("failed to read content: %v", err)
	}

	logger.Debug().Int("contentLength", len(content)).Msg("Read response content")
	logger.Debug().RawJSON("rawContent", content).Msg("Raw response content")

	// Parse response
	var response LSPResponse
	err = json.Unmarshal(content, &response)
	if err != nil {
		logger.Error().Err(err).Str("content", string(content)).Msg("Failed to unmarshal response")
		return nil, fmt.Errorf("failed to unmarshal response: %v", err)
	}

	logger.Debug().Int("id", response.ID).Msg("Response unmarshaled successfully")
	return &response, nil
}

// Close closes the LSP client
func (c *LSPClient) Close() error {
	logger := logCaller()
	logger.Debug().Msg("Closing LSP client")

	logger.Debug().Msg("Sending shutdown request")
	_, err := c.SendRequest("shutdown", nil)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to shutdown")
		return fmt.Errorf("failed to shutdown: %v", err)
	}

	logger.Debug().Msg("Sending exit notification")
	err = c.SendNotification("exit", nil)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to exit")
		return fmt.Errorf("failed to exit: %v", err)
	}

	logger.Debug().Msg("Killing gopls process")
	err = c.cmd.Process.Kill()
	if err != nil {
		logger.Error().Err(err).Msg("Failed to kill process")
		return fmt.Errorf("failed to kill process: %v", err)
	}

	logger.Debug().Msg("LSP client closed successfully")
	return nil
}
