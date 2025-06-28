// Package lsp provides Language Server Protocol client implementation.
package lsp

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"time"
)

// Client represents an LSP client that communicates with a language server.
type Client struct {
	cmd           *exec.Cmd
	stdin         io.WriteCloser
	stdout        io.ReadCloser
	stderr        io.ReadCloser
	
	// Request/response handling
	nextID        int64
	pendingReqs   map[interface{}]chan *ResponseMessage
	pendingMutex  sync.RWMutex
	
	// Notification handling
	notificationHandlers map[string]func(interface{})
	handlerMutex        sync.RWMutex
	
	// State management
	initialized   bool
	capabilities  *ServerCapabilities
	rootURI       string
	
	// Logging
	logger        *log.Logger
	debugMode     bool
	
	// Lifecycle
	ctx           context.Context
	cancel        context.CancelFunc
	done          chan struct{}
}

// ClientOptions represents options for creating an LSP client.
type ClientOptions struct {
	Command       string            // Command to start the language server
	Args          []string          // Arguments for the language server command
	RootURI       string            // Root URI of the workspace
	Logger        *log.Logger       // Logger for debugging (optional)
	DebugMode     bool              // Enable debug logging
	Env           []string          // Environment variables for the server process
	WorkingDir    string            // Working directory for the server process
}

// NewClient creates a new LSP client with the given options.
func NewClient(opts ClientOptions) (*Client, error) {
	if opts.Logger == nil {
		opts.Logger = log.New(os.Stderr, "[LSP] ", log.LstdFlags)
	}
	
	ctx, cancel := context.WithCancel(context.Background())
	
	client := &Client{
		nextID:               1,
		pendingReqs:          make(map[interface{}]chan *ResponseMessage),
		notificationHandlers: make(map[string]func(interface{})),
		rootURI:              opts.RootURI,
		logger:               opts.Logger,
		debugMode:            opts.DebugMode,
		ctx:                  ctx,
		cancel:               cancel,
		done:                 make(chan struct{}),
	}
	
	// Start the language server process
	if err := client.startServer(opts); err != nil {
		cancel()
		return nil, fmt.Errorf("failed to start language server: %w", err)
	}
	
	// Start message processing goroutines
	go client.readMessages()
	go client.processStderr()
	
	return client, nil
}

// startServer starts the language server process.
func (c *Client) startServer(opts ClientOptions) error {
	c.cmd = exec.CommandContext(c.ctx, opts.Command, opts.Args...)
	
	if opts.Env != nil {
		c.cmd.Env = opts.Env
	}
	
	if opts.WorkingDir != "" {
		c.cmd.Dir = opts.WorkingDir
	}
	
	var err error
	
	// Setup stdin pipe
	c.stdin, err = c.cmd.StdinPipe()
	if err != nil {
		return fmt.Errorf("failed to create stdin pipe: %w", err)
	}
	
	// Setup stdout pipe
	c.stdout, err = c.cmd.StdoutPipe()
	if err != nil {
		return fmt.Errorf("failed to create stdout pipe: %w", err)
	}
	
	// Setup stderr pipe
	c.stderr, err = c.cmd.StderrPipe()
	if err != nil {
		return fmt.Errorf("failed to create stderr pipe: %w", err)
	}
	
	// Start the process
	if err := c.cmd.Start(); err != nil {
		return fmt.Errorf("failed to start process: %w", err)
	}
	
	c.debugLog("Language server started with PID: %d", c.cmd.Process.Pid)
	return nil
}

// Initialize sends an initialize request to the language server.
func (c *Client) Initialize() error {
	params := InitializeParams{
		ProcessID: os.Getpid(),
		RootURI:   c.rootURI,
		Capabilities: ClientCapabilities{
			TextDocument: &TextDocumentClientCapabilities{
				Synchronization: &TextDocumentSyncCapabilities{
					DynamicRegistration: false,
					WillSave:            false,
					WillSaveWaitUntil:   false,
					DidSave:             true,
				},
				Completion: &CompletionCapabilities{
					DynamicRegistration: false,
					CompletionItem: &CompletionItemCapabilities{
						SnippetSupport:          false,
						CommitCharactersSupport: true,
						DocumentationFormat:     []string{"plaintext", "markdown"},
						DeprecatedSupport:       true,
						PreselectSupport:        true,
					},
				},
				Hover: &HoverCapabilities{
					DynamicRegistration: false,
					ContentFormat:       []string{"plaintext", "markdown"},
				},
				SignatureHelp: &SignatureHelpCapabilities{
					DynamicRegistration: false,
					SignatureInformation: &SignatureInformationCapabilities{
						DocumentationFormat: []string{"plaintext", "markdown"},
					},
				},
				References: &ReferencesCapabilities{
					DynamicRegistration: false,
				},
				DocumentHighlight: &DocumentHighlightCapabilities{
					DynamicRegistration: false,
				},
				DocumentSymbol: &DocumentSymbolCapabilities{
					DynamicRegistration: false,
				},
				Formatting: &DocumentFormattingCapabilities{
					DynamicRegistration: false,
				},
				RangeFormatting: &DocumentRangeFormattingCapabilities{
					DynamicRegistration: false,
				},
				OnTypeFormatting: &DocumentOnTypeFormattingCapabilities{
					DynamicRegistration: false,
				},
				Definition: &DefinitionCapabilities{
					DynamicRegistration: false,
				},
				CodeAction: &CodeActionCapabilities{
					DynamicRegistration: false,
				},
				CodeLens: &CodeLensCapabilities{
					DynamicRegistration: false,
				},
				DocumentLink: &DocumentLinkCapabilities{
					DynamicRegistration: false,
				},
				Rename: &RenameCapabilities{
					DynamicRegistration: false,
				},
			},
			Workspace: &WorkspaceClientCapabilities{
				ApplyEdit: true,
				WorkspaceEdit: &WorkspaceEditCapabilities{
					DocumentChanges: true,
				},
				DidChangeConfiguration: &DidChangeConfigurationCapabilities{
					DynamicRegistration: false,
				},
				DidChangeWatchedFiles: &DidChangeWatchedFilesCapabilities{
					DynamicRegistration: false,
				},
				Symbol: &WorkspaceSymbolCapabilities{
					DynamicRegistration: false,
				},
				ExecuteCommand: &ExecuteCommandCapabilities{
					DynamicRegistration: false,
				},
			},
		},
		Trace: "off",
	}
	
	response, err := c.sendRequest("initialize", params)
	if err != nil {
		return fmt.Errorf("initialize request failed: %w", err)
	}
	
	if response.Error != nil {
		return fmt.Errorf("initialize request returned error: %s", response.Error.Message)
	}
	
	// Parse the initialize result
	var result InitializeResult
	if err := json.Unmarshal(response.Result.(json.RawMessage), &result); err != nil {
		return fmt.Errorf("failed to parse initialize result: %w", err)
	}
	
	c.capabilities = &result.Capabilities
	c.initialized = true
	
	// Send initialized notification
	if err := c.sendNotification("initialized", struct{}{}); err != nil {
		return fmt.Errorf("failed to send initialized notification: %w", err)
	}
	
	c.debugLog("LSP client initialized successfully")
	return nil
}

// DidOpen sends a textDocument/didOpen notification.
func (c *Client) DidOpen(uri, languageID, text string, version int) error {
	if !c.initialized {
		return fmt.Errorf("client not initialized")
	}
	
	params := struct {
		TextDocument TextDocumentItem `json:"textDocument"`
	}{
		TextDocument: TextDocumentItem{
			URI:        uri,
			LanguageID: languageID,
			Version:    version,
			Text:       text,
		},
	}
	
	return c.sendNotification("textDocument/didOpen", params)
}

// DidChange sends a textDocument/didChange notification.
func (c *Client) DidChange(uri string, version int, text string) error {
	if !c.initialized {
		return fmt.Errorf("client not initialized")
	}
	
	params := struct {
		TextDocument   VersionedTextDocumentIdentifier `json:"textDocument"`
		ContentChanges []interface{}                   `json:"contentChanges"`
	}{
		TextDocument: VersionedTextDocumentIdentifier{
			TextDocumentIdentifier: TextDocumentIdentifier{URI: uri},
			Version:                version,
		},
		ContentChanges: []interface{}{
			map[string]interface{}{
				"text": text,
			},
		},
	}
	
	return c.sendNotification("textDocument/didChange", params)
}

// DidSave sends a textDocument/didSave notification.
func (c *Client) DidSave(uri string, text *string) error {
	if !c.initialized {
		return fmt.Errorf("client not initialized")
	}
	
	params := struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Text         *string                `json:"text,omitempty"`
	}{
		TextDocument: TextDocumentIdentifier{URI: uri},
		Text:         text,
	}
	
	return c.sendNotification("textDocument/didSave", params)
}

// Hover sends a textDocument/hover request.
func (c *Client) Hover(uri string, line, character int) (*Hover, error) {
	if !c.initialized {
		return nil, fmt.Errorf("client not initialized")
	}
	
	params := NewTextDocumentPositionParams(uri, line, character)
	
	response, err := c.sendRequest("textDocument/hover", params)
	if err != nil {
		return nil, fmt.Errorf("hover request failed: %w", err)
	}
	
	if response.Error != nil {
		return nil, fmt.Errorf("hover request returned error: %s", response.Error.Message)
	}
	
	if response.Result == nil {
		return nil, nil // No hover information available
	}
	
	var hover Hover
	if err := json.Unmarshal(response.Result.(json.RawMessage), &hover); err != nil {
		return nil, fmt.Errorf("failed to parse hover result: %w", err)
	}
	
	return &hover, nil
}

// Completion sends a textDocument/completion request.
func (c *Client) Completion(uri string, line, character int) ([]CompletionItem, error) {
	if !c.initialized {
		return nil, fmt.Errorf("client not initialized")
	}
	
	params := CompletionParams{
		TextDocumentPositionParams: NewTextDocumentPositionParams(uri, line, character),
	}
	
	response, err := c.sendRequest("textDocument/completion", params)
	if err != nil {
		return nil, fmt.Errorf("completion request failed: %w", err)
	}
	
	if response.Error != nil {
		return nil, fmt.Errorf("completion request returned error: %s", response.Error.Message)
	}
	
	if response.Result == nil {
		return []CompletionItem{}, nil
	}
	
	// Handle both CompletionList and CompletionItem[] formats
	var result json.RawMessage
	if err := json.Unmarshal(response.Result.(json.RawMessage), &result); err != nil {
		return nil, fmt.Errorf("failed to parse completion result: %w", err)
	}
	
	// Try to parse as CompletionList first
	var completionList struct {
		IsIncomplete bool             `json:"isIncomplete"`
		Items        []CompletionItem `json:"items"`
	}
	
	if err := json.Unmarshal(result, &completionList); err == nil && len(completionList.Items) > 0 {
		return completionList.Items, nil
	}
	
	// Try to parse as CompletionItem[] directly
	var items []CompletionItem
	if err := json.Unmarshal(result, &items); err != nil {
		return nil, fmt.Errorf("failed to parse completion items: %w", err)
	}
	
	return items, nil
}

// Definition sends a textDocument/definition request.
func (c *Client) Definition(uri string, line, character int) ([]Location, error) {
	if !c.initialized {
		return nil, fmt.Errorf("client not initialized")
	}
	
	params := NewTextDocumentPositionParams(uri, line, character)
	
	response, err := c.sendRequest("textDocument/definition", params)
	if err != nil {
		return nil, fmt.Errorf("definition request failed: %w", err)
	}
	
	if response.Error != nil {
		return nil, fmt.Errorf("definition request returned error: %s", response.Error.Message)
	}
	
	if response.Result == nil {
		return []Location{}, nil
	}
	
	// Handle both Location and Location[] formats
	var result json.RawMessage
	if err := json.Unmarshal(response.Result.(json.RawMessage), &result); err != nil {
		return nil, fmt.Errorf("failed to parse definition result: %w", err)
	}
	
	// Try to parse as Location[] first
	var locations []Location
	if err := json.Unmarshal(result, &locations); err == nil {
		return locations, nil
	}
	
	// Try to parse as single Location
	var location Location
	if err := json.Unmarshal(result, &location); err != nil {
		return nil, fmt.Errorf("failed to parse definition location: %w", err)
	}
	
	return []Location{location}, nil
}

// References sends a textDocument/references request.
func (c *Client) References(uri string, line, character int, includeDeclaration bool) ([]Location, error) {
	if !c.initialized {
		return nil, fmt.Errorf("client not initialized")
	}
	
	params := struct {
		TextDocumentPositionParams
		Context struct {
			IncludeDeclaration bool `json:"includeDeclaration"`
		} `json:"context"`
	}{
		TextDocumentPositionParams: NewTextDocumentPositionParams(uri, line, character),
		Context: struct {
			IncludeDeclaration bool `json:"includeDeclaration"`
		}{
			IncludeDeclaration: includeDeclaration,
		},
	}
	
	response, err := c.sendRequest("textDocument/references", params)
	if err != nil {
		return nil, fmt.Errorf("references request failed: %w", err)
	}
	
	if response.Error != nil {
		return nil, fmt.Errorf("references request returned error: %s", response.Error.Message)
	}
	
	if response.Result == nil {
		return []Location{}, nil
	}
	
	var locations []Location
	if err := json.Unmarshal(response.Result.(json.RawMessage), &locations); err != nil {
		return nil, fmt.Errorf("failed to parse references result: %w", err)
	}
	
	return locations, nil
}

// sendRequest sends a JSON-RPC request and waits for the response.
func (c *Client) sendRequest(method string, params interface{}) (*ResponseMessage, error) {
	id := atomic.AddInt64(&c.nextID, 1)
	
	request := RequestMessage{
		JSONRPC: "2.0",
		ID:      id,
		Method:  method,
		Params:  params,
	}
	
	// Create response channel
	responseChan := make(chan *ResponseMessage, 1)
	c.pendingMutex.Lock()
	c.pendingReqs[id] = responseChan
	c.pendingMutex.Unlock()
	
	// Clean up on exit
	defer func() {
		c.pendingMutex.Lock()
		delete(c.pendingReqs, id)
		c.pendingMutex.Unlock()
		close(responseChan)
	}()
	
	// Send the request
	if err := c.writeMessage(request); err != nil {
		return nil, fmt.Errorf("failed to send request: %w", err)
	}
	
	// Wait for response with timeout
	select {
	case response := <-responseChan:
		return response, nil
	case <-time.After(30 * time.Second):
		return nil, fmt.Errorf("request timeout")
	case <-c.ctx.Done():
		return nil, fmt.Errorf("client context cancelled")
	}
}

// sendNotification sends a JSON-RPC notification.
func (c *Client) sendNotification(method string, params interface{}) error {
	notification := NotificationMessage{
		JSONRPC: "2.0",
		Method:  method,
		Params:  params,
	}
	
	return c.writeMessage(notification)
}

// writeMessage writes a JSON-RPC message to the server.
func (c *Client) writeMessage(message interface{}) error {
	data, err := json.Marshal(message)
	if err != nil {
		return fmt.Errorf("failed to marshal message: %w", err)
	}
	
	content := fmt.Sprintf("Content-Length: %d\r\n\r\n%s", len(data), data)
	
	if c.debugMode {
		c.debugLog("Sending: %s", string(data))
	}
	
	_, err = c.stdin.Write([]byte(content))
	return err
}

// readMessages reads and processes messages from the server.
func (c *Client) readMessages() {
	defer close(c.done)
	
	scanner := bufio.NewScanner(c.stdout)
	
	for scanner.Scan() {
		select {
		case <-c.ctx.Done():
			return
		default:
		}
		
		line := scanner.Text()
		
		// Look for Content-Length header
		if strings.HasPrefix(line, "Content-Length:") {
			lengthStr := strings.TrimSpace(strings.TrimPrefix(line, "Content-Length:"))
			contentLength, err := strconv.Atoi(lengthStr)
			if err != nil {
				c.logger.Printf("Invalid Content-Length: %s", lengthStr)
				continue
			}
			
			// Skip the empty line after headers
			if !scanner.Scan() {
				break
			}
			
			// Read the JSON content
			if !scanner.Scan() {
				break
			}
			
			content := scanner.Bytes()
			
			// Verify content length
			if len(content) != contentLength {
				c.logger.Printf("Content length mismatch: expected %d, got %d", contentLength, len(content))
			}
			
			if c.debugMode {
				c.debugLog("Received: %s", string(content))
			}
			
			// Process the message
			c.processMessage(content)
		}
	}
	
	if err := scanner.Err(); err != nil {
		c.logger.Printf("Error reading from server: %v", err)
	}
}

// processMessage processes a received JSON-RPC message.
func (c *Client) processMessage(content []byte) {
	// Try to determine message type
	var baseMsg struct {
		JSONRPC string      `json:"jsonrpc"`
		ID      interface{} `json:"id,omitempty"`
		Method  string      `json:"method,omitempty"`
	}
	
	if err := json.Unmarshal(content, &baseMsg); err != nil {
		c.logger.Printf("Failed to parse message: %v", err)
		return
	}
	
	if baseMsg.ID != nil && baseMsg.Method == "" {
		// This is a response
		var response ResponseMessage
		if err := json.Unmarshal(content, &response); err != nil {
			c.logger.Printf("Failed to parse response: %v", err)
			return
		}
		
		c.handleResponse(&response)
	} else if baseMsg.Method != "" {
		// This is a notification
		var notification NotificationMessage
		if err := json.Unmarshal(content, &notification); err != nil {
			c.logger.Printf("Failed to parse notification: %v", err)
			return
		}
		
		c.handleNotification(&notification)
	}
}

// handleResponse handles a JSON-RPC response.
func (c *Client) handleResponse(response *ResponseMessage) {
	c.pendingMutex.RLock()
	responseChan, exists := c.pendingReqs[response.ID]
	c.pendingMutex.RUnlock()
	
	if !exists {
		c.logger.Printf("Received response for unknown request ID: %v", response.ID)
		return
	}
	
	select {
	case responseChan <- response:
	default:
		c.logger.Printf("Response channel full for request ID: %v", response.ID)
	}
}

// handleNotification handles a JSON-RPC notification.
func (c *Client) handleNotification(notification *NotificationMessage) {
	c.handlerMutex.RLock()
	handler, exists := c.notificationHandlers[notification.Method]
	c.handlerMutex.RUnlock()
	
	if exists {
		handler(notification.Params)
	} else {
		c.debugLog("Received unhandled notification: %s", notification.Method)
	}
}

// processStderr processes stderr output from the language server.
func (c *Client) processStderr() {
	scanner := bufio.NewScanner(c.stderr)
	for scanner.Scan() {
		select {
		case <-c.ctx.Done():
			return
		default:
		}
		
		line := scanner.Text()
		if c.debugMode {
			c.logger.Printf("Server stderr: %s", line)
		}
	}
}

// SetNotificationHandler sets a handler for a specific notification method.
func (c *Client) SetNotificationHandler(method string, handler func(interface{})) {
	c.handlerMutex.Lock()
	defer c.handlerMutex.Unlock()
	c.notificationHandlers[method] = handler
}

// GetCapabilities returns the server capabilities.
func (c *Client) GetCapabilities() *ServerCapabilities {
	return c.capabilities
}

// IsInitialized returns whether the client has been initialized.
func (c *Client) IsInitialized() bool {
	return c.initialized
}

// Close closes the LSP client and terminates the language server.
func (c *Client) Close() error {
	c.debugLog("Closing LSP client")
	
	// Cancel context to stop goroutines
	c.cancel()
	
	// Send shutdown request if initialized
	if c.initialized {
		if _, err := c.sendRequest("shutdown", nil); err != nil {
			c.logger.Printf("Failed to send shutdown request: %v", err)
		}
		
		// Send exit notification
		if err := c.sendNotification("exit", nil); err != nil {
			c.logger.Printf("Failed to send exit notification: %v", err)
		}
	}
	
	// Close pipes
	if c.stdin != nil {
		c.stdin.Close()
	}
	if c.stdout != nil {
		c.stdout.Close()
	}
	if c.stderr != nil {
		c.stderr.Close()
	}
	
	// Wait for the process to exit
	if c.cmd != nil {
		if err := c.cmd.Wait(); err != nil {
			c.logger.Printf("Language server process exited with error: %v", err)
		}
	}
	
	// Wait for message processing to complete
	select {
	case <-c.done:
	case <-time.After(5 * time.Second):
		c.logger.Printf("Timeout waiting for message processing to complete")
	}
	
	c.debugLog("LSP client closed")
	return nil
}

// debugLog logs a debug message if debug mode is enabled.
func (c *Client) debugLog(format string, args ...interface{}) {
	if c.debugMode {
		c.logger.Printf(format, args...)
	}
}

