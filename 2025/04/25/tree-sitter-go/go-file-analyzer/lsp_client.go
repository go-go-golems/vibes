package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"time"

	"github.com/sourcegraph/jsonrpc2"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// LSPClient represents a client to the Go Language Server
type LSPClient struct {
	conn     *jsonrpc2.Conn
	server   *exec.Cmd
	handler  jsonrpc2.Handler
	rootPath string
}

type lspHandler struct {
	client *LSPClient
}

// Handle implements the jsonrpc2.Handler interface
func (h *lspHandler) Handle(ctx context.Context, conn *jsonrpc2.Conn, req *jsonrpc2.Request) {
	// We're only using the connection for outgoing requests, 
	// so we can ignore incoming notifications and requests
	if req.Method == "workspace/configuration" {
		// Return empty configuration for each item
		var configParams struct {
			Items []struct{} `json:"items"`
		}
		if err := json.Unmarshal(*req.Params, &configParams); err != nil {
			log.Printf("Failed to unmarshal configuration params: %v", err)
			return
		}
		
		// Return empty configuration for each item
		configs := make([]interface{}, len(configParams.Items))
		if err := conn.Reply(ctx, req.ID, configs); err != nil {
			log.Printf("Failed to reply to configuration request: %v", err)
		}
		return
	}
	
	// Ignore all other notifications
	if req.Notif {
		return
	}
	
	// Reply with method not found for all other requests
	err := &jsonrpc2.Error{
		Code:    jsonrpc2.CodeMethodNotFound,
		Message: fmt.Sprintf("method not found: %s", req.Method),
	}
	if err := conn.ReplyWithError(ctx, req.ID, err); err != nil {
		log.Printf("Failed to reply with error: %v", err)
	}
}

// NewLSPClient creates a new LSP client
func NewLSPClient(rootPath string) (*LSPClient, error) {
	// Make sure gopls is installed
	_, err := exec.LookPath("gopls")
	if err != nil {
		return nil, fmt.Errorf("gopls is not installed or not in PATH: %v", err)
	}

	rootPath, err = filepath.Abs(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to get absolute path: %v", err)
	}

	client := &LSPClient{
		rootPath: rootPath,
	}
	handler := &lspHandler{client: client}
	client.handler = handler

	return client, nil
}

// Start starts the LSP server and initializes the connection
func (c *LSPClient) Start() error {
	// Start gopls server
	c.server = exec.Command("gopls", "serve")
	
	// Create pipes for stdin/stdout
	stdin, err := c.server.StdinPipe()
	if err != nil {
		return fmt.Errorf("failed to create stdin pipe: %v", err)
	}
	
	stdout, err := c.server.StdoutPipe()
	if err != nil {
		return fmt.Errorf("failed to create stdout pipe: %v", err)
	}
	
	c.server.Stderr = os.Stderr

	// Start the server
	if err := c.server.Start(); err != nil {
		return fmt.Errorf("failed to start gopls: %v", err)
	}
	
	// Create JSON-RPC connection with Object codec
	objStream := jsonrpc2.NewBufferedStream(stdout, stdin, jsonrpc2.VarintObjectCodec{})
	c.conn = jsonrpc2.NewConn(context.Background(), objStream, c.handler)
	
	// Initialize the server
	initParams := map[string]interface{}{
		"rootUri":      fmt.Sprintf("file://%s", c.rootPath),
		"capabilities": map[string]interface{}{},
	}
	
	var initResult protocol.InitializeResult
	if err := c.conn.Call(context.Background(), "initialize", initParams, &initResult); err != nil {
		c.Stop()
		return fmt.Errorf("failed to initialize LSP server: %v", err)
	}
	
	// Notify that we're initialized
	if err := c.conn.Notify(context.Background(), "initialized", &protocol.InitializedParams{}); err != nil {
		c.Stop()
		return fmt.Errorf("failed to send initialized notification: %v", err)
	}
	
	return nil
}

// Stop stops the LSP server and closes the connection
func (c *LSPClient) Stop() {
	if c.conn != nil {
		// Send shutdown request
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		
		c.conn.Call(ctx, "shutdown", nil, nil)
		c.conn.Notify(ctx, "exit", nil)
		c.conn.Close()
		c.conn = nil
	}
	
	if c.server != nil && c.server.Process != nil {
		c.server.Process.Kill()
		c.server.Wait()
		c.server = nil
	}
}

// FindReferences finds all references to a symbol at the given position
func (c *LSPClient) FindReferences(filePath string, line, column int) ([]protocol.Location, error) {
	absPath, err := filepath.Abs(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to get absolute path: %v", err)
	}
	
	// Create text document position params
	params := map[string]interface{}{
		"textDocument": map[string]interface{}{
			"uri": fmt.Sprintf("file://%s", absPath),
		},
		"position": map[string]interface{}{
			"line":      line - 1, // Convert to 0-based line numbering
			"character": column,
		},
		"context": map[string]interface{}{
			"includeDeclaration": true,
		},
	}
	
	// Call references
	var locations []protocol.Location
	if err := c.conn.Call(context.Background(), "textDocument/references", params, &locations); err != nil {
		return nil, fmt.Errorf("failed to find references: %v", err)
	}
	
	return locations, nil
}

// FindDefinition finds the definition of a symbol at the given position
func (c *LSPClient) FindDefinition(filePath string, line, column int) (protocol.Location, error) {
	absPath, err := filepath.Abs(filePath)
	if err != nil {
		return protocol.Location{}, fmt.Errorf("failed to get absolute path: %v", err)
	}
	
	// Create text document position params
	params := map[string]interface{}{
		"textDocument": map[string]interface{}{
			"uri": fmt.Sprintf("file://%s", absPath),
		},
		"position": map[string]interface{}{
			"line":      line - 1, // Convert to 0-based line numbering
			"character": column,
		},
	}
	
	// Call definition
	var location protocol.Location
	if err := c.conn.Call(context.Background(), "textDocument/definition", params, &location); err != nil {
		return protocol.Location{}, fmt.Errorf("failed to find definition: %v", err)
	}
	
	return location, nil
}

// Convert LSP locations to our Reference type
func locationsToReferences(locations []protocol.Location) []Reference {
	refs := make([]Reference, 0, len(locations))
	
	for _, loc := range locations {
		// Convert URI to filepath
		path := string(loc.URI)
		if len(path) > 7 && path[:7] == "file://" {
			path = path[7:]
		}
		
		ref := Reference{
			Filepath: path,
			Line:     int(loc.Range.Start.Line) + 1, // Convert to 1-based line numbering
			Column:   int(loc.Range.Start.Character),
			// We'll determine if it's a definition when processing all references
		}
		
		refs = append(refs, ref)
	}
	
	return refs
}