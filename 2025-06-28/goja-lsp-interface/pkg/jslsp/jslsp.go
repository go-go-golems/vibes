// Package jslsp provides JavaScript bindings for LSP client functionality using Goja.
package jslsp

import (
	"context"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"time"

	"github.com/dop251/goja"
	"goja-lsp-interface/pkg/lsp"
)

// JSLSPClient represents a JavaScript-accessible LSP client.
type JSLSPClient struct {
	client  *lsp.Client
	manager *lsp.LSPManager
	vm      *goja.Runtime
	logger  *log.Logger
}

// JSLSPOptions represents options for creating a JavaScript LSP client.
type JSLSPOptions struct {
	Command    string `json:"command"`    // Language server command
	Args       []string `json:"args"`     // Command arguments
	RootPath   string `json:"rootPath"`   // Root path of the workspace
	DebugMode  bool   `json:"debugMode"`  // Enable debug logging
	WorkingDir string `json:"workingDir"` // Working directory
}

// NewJSLSPClient creates a new JavaScript LSP client.
func NewJSLSPClient(vm *goja.Runtime, opts JSLSPOptions) (*JSLSPClient, error) {
	logger := log.New(os.Stderr, "[JSLSP] ", log.LstdFlags)
	
	// Convert root path to absolute path and URI
	rootPath := opts.RootPath
	if rootPath == "" {
		var err error
		rootPath, err = os.Getwd()
		if err != nil {
			return nil, fmt.Errorf("failed to get current directory: %w", err)
		}
	}
	
	absRootPath, err := filepath.Abs(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to get absolute path: %w", err)
	}
	
	// Set up LSP client options
	clientOpts := lsp.ClientOptions{
		Command:    opts.Command,
		Args:       opts.Args,
		RootURI:    lsp.FileToURI(absRootPath),
		DebugMode:  opts.DebugMode,
		Logger:     logger,
		WorkingDir: absRootPath,
	}
	
	// Create LSP client
	client, err := lsp.NewClient(clientOpts)
	if err != nil {
		return nil, fmt.Errorf("failed to create LSP client: %w", err)
	}
	
	// Create LSP manager
	manager := lsp.NewLSPManager(client)
	
	jsClient := &JSLSPClient{
		client:  client,
		manager: manager,
		vm:      vm,
		logger:  logger,
	}
	
	return jsClient, nil
}

// Initialize initializes the LSP client.
func (js *JSLSPClient) Initialize() error {
	return js.client.Initialize()
}

// OpenFile opens a file in the LSP session.
func (js *JSLSPClient) OpenFile(path string) (map[string]interface{}, error) {
	openFile, err := js.manager.OpenFile(path)
	if err != nil {
		return nil, err
	}
	
	return map[string]interface{}{
		"uri":        openFile.URI,
		"path":       openFile.Path,
		"languageId": openFile.LanguageID,
		"version":    openFile.Version,
		"content":    openFile.Content,
	}, nil
}

// GetHover gets hover information for a position in a file.
func (js *JSLSPClient) GetHover(uri string, line, character int) (interface{}, error) {
	hover, err := js.manager.GetHover(uri, line, character)
	if err != nil {
		return nil, err
	}
	
	if hover == nil {
		return nil, nil
	}
	
	return map[string]interface{}{
		"contents": hover.Contents,
		"range":    js.rangeToJS(hover.Range),
		"text":     lsp.FormatHover(hover),
	}, nil
}

// GetCompletion gets completion items for a position in a file.
func (js *JSLSPClient) GetCompletion(uri string, line, character int) ([]interface{}, error) {
	completions, err := js.manager.GetCompletion(uri, line, character)
	if err != nil {
		return nil, err
	}
	
	result := make([]interface{}, len(completions))
	for i, item := range completions {
		result[i] = map[string]interface{}{
			"label":         item.Label,
			"kind":          item.Kind,
			"detail":        item.Detail,
			"documentation": item.Documentation,
			"deprecated":    item.Deprecated,
			"preselect":     item.Preselect,
			"sortText":      item.SortText,
			"filterText":    item.FilterText,
			"insertText":    item.InsertText,
			"textEdit":      js.textEditToJS(item.TextEdit),
		}
	}
	
	return result, nil
}

// GetDefinition gets definition locations for a position in a file.
func (js *JSLSPClient) GetDefinition(uri string, line, character int) ([]interface{}, error) {
	definitions, err := js.manager.GetDefinition(uri, line, character)
	if err != nil {
		return nil, err
	}
	
	result := make([]interface{}, len(definitions))
	for i, def := range definitions {
		result[i] = map[string]interface{}{
			"uri":   def.URI,
			"range": js.rangeToJS(&def.Range),
			"path":  js.uriToPath(def.URI),
			"text":  lsp.FormatLocation(def),
		}
	}
	
	return result, nil
}

// GetReferences gets reference locations for a position in a file.
func (js *JSLSPClient) GetReferences(uri string, line, character int, includeDeclaration bool) ([]interface{}, error) {
	references, err := js.manager.GetReferences(uri, line, character, includeDeclaration)
	if err != nil {
		return nil, err
	}
	
	result := make([]interface{}, len(references))
	for i, ref := range references {
		result[i] = map[string]interface{}{
			"uri":   ref.URI,
			"range": js.rangeToJS(&ref.Range),
			"path":  js.uriToPath(ref.URI),
			"text":  lsp.FormatLocation(ref),
		}
	}
	
	return result, nil
}

// GetOpenFiles returns a list of currently open files.
func (js *JSLSPClient) GetOpenFiles() []interface{} {
	openFiles := js.manager.GetOpenFiles()
	result := make([]interface{}, len(openFiles))
	
	for i, file := range openFiles {
		result[i] = map[string]interface{}{
			"uri":        file.URI,
			"path":       file.Path,
			"languageId": file.LanguageID,
			"version":    file.Version,
		}
	}
	
	return result
}

// CloseFile closes a file in the LSP session.
func (js *JSLSPClient) CloseFile(uri string) error {
	return js.manager.CloseFile(uri)
}

// GetCapabilities returns the server capabilities.
func (js *JSLSPClient) GetCapabilities() map[string]interface{} {
	caps := js.client.GetCapabilities()
	if caps == nil {
		return nil
	}
	
	return map[string]interface{}{
		"hoverProvider":      caps.HoverProvider,
		"completionProvider": caps.CompletionProvider != nil,
		"definitionProvider": caps.DefinitionProvider,
		"referencesProvider": caps.ReferencesProvider,
		"documentSymbolProvider": caps.DocumentSymbolProvider,
		"workspaceSymbolProvider": caps.WorkspaceSymbolProvider,
		"codeActionProvider": caps.CodeActionProvider != nil,
		"documentFormattingProvider": caps.DocumentFormattingProvider,
		"renameProvider": caps.RenameProvider != nil,
	}
}

// IsInitialized returns whether the client has been initialized.
func (js *JSLSPClient) IsInitialized() bool {
	return js.client.IsInitialized()
}

// Close closes the LSP client.
func (js *JSLSPClient) Close() error {
	if err := js.manager.Close(); err != nil {
		js.logger.Printf("Error closing manager: %v", err)
	}
	return js.client.Close()
}

// Helper methods for converting Go types to JavaScript-friendly types

// rangeToJS converts an LSP Range to a JavaScript object.
func (js *JSLSPClient) rangeToJS(r *lsp.Range) interface{} {
	if r == nil {
		return nil
	}
	
	return map[string]interface{}{
		"start": map[string]interface{}{
			"line":      r.Start.Line,
			"character": r.Start.Character,
		},
		"end": map[string]interface{}{
			"line":      r.End.Line,
			"character": r.End.Character,
		},
	}
}

// textEditToJS converts an LSP TextEdit to a JavaScript object.
func (js *JSLSPClient) textEditToJS(edit *lsp.TextEdit) interface{} {
	if edit == nil {
		return nil
	}
	
	return map[string]interface{}{
		"range":   js.rangeToJS(&edit.Range),
		"newText": edit.NewText,
	}
}

// uriToPath converts a URI to a file path.
func (js *JSLSPClient) uriToPath(uri string) string {
	path, err := lsp.URIToFile(uri)
	if err != nil {
		return uri
	}
	return path
}

// JSLSPRuntime represents a Goja runtime with LSP bindings.
type JSLSPRuntime struct {
	vm     *goja.Runtime
	client *JSLSPClient
}

// NewJSLSPRuntime creates a new Goja runtime with LSP bindings.
func NewJSLSPRuntime() *JSLSPRuntime {
	vm := goja.New()
	
	runtime := &JSLSPRuntime{
		vm: vm,
	}
	
	// Set up global functions and objects
	runtime.setupGlobals()
	
	return runtime
}

// setupGlobals sets up global JavaScript functions and objects.
func (jr *JSLSPRuntime) setupGlobals() {
	// Add console.log function
	console := jr.vm.NewObject()
	console.Set("log", func(args ...interface{}) {
		fmt.Println(args...)
	})
	console.Set("error", func(args ...interface{}) {
		fmt.Fprintln(os.Stderr, args...)
	})
	jr.vm.Set("console", console)
	
	// Add LSP client creation function
	jr.vm.Set("createLSPClient", jr.createLSPClient)
	
	// Add utility functions
	jr.vm.Set("fileToURI", lsp.FileToURI)
	jr.vm.Set("uriToFile", func(uri string) string {
		path, err := lsp.URIToFile(uri)
		if err != nil {
			return ""
		}
		return path
	})
	jr.vm.Set("getLanguageID", lsp.GetLanguageID)
	jr.vm.Set("readFile", lsp.ReadFileContent)
}

// createLSPClient creates an LSP client from JavaScript.
func (jr *JSLSPRuntime) createLSPClient(opts map[string]interface{}) (*JSLSPClient, error) {
	// Convert JavaScript options to Go struct
	jsOpts := JSLSPOptions{
		Command:    getString(opts, "command", "gopls"),
		RootPath:   getString(opts, "rootPath", ""),
		DebugMode:  getBool(opts, "debugMode", false),
		WorkingDir: getString(opts, "workingDir", ""),
	}
	
	// Handle args array
	if argsVal, ok := opts["args"]; ok {
		if argsArray, ok := argsVal.([]interface{}); ok {
			jsOpts.Args = make([]string, len(argsArray))
			for i, arg := range argsArray {
				if str, ok := arg.(string); ok {
					jsOpts.Args[i] = str
				}
			}
		}
	}
	
	client, err := NewJSLSPClient(jr.vm, jsOpts)
	if err != nil {
		return nil, err
	}
	
	jr.client = client
	return client, nil
}

// RunScript runs a JavaScript script in the runtime.
func (jr *JSLSPRuntime) RunScript(script string) (goja.Value, error) {
	return jr.vm.RunString(script)
}

// RunFile runs a JavaScript file in the runtime.
func (jr *JSLSPRuntime) RunFile(filename string) (goja.Value, error) {
	content, err := lsp.ReadFileContent(filename)
	if err != nil {
		return nil, fmt.Errorf("failed to read file %s: %w", filename, err)
	}
	
	return jr.vm.RunString(content)
}

// GetVM returns the underlying Goja runtime.
func (jr *JSLSPRuntime) GetVM() *goja.Runtime {
	return jr.vm
}

// Close closes the runtime and any associated LSP clients.
func (jr *JSLSPRuntime) Close() error {
	if jr.client != nil {
		return jr.client.Close()
	}
	return nil
}

// SetGlobal sets a global variable in the JavaScript runtime.
func (jr *JSLSPRuntime) SetGlobal(name string, value interface{}) {
	jr.vm.Set(name, value)
}

// GetGlobal gets a global variable from the JavaScript runtime.
func (jr *JSLSPRuntime) GetGlobal(name string) goja.Value {
	return jr.vm.Get(name)
}

// Helper functions for type conversion

// getString gets a string value from a map with a default.
func getString(m map[string]interface{}, key, defaultValue string) string {
	if val, ok := m[key]; ok {
		if str, ok := val.(string); ok {
			return str
		}
	}
	return defaultValue
}

// getBool gets a boolean value from a map with a default.
func getBool(m map[string]interface{}, key string, defaultValue bool) bool {
	if val, ok := m[key]; ok {
		if b, ok := val.(bool); ok {
			return b
		}
	}
	return defaultValue
}

// getInt gets an integer value from a map with a default.
func getInt(m map[string]interface{}, key string, defaultValue int) int {
	if val, ok := m[key]; ok {
		if i, ok := val.(int); ok {
			return i
		}
		if f, ok := val.(float64); ok {
			return int(f)
		}
	}
	return defaultValue
}

// CreateStandaloneJSLSP creates a standalone JavaScript LSP environment.
func CreateStandaloneJSLSP(ctx context.Context) (*JSLSPRuntime, error) {
	runtime := NewJSLSPRuntime()
	
	// Add additional utilities for standalone usage
	runtime.SetGlobal("setTimeout", func(callback func(), delay int) {
		go func() {
			select {
			case <-ctx.Done():
				return
			case <-time.After(time.Duration(delay) * time.Millisecond):
				callback()
			}
		}()
	})
	
	return runtime, nil
}

