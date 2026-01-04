// Package lsp provides Language Server Protocol client implementation.
// This package implements LSP communication for use with language servers like gopls.
package lsp

import (
	"encoding/json"
)

// LSP Protocol Types and Structures

// Position represents a position in a text document.
type Position struct {
	Line      int `json:"line"`      // Line position in a document (zero-based)
	Character int `json:"character"` // Character offset on a line in a document (zero-based)
}

// Range represents a text range in a document.
type Range struct {
	Start Position `json:"start"` // The range's start position
	End   Position `json:"end"`   // The range's end position
}

// Location represents a location inside a resource.
type Location struct {
	URI   string `json:"uri"`   // The text document's URI
	Range Range  `json:"range"` // The range inside the text document
}

// TextDocumentIdentifier identifies a text document.
type TextDocumentIdentifier struct {
	URI string `json:"uri"` // The text document's URI
}

// VersionedTextDocumentIdentifier identifies a versioned text document.
type VersionedTextDocumentIdentifier struct {
	TextDocumentIdentifier
	Version int `json:"version"` // The version number of this document
}

// TextDocumentItem represents a text document.
type TextDocumentItem struct {
	URI        string `json:"uri"`        // The text document's URI
	LanguageID string `json:"languageId"` // The text document's language identifier
	Version    int    `json:"version"`    // The version number of this document
	Text       string `json:"text"`       // The content of the opened text document
}

// TextDocumentPositionParams represents parameters for text document position requests.
type TextDocumentPositionParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"` // The text document
	Position     Position               `json:"position"`     // The position inside the text document
}

// CompletionParams represents parameters for completion requests.
type CompletionParams struct {
	TextDocumentPositionParams
	Context *CompletionContext `json:"context,omitempty"` // The completion context
}

// CompletionContext represents additional information about completion.
type CompletionContext struct {
	TriggerKind      int    `json:"triggerKind"`                // How the completion was triggered
	TriggerCharacter string `json:"triggerCharacter,omitempty"` // The trigger character
}

// CompletionItem represents a completion item.
type CompletionItem struct {
	Label         string                 `json:"label"`                   // The label of this completion item
	Kind          int                    `json:"kind,omitempty"`          // The kind of this completion item
	Detail        string                 `json:"detail,omitempty"`        // A human-readable string with additional information
	Documentation interface{}            `json:"documentation,omitempty"` // A human-readable string that represents a doc-comment
	Deprecated    bool                   `json:"deprecated,omitempty"`    // Indicates if this item is deprecated
	Preselect     bool                   `json:"preselect,omitempty"`     // Select this item when showing
	SortText      string                 `json:"sortText,omitempty"`      // A string that should be used when comparing this item
	FilterText    string                 `json:"filterText,omitempty"`    // A string that should be used when filtering
	InsertText    string                 `json:"insertText,omitempty"`    // A string that should be inserted into a document
	TextEdit      *TextEdit              `json:"textEdit,omitempty"`      // An edit which is applied to a document
	Data          interface{}            `json:"data,omitempty"`          // A data entry field that is preserved between requests
}

// TextEdit represents a textual edit applicable to a text document.
type TextEdit struct {
	Range   Range  `json:"range"`   // The range of the text document to be manipulated
	NewText string `json:"newText"` // The string to be inserted
}

// Hover represents the result of a hover request.
type Hover struct {
	Contents interface{} `json:"contents"` // The hover's content
	Range    *Range      `json:"range,omitempty"` // An optional range
}

// MarkupContent represents a markup content.
type MarkupContent struct {
	Kind  string `json:"kind"`  // The type of the Markup
	Value string `json:"value"` // The content itself
}

// Diagnostic represents a diagnostic message.
type Diagnostic struct {
	Range              Range                        `json:"range"`                        // The range at which the message applies
	Severity           int                          `json:"severity,omitempty"`           // The diagnostic's severity
	Code               interface{}                  `json:"code,omitempty"`               // The diagnostic's code
	Source             string                       `json:"source,omitempty"`             // A human-readable string describing the source
	Message            string                       `json:"message"`                      // The diagnostic's message
	Tags               []int                        `json:"tags,omitempty"`               // Additional metadata about the diagnostic
	RelatedInformation []DiagnosticRelatedInformation `json:"relatedInformation,omitempty"` // An array of related diagnostic information
}

// DiagnosticRelatedInformation represents related information for a diagnostic.
type DiagnosticRelatedInformation struct {
	Location Location `json:"location"` // The location of this related diagnostic information
	Message  string   `json:"message"`  // The message of this related diagnostic information
}

// PublishDiagnosticsParams represents parameters for publishing diagnostics.
type PublishDiagnosticsParams struct {
	URI         string       `json:"uri"`         // The URI for which diagnostic information is reported
	Version     int          `json:"version,omitempty"` // Optional the version number of the document
	Diagnostics []Diagnostic `json:"diagnostics"` // An array of diagnostic information items
}

// JSON-RPC 2.0 Message Types

// RequestMessage represents a JSON-RPC request message.
type RequestMessage struct {
	JSONRPC string      `json:"jsonrpc"` // The JSON-RPC version (always "2.0")
	ID      interface{} `json:"id"`      // The request id
	Method  string      `json:"method"`  // The method to be invoked
	Params  interface{} `json:"params,omitempty"` // The method's params
}

// ResponseMessage represents a JSON-RPC response message.
type ResponseMessage struct {
	JSONRPC string      `json:"jsonrpc"` // The JSON-RPC version (always "2.0")
	ID      interface{} `json:"id"`      // The request id
	Result  interface{} `json:"result,omitempty"` // The result of a request
	Error   *RPCError   `json:"error,omitempty"`  // The error object in case a request fails
}

// NotificationMessage represents a JSON-RPC notification message.
type NotificationMessage struct {
	JSONRPC string      `json:"jsonrpc"` // The JSON-RPC version (always "2.0")
	Method  string      `json:"method"`  // The method to be invoked
	Params  interface{} `json:"params,omitempty"` // The notification's params
}

// RPCError represents a JSON-RPC error object.
type RPCError struct {
	Code    int         `json:"code"`    // A number indicating the error type
	Message string      `json:"message"` // A string providing a short description of the error
	Data    interface{} `json:"data,omitempty"` // Additional information about the error
}

// Error implements the error interface for RPCError.
func (e *RPCError) Error() string {
	return e.Message
}

// LSP Initialize Types

// InitializeParams represents parameters for the initialize request.
type InitializeParams struct {
	ProcessID             int                `json:"processId"`             // The process Id of the parent process
	RootPath              string             `json:"rootPath,omitempty"`    // The rootPath of the workspace (deprecated)
	RootURI               string             `json:"rootUri"`               // The rootUri of the workspace
	InitializationOptions interface{}        `json:"initializationOptions,omitempty"` // User provided initialization options
	Capabilities          ClientCapabilities `json:"capabilities"`          // The capabilities provided by the client
	Trace                 string             `json:"trace,omitempty"`       // The initial trace setting
	WorkspaceFolders      []WorkspaceFolder  `json:"workspaceFolders,omitempty"` // The workspace folders configured in the client
}

// ClientCapabilities represents client capabilities.
type ClientCapabilities struct {
	Workspace    *WorkspaceClientCapabilities    `json:"workspace,omitempty"`    // Workspace specific client capabilities
	TextDocument *TextDocumentClientCapabilities `json:"textDocument,omitempty"` // Text document specific client capabilities
	Experimental interface{}                     `json:"experimental,omitempty"` // Experimental client capabilities
}

// WorkspaceClientCapabilities represents workspace-specific client capabilities.
type WorkspaceClientCapabilities struct {
	ApplyEdit              bool                        `json:"applyEdit,omitempty"`              // The client supports applying batch edits
	WorkspaceEdit          *WorkspaceEditCapabilities  `json:"workspaceEdit,omitempty"`          // Capabilities specific to `WorkspaceEdit`
	DidChangeConfiguration *DidChangeConfigurationCapabilities `json:"didChangeConfiguration,omitempty"` // Capabilities specific to the `workspace/didChangeConfiguration` notification
	DidChangeWatchedFiles  *DidChangeWatchedFilesCapabilities  `json:"didChangeWatchedFiles,omitempty"`  // Capabilities specific to the `workspace/didChangeWatchedFiles` notification
	Symbol                 *WorkspaceSymbolCapabilities `json:"symbol,omitempty"`                 // Capabilities specific to the `workspace/symbol` request
	ExecuteCommand         *ExecuteCommandCapabilities  `json:"executeCommand,omitempty"`         // Capabilities specific to the `workspace/executeCommand` request
}

// WorkspaceEditCapabilities represents workspace edit capabilities.
type WorkspaceEditCapabilities struct {
	DocumentChanges bool `json:"documentChanges,omitempty"` // The client supports versioned document changes
}

// DidChangeConfigurationCapabilities represents did change configuration capabilities.
type DidChangeConfigurationCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Did change configuration notification supports dynamic registration
}

// DidChangeWatchedFilesCapabilities represents did change watched files capabilities.
type DidChangeWatchedFilesCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Did change watched files notification supports dynamic registration
}

// WorkspaceSymbolCapabilities represents workspace symbol capabilities.
type WorkspaceSymbolCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Symbol request supports dynamic registration
}

// ExecuteCommandCapabilities represents execute command capabilities.
type ExecuteCommandCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Execute command supports dynamic registration
}

// TextDocumentClientCapabilities represents text document specific client capabilities.
type TextDocumentClientCapabilities struct {
	Synchronization    *TextDocumentSyncCapabilities    `json:"synchronization,omitempty"`    // Defines which synchronization capabilities the client supports
	Completion         *CompletionCapabilities          `json:"completion,omitempty"`         // Capabilities specific to the `textDocument/completion` request
	Hover              *HoverCapabilities               `json:"hover,omitempty"`              // Capabilities specific to the `textDocument/hover` request
	SignatureHelp      *SignatureHelpCapabilities       `json:"signatureHelp,omitempty"`      // Capabilities specific to the `textDocument/signatureHelp` request
	References         *ReferencesCapabilities          `json:"references,omitempty"`         // Capabilities specific to the `textDocument/references` request
	DocumentHighlight  *DocumentHighlightCapabilities   `json:"documentHighlight,omitempty"`  // Capabilities specific to the `textDocument/documentHighlight` request
	DocumentSymbol     *DocumentSymbolCapabilities      `json:"documentSymbol,omitempty"`     // Capabilities specific to the `textDocument/documentSymbol` request
	Formatting         *DocumentFormattingCapabilities  `json:"formatting,omitempty"`         // Capabilities specific to the `textDocument/formatting` request
	RangeFormatting    *DocumentRangeFormattingCapabilities `json:"rangeFormatting,omitempty"`    // Capabilities specific to the `textDocument/rangeFormatting` request
	OnTypeFormatting   *DocumentOnTypeFormattingCapabilities `json:"onTypeFormatting,omitempty"`   // Capabilities specific to the `textDocument/onTypeFormatting` request
	Definition         *DefinitionCapabilities          `json:"definition,omitempty"`         // Capabilities specific to the `textDocument/definition` request
	CodeAction         *CodeActionCapabilities          `json:"codeAction,omitempty"`         // Capabilities specific to the `textDocument/codeAction` request
	CodeLens           *CodeLensCapabilities            `json:"codeLens,omitempty"`           // Capabilities specific to the `textDocument/codeLens` request
	DocumentLink       *DocumentLinkCapabilities        `json:"documentLink,omitempty"`       // Capabilities specific to the `textDocument/documentLink` request
	Rename             *RenameCapabilities              `json:"rename,omitempty"`             // Capabilities specific to the `textDocument/rename` request
}

// TextDocumentSyncCapabilities represents text document sync capabilities.
type TextDocumentSyncCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Whether text document synchronization supports dynamic registration
	WillSave            bool `json:"willSave,omitempty"`            // The client supports sending will save notifications
	WillSaveWaitUntil   bool `json:"willSaveWaitUntil,omitempty"`   // The client supports sending a will save request and waits for a response
	DidSave             bool `json:"didSave,omitempty"`             // The client supports did save notifications
}

// CompletionCapabilities represents completion capabilities.
type CompletionCapabilities struct {
	DynamicRegistration bool                      `json:"dynamicRegistration,omitempty"` // Whether completion supports dynamic registration
	CompletionItem      *CompletionItemCapabilities `json:"completionItem,omitempty"`      // The client supports the following `CompletionItem` specific capabilities
}

// CompletionItemCapabilities represents completion item capabilities.
type CompletionItemCapabilities struct {
	SnippetSupport          bool     `json:"snippetSupport,omitempty"`          // Client supports snippets as insert text
	CommitCharactersSupport bool     `json:"commitCharactersSupport,omitempty"` // Client supports commit characters on a completion item
	DocumentationFormat     []string `json:"documentationFormat,omitempty"`     // Client supports the following content formats for the documentation property
	DeprecatedSupport       bool     `json:"deprecatedSupport,omitempty"`       // Client supports the deprecated property on a completion item
	PreselectSupport        bool     `json:"preselectSupport,omitempty"`        // Client supports the preselect property on a completion item
}

// HoverCapabilities represents hover capabilities.
type HoverCapabilities struct {
	DynamicRegistration bool     `json:"dynamicRegistration,omitempty"` // Whether hover supports dynamic registration
	ContentFormat       []string `json:"contentFormat,omitempty"`       // Client supports the following content formats for the content property
}

// SignatureHelpCapabilities represents signature help capabilities.
type SignatureHelpCapabilities struct {
	DynamicRegistration bool                            `json:"dynamicRegistration,omitempty"` // Whether signature help supports dynamic registration
	SignatureInformation *SignatureInformationCapabilities `json:"signatureInformation,omitempty"` // The client supports the following `SignatureInformation` specific properties
}

// SignatureInformationCapabilities represents signature information capabilities.
type SignatureInformationCapabilities struct {
	DocumentationFormat []string `json:"documentationFormat,omitempty"` // Client supports the following content formats for the documentation property
}

// ReferencesCapabilities represents references capabilities.
type ReferencesCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Whether references supports dynamic registration
}

// DocumentHighlightCapabilities represents document highlight capabilities.
type DocumentHighlightCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Whether document highlight supports dynamic registration
}

// DocumentSymbolCapabilities represents document symbol capabilities.
type DocumentSymbolCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Whether document symbol supports dynamic registration
}

// DocumentFormattingCapabilities represents document formatting capabilities.
type DocumentFormattingCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Whether formatting supports dynamic registration
}

// DocumentRangeFormattingCapabilities represents document range formatting capabilities.
type DocumentRangeFormattingCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Whether range formatting supports dynamic registration
}

// DocumentOnTypeFormattingCapabilities represents document on type formatting capabilities.
type DocumentOnTypeFormattingCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Whether on type formatting supports dynamic registration
}

// DefinitionCapabilities represents definition capabilities.
type DefinitionCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Whether definition supports dynamic registration
}

// CodeActionCapabilities represents code action capabilities.
type CodeActionCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Whether code action supports dynamic registration
}

// CodeLensCapabilities represents code lens capabilities.
type CodeLensCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Whether code lens supports dynamic registration
}

// DocumentLinkCapabilities represents document link capabilities.
type DocumentLinkCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Whether document link supports dynamic registration
}

// RenameCapabilities represents rename capabilities.
type RenameCapabilities struct {
	DynamicRegistration bool `json:"dynamicRegistration,omitempty"` // Whether rename supports dynamic registration
}

// WorkspaceFolder represents a workspace folder.
type WorkspaceFolder struct {
	URI  string `json:"uri"`  // The associated URI for this workspace folder
	Name string `json:"name"` // The name of the workspace folder
}

// InitializeResult represents the result of an initialize request.
type InitializeResult struct {
	Capabilities ServerCapabilities `json:"capabilities"` // The capabilities the language server provides
}

// ServerCapabilities represents server capabilities.
type ServerCapabilities struct {
	TextDocumentSync                 interface{}                       `json:"textDocumentSync,omitempty"`                 // Defines how text documents are synced
	HoverProvider                    bool                              `json:"hoverProvider,omitempty"`                    // The server provides hover support
	CompletionProvider               *CompletionOptions                `json:"completionProvider,omitempty"`               // The server provides completion support
	SignatureHelpProvider            *SignatureHelpOptions             `json:"signatureHelpProvider,omitempty"`            // The server provides signature help support
	DefinitionProvider               bool                              `json:"definitionProvider,omitempty"`               // The server provides goto definition support
	TypeDefinitionProvider           interface{}                       `json:"typeDefinitionProvider,omitempty"`           // The server provides goto type definition support
	ImplementationProvider           interface{}                       `json:"implementationProvider,omitempty"`           // The server provides goto implementation support
	ReferencesProvider               bool                              `json:"referencesProvider,omitempty"`               // The server provides find references support
	DocumentHighlightProvider        bool                              `json:"documentHighlightProvider,omitempty"`        // The server provides document highlight support
	DocumentSymbolProvider           bool                              `json:"documentSymbolProvider,omitempty"`           // The server provides document symbol support
	WorkspaceSymbolProvider          bool                              `json:"workspaceSymbolProvider,omitempty"`          // The server provides workspace symbol support
	CodeActionProvider               interface{}                       `json:"codeActionProvider,omitempty"`               // The server provides code actions
	CodeLensProvider                 *CodeLensOptions                  `json:"codeLensProvider,omitempty"`                 // The server provides code lens
	DocumentFormattingProvider       bool                              `json:"documentFormattingProvider,omitempty"`       // The server provides document formatting
	DocumentRangeFormattingProvider  bool                              `json:"documentRangeFormattingProvider,omitempty"`  // The server provides document range formatting
	DocumentOnTypeFormattingProvider *DocumentOnTypeFormattingOptions  `json:"documentOnTypeFormattingProvider,omitempty"` // The server provides document on type formatting
	RenameProvider                   interface{}                       `json:"renameProvider,omitempty"`                   // The server provides rename support
	DocumentLinkProvider             *DocumentLinkOptions              `json:"documentLinkProvider,omitempty"`             // The server provides document link support
	ExecuteCommandProvider           *ExecuteCommandOptions            `json:"executeCommandProvider,omitempty"`           // The server provides execute command support
	Experimental                     interface{}                       `json:"experimental,omitempty"`                     // Experimental server capabilities
}

// CompletionOptions represents completion options.
type CompletionOptions struct {
	ResolveProvider   bool     `json:"resolveProvider,omitempty"`   // The server provides support to resolve additional information for a completion item
	TriggerCharacters []string `json:"triggerCharacters,omitempty"` // The characters that trigger completion automatically
}

// SignatureHelpOptions represents signature help options.
type SignatureHelpOptions struct {
	TriggerCharacters []string `json:"triggerCharacters,omitempty"` // The characters that trigger signature help automatically
}

// CodeLensOptions represents code lens options.
type CodeLensOptions struct {
	ResolveProvider bool `json:"resolveProvider,omitempty"` // Code lens has a resolve provider as well
}

// DocumentOnTypeFormattingOptions represents document on type formatting options.
type DocumentOnTypeFormattingOptions struct {
	FirstTriggerCharacter string   `json:"firstTriggerCharacter"`           // A character on which formatting should be triggered
	MoreTriggerCharacter  []string `json:"moreTriggerCharacter,omitempty"`  // More trigger characters
}

// DocumentLinkOptions represents document link options.
type DocumentLinkOptions struct {
	ResolveProvider bool `json:"resolveProvider,omitempty"` // Document links have a resolve provider as well
}

// ExecuteCommandOptions represents execute command options.
type ExecuteCommandOptions struct {
	Commands []string `json:"commands"` // The commands to be executed on the server
}

// Helper functions for creating common LSP structures

// NewPosition creates a new Position.
func NewPosition(line, character int) Position {
	return Position{Line: line, Character: character}
}

// NewRange creates a new Range.
func NewRange(startLine, startChar, endLine, endChar int) Range {
	return Range{
		Start: NewPosition(startLine, startChar),
		End:   NewPosition(endLine, endChar),
	}
}

// NewTextDocumentIdentifier creates a new TextDocumentIdentifier.
func NewTextDocumentIdentifier(uri string) TextDocumentIdentifier {
	return TextDocumentIdentifier{URI: uri}
}

// NewTextDocumentPositionParams creates new TextDocumentPositionParams.
func NewTextDocumentPositionParams(uri string, line, character int) TextDocumentPositionParams {
	return TextDocumentPositionParams{
		TextDocument: NewTextDocumentIdentifier(uri),
		Position:     NewPosition(line, character),
	}
}

// ToJSON converts a struct to JSON bytes.
func ToJSON(v interface{}) ([]byte, error) {
	return json.Marshal(v)
}

// FromJSON converts JSON bytes to a struct.
func FromJSON(data []byte, v interface{}) error {
	return json.Unmarshal(data, v)
}

