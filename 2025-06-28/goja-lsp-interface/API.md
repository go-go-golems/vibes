# API Documentation

## Go Package Documentation

### `pkg/lsp` - Core LSP Implementation

#### Types and Structures

The LSP package provides complete type definitions for the Language Server Protocol:

##### Core Position and Range Types

```go
type Position struct {
    Line      int `json:"line"`      // 0-based line number
    Character int `json:"character"` // 0-based character offset
}

type Range struct {
    Start Position `json:"start"`
    End   Position `json:"end"`
}

type Location struct {
    URI   string `json:"uri"`
    Range Range  `json:"range"`
}
```

##### Document Identification

```go
type TextDocumentIdentifier struct {
    URI string `json:"uri"`
}

type VersionedTextDocumentIdentifier struct {
    TextDocumentIdentifier
    Version int `json:"version"`
}

type TextDocumentItem struct {
    URI        string `json:"uri"`
    LanguageID string `json:"languageId"`
    Version    int    `json:"version"`
    Text       string `json:"text"`
}
```

##### LSP Request Parameters

```go
type TextDocumentPositionParams struct {
    TextDocument TextDocumentIdentifier `json:"textDocument"`
    Position     Position               `json:"position"`
}

type CompletionParams struct {
    TextDocumentPositionParams
    Context *CompletionContext `json:"context,omitempty"`
}
```

##### Response Types

```go
type CompletionItem struct {
    Label         string      `json:"label"`
    Kind          int         `json:"kind,omitempty"`
    Detail        string      `json:"detail,omitempty"`
    Documentation interface{} `json:"documentation,omitempty"`
    InsertText    string      `json:"insertText,omitempty"`
    // ... additional fields
}

type Hover struct {
    Contents interface{} `json:"contents"`
    Range    *Range      `json:"range,omitempty"`
}
```

#### Client Implementation

##### Client Structure

```go
type Client struct {
    cmd           *exec.Cmd
    stdin         io.WriteCloser
    stdout        io.ReadCloser
    stderr        io.ReadCloser
    nextID        int64
    pendingReqs   map[interface{}]chan *ResponseMessage
    capabilities  *ServerCapabilities
    initialized   bool
    // ... additional fields
}
```

##### Client Options

```go
type ClientOptions struct {
    Command       string
    Args          []string
    RootURI       string
    Logger        *log.Logger
    DebugMode     bool
    Env           []string
    WorkingDir    string
}
```

##### Core Client Methods

```go
func NewClient(opts ClientOptions) (*Client, error)
func (c *Client) Initialize() error
func (c *Client) DidOpen(uri, languageID, text string, version int) error
func (c *Client) Hover(uri string, line, character int) (*Hover, error)
func (c *Client) Completion(uri string, line, character int) ([]CompletionItem, error)
func (c *Client) Definition(uri string, line, character int) ([]Location, error)
func (c *Client) References(uri string, line, character int, includeDeclaration bool) ([]Location, error)
func (c *Client) Close() error
```

#### Utility Functions

##### URI and Path Conversion

```go
func FileToURI(path string) string
func URIToFile(uri string) (string, error)
```

##### Language Detection

```go
func GetLanguageID(path string) string
```

##### Text Processing

```go
func LineColumnToOffset(text string, line, column int) int
func OffsetToLineColumn(text string, offset int) (int, int)
func GetWordAtPosition(text string, line, column int) string
```

##### JSON Utilities

```go
func ToJSON(v interface{}) ([]byte, error)
func FromJSON(data []byte, v interface{}) error
```

##### Formatting Helpers

```go
func FormatHover(hover *Hover) string
func FormatLocation(location Location) string
func FormatCompletionItem(item CompletionItem) string
```

#### LSP Manager

The LSP Manager provides a higher-level interface for managing LSP operations:

```go
type LSPManager struct {
    client    *Client
    openFiles map[string]*OpenFile
}

type OpenFile struct {
    URI        string
    Path       string
    LanguageID string
    Version    int
    Content    string
}

func NewLSPManager(client *Client) *LSPManager
func (m *LSPManager) OpenFile(path string) (*OpenFile, error)
func (m *LSPManager) UpdateFile(uri, newContent string) error
func (m *LSPManager) SaveFile(uri string) error
func (m *LSPManager) GetHover(uri string, line, character int) (*Hover, error)
func (m *LSPManager) GetCompletion(uri string, line, character int) ([]CompletionItem, error)
func (m *LSPManager) GetDefinition(uri string, line, character int) ([]Location, error)
func (m *LSPManager) GetReferences(uri string, line, character int, includeDeclaration bool) ([]Location, error)
func (m *LSPManager) CloseFile(uri string) error
```

### `pkg/jslsp` - JavaScript Bindings

#### JavaScript LSP Client

```go
type JSLSPClient struct {
    client  *lsp.Client
    manager *lsp.LSPManager
    vm      *goja.Runtime
    logger  *log.Logger
}

type JSLSPOptions struct {
    Command    string   `json:"command"`
    Args       []string `json:"args"`
    RootPath   string   `json:"rootPath"`
    DebugMode  bool     `json:"debugMode"`
    WorkingDir string   `json:"workingDir"`
}
```

#### JavaScript Runtime

```go
type JSLSPRuntime struct {
    vm     *goja.Runtime
    client *JSLSPClient
}

func NewJSLSPRuntime() *JSLSPRuntime
func (jr *JSLSPRuntime) RunScript(script string) (goja.Value, error)
func (jr *JSLSPRuntime) RunFile(filename string) (goja.Value, error)
func (jr *JSLSPRuntime) SetGlobal(name string, value interface{})
func (jr *JSLSPRuntime) GetGlobal(name string) goja.Value
func (jr *JSLSPRuntime) Close() error
```

#### Standalone Environment

```go
func CreateStandaloneJSLSP(ctx context.Context) (*JSLSPRuntime, error)
```

## JavaScript API Reference

### Global Functions

#### LSP Client Creation

```javascript
createLSPClient(options) -> JSLSPClient
```

**Parameters:**
- `options.command` (string): Language server executable
- `options.args` (Array<string>): Command line arguments
- `options.rootPath` (string): Project root directory
- `options.debugMode` (boolean): Enable debug logging
- `options.workingDir` (string): Working directory

#### Utility Functions

```javascript
fileToURI(path) -> string
uriToFile(uri) -> string
getLanguageID(path) -> string
readFile(path) -> string
```

#### Console Functions

```javascript
console.log(...args)
console.error(...args)
```

#### Async Functions

```javascript
setTimeout(callback, delay)
```

### JSLSPClient Methods

#### Initialization

```javascript
Initialize() -> void
IsInitialized() -> boolean
GetCapabilities() -> Object
```

#### File Operations

```javascript
OpenFile(path) -> Object {
    uri: string,
    path: string,
    languageId: string,
    version: number,
    content: string
}

CloseFile(uri) -> void
GetOpenFiles() -> Array<Object>
```

#### LSP Features

```javascript
GetHover(uri, line, character) -> Object | null {
    contents: any,
    range: Object | null,
    text: string
}

GetCompletion(uri, line, character) -> Array<Object> {
    label: string,
    kind: number,
    detail: string,
    documentation: any,
    insertText: string,
    // ... additional fields
}

GetDefinition(uri, line, character) -> Array<Object> {
    uri: string,
    range: Object,
    path: string,
    text: string
}

GetReferences(uri, line, character, includeDeclaration) -> Array<Object> {
    uri: string,
    range: Object,
    path: string,
    text: string
}
```

#### Cleanup

```javascript
Close() -> void
```

### Data Structures

#### Position Object

```javascript
{
    line: number,      // 0-based line number
    character: number  // 0-based character position
}
```

#### Range Object

```javascript
{
    start: Position,
    end: Position
}
```

#### Location Object

```javascript
{
    uri: string,
    range: Range,
    path: string,  // Converted file path
    text: string   // Formatted location string
}
```

#### Server Capabilities

```javascript
{
    hoverProvider: boolean,
    completionProvider: boolean,
    definitionProvider: boolean,
    referencesProvider: boolean,
    documentSymbolProvider: boolean,
    workspaceSymbolProvider: boolean,
    codeActionProvider: boolean,
    documentFormattingProvider: boolean,
    renameProvider: boolean
}
```

## Error Handling

### Go Error Types

```go
type RPCError struct {
    Code    int         `json:"code"`
    Message string      `json:"message"`
    Data    interface{} `json:"data,omitempty"`
}

func (e *RPCError) Error() string
```

### JavaScript Error Handling

All JavaScript methods can throw errors. Use try-catch blocks:

```javascript
try {
    var client = createLSPClient(options);
    client.Initialize();
    // ... operations
} catch (error) {
    console.error("Error:", error.message);
} finally {
    if (client) {
        client.Close();
    }
}
```

## Protocol Details

### JSON-RPC Communication

The LSP client communicates with language servers using JSON-RPC 2.0 over stdio:

#### Request Format

```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "textDocument/hover",
    "params": {
        "textDocument": {
            "uri": "file:///path/to/file.go"
        },
        "position": {
            "line": 10,
            "character": 5
        }
    }
}
```

#### Response Format

```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "result": {
        "contents": {
            "kind": "markdown",
            "value": "Documentation text"
        }
    }
}
```

### Message Transport

Messages are transported using the Language Server Protocol message format:

```
Content-Length: 123\r\n
\r\n
{"jsonrpc":"2.0",...}
```

## Performance Optimization

### Connection Management

- Reuse LSP client instances
- Keep connections alive for multiple operations
- Close clients when done

### Memory Management

- Close files when no longer needed
- Monitor open file count
- Restart clients periodically for long-running applications

### Batch Operations

Group related operations to reduce round-trips:

```javascript
// Open multiple files at once
var files = [file1, file2, file3];
var openFiles = files.map(f => client.OpenFile(f));

// Perform operations on all files
openFiles.forEach(file => {
    var hover = client.GetHover(file.uri, 10, 5);
    // Process hover...
});
```

## Debugging

### Debug Mode

Enable debug logging in both Go and JavaScript:

```go
// Go
opts := lsp.ClientOptions{
    Command:   "gopls",
    DebugMode: true,
}
```

```javascript
// JavaScript
var client = createLSPClient({
    command: "gopls",
    debugMode: true
});
```

### Log Analysis

Debug logs show:
- JSON-RPC message exchange
- Server startup and shutdown
- Error conditions
- Performance metrics

### Common Debug Scenarios

1. **Server Not Starting**: Check command path and arguments
2. **No Response**: Verify message format and server capabilities
3. **Wrong Results**: Check file URIs and position coordinates
4. **Memory Leaks**: Monitor open files and client lifecycle

