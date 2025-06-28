// Package lsp provides utilities for working with Language Server Protocol.
package lsp

import (
	"fmt"
	"io/ioutil"
	"net/url"
	"os"
	"path/filepath"
	"strings"
)

// Utility functions for common LSP operations

// FileToURI converts a file path to a URI.
func FileToURI(path string) string {
	absPath, err := filepath.Abs(path)
	if err != nil {
		// Fallback to the original path if we can't get absolute path
		absPath = path
	}
	
	// Convert to URI format
	u := url.URL{
		Scheme: "file",
		Path:   filepath.ToSlash(absPath),
	}
	
	return u.String()
}

// URIToFile converts a URI to a file path.
func URIToFile(uri string) (string, error) {
	u, err := url.Parse(uri)
	if err != nil {
		return "", fmt.Errorf("invalid URI: %w", err)
	}
	
	if u.Scheme != "file" {
		return "", fmt.Errorf("unsupported URI scheme: %s", u.Scheme)
	}
	
	return filepath.FromSlash(u.Path), nil
}

// ReadFileContent reads the content of a file and returns it as a string.
func ReadFileContent(path string) (string, error) {
	content, err := ioutil.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("failed to read file %s: %w", path, err)
	}
	return string(content), nil
}

// GetLanguageID determines the language ID based on file extension.
func GetLanguageID(path string) string {
	ext := strings.ToLower(filepath.Ext(path))
	switch ext {
	case ".go":
		return "go"
	case ".js":
		return "javascript"
	case ".ts":
		return "typescript"
	case ".py":
		return "python"
	case ".java":
		return "java"
	case ".c":
		return "c"
	case ".cpp", ".cc", ".cxx":
		return "cpp"
	case ".cs":
		return "csharp"
	case ".php":
		return "php"
	case ".rb":
		return "ruby"
	case ".rs":
		return "rust"
	case ".swift":
		return "swift"
	case ".kt":
		return "kotlin"
	case ".scala":
		return "scala"
	case ".sh":
		return "shellscript"
	case ".json":
		return "json"
	case ".xml":
		return "xml"
	case ".html":
		return "html"
	case ".css":
		return "css"
	case ".scss":
		return "scss"
	case ".less":
		return "less"
	case ".yaml", ".yml":
		return "yaml"
	case ".toml":
		return "toml"
	case ".md":
		return "markdown"
	case ".tex":
		return "latex"
	case ".sql":
		return "sql"
	default:
		return "plaintext"
	}
}

// LSPManager provides a high-level interface for LSP operations.
type LSPManager struct {
	client    *Client
	openFiles map[string]*OpenFile
}

// OpenFile represents an open file in the LSP session.
type OpenFile struct {
	URI        string
	Path       string
	LanguageID string
	Version    int
	Content    string
}

// NewLSPManager creates a new LSP manager.
func NewLSPManager(client *Client) *LSPManager {
	return &LSPManager{
		client:    client,
		openFiles: make(map[string]*OpenFile),
	}
}

// OpenFile opens a file in the LSP session.
func (m *LSPManager) OpenFile(path string) (*OpenFile, error) {
	// Check if file exists
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return nil, fmt.Errorf("file does not exist: %s", path)
	}
	
	// Read file content
	content, err := ReadFileContent(path)
	if err != nil {
		return nil, err
	}
	
	// Create file info
	uri := FileToURI(path)
	languageID := GetLanguageID(path)
	
	openFile := &OpenFile{
		URI:        uri,
		Path:       path,
		LanguageID: languageID,
		Version:    1,
		Content:    content,
	}
	
	// Send didOpen notification
	if err := m.client.DidOpen(uri, languageID, content, openFile.Version); err != nil {
		return nil, fmt.Errorf("failed to send didOpen notification: %w", err)
	}
	
	// Store in open files
	m.openFiles[uri] = openFile
	
	return openFile, nil
}

// UpdateFile updates the content of an open file.
func (m *LSPManager) UpdateFile(uri, newContent string) error {
	openFile, exists := m.openFiles[uri]
	if !exists {
		return fmt.Errorf("file not open: %s", uri)
	}
	
	openFile.Version++
	openFile.Content = newContent
	
	// Send didChange notification
	return m.client.DidChange(uri, openFile.Version, newContent)
}

// SaveFile saves an open file.
func (m *LSPManager) SaveFile(uri string) error {
	openFile, exists := m.openFiles[uri]
	if !exists {
		return fmt.Errorf("file not open: %s", uri)
	}
	
	// Send didSave notification
	return m.client.DidSave(uri, &openFile.Content)
}

// GetHover gets hover information for a position in a file.
func (m *LSPManager) GetHover(uri string, line, character int) (*Hover, error) {
	if _, exists := m.openFiles[uri]; !exists {
		return nil, fmt.Errorf("file not open: %s", uri)
	}
	
	return m.client.Hover(uri, line, character)
}

// GetCompletion gets completion items for a position in a file.
func (m *LSPManager) GetCompletion(uri string, line, character int) ([]CompletionItem, error) {
	if _, exists := m.openFiles[uri]; !exists {
		return nil, fmt.Errorf("file not open: %s", uri)
	}
	
	return m.client.Completion(uri, line, character)
}

// GetDefinition gets definition locations for a position in a file.
func (m *LSPManager) GetDefinition(uri string, line, character int) ([]Location, error) {
	if _, exists := m.openFiles[uri]; !exists {
		return nil, fmt.Errorf("file not open: %s", uri)
	}
	
	return m.client.Definition(uri, line, character)
}

// GetReferences gets reference locations for a position in a file.
func (m *LSPManager) GetReferences(uri string, line, character int, includeDeclaration bool) ([]Location, error) {
	if _, exists := m.openFiles[uri]; !exists {
		return nil, fmt.Errorf("file not open: %s", uri)
	}
	
	return m.client.References(uri, line, character, includeDeclaration)
}

// GetOpenFiles returns a list of currently open files.
func (m *LSPManager) GetOpenFiles() []*OpenFile {
	files := make([]*OpenFile, 0, len(m.openFiles))
	for _, file := range m.openFiles {
		files = append(files, file)
	}
	return files
}

// GetOpenFile returns information about an open file.
func (m *LSPManager) GetOpenFile(uri string) (*OpenFile, bool) {
	file, exists := m.openFiles[uri]
	return file, exists
}

// CloseFile closes a file in the LSP session.
func (m *LSPManager) CloseFile(uri string) error {
	if _, exists := m.openFiles[uri]; !exists {
		return fmt.Errorf("file not open: %s", uri)
	}
	
	// Send didClose notification
	if err := m.client.sendNotification("textDocument/didClose", struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
	}{
		TextDocument: TextDocumentIdentifier{URI: uri},
	}); err != nil {
		return fmt.Errorf("failed to send didClose notification: %w", err)
	}
	
	// Remove from open files
	delete(m.openFiles, uri)
	
	return nil
}

// Close closes the LSP manager and all open files.
func (m *LSPManager) Close() error {
	// Close all open files
	for uri := range m.openFiles {
		if err := m.CloseFile(uri); err != nil {
			// Log error but continue closing other files
			fmt.Printf("Error closing file %s: %v\n", uri, err)
		}
	}
	
	return nil
}

// Helper functions for working with LSP data structures

// FormatHover formats hover information as a readable string.
func FormatHover(hover *Hover) string {
	if hover == nil {
		return ""
	}
	
	switch content := hover.Contents.(type) {
	case string:
		return content
	case map[string]interface{}:
		if kind, ok := content["kind"].(string); ok {
			if value, ok := content["value"].(string); ok {
				if kind == "markdown" {
					return value
				}
				return value
			}
		}
		return fmt.Sprintf("%v", content)
	case []interface{}:
		var parts []string
		for _, item := range content {
			if str, ok := item.(string); ok {
				parts = append(parts, str)
			} else if obj, ok := item.(map[string]interface{}); ok {
				if value, ok := obj["value"].(string); ok {
					parts = append(parts, value)
				}
			}
		}
		return strings.Join(parts, "\n")
	default:
		return fmt.Sprintf("%v", content)
	}
}

// FormatLocation formats a location as a readable string.
func FormatLocation(location Location) string {
	path, err := URIToFile(location.URI)
	if err != nil {
		path = location.URI
	}
	
	return fmt.Sprintf("%s:%d:%d", path, location.Range.Start.Line+1, location.Range.Start.Character+1)
}

// FormatCompletionItem formats a completion item as a readable string.
func FormatCompletionItem(item CompletionItem) string {
	result := item.Label
	if item.Detail != "" {
		result += " - " + item.Detail
	}
	return result
}

// LineColumnToOffset converts line/column position to byte offset in text.
func LineColumnToOffset(text string, line, column int) int {
	lines := strings.Split(text, "\n")
	if line >= len(lines) {
		return len(text)
	}
	
	offset := 0
	for i := 0; i < line; i++ {
		offset += len(lines[i]) + 1 // +1 for newline
	}
	
	if column > len(lines[line]) {
		column = len(lines[line])
	}
	
	return offset + column
}

// OffsetToLineColumn converts byte offset to line/column position in text.
func OffsetToLineColumn(text string, offset int) (int, int) {
	if offset >= len(text) {
		lines := strings.Split(text, "\n")
		lastLine := len(lines) - 1
		return lastLine, len(lines[lastLine])
	}
	
	line := 0
	column := 0
	
	for i, char := range text {
		if i == offset {
			break
		}
		
		if char == '\n' {
			line++
			column = 0
		} else {
			column++
		}
	}
	
	return line, column
}

// GetWordAtPosition gets the word at a specific position in text.
func GetWordAtPosition(text string, line, column int) string {
	lines := strings.Split(text, "\n")
	if line >= len(lines) {
		return ""
	}
	
	lineText := lines[line]
	if column >= len(lineText) {
		return ""
	}
	
	// Find word boundaries
	start := column
	end := column
	
	// Move start backwards to beginning of word
	for start > 0 && isWordChar(rune(lineText[start-1])) {
		start--
	}
	
	// Move end forwards to end of word
	for end < len(lineText) && isWordChar(rune(lineText[end])) {
		end++
	}
	
	if start == end {
		return ""
	}
	
	return lineText[start:end]
}

// isWordChar checks if a character is part of a word.
func isWordChar(r rune) bool {
	return (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') || r == '_'
}

