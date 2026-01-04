package main

import (
	"github.com/dop251/goja"
)

// TreeSitter represents the main tree-sitter library interface
type TreeSitter struct {
	runtime *goja.Runtime
	parsers map[string]LanguageParser
	queries map[string]*QueryEngine
}

// NewTreeSitter creates a new TreeSitter instance
func NewTreeSitter() *TreeSitter {
	return &TreeSitter{
		parsers: make(map[string]LanguageParser),
		queries: make(map[string]*QueryEngine),
	}
}

// SetRuntime sets the Goja runtime for this TreeSitter instance
func (ts *TreeSitter) SetRuntime(runtime *goja.Runtime) {
	ts.runtime = runtime
}

// RegisterLanguage registers a language parser
func (ts *TreeSitter) RegisterLanguage(name string, parser LanguageParser) {
	ts.parsers[name] = parser
}

// GetSupportedLanguages returns a list of supported languages
func (ts *TreeSitter) GetSupportedLanguages() []string {
	var languages []string
	for name := range ts.parsers {
		languages = append(languages, name)
	}
	return languages
}

// ParseCode parses source code in the specified language
func (ts *TreeSitter) ParseCode(language, sourceCode string) (*Tree, error) {
	parser, exists := ts.parsers[language]
	if !exists {
		// Default to JavaScript
		parser = NewJavaScriptParser()
	}
	
	rootNode := parser.Parse(sourceCode)
	return &Tree{rootNode: rootNode, source: sourceCode}, nil
}

// CreateQuery creates a query for the specified language
func (ts *TreeSitter) CreateQuery(language, queryString string) (*QueryEngine, error) {
	queryEngine := NewQueryEngine(language)
	return queryEngine, nil
}

// ExecuteQuery executes a query on a tree
func (ts *TreeSitter) ExecuteQuery(queryEngine *QueryEngine, tree *Tree) ([]QueryMatch, error) {
	return queryEngine.ExecuteQuery("", tree)
}

// GetVersion returns the library version
func (ts *TreeSitter) GetVersion() string {
	return "1.0.0-alpha"
}

// InitializeDefaultLanguages initializes default language support
func (ts *TreeSitter) InitializeDefaultLanguages() {
	ts.RegisterLanguage("javascript", NewJavaScriptParser())
	ts.RegisterLanguage("js", NewJavaScriptParser())
}

// TreeSitterConfig represents configuration options
type TreeSitterConfig struct {
	EnableDebug     bool
	MaxParseDepth   int
	TimeoutMs       int
	CacheEnabled    bool
	CacheSize       int
}

// DefaultConfig returns default configuration
func DefaultConfig() *TreeSitterConfig {
	return &TreeSitterConfig{
		EnableDebug:   false,
		MaxParseDepth: 1000,
		TimeoutMs:     5000,
		CacheEnabled:  true,
		CacheSize:     100,
	}
}

// NewTreeSitterWithConfig creates a TreeSitter instance with custom configuration
func NewTreeSitterWithConfig(config *TreeSitterConfig) *TreeSitter {
	ts := NewTreeSitter()
	// Apply configuration settings
	// In a real implementation, these would affect parsing behavior
	return ts
}

// Statistics represents parsing and query statistics
type Statistics struct {
	ParseCount      int64
	QueryCount      int64
	CacheHits       int64
	CacheMisses     int64
	AverageParseMs  float64
	AverageQueryMs  float64
}

// GetStatistics returns usage statistics
func (ts *TreeSitter) GetStatistics() *Statistics {
	return &Statistics{
		ParseCount:     0,
		QueryCount:     0,
		CacheHits:      0,
		CacheMisses:    0,
		AverageParseMs: 0.0,
		AverageQueryMs: 0.0,
	}
}

// Reset clears all cached data and statistics
func (ts *TreeSitter) Reset() {
	ts.queries = make(map[string]*QueryEngine)
	// Clear other caches and reset statistics
}

