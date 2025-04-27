package tools

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/goagent/framework/goagent/types"
	orderedmap "github.com/wk8/go-ordered-map/v2"
)

// WebSearchTool is a mock implementation of a web search tool
type WebSearchTool struct {
	results map[string][]SearchResult
}

// SearchResult represents a search result
type SearchResult struct {
	Title   string `json:"title"`
	URL     string `json:"url"`
	Snippet string `json:"snippet"`
}

// NewWebSearchTool creates a new WebSearchTool
func NewWebSearchTool() *WebSearchTool {
	return &WebSearchTool{
		results: make(map[string][]SearchResult),
	}
}

var _ Tool = &WebSearchTool{}

// AddSearchResults adds search results for a query
func (t *WebSearchTool) AddSearchResults(query string, results []SearchResult) {
	t.results[query] = results
}

// Name returns the name of the tool
func (t *WebSearchTool) Name() string {
	return "web_search"
}

// Description returns the description of the tool
func (t *WebSearchTool) Description() string {
	return "Search the web for information"
}

// Execute executes the tool with the given input
func (t *WebSearchTool) Execute(ctx context.Context, input string) (string, error) {
	query := struct {
		Query string `json:"query"`
	}{
		Query: input,
	}

	if err := json.Unmarshal([]byte(input), &query); err != nil {
		return "", fmt.Errorf("invalid input: %w", err)
	}

	results, ok := t.results[query.Query]
	if !ok {
		return "No results found", nil
	}

	resultJSON, err := json.MarshalIndent(results, "", "  ")
	if err != nil {
		return "", err
	}

	return string(resultJSON), nil
}

// Parameters returns the parameters schema of the tool
func (t *WebSearchTool) Parameters() *orderedmap.OrderedMap[string, types.ParameterSchema] {
	om := orderedmap.New[string, types.ParameterSchema]()
	om.Set("query", types.ParameterSchema{
		Type:        "string",
		Description: "The search query",
		Required:    true,
	})
	return om
}
