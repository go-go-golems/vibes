package extractor

import (
	"encoding/json"
	"fmt"
	"regexp"
	"strings"

	"github.com/fact-extraction/go-extractor/pkg/types"
)

// ResultParser parses LLM responses into structured extraction results
type ResultParser struct{}

// NewResultParser creates a new result parser
func NewResultParser() *ResultParser {
	return &ResultParser{}
}

// Parse extracts JSON from the assistant response and parses it
func (rp *ResultParser) Parse(assistantText string) (*types.ExtractionResponse, error) {
	// Extract JSON from the response (it might be wrapped in markdown code blocks)
	jsonStr := rp.extractJSON(assistantText)
	if jsonStr == "" {
		return nil, fmt.Errorf("no JSON found in response")
	}

	// Parse JSON
	var response types.ExtractionResponse
	if err := json.Unmarshal([]byte(jsonStr), &response); err != nil {
		return nil, fmt.Errorf("failed to parse JSON: %w", err)
	}

	// Validate triples
	validTriples := make([]types.RDFTriple, 0)
	for _, triple := range response.Triples {
		if triple.Actor == "" || triple.Action == "" {
			continue // Skip invalid triples
		}
		validTriples = append(validTriples, triple)
	}

	response.Triples = validTriples
	return &response, nil
}

// extractJSON extracts JSON from text that might contain markdown code blocks
func (rp *ResultParser) extractJSON(text string) string {
	// Try to find JSON in markdown code blocks
	codeBlockRegex := regexp.MustCompile("```(?:json)?\\s*\\n?([\\s\\S]*?)```")
	matches := codeBlockRegex.FindStringSubmatch(text)
	if len(matches) > 1 {
		return strings.TrimSpace(matches[1])
	}

	// Try to find raw JSON (look for { ... })
	jsonRegex := regexp.MustCompile(`\{[\s\S]*\}`)
	match := jsonRegex.FindString(text)
	if match != "" {
		return match
	}

	return ""
}
