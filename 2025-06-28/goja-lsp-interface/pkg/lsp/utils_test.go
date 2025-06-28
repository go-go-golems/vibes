// Package lsp provides tests for LSP utilities.
package lsp

import (
	"testing"
)

// TestFileToURI tests URI conversion functionality.
func TestFileToURI(t *testing.T) {
	testCases := []struct {
		input    string
		expected string
	}{
		{"/home/user/file.go", "file:///home/user/file.go"},
		{"./relative/path.go", "file://"},  // Will be converted to absolute
		{"file.go", "file://"},             // Will be converted to absolute
	}
	
	for _, tc := range testCases {
		result := FileToURI(tc.input)
		if result == "" {
			t.Errorf("FileToURI(%s) returned empty string", tc.input)
		}
		// Basic validation - should start with file://
		if len(result) < 7 || result[:7] != "file://" {
			t.Errorf("FileToURI(%s) = %s, expected to start with 'file://'", tc.input, result)
		}
	}
}

// TestURIToFile tests file path conversion functionality.
func TestURIToFile(t *testing.T) {
	testCases := []struct {
		input       string
		expectError bool
	}{
		{"file:///home/user/file.go", false},
		{"file://localhost/home/user/file.go", false},
		{"http://example.com/file.go", true},  // Should error on non-file scheme
		{"invalid-uri", true},                 // Should error on invalid URI
	}
	
	for _, tc := range testCases {
		result, err := URIToFile(tc.input)
		if tc.expectError {
			if err == nil {
				t.Errorf("URIToFile(%s) expected error but got none", tc.input)
			}
		} else {
			if err != nil {
				t.Errorf("URIToFile(%s) unexpected error: %v", tc.input, err)
			}
			if result == "" {
				t.Errorf("URIToFile(%s) returned empty string", tc.input)
			}
		}
	}
}

// TestGetLanguageID tests language ID detection.
func TestGetLanguageID(t *testing.T) {
	testCases := []struct {
		input    string
		expected string
	}{
		{"file.go", "go"},
		{"script.js", "javascript"},
		{"style.css", "css"},
		{"document.md", "markdown"},
		{"config.json", "json"},
		{"unknown.xyz", "plaintext"},
		{"", "plaintext"},
	}
	
	for _, tc := range testCases {
		result := GetLanguageID(tc.input)
		if result != tc.expected {
			t.Errorf("GetLanguageID(%s) = %s, expected %s", tc.input, result, tc.expected)
		}
	}
}

// TestNewPosition tests Position creation.
func TestNewPosition(t *testing.T) {
	pos := NewPosition(10, 5)
	if pos.Line != 10 {
		t.Errorf("NewPosition(10, 5).Line = %d, expected 10", pos.Line)
	}
	if pos.Character != 5 {
		t.Errorf("NewPosition(10, 5).Character = %d, expected 5", pos.Character)
	}
}

// TestNewRange tests Range creation.
func TestNewRange(t *testing.T) {
	r := NewRange(1, 2, 3, 4)
	if r.Start.Line != 1 || r.Start.Character != 2 {
		t.Errorf("NewRange(1,2,3,4).Start = {%d,%d}, expected {1,2}", r.Start.Line, r.Start.Character)
	}
	if r.End.Line != 3 || r.End.Character != 4 {
		t.Errorf("NewRange(1,2,3,4).End = {%d,%d}, expected {3,4}", r.End.Line, r.End.Character)
	}
}

// TestNewTextDocumentIdentifier tests TextDocumentIdentifier creation.
func TestNewTextDocumentIdentifier(t *testing.T) {
	uri := "file:///test.go"
	doc := NewTextDocumentIdentifier(uri)
	if doc.URI != uri {
		t.Errorf("NewTextDocumentIdentifier(%s).URI = %s, expected %s", uri, doc.URI, uri)
	}
}

// TestNewTextDocumentPositionParams tests TextDocumentPositionParams creation.
func TestNewTextDocumentPositionParams(t *testing.T) {
	uri := "file:///test.go"
	line, char := 5, 10
	params := NewTextDocumentPositionParams(uri, line, char)
	
	if params.TextDocument.URI != uri {
		t.Errorf("NewTextDocumentPositionParams URI = %s, expected %s", params.TextDocument.URI, uri)
	}
	if params.Position.Line != line {
		t.Errorf("NewTextDocumentPositionParams Line = %d, expected %d", params.Position.Line, line)
	}
	if params.Position.Character != char {
		t.Errorf("NewTextDocumentPositionParams Character = %d, expected %d", params.Position.Character, char)
	}
}

// TestFormatLocation tests location formatting.
func TestFormatLocation(t *testing.T) {
	loc := Location{
		URI: "file:///home/user/test.go",
		Range: Range{
			Start: Position{Line: 9, Character: 4},
			End:   Position{Line: 9, Character: 10},
		},
	}
	
	result := FormatLocation(loc)
	// Should contain line and character information (1-based)
	if result == "" {
		t.Error("FormatLocation returned empty string")
	}
	// Basic check - should contain line number
	if len(result) < 5 {
		t.Errorf("FormatLocation result too short: %s", result)
	}
}

// TestFormatCompletionItem tests completion item formatting.
func TestFormatCompletionItem(t *testing.T) {
	item := CompletionItem{
		Label:  "testFunction",
		Detail: "func() string",
	}
	
	result := FormatCompletionItem(item)
	if result != "testFunction - func() string" {
		t.Errorf("FormatCompletionItem = %s, expected 'testFunction - func() string'", result)
	}
	
	// Test without detail
	item2 := CompletionItem{
		Label: "simpleItem",
	}
	
	result2 := FormatCompletionItem(item2)
	if result2 != "simpleItem" {
		t.Errorf("FormatCompletionItem = %s, expected 'simpleItem'", result2)
	}
}

// TestLineColumnToOffset tests position conversion.
func TestLineColumnToOffset(t *testing.T) {
	text := "line1\nline2\nline3"
	
	testCases := []struct {
		line, column int
		expected     int
	}{
		{0, 0, 0},     // Beginning of first line
		{0, 5, 5},     // End of first line
		{1, 0, 6},     // Beginning of second line
		{1, 5, 11},    // End of second line
		{2, 0, 12},    // Beginning of third line
	}
	
	for _, tc := range testCases {
		result := LineColumnToOffset(text, tc.line, tc.column)
		if result != tc.expected {
			t.Errorf("LineColumnToOffset(text, %d, %d) = %d, expected %d", tc.line, tc.column, result, tc.expected)
		}
	}
}

// TestOffsetToLineColumn tests offset conversion.
func TestOffsetToLineColumn(t *testing.T) {
	text := "line1\nline2\nline3"
	
	testCases := []struct {
		offset       int
		expectedLine int
		expectedCol  int
	}{
		{0, 0, 0},   // Beginning of first line
		{5, 0, 5},   // End of first line
		{6, 1, 0},   // Beginning of second line
		{11, 1, 5},  // End of second line
		{12, 2, 0},  // Beginning of third line
	}
	
	for _, tc := range testCases {
		line, col := OffsetToLineColumn(text, tc.offset)
		if line != tc.expectedLine || col != tc.expectedCol {
			t.Errorf("OffsetToLineColumn(text, %d) = (%d, %d), expected (%d, %d)", 
				tc.offset, line, col, tc.expectedLine, tc.expectedCol)
		}
	}
}

// TestGetWordAtPosition tests word extraction.
func TestGetWordAtPosition(t *testing.T) {
	text := "func main() {\n    fmt.Println(\"hello\")\n}"
	
	testCases := []struct {
		line, column int
		expected     string
	}{
		{0, 0, "func"},     // Beginning of "func"
		{0, 2, "func"},     // Middle of "func"
		{0, 5, "main"},     // Beginning of "main"
		{1, 4, "fmt"},      // Beginning of "fmt"
		{1, 8, "Println"},  // Beginning of "Println"
		{1, 20, "hello"},   // In string literal (extracts word)
	}
	
	for _, tc := range testCases {
		result := GetWordAtPosition(text, tc.line, tc.column)
		if result != tc.expected {
			t.Errorf("GetWordAtPosition(text, %d, %d) = %s, expected %s", 
				tc.line, tc.column, result, tc.expected)
		}
	}
}

// TestToJSON tests JSON marshaling.
func TestToJSON(t *testing.T) {
	pos := Position{Line: 5, Character: 10}
	data, err := ToJSON(pos)
	if err != nil {
		t.Errorf("ToJSON failed: %v", err)
	}
	
	expected := `{"line":5,"character":10}`
	if string(data) != expected {
		t.Errorf("ToJSON = %s, expected %s", string(data), expected)
	}
}

// TestFromJSON tests JSON unmarshaling.
func TestFromJSON(t *testing.T) {
	data := []byte(`{"line":5,"character":10}`)
	var pos Position
	
	err := FromJSON(data, &pos)
	if err != nil {
		t.Errorf("FromJSON failed: %v", err)
	}
	
	if pos.Line != 5 || pos.Character != 10 {
		t.Errorf("FromJSON result = {%d, %d}, expected {5, 10}", pos.Line, pos.Character)
	}
}

