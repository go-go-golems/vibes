package parser

import (
	"context"
	"path/filepath"
	"testing"
)

func TestParseFileSimple(t *testing.T) {
	// Get the absolute path to the test file relative to this test file's location
	// This makes the test runnable from any directory
	testFilePath, err := filepath.Abs("../../testdata/simple.go")
	if err != nil {
		t.Fatalf("Failed to get absolute path for test file: %v", err)
	}

	p := NewParser()
	ctx := context.Background()

	functions, err := p.ParseFile(ctx, testFilePath)
	if err != nil {
		t.Fatalf("ParseFile failed: %v", err)
	}

	expectedFuncs := map[string]struct{ Start, End int }{
		"Add":      {Start: 6, End: 8},
		"helper":   {Start: 10, End: 12},
		"Subtract": {Start: 15, End: 21},
	}

	if len(functions) != len(expectedFuncs) {
		t.Fatalf("Expected %d functions, but found %d", len(expectedFuncs), len(functions))
	}

	foundFuncs := make(map[string]FunctionInfo)
	for _, fn := range functions {
		foundFuncs[fn.Name] = fn
	}

	for name, expected := range expectedFuncs {
		found, ok := foundFuncs[name]
		if !ok {
			t.Errorf("Expected function '%s' not found", name)
			continue
		}
		if found.StartLine != expected.Start {
			t.Errorf("Function '%s': expected start line %d, got %d", name, expected.Start, found.StartLine)
		}
		if found.EndLine != expected.End {
			t.Errorf("Function '%s': expected end line %d, got %d", name, expected.End, found.EndLine)
		}
		if found.Filepath != testFilePath {
			t.Errorf("Function '%s': expected filepath '%s', got '%s'", name, testFilePath, found.Filepath)
		}
	}
}
