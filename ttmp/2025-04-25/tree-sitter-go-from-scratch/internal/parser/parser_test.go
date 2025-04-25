package parser

import (
	"context"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestParseFileSimple(t *testing.T) {
	// Get the absolute path to the test file relative to this test file's location
	// This makes the test runnable from any directory
	testFilePath, err := filepath.Abs("../../testdata/simple.go")
	if err != nil {
		t.Fatalf("Failed to get absolute path for test file: %v", err)
	}

	p, err := NewParser()
	if err != nil {
		t.Fatalf("NewParser failed: %v", err)
	}
	defer p.Close() // Ensure resources are released

	ctx := context.Background()

	functions, pkgName, err := p.ParseFileAndPackage(ctx, testFilePath)
	if err != nil {
		t.Fatalf("ParseFileAndPackage failed: %v", err)
	}

	// --- Check Package Name ---
	expectedPackage := "simple"
	if pkgName != expectedPackage {
		t.Errorf("Expected package name '%s', got '%s'", expectedPackage, pkgName)
	}

	// --- Check Functions ---
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

// TestParseFileEnhanced tests the detailed extraction using the new ParseFileEnhanced.
func TestParseFileEnhanced(t *testing.T) {
	p, err := NewParser()
	require.NoError(t, err)
	require.NotNil(t, p)
	defer p.Close()

	ctx := context.Background()
	testFile := "../../testdata/detailed.go"
	absPath, err := filepath.Abs(testFile)
	require.NoError(t, err)

	fileInfo, err := p.ParseFileEnhanced(ctx, absPath)
	require.NoError(t, err)
	require.NotNil(t, fileInfo)

	assert.Equal(t, "detailed", fileInfo.Package, "Package name mismatch")
	require.Len(t, fileInfo.Functions, 6, "Should find 6 functions/methods")

	funcMap := make(map[string]EnhancedFunctionInfo)
	for _, f := range fileInfo.Functions {
		funcMap[f.Name] = f
	}

	// --- Test Point.String() method ---
	stringMethod, ok := funcMap["String"]
	require.True(t, ok, "Method 'String' not found")
	assert.True(t, stringMethod.IsMethod, "String should be a method")
	assert.True(t, stringMethod.IsExported, "String should be exported")
	require.NotNil(t, stringMethod.Receiver, "String receiver should not be nil")
	assert.Equal(t, "p", stringMethod.Receiver.Name, "String receiver name mismatch")
	assert.Equal(t, "Point", stringMethod.Receiver.Type, "String receiver type mismatch")
	assert.False(t, stringMethod.Receiver.Pointer, "String receiver should not be a pointer")
	assert.Empty(t, stringMethod.Parameters, "String should have no parameters")
	assert.Len(t, stringMethod.ReturnTypes, 1, "String should have one return type")
	assert.Equal(t, "string", stringMethod.ReturnTypes[0], "String return type mismatch")
	assert.Contains(t, stringMethod.Signature, "func (p Point) String() string", "String signature mismatch")
	assert.Len(t, stringMethod.Comments, 2, "String should have 2 comment lines")
	assert.Equal(t, "String returns a string representation of the Point.", stringMethod.Comments[0])
	assert.Equal(t, "This is a method on the Point struct.", stringMethod.Comments[1])
	assert.Equal(t, 17, stringMethod.StartLine, "String start line mismatch")
	assert.Equal(t, 19, stringMethod.EndLine, "String end line mismatch")

	// --- Test Point.Move() method ---
	moveMethod, ok := funcMap["Move"]
	require.True(t, ok, "Method 'Move' not found")
	assert.True(t, moveMethod.IsMethod, "Move should be a method")
	assert.True(t, moveMethod.IsExported, "Move should be exported")
	require.NotNil(t, moveMethod.Receiver, "Move receiver should not be nil")
	assert.Equal(t, "p", moveMethod.Receiver.Name, "Move receiver name mismatch")
	assert.Equal(t, "Point", moveMethod.Receiver.Type, "Move receiver type mismatch") // Type query gets underlying type
	assert.True(t, moveMethod.Receiver.Pointer, "Move receiver should be a pointer")
	require.Len(t, moveMethod.Parameters, 2, "Move should have 2 parameters")
	assert.Equal(t, "dx", moveMethod.Parameters[0].Name)
	assert.Equal(t, "int", moveMethod.Parameters[0].Type)
	assert.Equal(t, "dy", moveMethod.Parameters[1].Name)
	assert.Equal(t, "int", moveMethod.Parameters[1].Type)
	assert.Empty(t, moveMethod.ReturnTypes, "Move should have no return types")
	assert.Len(t, moveMethod.Comments, 3, "Move should have 3 comment lines")
	assert.Equal(t, 22, moveMethod.StartLine)
	assert.Equal(t, 27, moveMethod.EndLine)

	// --- Test Add() function ---
	addFunc, ok := funcMap["Add"]
	require.True(t, ok, "Function 'Add' not found")
	assert.False(t, addFunc.IsMethod, "Add should not be a method")
	assert.True(t, addFunc.IsExported, "Add should be exported")
	assert.Nil(t, addFunc.Receiver, "Add receiver should be nil")
	require.Len(t, addFunc.Parameters, 2, "Add should have 2 parameters")
	assert.Equal(t, "p1", addFunc.Parameters[0].Name)
	assert.Equal(t, "Point", addFunc.Parameters[0].Type)
	assert.Equal(t, "p2", addFunc.Parameters[1].Name)
	assert.Equal(t, "Point", addFunc.Parameters[1].Type)
	assert.Len(t, addFunc.ReturnTypes, 1, "Add should have one return type")
	assert.Equal(t, "Point", addFunc.ReturnTypes[0], "Add return type mismatch")
	assert.Len(t, addFunc.Comments, 2, "Add comments mismatch")
	assert.Equal(t, 30, addFunc.StartLine)
	assert.Equal(t, 33, addFunc.EndLine)

	// --- Test ProcessData() function ---
	processFunc, ok := funcMap["ProcessData"]
	require.True(t, ok, "Function 'ProcessData' not found")
	assert.False(t, processFunc.IsMethod, "ProcessData should not be a method")
	assert.True(t, processFunc.IsExported, "ProcessData should be exported")
	require.Len(t, processFunc.Parameters, 1, "ProcessData should have 1 parameter")
	assert.Equal(t, "r", processFunc.Parameters[0].Name)
	assert.Equal(t, "io.Reader", processFunc.Parameters[0].Type) // Type includes package
	assert.Len(t, processFunc.ReturnTypes, 2, "ProcessData should have 2 return types")
	assert.Equal(t, "int", processFunc.ReturnTypes[0])
	assert.Equal(t, "error", processFunc.ReturnTypes[1])
	assert.Len(t, processFunc.Comments, 2, "ProcessData comments mismatch")
	assert.Equal(t, 36, processFunc.StartLine)
	assert.Equal(t, 44, processFunc.EndLine)

	// --- Test internalHelper() function ---
	helperFunc, ok := funcMap["internalHelper"]
	require.True(t, ok, "Function 'internalHelper' not found")
	assert.False(t, helperFunc.IsMethod, "internalHelper should not be a method")
	assert.False(t, helperFunc.IsExported, "internalHelper should not be exported")
	assert.Len(t, helperFunc.Parameters, 1, "internalHelper should have 1 parameter")
	assert.Equal(t, "msg", helperFunc.Parameters[0].Name)
	assert.Equal(t, "string", helperFunc.Parameters[0].Type)
	assert.Empty(t, helperFunc.ReturnTypes, "internalHelper should have no return types")
	assert.Len(t, helperFunc.Comments, 1, "internalHelper comments mismatch")
	assert.Equal(t, 47, helperFunc.StartLine)
	assert.Equal(t, 49, helperFunc.EndLine)

	// --- Test VariadicFunction() function ---
	variadicFunc, ok := funcMap["VariadicFunction"]
	require.True(t, ok, "Function 'VariadicFunction' not found")
	assert.False(t, variadicFunc.IsMethod, "VariadicFunction should not be a method")
	assert.True(t, variadicFunc.IsExported, "VariadicFunction should be exported")
	require.Len(t, variadicFunc.Parameters, 2, "VariadicFunction should have 2 parameters listed by query")
	// Note: Tree-sitter query currently sees `values ...string` as one param decl
	assert.Equal(t, "prefix", variadicFunc.Parameters[0].Name)
	assert.Equal(t, "string", variadicFunc.Parameters[0].Type)
	assert.Equal(t, "values", variadicFunc.Parameters[1].Name)
	assert.Equal(t, "...string", variadicFunc.Parameters[1].Type) // Type includes ...
	assert.Len(t, variadicFunc.ReturnTypes, 1, "VariadicFunction should have 1 return type")
	assert.Equal(t, "string", variadicFunc.ReturnTypes[0])
	assert.Len(t, variadicFunc.Comments, 1, "VariadicFunction comments mismatch")
	assert.Equal(t, 52, variadicFunc.StartLine)
	assert.Equal(t, 54, variadicFunc.EndLine)
}
