package analysis

import (
	"context"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/your-org/go-analyzer/internal/parser"
)

func TestAnalyzeDirectory(t *testing.T) {
	p, err := parser.NewParser()
	require.NoError(t, err, "NewParser should succeed")
	require.NotNil(t, p, "Parser should be initialized")
	defer p.Close()

	testdataDir := "../../testdata" // Relative path from analyzer_test.go
	absTestdataDir, err := filepath.Abs(testdataDir)
	require.NoError(t, err, "Should get absolute path for testdata")

	// Define expected function names for verification
	expectedSimpleFuncs := map[string]bool{"Add": true, "helper": true, "Subtract": true}
	expectedSubpkgFuncs := map[string]bool{"Greet": true, "internalHelper": true}

	// Exclude nothing for this test
	excludeDirs := []string{}

	result, err := AnalyzeDirectory(context.Background(), testdataDir, p, excludeDirs)
	require.NoError(t, err, "AnalyzeDirectory should succeed")
	require.NotNil(t, result, "Result should not be nil")

	assert.Equal(t, absTestdataDir, result.BasePath, "BasePath should be the absolute path of testdata")
	require.Len(t, result.Files, 2, "Should find analysis results for 2 files")

	foundSimple := false
	foundSubpkg := false

	for _, fileAnalysis := range result.Files {
		absAnalysisPath, err := filepath.Abs(filepath.Join(result.BasePath, fileAnalysis.Path))
		require.NoError(t, err, "Should get absolute path for analysis result")

		relPath, err := filepath.Rel(absTestdataDir, absAnalysisPath)
		require.NoError(t, err, "Should get relative path for assertion")

		switch relPath {
		case "simple.go":
			foundSimple = true
			assert.Equal(t, "simple", fileAnalysis.Package, "Package name for simple.go should be 'simple'")
			assert.Len(t, fileAnalysis.Functions, len(expectedSimpleFuncs), "Incorrect number of functions found in simple.go")
			foundFuncNames := make(map[string]bool)
			for _, fn := range fileAnalysis.Functions {
				foundFuncNames[fn.Name] = true
				// Basic check for line numbers
				assert.Greater(t, fn.StartLine, 0, "StartLine should be positive")
				assert.GreaterOrEqual(t, fn.EndLine, fn.StartLine, "EndLine should be >= StartLine")
				assert.Equal(t, absAnalysisPath, fn.Filepath, "Function filepath should match absolute FileAnalysis path")
			}
			assert.Equal(t, expectedSimpleFuncs, foundFuncNames, "Function names mismatch in simple.go")

		case filepath.Join("subpkg", "another.go"): // Use filepath.Join for OS independence
			foundSubpkg = true
			assert.Equal(t, "subpkg", fileAnalysis.Package, "Package name for another.go should be 'subpkg'")
			assert.Len(t, fileAnalysis.Functions, len(expectedSubpkgFuncs), "Incorrect number of functions found in subpkg/another.go")
			foundFuncNames := make(map[string]bool)
			for _, fn := range fileAnalysis.Functions {
				foundFuncNames[fn.Name] = true
				assert.Greater(t, fn.StartLine, 0, "StartLine should be positive")
				assert.GreaterOrEqual(t, fn.EndLine, fn.StartLine, "EndLine should be >= StartLine")
				assert.Equal(t, absAnalysisPath, fn.Filepath, "Function filepath should match absolute FileAnalysis path")
			}
			assert.Equal(t, expectedSubpkgFuncs, foundFuncNames, "Function names mismatch in subpkg/another.go")

		default:
			t.Errorf("Unexpected file found in analysis results: %s (Absolute: %s)", relPath, absAnalysisPath)
		}
	}

	assert.True(t, foundSimple, "Analysis for simple.go not found")
	assert.True(t, foundSubpkg, "Analysis for subpkg/another.go not found")
}

func TestAnalyzeDirectoryWithExclusions(t *testing.T) {
	p, err := parser.NewParser()
	require.NoError(t, err, "NewParser should succeed")
	require.NotNil(t, p)
	defer p.Close()

	testdataDir := "../../testdata"
	absTestdataDir, err := filepath.Abs(testdataDir)
	require.NoError(t, err)

	// Exclude the subpkg directory
	excludeDirs := []string{"subpkg"}

	result, err := AnalyzeDirectory(context.Background(), testdataDir, p, excludeDirs)
	require.NoError(t, err)
	require.NotNil(t, result)

	assert.Equal(t, absTestdataDir, result.BasePath)
	// Should only find the top-level simple.go file
	require.Len(t, result.Files, 1, "Should find analysis results for only 1 file when excluding subpkg")

	// Verify it's simple.go
	absAnalysisPath, err := filepath.Abs(filepath.Join(result.BasePath, result.Files[0].Path))
	require.NoError(t, err, "Should get absolute path for analysis result")
	relPath, err := filepath.Rel(absTestdataDir, absAnalysisPath)
	require.NoError(t, err)
	assert.Equal(t, "simple.go", relPath)
	assert.Equal(t, "simple", result.Files[0].Package)
	assert.Len(t, result.Files[0].Functions, 3) // simple.go has 3 functions
}
