package formatter

import (
	"path/filepath"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/your-org/go-analyzer/internal/analysis"
	"github.com/your-org/go-analyzer/internal/parser"
)

func TestFormatEnhancedJSON(t *testing.T) {
	// --- Mock Input Data ---
	mockBasePath := "/mock/project"
	file1Path := "pkgA/file1.go"
	file2Path := "pkgB/sub/file2.go"
	absFile1Path := filepath.Join(mockBasePath, file1Path)
	absFile2Path := filepath.Join(mockBasePath, file2Path)

	// Mock Phase 2 Analysis Result (just the file list is important here)
	mockAnalysisResult := &analysis.AnalysisResult{
		BasePath: mockBasePath,
		Files: []analysis.FileAnalysis{
			{Path: file1Path}, // Only relative path needed
			{Path: file2Path},
		},
	}

	// Mock Enhanced File Info data (as if parser ran)
	mockEnhancedData := map[string]*parser.EnhancedFileInfo{
		absFile1Path: {
			Path:    absFile1Path,
			Package: "pkgA",
			Functions: []parser.EnhancedFunctionInfo{
				{
					Name:        "FuncA",
					Filepath:    absFile1Path,
					Package:     "pkgA",
					Signature:   "func FuncA(i int) string",
					IsExported:  true,
					IsMethod:    false,
					Parameters:  []parser.ParameterInfo{{Name: "i", Type: "int"}},
					ReturnTypes: []string{"string"},
					StartLine:   10,
					EndLine:     12,
				},
			},
		},
		absFile2Path: {
			Path:    absFile2Path,
			Package: "pkgB", // Assume same package name despite sub-dir
			Functions: []parser.EnhancedFunctionInfo{
				{
					Name:        "MethodB",
					Filepath:    absFile2Path,
					Package:     "pkgB",
					Signature:   "func (r *MyType) MethodB()",
					IsExported:  true,
					IsMethod:    true,
					Receiver:    &parser.ReceiverInfo{Name: "r", Type: "MyType", Pointer: true},
					Parameters:  []parser.ParameterInfo{},
					ReturnTypes: []string{},
					StartLine:   20,
					EndLine:     25,
				},
				{
					Name:        "helperB",
					Filepath:    absFile2Path,
					Package:     "pkgB",
					Signature:   "func helperB()",
					IsExported:  false,
					IsMethod:    false,
					Parameters:  []parser.ParameterInfo{},
					ReturnTypes: []string{},
					StartLine:   30,
					EndLine:     31,
				},
			},
		},
	}

	// Mock os.Stat and countLines (won't actually read files)
	// We need to override these if the test environment doesn't allow file access
	// For now, assume FormatEnhancedJSON uses mockable helpers or we accept potential error
	// A better approach would be dependency injection for os.Stat and readFile.

	// --- Execute Formatter ---
	start := time.Now()
	output, err := FormatEnhancedJSON(mockAnalysisResult, mockEnhancedData)
	require.NoError(t, err, "FormatEnhancedJSON should not return an error")
	require.NotNil(t, output, "Formatted output should not be nil")

	// --- Basic Assertions ---

	// Metadata
	assert.Equal(t, "project", output.Metadata.ProjectName)
	assert.WithinDuration(t, start, output.Metadata.AnalysisTimestamp, time.Second, "Timestamp should be recent")
	assert.NotEmpty(t, output.Metadata.GoVersion)
	assert.Equal(t, 2, output.Metadata.TotalFiles, "Metadata TotalFiles mismatch")
	assert.Equal(t, 2, output.Metadata.TotalPackages, "Metadata TotalPackages mismatch")
	assert.Equal(t, 3, output.Metadata.TotalFunctions, "Metadata TotalFunctions mismatch")
	// assert.Equal(t, ExpectedTotalLines, output.Metadata.TotalLines) // Hard to mock accurately without file access

	// Files
	require.Len(t, output.Files, 2, "Should have 2 files in output")
	assert.Equal(t, file1Path, output.Files[0].Path)
	assert.Equal(t, "pkgA", output.Files[0].Package)
	// assert.Greater(t, output.Files[0].Size, int64(0)) // Requires mock file access
	// assert.Greater(t, output.Files[0].Lines, 0) // Requires mock file access
	assert.Len(t, output.Files[0].FunctionIDs, 1)
	assert.Equal(t, "pkgA.FuncA", output.Files[0].FunctionIDs[0])

	assert.Equal(t, file2Path, output.Files[1].Path)
	assert.Equal(t, "pkgB", output.Files[1].Package)
	assert.Len(t, output.Files[1].FunctionIDs, 2)
	assert.Contains(t, output.Files[1].FunctionIDs, "pkgB.MyType.MethodB")
	assert.Contains(t, output.Files[1].FunctionIDs, "pkgB.helperB")

	// Functions
	require.Len(t, output.Functions, 3, "Should have 3 functions/methods in output")
	funcMap := make(map[string]FunctionDetail)
	for _, f := range output.Functions {
		funcMap[f.ID] = f
	}
	assert.Contains(t, funcMap, "pkgA.FuncA")
	assert.Contains(t, funcMap, "pkgB.MyType.MethodB")
	assert.Contains(t, funcMap, "pkgB.helperB")
	assert.Equal(t, "FuncA", funcMap["pkgA.FuncA"].Name)
	assert.Equal(t, true, funcMap["pkgA.FuncA"].IsExported)
	assert.Equal(t, false, funcMap["pkgA.FuncA"].IsMethod)
	assert.Equal(t, "helperB", funcMap["pkgB.helperB"].Name)
	assert.Equal(t, false, funcMap["pkgB.helperB"].IsExported)
	assert.Equal(t, "MethodB", funcMap["pkgB.MyType.MethodB"].Name)
	assert.Equal(t, true, funcMap["pkgB.MyType.MethodB"].IsMethod)
	assert.NotNil(t, funcMap["pkgB.MyType.MethodB"].Receiver)
	assert.Equal(t, "MyType", funcMap["pkgB.MyType.MethodB"].Receiver.Type)

	// Packages
	require.Len(t, output.Packages, 2, "Should have 2 packages in output")
	pkgOutputMap := make(map[string]PackageInfo)
	for _, p := range output.Packages {
		pkgOutputMap[p.Name] = p
	}
	require.Contains(t, pkgOutputMap, "pkgA")
	assert.Equal(t, "pkgA", pkgOutputMap["pkgA"].Path)
	assert.Len(t, pkgOutputMap["pkgA"].Files, 1)
	assert.Equal(t, file1Path, pkgOutputMap["pkgA"].Files[0])
	assert.Len(t, pkgOutputMap["pkgA"].FunctionIDs, 1)
	assert.Equal(t, "pkgA.FuncA", pkgOutputMap["pkgA"].FunctionIDs[0])

	require.Contains(t, pkgOutputMap, "pkgB")
	assert.Equal(t, "pkgB/sub", pkgOutputMap["pkgB"].Path)
	assert.Len(t, pkgOutputMap["pkgB"].Files, 1)
	assert.Equal(t, file2Path, pkgOutputMap["pkgB"].Files[0])
	assert.Len(t, pkgOutputMap["pkgB"].FunctionIDs, 2)

	// Call Graph (Basic)
	assert.Len(t, output.CallGraph.Nodes, 5, "Should have 5 nodes (2 pkg + 3 func)")
	assert.Len(t, output.CallGraph.Edges, 3, "Should have 3 edges (func -> pkg)")
	// Could add more specific node/edge checks

	// Visualization Data (Basic checks)
	assert.NotNil(t, output.VisualizationData.PackageHierarchy)
	assert.Equal(t, "/", output.VisualizationData.PackageHierarchy.Name)
	// assert.True(t, len(output.VisualizationData.PackageHierarchy.Children) > 0) // Depends on implementation details

	assert.Len(t, output.VisualizationData.CallNetwork.Nodes, 3)
	assert.Empty(t, output.VisualizationData.CallNetwork.Links) // No call links yet

	// assert.Len(t, output.VisualizationData.ModuleDependencies.Labels, 2) // Requires import analysis

	assert.Len(t, output.VisualizationData.FunctionComplexity, 3)
}
