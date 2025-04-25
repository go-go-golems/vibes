package formatter

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"time"

	"github.com/pkg/errors"
	"github.com/your-org/go-analyzer/internal/analysis" // For AnalysisResult (Phase 2)
	"github.com/your-org/go-analyzer/internal/parser"   // For EnhancedFunctionInfo
)

// FormatEnhancedJSON transforms the raw analysis results into the rich JSON structure.
// NOTE: This currently takes the Phase 2 AnalysisResult as input.
// It assumes the parser used by the analyzer was the *enhanced* one,
// even though the structures might not fully align yet. We will need
// to refactor AnalyzeDirectory later to use ParseFileEnhanced directly.
func FormatEnhancedJSON(analysisResult *analysis.AnalysisResult, enhancedData map[string]*parser.EnhancedFileInfo) (*EnhancedOutput, error) {
	if analysisResult == nil {
		return nil, errors.New("analysis result cannot be nil")
	}
	if enhancedData == nil {
		return nil, errors.New("enhanced file data cannot be nil")
	}

	output := EnhancedOutput{
		Metadata: CodebaseMetadata{
			ProjectName:       filepath.Base(analysisResult.BasePath),
			AnalysisTimestamp: time.Now().UTC(),
			GoVersion:         runtime.Version(), // Get Go version used by this tool
		},
		Files:     []FileInfo{},
		Functions: []FunctionDetail{},
		Packages:  []PackageInfo{},
		CallGraph: Graph{Nodes: []GraphNode{}, Edges: []GraphEdge{}},
		VisualizationData: VisualizationData{
			PackageHierarchy:   &HierarchyNode{Name: "root"}, // Initialize root
			CallNetwork:        NetworkVisualization{Nodes: []NetworkNode{}, Links: []NetworkLink{}},
			ModuleDependencies: MatrixVisualization{Labels: []string{}, Matrix: [][]float64{}},
			FunctionComplexity: []ComplexityPoint{},
		},
	}

	packageMap := make(map[string]*PackageInfo)
	var allFunctions []FunctionDetail
	totalLines := 0

	// --- Process Files and Extract Functions ---
	for _, fileAnalysis := range analysisResult.Files {
		absPath := filepath.Join(analysisResult.BasePath, fileAnalysis.Path)
		enhancedFileInfo, ok := enhancedData[absPath]
		if !ok {
			// This shouldn't happen if AnalyzeDirectory provided all paths
			fmt.Fprintf(os.Stderr, "Warning: Enhanced data not found for file %s\n", absPath)
			continue
		}

		fileInfo, err := os.Stat(absPath)
		if err != nil {
			return nil, errors.Wrapf(err, "failed to get file info for %s", absPath)
		}

		lines, err := countLinesInFile(absPath)
		if err != nil {
			return nil, errors.Wrapf(err, "failed to count lines in %s", absPath)
		}
		totalLines += lines

		currentFileFunctions := []string{} // Store IDs for this file

		for _, funcInfo := range enhancedFileInfo.Functions {
			funcID := generateFunctionID(funcInfo)
			currentFileFunctions = append(currentFileFunctions, funcID)

			// Add to the global list of functions
			allFunctions = append(allFunctions, FunctionDetail{
				EnhancedFunctionInfo: funcInfo, // Embed parser info
				ID:                   funcID,
				Calls:                []string{}, // Placeholder
				CalledBy:             []string{}, // Placeholder
			})
		}

		// Add file info to output
		output.Files = append(output.Files, FileInfo{
			Path:        fileAnalysis.Path, // Use relative path from analysis result
			Package:     enhancedFileInfo.Package,
			Size:        fileInfo.Size(),
			Lines:       lines,
			Imports:     []string{}, // Placeholder - TODO: Extract imports in parser
			FunctionIDs: currentFileFunctions,
		})

		// --- Aggregate Package Info ---
		pkgName := enhancedFileInfo.Package
		if pkgName == "" {
			pkgName = "_unknown" // Handle files without package declaration
		}
		pkgPath := filepath.Dir(fileAnalysis.Path)
		if pkgPath == "." {
			pkgPath = "" // Represent root package path consistently
		}

		if _, exists := packageMap[pkgName]; !exists {
			packageMap[pkgName] = &PackageInfo{
				Name:        pkgName,
				Path:        pkgPath, // Store initial path
				Files:       []string{},
				FunctionIDs: []string{},
			}
		}
		packageMap[pkgName].Files = append(packageMap[pkgName].Files, fileAnalysis.Path)
		packageMap[pkgName].FunctionIDs = append(packageMap[pkgName].FunctionIDs, currentFileFunctions...)
		// Note: Path might be ambiguous if same package name exists in multiple dirs.
		// For simplicity, we store the path of the first file encountered.
	}

	// --- Populate Top-Level Functions List ---
	output.Functions = allFunctions

	// --- Populate Packages List from Map ---
	for _, pkgInfo := range packageMap {
		// Sort files and function IDs for deterministic output
		sort.Strings(pkgInfo.Files)
		sort.Strings(pkgInfo.FunctionIDs)
		output.Packages = append(output.Packages, *pkgInfo)
	}
	// Sort packages by name
	sort.Slice(output.Packages, func(i, j int) bool {
		return output.Packages[i].Name < output.Packages[j].Name
	})

	// --- Update Metadata Counts ---
	output.Metadata.TotalFiles = len(output.Files)
	output.Metadata.TotalPackages = len(output.Packages)
	output.Metadata.TotalFunctions = len(output.Functions)
	output.Metadata.TotalLines = totalLines

	// --- Build Basic Call Graph (Nodes and belongs_to edges) ---
	addPackagesToGraph(&output.CallGraph, output.Packages)
	addFunctionsToGraph(&output.CallGraph, output.Functions)

	// --- Build Visualization Data ---
	output.VisualizationData.PackageHierarchy = buildPackageHierarchy(output.Packages)
	output.VisualizationData.CallNetwork = buildCallNetwork(output.Functions, output.Packages) // Basic version
	output.VisualizationData.FunctionComplexity = buildFunctionComplexity(output.Functions)
	// ModuleDependencies requires import analysis (TODO)

	return &output, nil
}

// --- Helper Functions ---

func generateFunctionID(f parser.EnhancedFunctionInfo) string {
	if f.IsMethod && f.Receiver != nil {
		// For methods, include receiver type for uniqueness
		// Pointer distinction might be useful but adds complexity, skip for now.
		return fmt.Sprintf("%s.%s.%s", f.Package, f.Receiver.Type, f.Name)
	}
	return fmt.Sprintf("%s.%s", f.Package, f.Name)
}

func countLinesInFile(filePath string) (int, error) {
	content, err := os.ReadFile(filePath)
	if err != nil {
		return 0, err
	}
	// Simple line counting, might not be perfect for all edge cases
	lines := strings.Split(string(content), "\n")
	count := len(lines)
	// Adjust for trailing newline which might add an empty string
	if count > 0 && lines[count-1] == "" {
		count--
	}
	return count, nil
}

func addPackagesToGraph(graph *Graph, packages []PackageInfo) {
	for _, pkg := range packages {
		graph.Nodes = append(graph.Nodes, GraphNode{
			ID:    pkg.Name, // Use package name as ID for simplicity
			Label: pkg.Name,
			Type:  "package",
		})
	}
}

func addFunctionsToGraph(graph *Graph, functions []FunctionDetail) {
	for _, fun := range functions {
		nodeType := "function"
		if fun.IsMethod {
			nodeType = "method"
		}
		graph.Nodes = append(graph.Nodes, GraphNode{
			ID:    fun.ID,
			Label: fun.Name,
			Type:  nodeType,
		})

		// Add edge from function/method to its package
		graph.Edges = append(graph.Edges, GraphEdge{
			Source: fun.ID,
			Target: fun.Package, // Assumes package node ID is package name
			Type:   "belongs_to",
		})

		// TODO: Add "calls" edges when call analysis is implemented
	}
}

// buildPackageHierarchy creates a tree structure from package paths.
func buildPackageHierarchy(packages []PackageInfo) *HierarchyNode {
	root := &HierarchyNode{Name: "/", Path: "/"}
	lookup := map[string]*HierarchyNode{"/": root}

	// Sort packages by path depth to build parent directories first
	sort.Slice(packages, func(i, j int) bool {
		return len(strings.Split(packages[i].Path, string(filepath.Separator))) <
			len(strings.Split(packages[j].Path, string(filepath.Separator)))
	})

	for _, pkg := range packages {
		pathParts := strings.Split(pkg.Path, string(filepath.Separator))
		if pkg.Path == "" { // Handle root package
			pathParts = []string{}
		}

		currentPath := "/"
		parentNode := root

		for _, part := range pathParts {
			if part == "" {
				continue
			}
			childPath := filepath.Join(currentPath, part)
			if _, exists := lookup[childPath]; !exists {
				newNode := &HierarchyNode{Name: part, Path: childPath}
				parentNode.Children = append(parentNode.Children, newNode)
				lookup[childPath] = newNode
			}
			parentNode = lookup[childPath]
			currentPath = childPath
		}

		// Add the package node itself
		if _, exists := lookup[pkg.Path]; !exists {
			// This case handles packages directly under root or ensures the node exists
			pkgNode := &HierarchyNode{
				Name:  pkg.Name,
				Path:  pkg.Path,
				Value: float64(len(pkg.FunctionIDs)), // Use function count as value
			}
			parentNode.Children = append(parentNode.Children, pkgNode)
			lookup[pkg.Path] = pkgNode
		} else {
			// Update existing node (e.g., directory node becoming a package node)
			lookup[pkg.Path].Name = pkg.Name // Use package name as the final name
			lookup[pkg.Path].Value = float64(len(pkg.FunctionIDs))
		}
	}
	// TODO: Recursively sum values up the hierarchy?
	return root
}

// buildCallNetwork creates nodes and links for visualization (basic version).
func buildCallNetwork(functions []FunctionDetail, packages []PackageInfo) NetworkVisualization {
	network := NetworkVisualization{Nodes: []NetworkNode{}, Links: []NetworkLink{}}
	for _, fun := range functions {
		lines := fun.EndLine - fun.StartLine + 1
		network.Nodes = append(network.Nodes, NetworkNode{
			ID:    fun.ID,
			Group: fun.Package,
			Label: fun.Name,
			Value: float64(lines), // Use line count as node size
		})

		// TODO: Add links based on fun.Calls when available
	}
	return network
}

// buildFunctionComplexity extracts data for the complexity plot.
func buildFunctionComplexity(functions []FunctionDetail) []ComplexityPoint {
	points := []ComplexityPoint{}
	for _, fun := range functions {
		lines := fun.EndLine - fun.StartLine + 1
		points = append(points, ComplexityPoint{
			ID:         fun.ID,
			Package:    fun.Package,
			Name:       fun.Name,
			Lines:      lines,
			Complexity: float64(lines), // Use lines as complexity proxy for now
		})
	}
	return points
}
