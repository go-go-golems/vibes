package formatter

import (
	"time"

	"github.com/your-org/go-analyzer/internal/parser" // For EnhancedFunctionInfo
)

// --- Enhanced JSON Output Structure ---
// Based on enhanced-json-format-documentation.md (as described in tutorial)

// EnhancedOutput is the root object for the detailed JSON output.
type EnhancedOutput struct {
	Metadata          CodebaseMetadata  `json:"metadata"`
	Files             []FileInfo        `json:"files"`
	Functions         []FunctionDetail  `json:"functions"` // List of all functions/methods
	Packages          []PackageInfo     `json:"packages"`
	CallGraph         Graph             `json:"callGraph"`
	VisualizationData VisualizationData `json:"visualizationData"`
	// Errors []AnalysisError `json:"errors,omitempty"` // Optional: for reporting parsing/analysis errors
}

// CodebaseMetadata contains high-level information about the analyzed codebase.
type CodebaseMetadata struct {
	ProjectName       string    `json:"projectName"` // e.g., directory name
	AnalysisTimestamp time.Time `json:"analysisTimestamp"`
	GoVersion         string    `json:"goVersion"`  // Version used for analysis (can be placeholder)
	TotalFiles        int       `json:"totalFiles"` // Count of .go files analyzed
	TotalPackages     int       `json:"totalPackages"`
	TotalFunctions    int       `json:"totalFunctions"` // Total funcs + methods
	TotalLines        int       `json:"totalLines"`     // Total lines of code analyzed
}

// FileInfo contains information about a single analyzed file.
type FileInfo struct {
	Path        string   `json:"path"`        // Relative path from the project root
	Package     string   `json:"package"`     // Package name
	Size        int64    `json:"size"`        // File size in bytes
	Lines       int      `json:"lines"`       // Number of lines in the file
	Imports     []string `json:"imports"`     // List of imported packages (TODO: Extract in parser)
	FunctionIDs []string `json:"functionIds"` // IDs of functions/methods defined in this file
	// ComplexityMetrics FileComplexity `json:"complexityMetrics,omitempty"` // Optional file-level metrics
}

// FunctionDetail provides the comprehensive details for a single function or method.
// It embeds EnhancedFunctionInfo and adds a unique ID and call information.
type FunctionDetail struct {
	parser.EnhancedFunctionInfo          // Embed the detailed info from the parser
	ID                          string   `json:"id"`       // Unique ID (e.g., "package.FuncName" or "package.Receiver.MethodName")
	Calls                       []string `json:"calls"`    // List of function IDs this function calls (Placeholder/Advanced)
	CalledBy                    []string `json:"calledBy"` // List of function IDs that call this function (Placeholder/Advanced)
	// Complexity Score float64 `json:"complexityScore,omitempty"` // e.g., Cyclomatic complexity
}

// PackageInfo aggregates information about a single package.
type PackageInfo struct {
	Name        string   `json:"name"`        // Package name
	Path        string   `json:"path"`        // Relative path to the package directory
	Files       []string `json:"files"`       // List of relative file paths belonging to this package
	FunctionIDs []string `json:"functionIds"` // List of function IDs defined in this package
}

// --- Graph Structures ---

// Graph represents relationships (e.g., call graph, package dependencies).
type Graph struct {
	Nodes []GraphNode `json:"nodes"`
	Edges []GraphEdge `json:"edges"`
}

// GraphNode represents a node in a graph.
type GraphNode struct {
	ID    string `json:"id"`    // Unique identifier (e.g., package name, function ID)
	Label string `json:"label"` // Display label (e.g., function name)
	Type  string `json:"type"`  // Node type (e.g., "package", "function", "method")
	// Attributes map[string]interface{} `json:"attributes,omitempty"` // Optional extra data
}

// GraphEdge represents a directed edge between two nodes.
type GraphEdge struct {
	Source string `json:"source"` // ID of the source node
	Target string `json:"target"` // ID of the target node
	Type   string `json:"type"`   // Type of relationship (e.g., "calls", "belongs_to", "imports")
	// Weight float64 `json:"weight,omitempty"` // Optional edge weight
}

// --- Visualization-Specific Data Structures ---

// VisualizationData holds data pre-formatted for specific chart types.
type VisualizationData struct {
	PackageHierarchy   *HierarchyNode       `json:"packageHierarchy"`   // For treemaps or hierarchical views
	CallNetwork        NetworkVisualization `json:"callNetwork"`        // For force-directed graphs
	ModuleDependencies MatrixVisualization  `json:"moduleDependencies"` // For chord diagrams or dependency matrices (Packages)
	FunctionComplexity []ComplexityPoint    `json:"functionComplexity"` // For scatter plots
}

// HierarchyNode represents a node in a tree structure (e.g., for D3 hierarchy layout).
type HierarchyNode struct {
	Name     string           `json:"name"`               // Name of the node (e.g., package or directory name)
	Path     string           `json:"path,omitempty"`     // Full path (relevant for packages)
	Value    float64          `json:"value,omitempty"`    // Size metric (e.g., lines of code, number of functions)
	Children []*HierarchyNode `json:"children,omitempty"` // Child nodes
}

// NetworkVisualization holds nodes and links for force-directed graphs.
type NetworkVisualization struct {
	Nodes []NetworkNode `json:"nodes"`
	Links []NetworkLink `json:"links"`
}

// NetworkNode represents a node in a network graph (like D3 force layout).
type NetworkNode struct {
	ID    string  `json:"id"`    // Function ID
	Group string  `json:"group"` // Grouping attribute (e.g., package name)
	Label string  `json:"label"` // Display label (e.g., function name)
	Value float64 `json:"value"` // Size metric (e.g., lines of code)
}

// NetworkLink represents a link between nodes in a network graph.
type NetworkLink struct {
	Source string  `json:"source"` // Source function ID
	Target string  `json:"target"` // Target function ID
	Value  float64 `json:"value"`  // Strength of the link (e.g., number of calls)
}

// MatrixVisualization holds data for matrix-based charts (like chord diagrams).
type MatrixVisualization struct {
	Labels []string    `json:"labels"` // Names of the entities (e.g., package names)
	Matrix [][]float64 `json:"matrix"` // Adjacency matrix representing relationships (e.g., import counts)
}

// ComplexityPoint holds data for a single point in a complexity scatter plot.
type ComplexityPoint struct {
	ID         string  `json:"id"` // Function ID
	Package    string  `json:"package"`
	Name       string  `json:"name"`
	Lines      int     `json:"lines"`      // Lines of code for the function
	Complexity float64 `json:"complexity"` // Complexity score (e.g., cyclomatic, or just lines as proxy)
}
