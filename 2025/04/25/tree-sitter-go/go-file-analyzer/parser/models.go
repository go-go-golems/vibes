package parser

// FileStructure represents the structure of a Go file
type FileStructure struct {
	Path      string         `json:"path"`
	Functions []FunctionInfo `json:"functions"`
	Imports   []ImportInfo   `json:"imports"`
	Package   string         `json:"package"`
}

// ImportInfo represents an import statement in a Go file
type ImportInfo struct {
	Path  string `json:"path"`
	Alias string `json:"alias,omitempty"`
}

// AnalysisResult represents the complete analysis of a codebase
type AnalysisResult struct {
	Files     []FileStructure `json:"files"`
	Functions []FunctionInfo  `json:"functions"`
	Packages  []PackageInfo   `json:"packages"`
}

// PackageInfo represents a Go package
type PackageInfo struct {
	Name      string   `json:"name"`
	FilePaths []string `json:"filePaths"`
}

// FunctionDetail extends FunctionInfo with additional details about the function
type FunctionDetail struct {
	FunctionInfo
	Parameters []ParameterInfo `json:"parameters"`
	ReturnType []string        `json:"returnType"`
	Comments   string          `json:"comments,omitempty"`
}

// ParameterInfo represents a function parameter
type ParameterInfo struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// GraphNode represents a node in a function call graph
type GraphNode struct {
	ID       string `json:"id"`
	Name     string `json:"name"`
	Package  string `json:"package"`
	FilePath string `json:"filePath"`
}

// GraphEdge represents an edge in a function call graph
type GraphEdge struct {
	Source string `json:"source"`
	Target string `json:"target"`
}

// Graph represents a function call graph
type Graph struct {
	Nodes []GraphNode `json:"nodes"`
	Edges []GraphEdge `json:"edges"`
}