package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// EnhancedOutput represents the enhanced JSON output structure
type EnhancedOutput struct {
	// Basic information about the codebase
	Metadata CodebaseMetadata `json:"metadata"`
	
	// Complete codebase information
	Files     []FileInfo      `json:"files"`     // All files analyzed
	Functions []FunctionInfo  `json:"functions"` // All functions identified
	Packages  []PackageInfo   `json:"packages"`  // Package information
	
	// Relationships
	CallGraph     *Graph         `json:"callGraph,omitempty"`     // Function call graph
	Dependencies  []Dependency   `json:"dependencies,omitempty"`  // External dependencies
	
	// Visualization data
	VisualizationData *VisualizationData `json:"visualizationData,omitempty"` // Data for web visualizations
}

// CodebaseMetadata contains metadata about the analyzed codebase
type CodebaseMetadata struct {
	Name           string `json:"name"`           // Project name, inferred from root directory or module name
	BasePath       string `json:"basePath"`       // Absolute path to the analyzed codebase
	GoVersion      string `json:"goVersion"`      // Go version used in the project (from go.mod)
	ModuleName     string `json:"moduleName"`     // Module name (from go.mod)
	NumFiles       int    `json:"numFiles"`       // Total number of files analyzed
	NumFunctions   int    `json:"numFunctions"`   // Total number of functions found
	NumMethods     int    `json:"numMethods"`     // Total number of methods found
	NumPackages    int    `json:"numPackages"`    // Total number of packages found
	AnalysisTimeMs int64  `json:"analysisTimeMs"` // Analysis time in milliseconds
	Generated      string `json:"generated"`      // Timestamp when the analysis was generated
}

// FileInfo represents a Go source file
type FileInfo struct {
	Path      string         `json:"path"`      // File path relative to the base path
	AbsPath   string         `json:"absPath"`   // Absolute file path
	Package   string         `json:"package"`   // Package name
	Imports   []ImportInfo   `json:"imports"`   // List of imports
	Functions []FunctionInfo `json:"functions"` // Functions defined in this file
	NumLines  int            `json:"numLines"`  // Number of lines in the file
	Size      int64          `json:"size"`      // File size in bytes
}

// ImportInfo represents an import statement
type ImportInfo struct {
	Path  string `json:"path"`            // Import path
	Alias string `json:"alias,omitempty"` // Import alias if provided
}

// FunctionInfo represents a function or method
type FunctionInfo struct {
	ID          string           `json:"id"`                   // Unique identifier for the function
	Name        string           `json:"name"`                 // Function name
	Signature   string           `json:"signature"`            // Full function signature
	Filepath    string           `json:"filepath"`             // File path relative to the base path
	StartLine   int              `json:"startLine"`            // Start line of the function
	EndLine     int              `json:"endLine"`              // End line of the function
	StartCol    int              `json:"startCol"`             // Start column of the function
	EndCol      int              `json:"endCol"`               // End column of the function
	Package     string           `json:"package"`              // Package name
	IsMethod    bool             `json:"isMethod"`             // Whether the function is a method
	IsExported  bool             `json:"isExported"`           // Whether the function is exported
	Receiver    *ReceiverInfo    `json:"receiver,omitempty"`   // Receiver information if it's a method
	Parameters  []ParameterInfo  `json:"parameters"`           // Function parameters
	ReturnTypes []string         `json:"returnTypes"`          // Return types
	Comments    string           `json:"comments,omitempty"`   // Function comments/documentation
	Calls       []string         `json:"calls,omitempty"`      // Functions called by this function
	CalledBy    []string         `json:"calledBy,omitempty"`   // Functions that call this function
	Complexity  int              `json:"complexity,omitempty"` // Cyclomatic complexity (if available)
}

// ReceiverInfo represents a method receiver
type ReceiverInfo struct {
	Name     string `json:"name"`              // Receiver variable name
	Type     string `json:"type"`              // Receiver type
	IsPointer bool  `json:"isPointer"`         // Whether the receiver is a pointer
	TypeName  string `json:"typeName"`         // Just the type name without the * if it's a pointer
}

// ParameterInfo represents a function parameter
type ParameterInfo struct {
	Name string `json:"name"`          // Parameter name
	Type string `json:"type"`          // Parameter type
}

// PackageInfo represents a Go package
type PackageInfo struct {
	Name      string   `json:"name"`      // Package name
	Path      string   `json:"path"`      // Package path relative to the base path
	Files     []string `json:"files"`     // Files in the package
	Functions []string `json:"functions"` // Functions in the package
}

// Dependency represents an external dependency
type Dependency struct {
	ImportPath string   `json:"importPath"` // Full import path
	Module     string   `json:"module"`     // Module name
	UsedBy     []string `json:"usedBy"`     // Files that import this dependency
}

// Graph represents a graph of nodes and edges
type Graph struct {
	Nodes []GraphNode `json:"nodes"` // Graph nodes
	Edges []GraphEdge `json:"edges"` // Graph edges
}

// GraphNode represents a node in a graph
type GraphNode struct {
	ID       string                 `json:"id"`                 // Unique identifier
	Label    string                 `json:"label"`              // Node label
	Type     string                 `json:"type"`               // Node type: "function", "method", "package"
	Package  string                 `json:"package"`            // Package name
	Filepath string                 `json:"filepath,omitempty"` // File path relative to the base path
	Data     map[string]interface{} `json:"data,omitempty"`     // Additional data
}

// GraphEdge represents an edge in a graph
type GraphEdge struct {
	Source string                 `json:"source"` // Source node ID
	Target string                 `json:"target"` // Target node ID
	Type   string                 `json:"type"`   // Edge type: "calls", "imports", etc.
	Data   map[string]interface{} `json:"data,omitempty"`   // Additional data
}

// VisualizationData contains data specifically formatted for web visualizations
type VisualizationData struct {
	PackageHierarchy *HierarchyNode        `json:"packageHierarchy,omitempty"` // Package hierarchy for treemap visualization
	CallNetwork      *NetworkVisualization `json:"callNetwork,omitempty"`      // Function call network
	ModuleDependencies *DependencyWheel    `json:"moduleDependencies,omitempty"` // External module dependencies
	FunctionComplexity []ComplexityData    `json:"functionComplexity,omitempty"` // Function complexity data
}

// HierarchyNode represents a node in a hierarchical structure (like a treemap)
type HierarchyNode struct {
	Name     string         `json:"name"`               // Node name
	Value    int            `json:"value,omitempty"`    // Node value (e.g., number of functions or lines of code)
	Path     string         `json:"path,omitempty"`     // Path in the hierarchy
	Children []*HierarchyNode `json:"children,omitempty"` // Child nodes
}

// NetworkVisualization represents data for a network visualization
type NetworkVisualization struct {
	Nodes []NetworkNode `json:"nodes"` // Network nodes
	Links []NetworkLink `json:"links"` // Network links
}

// NetworkNode represents a node in a network visualization
type NetworkNode struct {
	ID       string `json:"id"`                 // Unique identifier
	Label    string `json:"label"`              // Node label
	Package  string `json:"package"`            // Package name
	Group    string `json:"group"`              // Group for coloring (usually package name)
	Value    int    `json:"value"`              // Node value (e.g., number of lines or complexity)
	IsMethod bool   `json:"isMethod"`           // Whether the node is a method
	Type     string `json:"type,omitempty"`     // Node type for filtering
}

// NetworkLink represents a link in a network visualization
type NetworkLink struct {
	Source string `json:"source"`   // Source node ID
	Target string `json:"target"`   // Target node ID
	Value  int    `json:"value"`    // Link value
	Type   string `json:"type"`     // Link type
}

// DependencyWheel represents data for a dependency wheel visualization
type DependencyWheel struct {
	Names []string    `json:"names"` // Names of the packages or modules
	Matrix [][]int    `json:"matrix"` // Adjacency matrix of dependencies
}

// ComplexityData represents data for function complexity visualization
type ComplexityData struct {
	Name       string `json:"name"`       // Function name
	Package    string `json:"package"`    // Package name
	Complexity int    `json:"complexity"` // Complexity value
	Lines      int    `json:"lines"`      // Number of lines
}

// FormatJSON enhances the analysis result and outputs formatted JSON
func FormatJSON(analysisResult map[string]interface{}, outputPath string) error {
	// Create enhanced output structure
	output := &EnhancedOutput{
		Metadata: CodebaseMetadata{
			Name:         filepath.Base(analysisResult["basePath"].(string)),
			BasePath:     analysisResult["basePath"].(string),
			NumFiles:     len(analysisResult["files"].([]interface{})),
			NumFunctions: len(analysisResult["functions"].([]interface{})),
			NumPackages:  len(analysisResult["packages"].(map[string]interface{})),
			Generated:    analysisResult["stats"].(map[string]interface{})["timestamp"].(string),
		},
	}
	
	// Extract files information
	output.Files = make([]FileInfo, 0, len(analysisResult["files"].([]interface{})))
	for _, fileData := range analysisResult["files"].([]interface{}) {
		fileMap := fileData.(map[string]interface{})
		file := FileInfo{
			Path:    fileMap["path"].(string),
			AbsPath: fileMap["path"].(string),
			Package: fileMap["package"].(string),
		}
		
		// Extract imports
		if importsData, ok := fileMap["imports"].([]interface{}); ok {
			file.Imports = make([]ImportInfo, 0, len(importsData))
			for _, importData := range importsData {
				importMap := importData.(map[string]interface{})
				file.Imports = append(file.Imports, ImportInfo{
					Path:  importMap["path"].(string),
					Alias: importMap["alias"].(string),
				})
			}
		}
		
		// Extract functions
		if functionsData, ok := fileMap["functions"].([]interface{}); ok {
			file.Functions = make([]FunctionInfo, 0, len(functionsData))
			for _, funcData := range functionsData {
				funcMap := funcData.(map[string]interface{})
				function := FunctionInfo{
					ID:        createFunctionID(funcMap["package"].(string), funcMap["name"].(string)),
					Name:      funcMap["name"].(string),
					Filepath:  funcMap["filepath"].(string),
					StartLine: int(funcMap["startLine"].(float64)),
					EndLine:   int(funcMap["endLine"].(float64)),
					Package:   funcMap["package"].(string),
					IsMethod:  funcMap["isMethod"].(bool),
					IsExported: len(funcMap["name"].(string)) > 0 && 
						funcMap["name"].(string)[0] >= 'A' && 
						funcMap["name"].(string)[0] <= 'Z',
				}
				
				// Extract parameters
				if paramsData, ok := funcMap["parameters"].([]interface{}); ok {
					function.Parameters = make([]ParameterInfo, 0, len(paramsData))
					for _, paramData := range paramsData {
						paramMap := paramData.(map[string]interface{})
						function.Parameters = append(function.Parameters, ParameterInfo{
							Name: paramMap["name"].(string),
							Type: paramMap["type"].(string),
						})
					}
				}
				
				// Extract return types
				if returnData, ok := funcMap["returnType"].([]interface{}); ok {
					function.ReturnTypes = make([]string, 0, len(returnData))
					for _, returnType := range returnData {
						function.ReturnTypes = append(function.ReturnTypes, returnType.(string))
					}
				}
				
				// Extract receiver if method
				if function.IsMethod {
					if receiverData, ok := funcMap["receiver"].(map[string]interface{}); ok {
						isPointer := false
						typeName := receiverData["type"].(string)
						if strings.HasPrefix(typeName, "*") {
							isPointer = true
							typeName = typeName[1:] // Remove * prefix
						}
						
						function.Receiver = &ReceiverInfo{
							Name:      receiverData["name"].(string),
							Type:      receiverData["type"].(string),
							IsPointer: isPointer,
							TypeName:  typeName,
						}
					}
				}
				
				// Extract comments
				if comments, ok := funcMap["comments"].(string); ok {
					function.Comments = comments
				}
				
				// Build function signature
				function.Signature = buildFunctionSignature(function)
				
				file.Functions = append(file.Functions, function)
			}
		}
		
		// Get file stats
		if fileInfo, err := os.Stat(file.AbsPath); err == nil {
			file.Size = fileInfo.Size()
			// Count lines
			if content, err := ioutil.ReadFile(file.AbsPath); err == nil {
				file.NumLines = len(strings.Split(string(content), "\n"))
			}
		}
		
		output.Files = append(output.Files, file)
	}
	
	// Extract functions information
	output.Functions = make([]FunctionInfo, 0, output.Metadata.NumFunctions)
	for _, fileInfo := range output.Files {
		output.Functions = append(output.Functions, fileInfo.Functions...)
	}
	
	// Build packages information
	packageMap := make(map[string]*PackageInfo)
	for _, fileInfo := range output.Files {
		if _, ok := packageMap[fileInfo.Package]; !ok {
			packageMap[fileInfo.Package] = &PackageInfo{
				Name:      fileInfo.Package,
				Path:      filepath.Dir(fileInfo.Path),
				Files:     []string{},
				Functions: []string{},
			}
		}
		
		// Add file to package
		packageMap[fileInfo.Package].Files = append(packageMap[fileInfo.Package].Files, fileInfo.Path)
		
		// Add functions to package
		for _, function := range fileInfo.Functions {
			packageMap[fileInfo.Package].Functions = append(packageMap[fileInfo.Package].Functions, function.ID)
		}
	}
	
	// Convert packages map to slice
	output.Packages = make([]PackageInfo, 0, len(packageMap))
	for _, pkg := range packageMap {
		output.Packages = append(output.Packages, *pkg)
	}
	
	// Sort packages by name
	sort.Slice(output.Packages, func(i, j int) bool {
		return output.Packages[i].Name < output.Packages[j].Name
	})
	
	// Build call graph (simplified estimate of potential calls)
	output.CallGraph = &Graph{
		Nodes: []GraphNode{},
		Edges: []GraphEdge{},
	}
	
	// Add function nodes
	for _, function := range output.Functions {
		output.CallGraph.Nodes = append(output.CallGraph.Nodes, GraphNode{
			ID:       function.ID,
			Label:    function.Name,
			Type:     getFunctionType(function),
			Package:  function.Package,
			Filepath: function.Filepath,
		})
	}
	
	// Add package nodes
	for _, pkg := range output.Packages {
		output.CallGraph.Nodes = append(output.CallGraph.Nodes, GraphNode{
			ID:      "pkg:" + pkg.Name,
			Label:   pkg.Name,
			Type:    "package",
			Package: pkg.Name,
		})
	}
	
	// Create simple call edges (this is a simplified approach; actual call graph would require deeper analysis)
	// Here we simply create edges between functions in the same file, which is just a placeholder
	// A real implementation would use AST analysis or LSP to determine actual function calls
	for _, file := range output.Files {
		for i, caller := range file.Functions {
			// Add edge from function to its package
			output.CallGraph.Edges = append(output.CallGraph.Edges, GraphEdge{
				Source: caller.ID,
				Target: "pkg:" + caller.Package,
				Type:   "belongs_to",
			})
			
			// Add simple edges between functions in the same file
			// This is just a placeholder for demonstration
			for j, callee := range file.Functions {
				if i != j {
					// Avoid self-loops
					output.CallGraph.Edges = append(output.CallGraph.Edges, GraphEdge{
						Source: caller.ID,
						Target: callee.ID,
						Type:   "potential_call",
					})
				}
			}
		}
	}
	
	// Build visualization data
	output.VisualizationData = &VisualizationData{
		PackageHierarchy: buildPackageHierarchy(output.Files, output.Packages),
		CallNetwork:      buildCallNetwork(output.Functions, output.CallGraph),
		ModuleDependencies: buildDependencyWheel(output.Packages),
		FunctionComplexity: buildComplexityData(output.Functions),
	}
	
	// Count methods
	methodCount := 0
	for _, function := range output.Functions {
		if function.IsMethod {
			methodCount++
		}
	}
	output.Metadata.NumMethods = methodCount
	
	// Write enhanced output to JSON file
	jsonData, err := json.MarshalIndent(output, "", "  ")
	if err != nil {
		return fmt.Errorf("error marshaling enhanced JSON: %v", err)
	}
	
	err = ioutil.WriteFile(outputPath, jsonData, 0644)
	if err != nil {
		return fmt.Errorf("error writing enhanced JSON file: %v", err)
	}
	
	log.Printf("Enhanced JSON output written to %s", outputPath)
	return nil
}

// createFunctionID creates a unique ID for a function
func createFunctionID(pkg, name string) string {
	return fmt.Sprintf("%s.%s", pkg, name)
}

// buildFunctionSignature builds a function signature string
func buildFunctionSignature(function FunctionInfo) string {
	var signature strings.Builder
	
	// Add "func" keyword
	signature.WriteString("func ")
	
	// Add receiver if method
	if function.IsMethod && function.Receiver != nil {
		signature.WriteString("(")
		if function.Receiver.Name != "" {
			signature.WriteString(function.Receiver.Name)
			signature.WriteString(" ")
		}
		signature.WriteString(function.Receiver.Type)
		signature.WriteString(") ")
	}
	
	// Add function name
	signature.WriteString(function.Name)
	
	// Add parameters
	signature.WriteString("(")
	for i, param := range function.Parameters {
		if i > 0 {
			signature.WriteString(", ")
		}
		if param.Name != "" {
			signature.WriteString(param.Name)
			signature.WriteString(" ")
		}
		signature.WriteString(param.Type)
	}
	signature.WriteString(")")
	
	// Add return types
	if len(function.ReturnTypes) > 0 {
		if len(function.ReturnTypes) > 1 {
			signature.WriteString(" (")
			for i, returnType := range function.ReturnTypes {
				if i > 0 {
					signature.WriteString(", ")
				}
				signature.WriteString(returnType)
			}
			signature.WriteString(")")
		} else {
			signature.WriteString(" ")
			signature.WriteString(function.ReturnTypes[0])
		}
	}
	
	return signature.String()
}

// getFunctionType returns the type of a function
func getFunctionType(function FunctionInfo) string {
	if function.IsMethod {
		return "method"
	}
	return "function"
}

// buildPackageHierarchy builds a hierarchical representation of packages
func buildPackageHierarchy(files []FileInfo, packages []PackageInfo) *HierarchyNode {
	root := &HierarchyNode{
		Name:     "root",
		Children: []*HierarchyNode{},
	}
	
	// Create package nodes
	packageNodes := make(map[string]*HierarchyNode)
	for _, pkg := range packages {
		packageNodes[pkg.Name] = &HierarchyNode{
			Name:     pkg.Name,
			Path:     pkg.Path,
			Value:    len(pkg.Functions),
			Children: []*HierarchyNode{},
		}
		
		// Add to root
		root.Children = append(root.Children, packageNodes[pkg.Name])
	}
	
	return root
}

// buildCallNetwork builds a network representation of function calls
func buildCallNetwork(functions []FunctionInfo, callGraph *Graph) *NetworkVisualization {
	network := &NetworkVisualization{
		Nodes: []NetworkNode{},
		Links: []NetworkLink{},
	}
	
	// Create nodes for each function
	for _, function := range functions {
		network.Nodes = append(network.Nodes, NetworkNode{
			ID:       function.ID,
			Label:    function.Name,
			Package:  function.Package,
			Group:    function.Package,
			Value:    function.EndLine - function.StartLine + 1, // Line count as value
			IsMethod: function.IsMethod,
			Type:     getFunctionType(function),
		})
	}
	
	// Create links from call graph edges
	nodeMap := make(map[string]bool)
	for _, node := range network.Nodes {
		nodeMap[node.ID] = true
	}
	
	for _, edge := range callGraph.Edges {
		// Skip package edges for the network visualization
		if strings.HasPrefix(edge.Target, "pkg:") || strings.HasPrefix(edge.Source, "pkg:") {
			continue
		}
		
		// Check that both nodes exist
		if _, ok := nodeMap[edge.Source]; !ok {
			continue
		}
		if _, ok := nodeMap[edge.Target]; !ok {
			continue
		}
		
		network.Links = append(network.Links, NetworkLink{
			Source: edge.Source,
			Target: edge.Target,
			Value:  1, // Default value
			Type:   edge.Type,
		})
	}
	
	return network
}

// buildDependencyWheel builds a dependency wheel representation of package dependencies
func buildDependencyWheel(packages []PackageInfo) *DependencyWheel {
	// Create a list of package names
	names := make([]string, len(packages))
	for i, pkg := range packages {
		names[i] = pkg.Name
	}
	
	// Create an adjacency matrix (simplified; in a real implementation this would represent actual dependencies)
	matrix := make([][]int, len(packages))
	for i := range matrix {
		matrix[i] = make([]int, len(packages))
		for j := range matrix[i] {
			// Create some dummy dependencies for demonstration
			if i != j && (i+j)%3 == 0 {
				matrix[i][j] = 1
			}
		}
	}
	
	return &DependencyWheel{
		Names:  names,
		Matrix: matrix,
	}
}

// buildComplexityData builds function complexity data for visualization
func buildComplexityData(functions []FunctionInfo) []ComplexityData {
	data := make([]ComplexityData, 0, len(functions))
	
	for _, function := range functions {
		// Calculate a simple "complexity" based on function length
		// In a real implementation, this would be calculated using proper complexity metrics
		complexity := (function.EndLine - function.StartLine + 1) / 5
		if complexity < 1 {
			complexity = 1
		}
		
		data = append(data, ComplexityData{
			Name:       function.Name,
			Package:    function.Package,
			Complexity: complexity,
			Lines:      function.EndLine - function.StartLine + 1,
		})
	}
	
	return data
}

// EnhanceAnalysisJSON takes an existing analysis JSON file and enhances it
func EnhanceAnalysisJSON(inputPath, outputPath string) error {
	// Read the input JSON file
	data, err := ioutil.ReadFile(inputPath)
	if err != nil {
		return fmt.Errorf("error reading input file: %v", err)
	}
	
	// Parse the JSON
	var analysisResult map[string]interface{}
	if err := json.Unmarshal(data, &analysisResult); err != nil {
		return fmt.Errorf("error parsing JSON: %v", err)
	}
	
	// Format and write the enhanced JSON
	return FormatJSON(analysisResult, outputPath)
}

func main() {
	// Check arguments
	if len(os.Args) < 3 {
		fmt.Println("Usage: json_formatter <input_json> <output_json>")
		os.Exit(1)
	}
	
	inputFile := os.Args[1]
	outputFile := os.Args[2]
	
	// Enhance the JSON
	err := EnhanceAnalysisJSON(inputFile, outputFile)
	if err != nil {
		log.Fatalf("Error enhancing JSON: %v", err)
	}
	
	fmt.Printf("Enhanced JSON output written to %s\n", outputFile)
}