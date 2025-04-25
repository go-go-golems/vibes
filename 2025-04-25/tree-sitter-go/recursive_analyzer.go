package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

// FileInfo represents information about a Go file
type FileInfo struct {
	Path       string         `json:"path"`
	Package    string         `json:"package"`
	Imports    []ImportInfo   `json:"imports"`
	Functions  []FunctionInfo `json:"functions"`
	LastUpdate time.Time      `json:"lastUpdate"`
}

// ImportInfo represents an import statement
type ImportInfo struct {
	Path  string `json:"path"`
	Alias string `json:"alias,omitempty"`
}

// FunctionInfo represents information about a function
type FunctionInfo struct {
	Name       string           `json:"name"`
	Filepath   string           `json:"filepath"`
	StartLine  int              `json:"startLine"`
	EndLine    int              `json:"endLine"`
	StartCol   int              `json:"startCol"`
	EndCol     int              `json:"endCol"`
	IsExported bool             `json:"isExported"`
	IsMethod   bool             `json:"isMethod"`
	Receiver   *ReceiverInfo    `json:"receiver,omitempty"`
	Parameters []ParameterInfo  `json:"parameters"`
	ReturnType []string         `json:"returnType"`
	Comments   string           `json:"comments,omitempty"`
	Package    string           `json:"package"`
}

// ReceiverInfo represents information about a method receiver
type ReceiverInfo struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// ParameterInfo represents a function parameter
type ParameterInfo struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// CodebaseAnalysis represents the result of analyzing a codebase
type CodebaseAnalysis struct {
	BasePath     string            `json:"basePath"`
	Files        []FileInfo        `json:"files"`
	Functions    []FunctionInfo    `json:"functions"`
	Packages     map[string]string `json:"packages"` // package name -> directory
	Dependencies []string          `json:"dependencies"`
	Stats        AnalysisStats     `json:"stats"`
}

// AnalysisStats provides statistics about the analysis
type AnalysisStats struct {
	TotalFiles     int       `json:"totalFiles"`
	TotalFunctions int       `json:"totalFunctions"`
	TotalMethods   int       `json:"totalMethods"`
	TotalPackages  int       `json:"totalPackages"`
	AnalysisTime   float64   `json:"analysisTimeSeconds"`
	Timestamp      time.Time `json:"timestamp"`
}

// FileAnalyzer handles the analysis of Go files
type FileAnalyzer struct {
	parser   *sitter.Parser
	language *sitter.Language
	queries  map[string]*sitter.Query
}

// NewFileAnalyzer creates a new file analyzer
func NewFileAnalyzer() (*FileAnalyzer, error) {
	parser := sitter.NewParser()
	language := golang.GetLanguage()
	parser.SetLanguage(language)

	// Prepare queries
	queries := make(map[string]*sitter.Query)

	// Query for package names
	packageQuery, err := sitter.NewQuery(
		[]byte(`(package_clause (package_identifier) @package_name)`),
		language,
	)
	if err != nil {
		return nil, fmt.Errorf("failed to create package query: %v", err)
	}
	queries["package"] = packageQuery

	// Query for imports
	importQuery, err := sitter.NewQuery(
		[]byte(`
			(import_declaration 
				(import_spec_list
					(import_spec 
						path: [(interpreted_string_literal) (raw_string_literal)] @import_path 
						name: (package_identifier)? @import_alias
					)
				)
			)
			(import_declaration 
				(import_spec 
					path: [(interpreted_string_literal) (raw_string_literal)] @import_path 
					name: (package_identifier)? @import_alias
				)
			)
		`),
		language,
	)
	if err != nil {
		return nil, fmt.Errorf("failed to create import query: %v", err)
	}
	queries["import"] = importQuery

	// Query for functions
	functionQuery, err := sitter.NewQuery(
		[]byte(`
			(function_declaration
				name: (identifier) @func_name
				parameters: (parameter_list) @parameters
				result: [
					(type_identifier) @return_type
					(parameter_list) @return_params
				]?
			) @function
		`),
		language,
	)
	if err != nil {
		return nil, fmt.Errorf("failed to create function query: %v", err)
	}
	queries["function"] = functionQuery

	// Query for methods
	methodQuery, err := sitter.NewQuery(
		[]byte(`
			(method_declaration
				receiver: (parameter_list
					(parameter_declaration
						name: (identifier)? @receiver_name
						type: [
							(type_identifier) @receiver_type
							(pointer_type
								(type_identifier) @receiver_pointer_type)
						]
					)
				)
				name: (field_identifier) @method_name
				parameters: (parameter_list) @parameters
				result: [
					(type_identifier) @return_type
					(parameter_list) @return_params
				]?
			) @method
		`),
		language,
	)
	if err != nil {
		return nil, fmt.Errorf("failed to create method query: %v", err)
	}
	queries["method"] = methodQuery

	// Query for parameters
	parameterQuery, err := sitter.NewQuery(
		[]byte(`
			(parameter_declaration
				name: (identifier)? @param_name
				type: [
					(type_identifier) @param_type
					(array_type) @param_type
					(slice_type) @param_type
					(map_type) @param_type
					(pointer_type) @param_type
					(qualified_type) @param_type
					(interface_type) @param_type
					(struct_type) @param_type
				]
			) @parameter
		`),
		language,
	)
	if err != nil {
		return nil, fmt.Errorf("failed to create parameter query: %v", err)
	}
	queries["parameter"] = parameterQuery

	// Query for comments
	commentQuery, err := sitter.NewQuery(
		[]byte(`(comment) @comment`),
		language,
	)
	if err != nil {
		return nil, fmt.Errorf("failed to create comment query: %v", err)
	}
	queries["comment"] = commentQuery

	return &FileAnalyzer{
		parser:   parser,
		language: language,
		queries:  queries,
	}, nil
}

// AnalyzeFile analyzes a single Go file
func (a *FileAnalyzer) AnalyzeFile(filePath string) (*FileInfo, error) {
	// Read file content
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read file: %v", err)
	}

	// Parse the file
	tree, err := a.parser.ParseCtx(nil, nil, content)
	if err != nil {
		return nil, fmt.Errorf("failed to parse file: %v", err)
	}
	defer tree.Close()

	// Extract package name
	packageName, err := a.extractPackageName(tree.RootNode(), content)
	if err != nil {
		return nil, fmt.Errorf("failed to extract package name: %v", err)
	}

	// Extract imports
	imports, err := a.extractImports(tree.RootNode(), content)
	if err != nil {
		return nil, fmt.Errorf("failed to extract imports: %v", err)
	}

	// Extract functions
	functions, err := a.extractFunctions(tree.RootNode(), content, filePath, packageName)
	if err != nil {
		return nil, fmt.Errorf("failed to extract functions: %v", err)
	}

	// Extract methods
	methods, err := a.extractMethods(tree.RootNode(), content, filePath, packageName)
	if err != nil {
		return nil, fmt.Errorf("failed to extract methods: %v", err)
	}

	// Combine functions and methods
	allFunctions := append(functions, methods...)

	// Extract comments
	a.attachCommentsToFunctions(tree.RootNode(), content, allFunctions)

	// Create file info
	fileInfo := &FileInfo{
		Path:       filePath,
		Package:    packageName,
		Imports:    imports,
		Functions:  allFunctions,
		LastUpdate: time.Now(),
	}

	return fileInfo, nil
}

// extractPackageName extracts the package name from a file
func (a *FileAnalyzer) extractPackageName(node *sitter.Node, content []byte) (string, error) {
	cursor := sitter.NewQueryCursor()
	cursor.Exec(a.queries["package"], node)

	// Get the first match (there should only be one package clause)
	match, ok := cursor.NextMatch()
	if !ok {
		return "", fmt.Errorf("no package clause found")
	}

	// Extract package name
	for _, capture := range match.Captures {
		if capture.Node != nil {
			return string(content[capture.Node.StartByte():capture.Node.EndByte()]), nil
		}
	}

	return "", fmt.Errorf("failed to extract package name")
}

// extractImports extracts import statements from a file
func (a *FileAnalyzer) extractImports(node *sitter.Node, content []byte) ([]ImportInfo, error) {
	cursor := sitter.NewQueryCursor()
	cursor.Exec(a.queries["import"], node)

	var imports []ImportInfo

	// Process all matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}

		var importPath string
		var importAlias string

		// Extract import path and alias
		for _, capture := range match.Captures {
			switch capture.Index {
			case 0: // import_path
				// Remove quotes from import path
				path := string(content[capture.Node.StartByte()+1 : capture.Node.EndByte()-1])
				importPath = path
			case 1: // import_alias (optional)
				if capture.Node != nil {
					importAlias = string(content[capture.Node.StartByte():capture.Node.EndByte()])
				}
			}
		}

		if importPath != "" {
			imports = append(imports, ImportInfo{
				Path:  importPath,
				Alias: importAlias,
			})
		}
	}

	return imports, nil
}

// extractFunctions extracts function declarations from a file
func (a *FileAnalyzer) extractFunctions(node *sitter.Node, content []byte, filePath, packageName string) ([]FunctionInfo, error) {
	cursor := sitter.NewQueryCursor()
	cursor.Exec(a.queries["function"], node)

	var functions []FunctionInfo

	// Process all matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}

		var funcName string
		var funcNode *sitter.Node
		var paramNode *sitter.Node
		var returnTypeNode *sitter.Node
		var returnParamsNode *sitter.Node

		// Extract function information
		for _, capture := range match.Captures {
			switch capture.Index {
			case 0: // func_name
				funcName = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			case 1: // parameters
				paramNode = capture.Node
			case 2: // return_type or return_params
				returnTypeNode = capture.Node
			case 3: // return_params if return_type is set
				returnParamsNode = capture.Node
			case 4: // function
				funcNode = capture.Node
			}
		}

		if funcNode != nil && funcName != "" {
			// Determine if the function is exported
			isExported := len(funcName) > 0 && funcName[0] >= 'A' && funcName[0] <= 'Z'

			// Get function position
			startPoint := funcNode.StartPoint()
			endPoint := funcNode.EndPoint()

			// Extract parameters
			parameters := a.extractParameters(paramNode, content)

			// Extract return types
			returnTypes := a.extractReturnTypes(returnTypeNode, returnParamsNode, content)

			// Create function info
			functionInfo := FunctionInfo{
				Name:       funcName,
				Filepath:   filePath,
				StartLine:  int(startPoint.Row) + 1,
				EndLine:    int(endPoint.Row) + 1,
				StartCol:   int(startPoint.Column),
				EndCol:     int(endPoint.Column),
				IsExported: isExported,
				IsMethod:   false,
				Parameters: parameters,
				ReturnType: returnTypes,
				Package:    packageName,
			}

			functions = append(functions, functionInfo)
		}
	}

	return functions, nil
}

// extractMethods extracts method declarations from a file
func (a *FileAnalyzer) extractMethods(node *sitter.Node, content []byte, filePath, packageName string) ([]FunctionInfo, error) {
	cursor := sitter.NewQueryCursor()
	cursor.Exec(a.queries["method"], node)

	var methods []FunctionInfo

	// Process all matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}

		var methodName string
		var methodNode *sitter.Node
		var receiverName string
		var receiverType string
		var receiverPointerType string
		var paramNode *sitter.Node
		var returnTypeNode *sitter.Node
		var returnParamsNode *sitter.Node

		// Extract method information
		for _, capture := range match.Captures {
			switch capture.Index {
			case 0: // receiver_name
				if capture.Node != nil {
					receiverName = string(content[capture.Node.StartByte():capture.Node.EndByte()])
				}
			case 1: // receiver_type
				receiverType = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			case 2: // receiver_pointer_type
				receiverPointerType = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			case 3: // method_name
				methodName = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			case 4: // parameters
				paramNode = capture.Node
			case 5: // return_type
				returnTypeNode = capture.Node
			case 6: // return_params
				returnParamsNode = capture.Node
			case 7: // method
				methodNode = capture.Node
			}
		}

		if methodNode != nil && methodName != "" {
			// Determine if the method is exported
			isExported := len(methodName) > 0 && methodName[0] >= 'A' && methodName[0] <= 'Z'

			// Get method position
			startPoint := methodNode.StartPoint()
			endPoint := methodNode.EndPoint()

			// Use pointer type if available
			finalReceiverType := receiverType
			if receiverPointerType != "" {
				finalReceiverType = "*" + receiverPointerType
			}

			// Extract parameters
			parameters := a.extractParameters(paramNode, content)

			// Extract return types
			returnTypes := a.extractReturnTypes(returnTypeNode, returnParamsNode, content)

			// Create method info
			methodInfo := FunctionInfo{
				Name:       methodName,
				Filepath:   filePath,
				StartLine:  int(startPoint.Row) + 1,
				EndLine:    int(endPoint.Row) + 1,
				StartCol:   int(startPoint.Column),
				EndCol:     int(endPoint.Column),
				IsExported: isExported,
				IsMethod:   true,
				Receiver: &ReceiverInfo{
					Name: receiverName,
					Type: finalReceiverType,
				},
				Parameters: parameters,
				ReturnType: returnTypes,
				Package:    packageName,
			}

			methods = append(methods, methodInfo)
		}
	}

	return methods, nil
}

// extractParameters extracts parameters from a parameter list
func (a *FileAnalyzer) extractParameters(paramNode *sitter.Node, content []byte) []ParameterInfo {
	if paramNode == nil {
		return nil
	}

	cursor := sitter.NewQueryCursor()
	cursor.Exec(a.queries["parameter"], paramNode)

	var parameters []ParameterInfo

	// Process all matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}

		var paramName string
		var paramType string

		// Extract parameter information
		for _, capture := range match.Captures {
			switch capture.Index {
			case 0: // param_name
				if capture.Node != nil {
					paramName = string(content[capture.Node.StartByte():capture.Node.EndByte()])
				}
			case 1: // param_type
				paramType = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			}
		}

		// Add parameter only if we have a type
		if paramType != "" {
			parameters = append(parameters, ParameterInfo{
				Name: paramName,
				Type: paramType,
			})
		}
	}

	return parameters
}

// extractReturnTypes extracts return types from return nodes
func (a *FileAnalyzer) extractReturnTypes(returnTypeNode, returnParamsNode *sitter.Node, content []byte) []string {
	var returnTypes []string

	// Single return type
	if returnTypeNode != nil && returnParamsNode == nil {
		returnType := string(content[returnTypeNode.StartByte():returnTypeNode.EndByte()])
		returnTypes = append(returnTypes, returnType)
	}

	// Multiple return types
	if returnParamsNode != nil {
		cursor := sitter.NewQueryCursor()
		cursor.Exec(a.queries["parameter"], returnParamsNode)

		// Process all matches
		for {
			match, ok := cursor.NextMatch()
			if !ok {
				break
			}

			// Extract return type
			for _, capture := range match.Captures {
				if capture.Index == 1 { // param_type
					returnType := string(content[capture.Node.StartByte():capture.Node.EndByte()])
					returnTypes = append(returnTypes, returnType)
				}
			}
		}
	}

	return returnTypes
}

// attachCommentsToFunctions attaches comments to functions
func (a *FileAnalyzer) attachCommentsToFunctions(node *sitter.Node, content []byte, functions []FunctionInfo) {
	cursor := sitter.NewQueryCursor()
	cursor.Exec(a.queries["comment"], node)

	// Map of line numbers to comments
	lineComments := make(map[int]string)

	// Process all comments
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}

		// Extract comment
		for _, capture := range match.Captures {
			commentNode := capture.Node
			startPoint := commentNode.StartPoint()
			line := int(startPoint.Row) + 1

			commentText := string(content[commentNode.StartByte():commentNode.EndByte()])
			lineComments[line] = commentText
		}
	}

	// Attach comments to functions (comments are typically right before the function)
	for i := range functions {
		var commentLines []string

		// Look for comments in the lines before the function
		for line := functions[i].StartLine - 5; line < functions[i].StartLine; line++ {
			if comment, ok := lineComments[line]; ok {
				// Clean up comment
				comment = strings.TrimSpace(comment)
				if strings.HasPrefix(comment, "//") {
					comment = strings.TrimSpace(comment[2:])
				} else if strings.HasPrefix(comment, "/*") && strings.HasSuffix(comment, "*/") {
					comment = strings.TrimSpace(comment[2 : len(comment)-2])
				}

				commentLines = append(commentLines, comment)
			}
		}

		// Join comments
		if len(commentLines) > 0 {
			functions[i].Comments = strings.Join(commentLines, "\n")
		}
	}
}

// CodebaseAnalyzer handles the analysis of a codebase
type CodebaseAnalyzer struct {
	fileAnalyzer     *FileAnalyzer
	excludeDirs      map[string]bool
	excludeFileRegex []string
	concurrency      int
}

// NewCodebaseAnalyzer creates a new codebase analyzer
func NewCodebaseAnalyzer(excludeDirs []string, excludeFileRegex []string, concurrency int) (*CodebaseAnalyzer, error) {
	// Create file analyzer
	fileAnalyzer, err := NewFileAnalyzer()
	if err != nil {
		return nil, err
	}

	// Convert exclude dirs to map for fast lookup
	excludeDirMap := make(map[string]bool)
	for _, dir := range excludeDirs {
		excludeDirMap[dir] = true
	}

	return &CodebaseAnalyzer{
		fileAnalyzer:     fileAnalyzer,
		excludeDirs:      excludeDirMap,
		excludeFileRegex: excludeFileRegex,
		concurrency:      concurrency,
	}, nil
}

// AnalyzeCodebase analyzes a codebase
func (a *CodebaseAnalyzer) AnalyzeCodebase(basePath string) (*CodebaseAnalysis, error) {
	startTime := time.Now()

	// Convert to absolute path
	absBasePath, err := filepath.Abs(basePath)
	if err != nil {
		return nil, fmt.Errorf("failed to get absolute path: %v", err)
	}

	// Find all Go files
	goFiles, err := a.findGoFiles(absBasePath)
	if err != nil {
		return nil, fmt.Errorf("failed to find Go files: %v", err)
	}

	log.Printf("Found %d Go files in %s", len(goFiles), absBasePath)

	// Analyze Go files
	fileInfos, err := a.analyzeGoFiles(goFiles)
	if err != nil {
		return nil, fmt.Errorf("failed to analyze Go files: %v", err)
	}

	// Extract unique functions and packages
	var allFunctions []FunctionInfo
	packages := make(map[string]string)

	for _, fileInfo := range fileInfos {
		// Add functions
		allFunctions = append(allFunctions, fileInfo.Functions...)

		// Add package
		if fileInfo.Package != "" {
			dir := filepath.Dir(fileInfo.Path)
			// Remove base path prefix for relative path
			relDir, err := filepath.Rel(absBasePath, dir)
			if err == nil {
				packages[fileInfo.Package] = relDir
			}
		}
	}

	// Extract dependencies
	dependencies := a.extractDependencies(fileInfos)

	// Calculate stats
	methodCount := 0
	for _, f := range allFunctions {
		if f.IsMethod {
			methodCount++
		}
	}

	stats := AnalysisStats{
		TotalFiles:     len(fileInfos),
		TotalFunctions: len(allFunctions),
		TotalMethods:   methodCount,
		TotalPackages:  len(packages),
		AnalysisTime:   time.Since(startTime).Seconds(),
		Timestamp:      time.Now(),
	}

	// Create analysis result
	analysis := &CodebaseAnalysis{
		BasePath:     absBasePath,
		Files:        fileInfos,
		Functions:    allFunctions,
		Packages:     packages,
		Dependencies: dependencies,
		Stats:        stats,
	}

	return analysis, nil
}

// findGoFiles recursively finds all Go files in a directory
func (a *CodebaseAnalyzer) findGoFiles(basePath string) ([]string, error) {
	var goFiles []string

	err := filepath.Walk(basePath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Skip excluded directories
		if info.IsDir() {
			dirName := info.Name()
			if a.excludeDirs[dirName] || strings.HasPrefix(dirName, ".") {
				return filepath.SkipDir
			}
			return nil
		}

		// Process only Go files
		if filepath.Ext(path) == ".go" {
			// Skip test files
			if strings.HasSuffix(path, "_test.go") {
				return nil
			}

			// Skip excluded files
			fileName := filepath.Base(path)
			for _, pattern := range a.excludeFileRegex {
				if strings.Contains(fileName, pattern) {
					return nil
				}
			}

			goFiles = append(goFiles, path)
		}

		return nil
	})

	return goFiles, err
}

// analyzeGoFiles analyzes Go files in parallel
func (a *CodebaseAnalyzer) analyzeGoFiles(filePaths []string) ([]FileInfo, error) {
	var fileInfos []FileInfo
	var mutex sync.Mutex
	var wg sync.WaitGroup
	semaphore := make(chan struct{}, a.concurrency)
	errorCh := make(chan error, len(filePaths))

	for _, filePath := range filePaths {
		wg.Add(1)
		semaphore <- struct{}{} // Acquire semaphore

		go func(path string) {
			defer wg.Done()
			defer func() { <-semaphore }() // Release semaphore

			fileInfo, err := a.fileAnalyzer.AnalyzeFile(path)
			if err != nil {
				log.Printf("Error analyzing %s: %v", path, err)
				errorCh <- fmt.Errorf("failed to analyze %s: %v", path, err)
				return
			}

			mutex.Lock()
			fileInfos = append(fileInfos, *fileInfo)
			mutex.Unlock()
		}(filePath)
	}

	wg.Wait()
	close(errorCh)

	// Check for errors
	var errors []string
	for err := range errorCh {
		errors = append(errors, err.Error())
	}

	if len(errors) > 0 {
		return fileInfos, fmt.Errorf("some files failed to analyze: %s", strings.Join(errors, "; "))
	}

	return fileInfos, nil
}

// extractDependencies extracts dependencies from imports
func (a *CodebaseAnalyzer) extractDependencies(fileInfos []FileInfo) []string {
	dependencyMap := make(map[string]bool)

	for _, fileInfo := range fileInfos {
		for _, importInfo := range fileInfo.Imports {
			// Skip standard library and internal imports
			if !strings.Contains(importInfo.Path, ".") {
				continue
			}

			// Extract root package
			parts := strings.Split(importInfo.Path, "/")
			if len(parts) > 0 {
				rootPkg := parts[0]
				if strings.Contains(rootPkg, ".") {
					dependencyMap[importInfo.Path] = true
				}
			}
		}
	}

	// Convert map to slice
	var dependencies []string
	for dep := range dependencyMap {
		dependencies = append(dependencies, dep)
	}

	return dependencies
}

// SaveAnalysis saves analysis results to a JSON file
func SaveAnalysis(analysis *CodebaseAnalysis, outputPath string) error {
	// Create JSON
	data, err := json.MarshalIndent(analysis, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal analysis: %v", err)
	}

	// Write to file
	err = ioutil.WriteFile(outputPath, data, 0644)
	if err != nil {
		return fmt.Errorf("failed to write analysis file: %v", err)
	}

	return nil
}

func main() {
	// Define command-line flags
	basePath := flag.String("path", ".", "Path to the codebase to analyze")
	outputPath := flag.String("output", "codebase_analysis.json", "Output JSON file path")
	excludeFlag := flag.String("exclude", "vendor,node_modules,dist,build", "Comma-separated list of directories to exclude")
	excludeFilesFlag := flag.String("exclude-files", "generated,auto_gen", "Comma-separated list of file name patterns to exclude")
	concurrency := flag.Int("concurrency", 4, "Number of files to analyze concurrently")
	verbose := flag.Bool("verbose", false, "Enable verbose logging")
	flag.Parse()

	if *verbose {
		log.Printf("Analyzing %s...", *basePath)
	}

	// Parse exclude directories
	var excludeDirs []string
	if *excludeFlag != "" {
		excludeDirs = strings.Split(*excludeFlag, ",")
	}

	// Parse exclude file patterns
	var excludeFileRegex []string
	if *excludeFilesFlag != "" {
		excludeFileRegex = strings.Split(*excludeFilesFlag, ",")
	}

	// Create codebase analyzer
	analyzer, err := NewCodebaseAnalyzer(excludeDirs, excludeFileRegex, *concurrency)
	if err != nil {
		log.Fatalf("Failed to create analyzer: %v", err)
	}

	// Analyze codebase
	analysis, err := analyzer.AnalyzeCodebase(*basePath)
	if err != nil {
		log.Fatalf("Failed to analyze codebase: %v", err)
	}

	// Save analysis results
	err = SaveAnalysis(analysis, *outputPath)
	if err != nil {
		log.Fatalf("Failed to save analysis: %v", err)
	}

	// Print summary
	fmt.Printf("Analysis complete.\n")
	fmt.Printf("Files analyzed: %d\n", analysis.Stats.TotalFiles)
	fmt.Printf("Functions found: %d\n", analysis.Stats.TotalFunctions-analysis.Stats.TotalMethods)
	fmt.Printf("Methods found: %d\n", analysis.Stats.TotalMethods)
	fmt.Printf("Packages found: %d\n", analysis.Stats.TotalPackages)
	fmt.Printf("Analysis time: %.2f seconds\n", analysis.Stats.AnalysisTime)
	fmt.Printf("Results written to %s\n", *outputPath)
}