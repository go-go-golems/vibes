package parser

import (
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

// EnhancedFunctionInfo represents detailed information about a function
type EnhancedFunctionInfo struct {
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

// EnhancedGoFileParser represents an enhanced parser for Go files using Tree-sitter
type EnhancedGoFileParser struct {
	parser        *sitter.Parser
	language      *sitter.Language
	funcQuery     string
	methodQuery   string
	parameterQuery string
	commentQuery  string
	packageQuery  string
}

// NewEnhancedGoFileParser creates a new enhanced parser for Go files
func NewEnhancedGoFileParser() *EnhancedGoFileParser {
	parser := sitter.NewParser()
	
	// Set Go language
	language := golang.GetLanguage()
	parser.SetLanguage(language)
	
	// Define the queries for various aspects of functions
	
	// Function declarations
	funcQuery := `
		(function_declaration
			name: (identifier) @func_name
			parameters: (parameter_list) @parameters
			result: [
				(type_identifier) @return_type
				(parameter_list) @return_params
			]?
		) @function
	`
	
	// Method declarations
	methodQuery := `
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
	`
	
	// Parameter extraction
	parameterQuery := `
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
	`
	
	// Comment extraction
	commentQuery := `
		(comment) @comment
	`
	
	// Package name extraction
	packageQuery := `
		(package_clause (package_identifier) @package_name)
	`
	
	return &EnhancedGoFileParser{
		parser:        parser,
		language:      language,
		funcQuery:     funcQuery,
		methodQuery:   methodQuery,
		parameterQuery: parameterQuery,
		commentQuery:  commentQuery,
		packageQuery:  packageQuery,
	}
}

// ParseFile parses a Go file and returns enhanced information about all functions
func (p *EnhancedGoFileParser) ParseFile(filePath string) ([]EnhancedFunctionInfo, error) {
	// Read file content
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, err
	}
	
	// Parse file with Tree-sitter
	tree, err := p.parser.ParseCtx(nil, nil, content)
	if err != nil {
		return nil, err
	}
	defer tree.Close()
	
	// Get package name first
	packageName, err := p.extractPackageName(tree.RootNode(), content)
	if err != nil {
		return nil, err
	}
	
	// Extract functions
	functions, err := p.extractFunctions(tree.RootNode(), content, filePath, packageName)
	if err != nil {
		return nil, err
	}
	
	// Extract methods
	methods, err := p.extractMethods(tree.RootNode(), content, filePath, packageName)
	if err != nil {
		return nil, err
	}
	
	// Combine functions and methods
	allFunctions := append(functions, methods...)
	
	// Extract comments for each function
	p.attachCommentsToFunctions(tree.RootNode(), content, allFunctions)
	
	return allFunctions, nil
}

// extractPackageName extracts the package name from a Go file
func (p *EnhancedGoFileParser) extractPackageName(rootNode *sitter.Node, content []byte) (string, error) {
	// Create and execute the query
	query, err := sitter.NewQuery([]byte(p.packageQuery), p.language)
	if err != nil {
		return "", err
	}
	
	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, rootNode)
	
	// Get the first match (there should only be one package declaration)
	match, ok := cursor.NextMatch()
	if !ok {
		return "", nil
	}
	
	// Extract the package name
	for _, capture := range match.Captures {
		if capture.Index == 0 { // package_name
			return string(content[capture.Node.StartByte():capture.Node.EndByte()]), nil
		}
	}
	
	return "", nil
}

// extractFunctions extracts all functions from a Go file
func (p *EnhancedGoFileParser) extractFunctions(rootNode *sitter.Node, content []byte, filePath string, packageName string) ([]EnhancedFunctionInfo, error) {
	// Create and execute the query
	query, err := sitter.NewQuery([]byte(p.funcQuery), p.language)
	if err != nil {
		return nil, err
	}
	
	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, rootNode)
	
	var functions []EnhancedFunctionInfo
	
	// Process each match
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
		
		// Extract function information from captures
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
			case 4: // function (the whole function declaration)
				funcNode = capture.Node
			}
		}
		
		if funcNode != nil && funcName != "" {
			// Determine if the function is exported (starts with uppercase)
			isExported := false
			if len(funcName) > 0 && strings.ToUpper(funcName[0:1]) == funcName[0:1] {
				isExported = true
			}
			
			// Get the position information
			startPoint := funcNode.StartPoint()
			endPoint := funcNode.EndPoint()
			
			// Extract parameters
			parameters := p.extractParameters(paramNode, content)
			
			// Extract return types
			returnTypes := p.extractReturnTypes(returnTypeNode, returnParamsNode, content)
			
			// Create function info
			functionInfo := EnhancedFunctionInfo{
				Name:       funcName,
				Filepath:   filePath,
				StartLine:  int(startPoint.Row) + 1, // Convert to 1-based line numbering
				EndLine:    int(endPoint.Row) + 1,   // Convert to 1-based line numbering
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

// extractMethods extracts all methods from a Go file
func (p *EnhancedGoFileParser) extractMethods(rootNode *sitter.Node, content []byte, filePath string, packageName string) ([]EnhancedFunctionInfo, error) {
	// Create and execute the query
	query, err := sitter.NewQuery([]byte(p.methodQuery), p.language)
	if err != nil {
		return nil, err
	}
	
	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, rootNode)
	
	var methods []EnhancedFunctionInfo
	
	// Process each match
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
		
		// Extract method information from captures
		for _, capture := range match.Captures {
			switch capture.Index {
			case 0: // receiver_name (optional)
				receiverName = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			case 1: // receiver_type or receiver_pointer_type
				receiverType = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			case 2: // receiver_pointer_type (if the receiver is a pointer)
				receiverPointerType = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			case 3: // method_name
				methodName = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			case 4: // parameters
				paramNode = capture.Node
			case 5: // return_type or return_params
				returnTypeNode = capture.Node
			case 6: // return_params if return_type is set
				returnParamsNode = capture.Node
			case 7: // method (the whole method declaration)
				methodNode = capture.Node
			}
		}
		
		if methodNode != nil && methodName != "" {
			// Determine if the method is exported (starts with uppercase)
			isExported := false
			if len(methodName) > 0 && strings.ToUpper(methodName[0:1]) == methodName[0:1] {
				isExported = true
			}
			
			// Get the position information
			startPoint := methodNode.StartPoint()
			endPoint := methodNode.EndPoint()
			
			// Use pointer type if available
			finalReceiverType := receiverType
			if receiverPointerType != "" {
				finalReceiverType = "*" + receiverPointerType
			}
			
			// Extract parameters
			parameters := p.extractParameters(paramNode, content)
			
			// Extract return types
			returnTypes := p.extractReturnTypes(returnTypeNode, returnParamsNode, content)
			
			// Create method info
			methodInfo := EnhancedFunctionInfo{
				Name:       methodName,
				Filepath:   filePath,
				StartLine:  int(startPoint.Row) + 1, // Convert to 1-based line numbering
				EndLine:    int(endPoint.Row) + 1,   // Convert to 1-based line numbering
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

// extractParameters extracts parameters from a parameter list node
func (p *EnhancedGoFileParser) extractParameters(paramNode *sitter.Node, content []byte) []ParameterInfo {
	if paramNode == nil {
		return nil
	}
	
	// Create and execute the query
	query, err := sitter.NewQuery([]byte(p.parameterQuery), p.language)
	if err != nil {
		return nil
	}
	
	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, paramNode)
	
	var parameters []ParameterInfo
	
	// Process each match
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		
		var paramName string
		var paramType string
		
		// Extract parameter information from captures
		for _, capture := range match.Captures {
			switch capture.Index {
			case 0: // param_name (might be nil for unnamed parameters)
				if capture.Node != nil {
					paramName = string(content[capture.Node.StartByte():capture.Node.EndByte()])
				}
			case 1: // param_type
				paramType = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			}
		}
		
		// Only add parameters with a type
		if paramType != "" {
			param := ParameterInfo{
				Name: paramName,
				Type: paramType,
			}
			parameters = append(parameters, param)
		}
	}
	
	return parameters
}

// extractReturnTypes extracts return types from return nodes
func (p *EnhancedGoFileParser) extractReturnTypes(returnTypeNode, returnParamsNode *sitter.Node, content []byte) []string {
	var returnTypes []string
	
	// Single return type
	if returnTypeNode != nil && returnParamsNode == nil {
		returnType := string(content[returnTypeNode.StartByte():returnTypeNode.EndByte()])
		returnTypes = append(returnTypes, returnType)
	}
	
	// Multiple return types
	if returnParamsNode != nil {
		// Use the parameter extraction query for return parameters
		query, err := sitter.NewQuery([]byte(p.parameterQuery), p.language)
		if err != nil {
			return returnTypes
		}
		
		cursor := sitter.NewQueryCursor()
		cursor.Exec(query, returnParamsNode)
		
		// Process each match (each return parameter)
		for {
			match, ok := cursor.NextMatch()
			if !ok {
				break
			}
			
			// Extract parameter type
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

// attachCommentsToFunctions finds and attaches comments to functions
func (p *EnhancedGoFileParser) attachCommentsToFunctions(rootNode *sitter.Node, content []byte, functions []EnhancedFunctionInfo) {
	// Create and execute the query
	query, err := sitter.NewQuery([]byte(p.commentQuery), p.language)
	if err != nil {
		return
	}
	
	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, rootNode)
	
	// Create a map of line numbers to comments
	lineComments := make(map[int]string)
	
	// Process each match (each comment)
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		
		// Extract the comment
		for _, capture := range match.Captures {
			commentNode := capture.Node
			startPoint := commentNode.StartPoint()
			line := int(startPoint.Row) + 1 // Convert to 1-based line numbering
			
			// Get the comment text
			commentText := string(content[commentNode.StartByte():commentNode.EndByte()])
			
			// Store the comment by line number
			lineComments[line] = commentText
		}
	}
	
	// Attach comments to functions (comments typically appear right before the function)
	for i := range functions {
		var commentLines []string
		
		// Look for comments in the 5 lines before the function
		for line := functions[i].StartLine - 5; line < functions[i].StartLine; line++ {
			if comment, ok := lineComments[line]; ok {
				// Clean up the comment (remove // or /* */ markers)
				comment = strings.TrimSpace(comment)
				if strings.HasPrefix(comment, "//") {
					comment = strings.TrimSpace(comment[2:])
				} else if strings.HasPrefix(comment, "/*") && strings.HasSuffix(comment, "*/") {
					comment = strings.TrimSpace(comment[2 : len(comment)-2])
				}
				
				commentLines = append(commentLines, comment)
			}
		}
		
		// Join all comment lines
		if len(commentLines) > 0 {
			functions[i].Comments = strings.Join(commentLines, "\n")
		}
	}
}

// ParseDirectory recursively parses all Go files in a directory and its subdirectories
func (p *EnhancedGoFileParser) ParseDirectory(dirPath string) ([]EnhancedFunctionInfo, error) {
	var allFunctions []EnhancedFunctionInfo
	
	// Walk the directory and process all Go files
	err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Printf("Error accessing path %s: %v", path, err)
			return nil // Continue walking
		}
		
		// Skip directories
		if info.IsDir() {
			return nil
		}
		
		// Only process Go files
		if filepath.Ext(path) != ".go" {
			return nil
		}
		
		// Skip test files
		if strings.HasSuffix(path, "_test.go") {
			return nil
		}
		
		// Parse the file
		functions, err := p.ParseFile(path)
		if err != nil {
			log.Printf("Error parsing file %s: %v", path, err)
			return nil // Continue walking
		}
		
		// Add functions to the result
		allFunctions = append(allFunctions, functions...)
		return nil
	})
	
	if err != nil {
		return nil, err
	}
	
	return allFunctions, nil
}

// ParsePath parses a file or directory path and returns all functions
func (p *EnhancedGoFileParser) ParsePath(path string) ([]EnhancedFunctionInfo, error) {
	// Check if path is a directory or a file
	fileInfo, err := os.Stat(path)
	if err != nil {
		return nil, err
	}
	
	if fileInfo.IsDir() {
		return p.ParseDirectory(path)
	} else if filepath.Ext(path) == ".go" {
		return p.ParseFile(path)
	}
	
	// Return empty slice for non-Go files
	return []EnhancedFunctionInfo{}, nil
}