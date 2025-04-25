package parser

import (
	"context"
	"io/ioutil"
	"log"
	"strings"
	"time"

	"github.com/pkg/errors"
	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"

	"github.com/wesen/tree-sitter-go-cleaned-up/pkg/models"
)

// GoParser handles the analysis of Go files using Tree-sitter
type GoParser struct {
	parser   *sitter.Parser
	language *sitter.Language
	queries  map[string]*sitter.Query
}

// NewGoParser creates a new Go file parser
func NewGoParser() (*GoParser, error) {
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
		return nil, errors.Wrap(err, "failed to create package query")
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
		return nil, errors.Wrap(err, "failed to create import query")
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
		return nil, errors.Wrap(err, "failed to create function query")
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
		return nil, errors.Wrap(err, "failed to create method query")
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
		return nil, errors.Wrap(err, "failed to create parameter query")
	}
	queries["parameter"] = parameterQuery

	// Query for comments
	commentQuery, err := sitter.NewQuery(
		[]byte(`(comment) @comment`),
		language,
	)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create comment query")
	}
	queries["comment"] = commentQuery

	return &GoParser{
		parser:   parser,
		language: language,
		queries:  queries,
	}, nil
}

// AnalyzeFileContent analyzes a single Go file's content
func (a *GoParser) AnalyzeFileContent(ctx context.Context, filePath string, content []byte) (*models.FileInfo, error) {
	// Parse the file content
	tree, err := a.parser.ParseCtx(ctx, nil, content)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to parse content for file %s", filePath)
	}
	defer tree.Close()

	// Extract package name
	packageName, err := a.extractPackageName(tree.RootNode(), content)
	if err != nil {
		log.Printf("Warning: failed to extract package name from %s: %v", filePath, err)
		packageName = ""
	}

	// Extract imports
	imports, err := a.extractImports(tree.RootNode(), content)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to extract imports from %s", filePath)
	}

	// Extract functions
	functions, err := a.extractFunctions(tree.RootNode(), content, filePath, packageName)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to extract functions from %s", filePath)
	}

	// Extract methods
	methods, err := a.extractMethods(tree.RootNode(), content, filePath, packageName)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to extract methods from %s", filePath)
	}

	// Combine functions and methods
	allFunctions := append(functions, methods...)

	// Extract comments
	a.attachCommentsToFunctions(tree.RootNode(), content, allFunctions)

	// Create file info using models.FileInfo
	fileInfo := &models.FileInfo{
		Path:       filePath,
		AbsPath:    filePath,
		Package:    packageName,
		Imports:    imports,
		Functions:  allFunctions,
		LastUpdate: time.Now(),
	}

	return fileInfo, nil
}

// AnalyzeFile analyzes a single Go file by reading it first
func (a *GoParser) AnalyzeFile(ctx context.Context, filePath string) (*models.FileInfo, error) {
	// Read file content
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to read file %s", filePath)
	}
	return a.AnalyzeFileContent(ctx, filePath, content)
}

// extractPackageName extracts the package name from a file
func (a *GoParser) extractPackageName(node *sitter.Node, content []byte) (string, error) {
	cursor := sitter.NewQueryCursor()
	cursor.Exec(a.queries["package"], node)

	// Get the first match (there should only be one package clause)
	match, ok := cursor.NextMatch()
	if !ok {
		return "", errors.New("no package clause found")
	}

	// Extract package name
	for _, capture := range match.Captures {
		if capture.Node != nil && capture.CaptureName == "package_name" {
			return string(content[capture.Node.StartByte():capture.Node.EndByte()]), nil
		}
	}

	return "", errors.New("failed to extract package name from capture")
}

// extractImports extracts import statements from a file
func (a *GoParser) extractImports(node *sitter.Node, content []byte) ([]ImportInfo, error) {
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

		importPathNode := getNodeByCaptureName(match, "import_path")
		importAliasNode := getNodeByCaptureName(match, "import_alias")

		if importPathNode != nil {
			// Remove quotes from import path
			path := string(content[importPathNode.StartByte()+1 : importPathNode.EndByte()-1])
			importPath = path
		}

		if importAliasNode != nil {
			importAlias = string(content[importAliasNode.StartByte():importAliasNode.EndByte()])
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
func (a *GoParser) extractFunctions(node *sitter.Node, content []byte, filePath, packageName string) ([]FunctionInfo, error) {
	cursor := sitter.NewQueryCursor()
	cursor.Exec(a.queries["function"], node)

	var functions []FunctionInfo

	// Process all matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}

		funcNameNode := getNodeByCaptureName(match, "func_name")
		funcNode := getNodeByCaptureName(match, "function")
		paramNode := getNodeByCaptureName(match, "parameters")
		returnTypeNode := getNodeByCaptureName(match, "return_type")
		returnParamsNode := getNodeByCaptureName(match, "return_params")

		if funcNode != nil && funcNameNode != nil {
			funcName := string(content[funcNameNode.StartByte():funcNameNode.EndByte()])
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
func (a *GoParser) extractMethods(node *sitter.Node, content []byte, filePath, packageName string) ([]FunctionInfo, error) {
	cursor := sitter.NewQueryCursor()
	cursor.Exec(a.queries["method"], node)

	var methods []FunctionInfo

	// Process all matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}

		receiverNameNode := getNodeByCaptureName(match, "receiver_name")
		receiverTypeNode := getNodeByCaptureName(match, "receiver_type")
		receiverPointerTypeNode := getNodeByCaptureName(match, "receiver_pointer_type")
		methodNameNode := getNodeByCaptureName(match, "method_name")
		methodNode := getNodeByCaptureName(match, "method")
		paramNode := getNodeByCaptureName(match, "parameters")
		returnTypeNode := getNodeByCaptureName(match, "return_type")
		returnParamsNode := getNodeByCaptureName(match, "return_params")

		if methodNode != nil && methodNameNode != nil {
			methodName := string(content[methodNameNode.StartByte():methodNameNode.EndByte()])
			// Determine if the method is exported
			isExported := len(methodName) > 0 && methodName[0] >= 'A' && methodName[0] <= 'Z'

			// Get method position
			startPoint := methodNode.StartPoint()
			endPoint := methodNode.EndPoint()

			// Extract receiver info
			receiverName := ""
			if receiverNameNode != nil {
				receiverName = string(content[receiverNameNode.StartByte():receiverNameNode.EndByte()])
			}
			finalReceiverType := ""
			if receiverTypeNode != nil {
				finalReceiverType = string(content[receiverTypeNode.StartByte():receiverTypeNode.EndByte()])
			}
			if receiverPointerTypeNode != nil {
				// If pointer type node exists, override/prepend with *
				finalReceiverType = "*" + string(content[receiverPointerTypeNode.StartByte():receiverPointerTypeNode.EndByte()])
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
func (a *GoParser) extractParameters(paramListNode *sitter.Node, content []byte) []ParameterInfo {
	if paramListNode == nil {
		return nil
	}

	cursor := sitter.NewQueryCursor()
	cursor.Exec(a.queries["parameter"], paramListNode)

	var parameters []ParameterInfo

	// Process all matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}

		paramNameNode := getNodeByCaptureName(match, "param_name")
		paramTypeNode := getNodeByCaptureName(match, "param_type")

		var paramName string
		var paramType string

		if paramNameNode != nil {
			paramName = string(content[paramNameNode.StartByte():paramNameNode.EndByte()])
		}
		if paramTypeNode != nil {
			paramType = string(content[paramTypeNode.StartByte():paramTypeNode.EndByte()])
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
func (a *GoParser) extractReturnTypes(returnTypeNode, returnParamsNode *sitter.Node, content []byte) []string {
	var returnTypes []string

	// Single return type (node exists but is not a parameter list)
	if returnTypeNode != nil && returnTypeNode.Type() != "parameter_list" {
		returnType := string(content[returnTypeNode.StartByte():returnTypeNode.EndByte()])
		returnTypes = append(returnTypes, returnType)
		return returnTypes // Return early if single type found
	}

	// Multiple return types (represented as a parameter_list node)
	// Use returnParamsNode if available, otherwise use returnTypeNode if it's a param list
	paramListForReturn := returnParamsNode
	if paramListForReturn == nil && returnTypeNode != nil && returnTypeNode.Type() == "parameter_list" {
		paramListForReturn = returnTypeNode
	}

	if paramListForReturn != nil {
		cursor := sitter.NewQueryCursor()
		cursor.Exec(a.queries["parameter"], paramListForReturn) // Query within the return param list

		// Process all matches
		for {
			match, ok := cursor.NextMatch()
			if !ok {
				break
			}

			// Extract return type (from the 'type' capture of the parameter query)
			paramTypeNode := getNodeByCaptureName(match, "param_type")
			if paramTypeNode != nil {
				returnType := string(content[paramTypeNode.StartByte():paramTypeNode.EndByte()])
				returnTypes = append(returnTypes, returnType)
			}
		}
	}

	return returnTypes
}

// attachCommentsToFunctions attaches comments to functions
func (a *GoParser) attachCommentsToFunctions(node *sitter.Node, content []byte, functions []FunctionInfo) {
	cursor := sitter.NewQueryCursor()
	cursor.Exec(a.queries["comment"], node)

	// Map of line numbers to comments
	lineComments := make(map[int]string)
	commentEndLine := make(map[int]int)

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
			endPoint := commentNode.EndPoint()
			startLine := int(startPoint.Row) + 1
			endLine := int(endPoint.Row) + 1

			commentText := string(content[commentNode.StartByte():commentNode.EndByte()])

			// Store comment associated with its starting line
			if _, exists := lineComments[startLine]; !exists {
				lineComments[startLine] = commentText
				commentEndLine[startLine] = endLine
			} else {
				// Append if another comment starts on the same line (unlikely but handle)
				lineComments[startLine] += "\n" + commentText
				commentEndLine[startLine] = endLine // Update end line
			}
		}
	}

	// Attach comments to functions (comments immediately preceding the function)
	for i := range functions {
		var associatedComments []string
		lastCommentEndLine := -1

		// Look for comments ending on the line directly before the function start line
		targetLine := functions[i].StartLine - 1
		for startLine, endLine := range commentEndLine {
			if endLine == targetLine {
				comment := lineComments[startLine]
				comment = cleanComment(comment)
				associatedComments = append(associatedComments, comment)
				lastCommentEndLine = endLine // Track the end line of the block
			}
		}

		// If comments were found, look for contiguous comment blocks immediately preceding
		if len(associatedComments) > 0 && lastCommentEndLine != -1 {
			for currentLine := lastCommentEndLine - 1; currentLine > 0; currentLine-- {
				foundContiguous := false
				for startLine, endLine := range commentEndLine {
					if endLine == currentLine {
						comment := lineComments[startLine]
						comment = cleanComment(comment)
						// Prepend to maintain order
						associatedComments = append([]string{comment}, associatedComments...)
						lastCommentEndLine = startLine // Update the start line we are checking against
						foundContiguous = true
						break
					}
				}
				// Stop if the line before wasn't the end of another comment
				if !foundContiguous {
					break
				}
			}
		}

		// Join comments
		if len(associatedComments) > 0 {
			functions[i].Comments = strings.Join(associatedComments, "\n")
		}
	}
}

// cleanComment removes comment markers and trims whitespace
func cleanComment(commentText string) string {
	commentText = strings.TrimSpace(commentText)
	if strings.HasPrefix(commentText, "//") {
		commentText = strings.TrimSpace(commentText[2:])
	} else if strings.HasPrefix(commentText, "/*") && strings.HasSuffix(commentText, "*/") {
		commentText = strings.TrimSpace(commentText[2 : len(commentText)-2])
		// Handle multi-line block comments - split, trim lines, rejoin
		lines := strings.Split(commentText, "\n")
		for j, line := range lines {
			lines[j] = strings.TrimSpace(strings.TrimPrefix(line, "*"))
			lines[j] = strings.TrimSpace(lines[j])
		}
		commentText = strings.Join(lines, "\n")
	}
	return commentText
}

// Helper to get node by capture name
func getNodeByCaptureName(match *sitter.QueryMatch, name string) *sitter.Node {
	for _, capture := range match.Captures {
		if capture.CaptureName == name {
			return capture.Node
		}
	}
	return nil
}
