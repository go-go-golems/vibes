package parser

import (
	"context"
	"io/ioutil"
	"strings"

	"github.com/pkg/errors"
	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

// --- Enhanced Data Structures ---

type ReceiverInfo struct {
	Name    string `json:"name"`
	Type    string `json:"type"`
	Pointer bool   `json:"pointer"`
}

type ParameterInfo struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// EnhancedFunctionInfo holds detailed information about a function or method.
type EnhancedFunctionInfo struct {
	Name        string          `json:"name"`
	Filepath    string          `json:"filepath"`  // Absolute path
	Package     string          `json:"package"`   // Package name
	Signature   string          `json:"signature"` // e.g., func Add(a, b int) int
	Comments    []string        `json:"comments"`
	IsExported  bool            `json:"isExported"`
	IsMethod    bool            `json:"isMethod"`
	Receiver    *ReceiverInfo   `json:"receiver,omitempty"` // Pointer to allow nil for functions
	Parameters  []ParameterInfo `json:"parameters"`
	ReturnTypes []string        `json:"returnTypes"`
	StartLine   int             `json:"startLine"`
	EndLine     int             `json:"endLine"`
	StartCol    int             `json:"startCol"` // 0-based column from Tree-sitter
	EndCol      int             `json:"endCol"`   // 0-based column from Tree-sitter
}

// --- Parser Implementation ---

type Parser struct {
	parser         *sitter.Parser
	language       *sitter.Language
	packageQuery   *sitter.Query
	funcQuery      *sitter.Query
	methodQuery    *sitter.Query
	commentQuery   *sitter.Query
	parameterQuery *sitter.Query // Query to find parameters within func/method signature
	returnQuery    *sitter.Query // Query to find return types within func/method signature
}

func NewParser() (*Parser, error) {
	p := sitter.NewParser()
	lang := golang.GetLanguage()
	p.SetLanguage(lang)

	queries := map[string]string{
		"package": `(package_clause (package_identifier) @package_name)`,
		"func": `
            (function_declaration
                name: (identifier) @name
                parameters: (parameter_list) @params
                result: (_)? @returns
            ) @func`, // Use @name for consistency
		"method": `
            (method_declaration
                receiver: (parameter_list) @receiver
                name: (field_identifier) @name
                parameters: (parameter_list) @params
                result: (_)? @returns
            ) @method`,
		"comment": `(comment) @comment`,
		"param":   `(parameter_declaration name: (identifier)? @param_name type: (_) @param_type)`,
		"return":  `(type_identifier) @return_type`, // Basic return type identifier
		// TODO: Add queries for more complex return types (pointer_type, qualified_type, etc.)
	}

	compiledQueries := make(map[string]*sitter.Query)
	var err error
	for name, queryStr := range queries {
		compiledQueries[name], err = sitter.NewQuery([]byte(queryStr), lang)
		if err != nil {
			// Clean up already compiled queries on error
			for _, q := range compiledQueries {
				if q != nil {
					q.Close()
				}
			}
			return nil, errors.Wrapf(err, "failed to compile query '%s'", name)
		}
	}

	return &Parser{
		parser:         p,
		language:       lang,
		packageQuery:   compiledQueries["package"],
		funcQuery:      compiledQueries["func"],
		methodQuery:    compiledQueries["method"],
		commentQuery:   compiledQueries["comment"],
		parameterQuery: compiledQueries["param"],
		returnQuery:    compiledQueries["return"],
	}, nil
}

func (p *Parser) Close() {
	if p.packageQuery != nil {
		p.packageQuery.Close()
	}
	if p.funcQuery != nil {
		p.funcQuery.Close()
	}
	if p.methodQuery != nil {
		p.methodQuery.Close()
	}
	if p.commentQuery != nil {
		p.commentQuery.Close()
	}
	if p.parameterQuery != nil {
		p.parameterQuery.Close()
	}
	if p.returnQuery != nil {
		p.returnQuery.Close()
	}
}

// ParseFileEnhanced parses a Go file, extracting detailed function/method information.
func (p *Parser) ParseFileEnhanced(ctx context.Context, filePath string) (*EnhancedFileInfo, error) {
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to read file %s", filePath)
	}

	tree, err := p.parser.ParseCtx(ctx, nil, content)
	if err != nil {
		if errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded) {
			return nil, err
		}
		return nil, errors.Wrapf(err, "failed to parse file %s", filePath)
	}
	defer tree.Close()

	rootNode := tree.RootNode()
	packageName := p.extractPackageName(rootNode, content)

	// TODO: Extract Imports

	functions := p.extractFunctionsAndMethods(rootNode, content, filePath, packageName)

	// TODO: Associate comments more accurately

	return &EnhancedFileInfo{
		Path:      filePath, // Keep absolute for now, formatter will make relative
		Package:   packageName,
		Functions: functions,
		// Imports: extractedImports, // Add later
		// Size: len(content),
		// Lines: countLines(content),
	}, nil
}

// Helper function to extract package name
func (p *Parser) extractPackageName(rootNode *sitter.Node, content []byte) string {
	pkgCursor := sitter.NewQueryCursor()
	pkgCursor.Exec(p.packageQuery, rootNode)
	defer pkgCursor.Close()
	if pkgMatch, ok := pkgCursor.NextMatch(); ok {
		for _, capture := range pkgMatch.Captures {
			// Assuming capture name is "package_name"
			return capture.Node.Content(content)
		}
	}
	return ""
}

// Helper to extract functions and methods
func (p *Parser) extractFunctionsAndMethods(rootNode *sitter.Node, content []byte, filePath, packageName string) []EnhancedFunctionInfo {
	var results []EnhancedFunctionInfo
	cursor := sitter.NewQueryCursor()

	// Combine func and method queries for single pass (optional optimization, might complicate capture handling)
	// For clarity, we'll query separately

	// --- Functions ---
	cursor.Exec(p.funcQuery, rootNode)
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		match = cursor.FilterPredicates(match, content) // Handle potential predicates
		if len(match.Captures) == 0 {
			continue
		}

		info := p.processMatch(match, p.funcQuery, content, filePath, packageName, false)
		if info != nil {
			results = append(results, *info)
		}
	}
	cursor.Close() // Close after use

	// --- Methods ---
	cursor.Exec(p.methodQuery, rootNode)
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		match = cursor.FilterPredicates(match, content)
		if len(match.Captures) == 0 {
			continue
		}

		info := p.processMatch(match, p.methodQuery, content, filePath, packageName, true)
		if info != nil {
			results = append(results, *info)
		}
	}
	cursor.Close()

	return results
}

// Process a query match for function or method
func (p *Parser) processMatch(match *sitter.QueryMatch, query *sitter.Query, content []byte, filePath, packageName string, isMethod bool) *EnhancedFunctionInfo {
	var name, signature string
	var funcNode, paramsNode, returnsNode, receiverNode *sitter.Node

	captureMap := map[string]*sitter.Node{}
	for _, capture := range match.Captures {
		captureName := query.CaptureNameForId(capture.Index)
		captureMap[captureName] = capture.Node
	}

	if node, ok := captureMap["name"]; ok {
		name = node.Content(content)
	} else {
		return nil // Must have a name
	}

	if isMethod {
		if node, ok := captureMap["method"]; ok {
			funcNode = node
			signature = funcNode.Content(content) // Basic signature
		}
		if node, ok := captureMap["receiver"]; ok {
			receiverNode = node
		}
	} else {
		if node, ok := captureMap["func"]; ok {
			funcNode = node
			signature = funcNode.Content(content) // Basic signature
		}
	}
	if funcNode == nil {
		return nil
	}

	if node, ok := captureMap["params"]; ok {
		paramsNode = node
	}
	if node, ok := captureMap["returns"]; ok {
		returnsNode = node
	}

	start := funcNode.StartPoint()
	end := funcNode.EndPoint()

	// Determine if exported
	isExported := name != "" && rune(name[0]) >= 'A' && rune(name[0]) <= 'Z'

	// Extract receiver info (if method)
	var receiver *ReceiverInfo
	if isMethod && receiverNode != nil {
		receiver = p.extractReceiver(receiverNode, content)
	}

	// Extract parameters
	parameters := p.extractParameters(paramsNode, content)

	// Extract return types
	returnTypes := p.extractReturnTypes(returnsNode, content)

	// TODO: Extract associated comments (e.g., preceding comments)
	comments := p.findPrecedingComments(funcNode, content)

	return &EnhancedFunctionInfo{
		Name:        name,
		Filepath:    filePath,
		Package:     packageName,
		Signature:   signature, // Can be refined later
		Comments:    comments,
		IsExported:  isExported,
		IsMethod:    isMethod,
		Receiver:    receiver,
		Parameters:  parameters,
		ReturnTypes: returnTypes,
		StartLine:   int(start.Row) + 1,
		EndLine:     int(end.Row) + 1,
		StartCol:    int(start.Column),
		EndCol:      int(end.Column),
	}
}

func (p *Parser) extractReceiver(receiverNode *sitter.Node, content []byte) *ReceiverInfo {
	// Receiver parameter list usually has one parameter_declaration
	cursor := sitter.NewQueryCursor()
	cursor.Exec(p.parameterQuery, receiverNode)
	defer cursor.Close()

	if match, ok := cursor.NextMatch(); ok {
		var recName, recType string
		var recTypeNode *sitter.Node
		pointer := false

		for _, capture := range match.Captures {
			captureName := p.parameterQuery.CaptureNameForId(capture.Index)
			if captureName == "param_name" {
				recName = capture.Node.Content(content)
			} else if captureName == "param_type" {
				recTypeNode = capture.Node
			}
		}

		if recTypeNode != nil {
			// Check if the type is a pointer type
			if recTypeNode.Type() == "pointer_type" {
				pointer = true
				// Get the underlying type node (skip the '*')
				if recTypeNode.ChildCount() > 0 {
					recType = recTypeNode.Child(1).Content(content)
				} else {
					recType = recTypeNode.Content(content) // Fallback
				}
			} else {
				recType = recTypeNode.Content(content)
			}
		}

		return &ReceiverInfo{Name: recName, Type: recType, Pointer: pointer}
	}
	return nil
}

func (p *Parser) extractParameters(paramsNode *sitter.Node, content []byte) []ParameterInfo {
	var params []ParameterInfo
	if paramsNode == nil {
		return params
	}

	cursor := sitter.NewQueryCursor()
	cursor.Exec(p.parameterQuery, paramsNode)
	defer cursor.Close()

	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		var paramName, paramType string
		for _, capture := range match.Captures {
			captureName := p.parameterQuery.CaptureNameForId(capture.Index)
			if captureName == "param_name" {
				paramName = capture.Node.Content(content)
			} else if captureName == "param_type" {
				paramType = capture.Node.Content(content) // Keep it simple for now
			}
		}
		if paramType != "" { // Must have a type
			params = append(params, ParameterInfo{Name: paramName, Type: paramType})
		}
	}
	return params
}

func (p *Parser) extractReturnTypes(returnsNode *sitter.Node, content []byte) []string {
	var returnTypes []string
	if returnsNode == nil {
		return returnTypes
	}

	cursor := sitter.NewQueryCursor()
	// Execute query on the specific returns node
	// Use a broader query to find type nodes within the return clause
	// Note: This simplified query might miss complex types (interfaces, funcs, maps etc.)
	returnQueryStr := `[ (type_identifier) (pointer_type) (qualified_type) (struct_type) (array_type) (slice_type) (map_type) ] @return_type`
	tempReturnQuery, _ := sitter.NewQuery([]byte(returnQueryStr), p.language)
	if tempReturnQuery != nil {
		defer tempReturnQuery.Close()
		cursor.Exec(tempReturnQuery, returnsNode)
		defer cursor.Close()

		for {
			match, ok := cursor.NextMatch()
			if !ok {
				break
			}
			for _, capture := range match.Captures {
				// We just capture the whole type node's content for simplicity
				returnTypes = append(returnTypes, capture.Node.Content(content))
			}
		}
	}
	return returnTypes
}

// findPrecedingComments tries to find comments immediately preceding a node.
func (p *Parser) findPrecedingComments(node *sitter.Node, content []byte) []string {
	var comments []string
	prev := node.PrevNamedSibling()
	for prev != nil && prev.Type() == "comment" {
		commentText := strings.TrimSpace(strings.TrimPrefix(prev.Content(content), "//"))
		// Prepend comments as we walk backwards
		comments = append([]string{commentText}, comments...)
		prev = prev.PrevNamedSibling()
	}
	return comments
}

// --- Deprecated / Old Structs and Functions ---
// Kept temporarily for compatibility during transition, remove later

type FunctionInfo struct {
	Name      string `json:"name"`
	Filepath  string `json:"filepath"` // Keep absolute path here for internal use, relative path in analysis result
	StartLine int    `json:"startLine"`
	EndLine   int    `json:"endLine"`
	// Package field removed, available in EnhancedFunctionInfo
}

// ParseFileAndPackage remains for Phase 2 compatibility, but ideally deprecated.
func (p *Parser) ParseFileAndPackage(ctx context.Context, filePath string) ([]FunctionInfo, string, error) {
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, "", errors.Wrapf(err, "failed to read file %s", filePath)
	}

	tree, err := p.parser.ParseCtx(ctx, nil, content)
	if err != nil {
		if errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded) {
			return nil, "", err
		}
		return nil, "", errors.Wrapf(err, "failed to parse file %s", filePath)
	}
	defer tree.Close()

	rootNode := tree.RootNode()
	pkgName := p.extractPackageName(rootNode, content)

	// --- Extract Functions (Simplified) ---
	var functions []FunctionInfo
	cursor := sitter.NewQueryCursor()
	cursor.Exec(p.funcQuery, rootNode)
	defer cursor.Close()

	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		match = cursor.FilterPredicates(match, content)
		if len(match.Captures) == 0 {
			continue
		}

		var funcName string
		var funcNode *sitter.Node
		for _, capture := range match.Captures {
			captureName := p.funcQuery.CaptureNameForId(capture.Index)
			node := capture.Node
			if captureName == "name" { // Changed from func_name to name
				funcName = node.Content(content)
			} else if captureName == "func" { // Changed from function to func
				funcNode = node
			}
		}

		if funcNode != nil && funcName != "" {
			start := funcNode.StartPoint()
			end := funcNode.EndPoint()
			functions = append(functions, FunctionInfo{
				Name:      funcName,
				Filepath:  filePath,
				StartLine: int(start.Row) + 1,
				EndLine:   int(end.Row) + 1,
			})
		}
	}

	return functions, pkgName, nil
}

// Placeholder for EnhancedFileInfo used by ParseFileEnhanced
type EnhancedFileInfo struct {
	Path      string
	Package   string
	Functions []EnhancedFunctionInfo
	// Imports, Size, Lines etc. to be added
}

// Helper function to get content - keeping original example structure
func nodeContent(node *sitter.Node, content []byte) string {
	return string(content[node.StartByte():node.EndByte()])
}
