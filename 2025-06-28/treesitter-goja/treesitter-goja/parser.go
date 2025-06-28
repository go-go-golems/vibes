package main

import (
	"regexp"
	"strings"
)

// LanguageParser interface defines the contract for language parsers
type LanguageParser interface {
	Parse(sourceCode string) *Node
	GetLanguageName() string
}

// JavaScriptParser implements comprehensive JavaScript parsing using regex
type JavaScriptParser struct{}

// NewJavaScriptParser creates a new JavaScript parser
func NewJavaScriptParser() *JavaScriptParser {
	return &JavaScriptParser{}
}

// GetLanguageName returns the language name
func (p *JavaScriptParser) GetLanguageName() string {
	return "javascript"
}

// Parse parses JavaScript source code and returns an AST
func (p *JavaScriptParser) Parse(sourceCode string) *Node {
	root := &Node{
		Type_:      "program",
		Text_:      sourceCode,
		StartByte_: 0,
		EndByte_:   len(sourceCode),
		Children_:  []*Node{},
		Properties: make(map[string]string),
	}
	
	// Parse different JavaScript constructs in order
	p.parseFunctions(sourceCode, root)
	p.parseAsyncFunctions(sourceCode, root)
	p.parseClasses(sourceCode, root)
	p.parseClassMethods(sourceCode, root)
	p.parseVariables(sourceCode, root)
	p.parseArrowFunctions(sourceCode, root)
	p.parseExports(sourceCode, root)
	p.parseImports(sourceCode, root)
	p.parseCallExpressions(sourceCode, root)
	
	return root
}

// parseFunctions finds and parses function declarations
func (p *JavaScriptParser) parseFunctions(sourceCode string, root *Node) {
	funcRegex := regexp.MustCompile(`function\s+(\w+)\s*\([^)]*\)\s*\{`)
	funcMatches := funcRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range funcMatches {
		funcName := sourceCode[match[2]:match[3]]
		funcNode := &Node{
			Type_:      "function_declaration",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": funcName},
		}
		
		// Add identifier node for function name
		nameNode := &Node{
			Type_:      "identifier",
			Text_:      funcName,
			StartByte_: match[2],
			EndByte_:   match[3],
			Children_:  []*Node{},
			Properties: map[string]string{"name": funcName},
		}
		funcNode.Children_ = append(funcNode.Children_, nameNode)
		root.Children_ = append(root.Children_, funcNode)
	}
}

// parseAsyncFunctions finds and parses async function declarations
func (p *JavaScriptParser) parseAsyncFunctions(sourceCode string, root *Node) {
	asyncRegex := regexp.MustCompile(`async\s+function\s+(\w+)\s*\([^)]*\)\s*\{`)
	asyncMatches := asyncRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range asyncMatches {
		funcName := sourceCode[match[2]:match[3]]
		funcNode := &Node{
			Type_:      "function_declaration",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": funcName, "async": "true"},
		}
		
		// Add identifier node for function name
		nameNode := &Node{
			Type_:      "identifier",
			Text_:      funcName,
			StartByte_: match[2],
			EndByte_:   match[3],
			Children_:  []*Node{},
			Properties: map[string]string{"name": funcName},
		}
		funcNode.Children_ = append(funcNode.Children_, nameNode)
		root.Children_ = append(root.Children_, funcNode)
	}
}

// parseClasses finds and parses class declarations
func (p *JavaScriptParser) parseClasses(sourceCode string, root *Node) {
	classRegex := regexp.MustCompile(`class\s+(\w+)`)
	classMatches := classRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range classMatches {
		className := sourceCode[match[2]:match[3]]
		classNode := &Node{
			Type_:      "class_declaration",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": className},
		}
		
		// Add identifier node for class name
		nameNode := &Node{
			Type_:      "identifier",
			Text_:      className,
			StartByte_: match[2],
			EndByte_:   match[3],
			Children_:  []*Node{},
			Properties: map[string]string{"name": className},
		}
		classNode.Children_ = append(classNode.Children_, nameNode)
		root.Children_ = append(root.Children_, classNode)
	}
}

// parseClassMethods finds and parses class methods
func (p *JavaScriptParser) parseClassMethods(sourceCode string, root *Node) {
	methodRegex := regexp.MustCompile(`\s+(\w+)\s*\([^)]*\)\s*\{`)
	methodMatches := methodRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range methodMatches {
		methodName := sourceCode[match[2]:match[3]]
		// Skip if it looks like a function declaration or other construct
		if strings.Contains(sourceCode[match[0]:match[1]], "function") ||
		   strings.Contains(sourceCode[match[0]:match[1]], "if") ||
		   strings.Contains(sourceCode[match[0]:match[1]], "for") ||
		   strings.Contains(sourceCode[match[0]:match[1]], "while") {
			continue
		}
		
		methodNode := &Node{
			Type_:      "method_definition",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": methodName},
		}
		
		// Add property_identifier node for method name
		nameNode := &Node{
			Type_:      "property_identifier",
			Text_:      methodName,
			StartByte_: match[2],
			EndByte_:   match[3],
			Children_:  []*Node{},
			Properties: map[string]string{"name": methodName},
		}
		methodNode.Children_ = append(methodNode.Children_, nameNode)
		root.Children_ = append(root.Children_, methodNode)
	}
}

// parseVariables finds and parses variable declarations
func (p *JavaScriptParser) parseVariables(sourceCode string, root *Node) {
	varRegex := regexp.MustCompile(`(?:const|let|var)\s+(\w+)`)
	varMatches := varRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range varMatches {
		varName := sourceCode[match[2]:match[3]]
		varNode := &Node{
			Type_:      "variable_declarator",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": varName},
		}
		
		// Add identifier node for variable name
		nameNode := &Node{
			Type_:      "identifier",
			Text_:      varName,
			StartByte_: match[2],
			EndByte_:   match[3],
			Children_:  []*Node{},
			Properties: map[string]string{"name": varName},
		}
		varNode.Children_ = append(varNode.Children_, nameNode)
		root.Children_ = append(root.Children_, varNode)
	}
}

// parseArrowFunctions finds and parses arrow functions
func (p *JavaScriptParser) parseArrowFunctions(sourceCode string, root *Node) {
	arrowRegex := regexp.MustCompile(`(?:const|let|var)\s+(\w+)\s*=\s*[^=]*=>\s*`)
	arrowMatches := arrowRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range arrowMatches {
		funcName := sourceCode[match[2]:match[3]]
		funcNode := &Node{
			Type_:      "arrow_function",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": funcName},
		}
		
		// Add identifier node for function name
		nameNode := &Node{
			Type_:      "identifier",
			Text_:      funcName,
			StartByte_: match[2],
			EndByte_:   match[3],
			Children_:  []*Node{},
			Properties: map[string]string{"name": funcName},
		}
		funcNode.Children_ = append(funcNode.Children_, nameNode)
		root.Children_ = append(root.Children_, funcNode)
	}
}

// parseExports finds and parses export statements
func (p *JavaScriptParser) parseExports(sourceCode string, root *Node) {
	exportRegex := regexp.MustCompile(`export\s*\{[^}]*\}`)
	exportMatches := exportRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range exportMatches {
		exportNode := &Node{
			Type_:      "export_statement",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: make(map[string]string),
		}
		root.Children_ = append(root.Children_, exportNode)
	}
}

// parseImports finds and parses import statements
func (p *JavaScriptParser) parseImports(sourceCode string, root *Node) {
	importRegex := regexp.MustCompile(`import\s+[^;]+;`)
	importMatches := importRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range importMatches {
		importNode := &Node{
			Type_:      "import_statement",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: make(map[string]string),
		}
		
		// Extract source string
		sourceRegex := regexp.MustCompile(`["']([^"']+)["']`)
		sourceMatch := sourceRegex.FindStringSubmatch(sourceCode[match[0]:match[1]])
		if len(sourceMatch) > 1 {
			sourceNode := &Node{
				Type_:      "string",
				Text_:      sourceMatch[0],
				StartByte_: match[0] + strings.Index(sourceCode[match[0]:match[1]], sourceMatch[0]),
				EndByte_:   match[0] + strings.Index(sourceCode[match[0]:match[1]], sourceMatch[0]) + len(sourceMatch[0]),
				Children_:  []*Node{},
				Properties: map[string]string{"value": sourceMatch[1]},
			}
			importNode.Children_ = append(importNode.Children_, sourceNode)
		}
		
		root.Children_ = append(root.Children_, importNode)
	}
}

// parseCallExpressions finds and parses function calls
func (p *JavaScriptParser) parseCallExpressions(sourceCode string, root *Node) {
	callRegex := regexp.MustCompile(`(\w+)\s*\(`)
	callMatches := callRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range callMatches {
		funcName := sourceCode[match[2]:match[3]]
		// Skip if it's a function declaration
		if match[0] > 0 && sourceCode[match[0]-8:match[0]] == "function" {
			continue
		}
		
		callNode := &Node{
			Type_:      "call_expression",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"function": funcName},
		}
		
		// Add identifier node for function name
		nameNode := &Node{
			Type_:      "identifier",
			Text_:      funcName,
			StartByte_: match[2],
			EndByte_:   match[3],
			Children_:  []*Node{},
			Properties: map[string]string{"name": funcName},
		}
		callNode.Children_ = append(callNode.Children_, nameNode)
		root.Children_ = append(root.Children_, callNode)
	}
}

// ParserFactory creates parsers for different languages
type ParserFactory struct{}

// NewParserFactory creates a new parser factory
func NewParserFactory() *ParserFactory {
	return &ParserFactory{}
}

// CreateParser creates a parser for the specified language
func (f *ParserFactory) CreateParser(language string) LanguageParser {
	switch strings.ToLower(language) {
	case "javascript", "js":
		return NewJavaScriptParser()
	default:
		// Default to JavaScript for now
		return NewJavaScriptParser()
	}
}

