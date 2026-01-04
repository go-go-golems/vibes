package main

import (
	"regexp"
	"strings"
)

// AdvancedJavaScriptParser implements comprehensive JavaScript parsing
type AdvancedJavaScriptParser struct {
	config *ParserConfig
}

// ParserConfig represents parser configuration
type ParserConfig struct {
	ParseComments     bool
	ParseJSX          bool
	ParseTypeScript   bool
	StrictMode        bool
	ECMAVersion       int
}

// NewAdvancedJavaScriptParser creates a new advanced JavaScript parser
func NewAdvancedJavaScriptParser(config *ParserConfig) *AdvancedJavaScriptParser {
	if config == nil {
		config = &ParserConfig{
			ParseComments:   true,
			ParseJSX:        false,
			ParseTypeScript: false,
			StrictMode:      false,
			ECMAVersion:     2023,
		}
	}
	return &AdvancedJavaScriptParser{config: config}
}

// GetLanguageName returns the language name
func (p *AdvancedJavaScriptParser) GetLanguageName() string {
	return "javascript"
}

// Parse parses JavaScript source code with advanced features
func (p *AdvancedJavaScriptParser) Parse(sourceCode string) *Node {
	root := &Node{
		Type_:      "program",
		Text_:      sourceCode,
		StartByte_: 0,
		EndByte_:   len(sourceCode),
		Children_:  []*Node{},
		Properties: make(map[string]string),
	}
	
	// Parse in order of precedence
	if p.config.ParseComments {
		p.parseComments(sourceCode, root)
	}
	
	p.parseImports(sourceCode, root)
	p.parseExports(sourceCode, root)
	p.parseClasses(sourceCode, root)
	p.parseClassMethods(sourceCode, root)
	p.parseFunctions(sourceCode, root)
	p.parseAsyncFunctions(sourceCode, root)
	p.parseArrowFunctions(sourceCode, root)
	p.parseGeneratorFunctions(sourceCode, root)
	p.parseVariables(sourceCode, root)
	p.parseObjectLiterals(sourceCode, root)
	p.parseArrayLiterals(sourceCode, root)
	p.parseCallExpressions(sourceCode, root)
	p.parseMemberExpressions(sourceCode, root)
	p.parseConditionals(sourceCode, root)
	p.parseLoops(sourceCode, root)
	p.parseTryCatch(sourceCode, root)
	p.parseTemplateLiterals(sourceCode, root)
	p.parseDestructuring(sourceCode, root)
	
	return root
}

// parseComments finds and parses comments
func (p *AdvancedJavaScriptParser) parseComments(sourceCode string, root *Node) {
	// Single line comments
	singleLineRegex := regexp.MustCompile(`//.*`)
	matches := singleLineRegex.FindAllStringIndex(sourceCode, -1)
	for _, match := range matches {
		commentNode := &Node{
			Type_:      "comment",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"style": "single_line"},
		}
		root.Children_ = append(root.Children_, commentNode)
	}
	
	// Multi-line comments
	multiLineRegex := regexp.MustCompile(`/\*[\s\S]*?\*/`)
	matches = multiLineRegex.FindAllStringIndex(sourceCode, -1)
	for _, match := range matches {
		commentNode := &Node{
			Type_:      "comment",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"style": "multi_line"},
		}
		root.Children_ = append(root.Children_, commentNode)
	}
}

// parseGeneratorFunctions finds and parses generator functions
func (p *AdvancedJavaScriptParser) parseGeneratorFunctions(sourceCode string, root *Node) {
	genRegex := regexp.MustCompile(`function\s*\*\s*(\w+)\s*\([^)]*\)\s*\{`)
	matches := genRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range matches {
		funcName := sourceCode[match[2]:match[3]]
		funcNode := &Node{
			Type_:      "generator_function_declaration",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": funcName, "generator": "true"},
		}
		
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

// parseObjectLiterals finds and parses object literals
func (p *AdvancedJavaScriptParser) parseObjectLiterals(sourceCode string, root *Node) {
	objRegex := regexp.MustCompile(`(?:const|let|var)\s+(\w+)\s*=\s*\{`)
	matches := objRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range matches {
		objName := sourceCode[match[2]:match[3]]
		objNode := &Node{
			Type_:      "object_expression",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": objName},
		}
		
		nameNode := &Node{
			Type_:      "identifier",
			Text_:      objName,
			StartByte_: match[2],
			EndByte_:   match[3],
			Children_:  []*Node{},
			Properties: map[string]string{"name": objName},
		}
		objNode.Children_ = append(objNode.Children_, nameNode)
		root.Children_ = append(root.Children_, objNode)
	}
}

// parseArrayLiterals finds and parses array literals
func (p *AdvancedJavaScriptParser) parseArrayLiterals(sourceCode string, root *Node) {
	arrRegex := regexp.MustCompile(`(?:const|let|var)\s+(\w+)\s*=\s*\[`)
	matches := arrRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range matches {
		arrName := sourceCode[match[2]:match[3]]
		arrNode := &Node{
			Type_:      "array_expression",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": arrName},
		}
		
		nameNode := &Node{
			Type_:      "identifier",
			Text_:      arrName,
			StartByte_: match[2],
			EndByte_:   match[3],
			Children_:  []*Node{},
			Properties: map[string]string{"name": arrName},
		}
		arrNode.Children_ = append(arrNode.Children_, nameNode)
		root.Children_ = append(root.Children_, arrNode)
	}
}

// parseMemberExpressions finds and parses member expressions
func (p *AdvancedJavaScriptParser) parseMemberExpressions(sourceCode string, root *Node) {
	memberRegex := regexp.MustCompile(`(\w+)\.(\w+)`)
	matches := memberRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range matches {
		object := sourceCode[match[2]:match[3]]
		property := sourceCode[match[4]:match[5]]
		
		memberNode := &Node{
			Type_:      "member_expression",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"object": object, "property": property},
		}
		
		objectNode := &Node{
			Type_:      "identifier",
			Text_:      object,
			StartByte_: match[2],
			EndByte_:   match[3],
			Children_:  []*Node{},
			Properties: map[string]string{"name": object},
		}
		
		propertyNode := &Node{
			Type_:      "property_identifier",
			Text_:      property,
			StartByte_: match[4],
			EndByte_:   match[5],
			Children_:  []*Node{},
			Properties: map[string]string{"name": property},
		}
		
		memberNode.Children_ = append(memberNode.Children_, objectNode, propertyNode)
		root.Children_ = append(root.Children_, memberNode)
	}
}

// parseConditionals finds and parses if statements
func (p *AdvancedJavaScriptParser) parseConditionals(sourceCode string, root *Node) {
	ifRegex := regexp.MustCompile(`if\s*\([^)]+\)\s*\{`)
	matches := ifRegex.FindAllStringIndex(sourceCode, -1)
	
	for _, match := range matches {
		ifNode := &Node{
			Type_:      "if_statement",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: make(map[string]string),
		}
		root.Children_ = append(root.Children_, ifNode)
	}
}

// parseLoops finds and parses loop statements
func (p *AdvancedJavaScriptParser) parseLoops(sourceCode string, root *Node) {
	// For loops
	forRegex := regexp.MustCompile(`for\s*\([^)]+\)\s*\{`)
	matches := forRegex.FindAllStringIndex(sourceCode, -1)
	for _, match := range matches {
		forNode := &Node{
			Type_:      "for_statement",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: make(map[string]string),
		}
		root.Children_ = append(root.Children_, forNode)
	}
	
	// While loops
	whileRegex := regexp.MustCompile(`while\s*\([^)]+\)\s*\{`)
	matches = whileRegex.FindAllStringIndex(sourceCode, -1)
	for _, match := range matches {
		whileNode := &Node{
			Type_:      "while_statement",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: make(map[string]string),
		}
		root.Children_ = append(root.Children_, whileNode)
	}
}

// parseTryCatch finds and parses try-catch statements
func (p *AdvancedJavaScriptParser) parseTryCatch(sourceCode string, root *Node) {
	tryRegex := regexp.MustCompile(`try\s*\{`)
	matches := tryRegex.FindAllStringIndex(sourceCode, -1)
	
	for _, match := range matches {
		tryNode := &Node{
			Type_:      "try_statement",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: make(map[string]string),
		}
		root.Children_ = append(root.Children_, tryNode)
	}
	
	catchRegex := regexp.MustCompile(`catch\s*\([^)]*\)\s*\{`)
	matches = catchRegex.FindAllStringIndex(sourceCode, -1)
	
	for _, match := range matches {
		catchNode := &Node{
			Type_:      "catch_clause",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: make(map[string]string),
		}
		root.Children_ = append(root.Children_, catchNode)
	}
}

// parseTemplateLiterals finds and parses template literals
func (p *AdvancedJavaScriptParser) parseTemplateLiterals(sourceCode string, root *Node) {
	templateRegex := regexp.MustCompile("`[^`]*`")
	matches := templateRegex.FindAllStringIndex(sourceCode, -1)
	
	for _, match := range matches {
		templateNode := &Node{
			Type_:      "template_literal",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: make(map[string]string),
		}
		root.Children_ = append(root.Children_, templateNode)
	}
}

// parseDestructuring finds and parses destructuring assignments
func (p *AdvancedJavaScriptParser) parseDestructuring(sourceCode string, root *Node) {
	// Object destructuring
	objDestructRegex := regexp.MustCompile(`(?:const|let|var)\s*\{[^}]+\}\s*=`)
	matches := objDestructRegex.FindAllStringIndex(sourceCode, -1)
	
	for _, match := range matches {
		destructNode := &Node{
			Type_:      "object_pattern",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: make(map[string]string),
		}
		root.Children_ = append(root.Children_, destructNode)
	}
	
	// Array destructuring
	arrDestructRegex := regexp.MustCompile(`(?:const|let|var)\s*\[[^\]]+\]\s*=`)
	matches = arrDestructRegex.FindAllStringIndex(sourceCode, -1)
	
	for _, match := range matches {
		destructNode := &Node{
			Type_:      "array_pattern",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: make(map[string]string),
		}
		root.Children_ = append(root.Children_, destructNode)
	}
}

// Implement the remaining methods from the original parser
func (p *AdvancedJavaScriptParser) parseImports(sourceCode string, root *Node) {
	importRegex := regexp.MustCompile(`import\s+[^;]+;`)
	matches := importRegex.FindAllStringIndex(sourceCode, -1)
	
	for _, match := range matches {
		importNode := &Node{
			Type_:      "import_statement",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: make(map[string]string),
		}
		root.Children_ = append(root.Children_, importNode)
	}
}

func (p *AdvancedJavaScriptParser) parseExports(sourceCode string, root *Node) {
	exportRegex := regexp.MustCompile(`export\s*\{[^}]*\}`)
	matches := exportRegex.FindAllStringIndex(sourceCode, -1)
	
	for _, match := range matches {
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

func (p *AdvancedJavaScriptParser) parseClasses(sourceCode string, root *Node) {
	classRegex := regexp.MustCompile(`class\s+(\w+)`)
	matches := classRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range matches {
		className := sourceCode[match[2]:match[3]]
		classNode := &Node{
			Type_:      "class_declaration",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": className},
		}
		
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

func (p *AdvancedJavaScriptParser) parseClassMethods(sourceCode string, root *Node) {
	methodRegex := regexp.MustCompile(`\s+(\w+)\s*\([^)]*\)\s*\{`)
	matches := methodRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range matches {
		methodName := sourceCode[match[2]:match[3]]
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

func (p *AdvancedJavaScriptParser) parseFunctions(sourceCode string, root *Node) {
	funcRegex := regexp.MustCompile(`function\s+(\w+)\s*\([^)]*\)\s*\{`)
	matches := funcRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range matches {
		funcName := sourceCode[match[2]:match[3]]
		funcNode := &Node{
			Type_:      "function_declaration",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": funcName},
		}
		
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

func (p *AdvancedJavaScriptParser) parseAsyncFunctions(sourceCode string, root *Node) {
	asyncRegex := regexp.MustCompile(`async\s+function\s+(\w+)\s*\([^)]*\)\s*\{`)
	matches := asyncRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range matches {
		funcName := sourceCode[match[2]:match[3]]
		funcNode := &Node{
			Type_:      "function_declaration",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": funcName, "async": "true"},
		}
		
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

func (p *AdvancedJavaScriptParser) parseArrowFunctions(sourceCode string, root *Node) {
	arrowRegex := regexp.MustCompile(`(?:const|let|var)\s+(\w+)\s*=\s*[^=]*=>\s*`)
	matches := arrowRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range matches {
		funcName := sourceCode[match[2]:match[3]]
		funcNode := &Node{
			Type_:      "arrow_function",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": funcName},
		}
		
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

func (p *AdvancedJavaScriptParser) parseVariables(sourceCode string, root *Node) {
	varRegex := regexp.MustCompile(`(?:const|let|var)\s+(\w+)`)
	matches := varRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range matches {
		varName := sourceCode[match[2]:match[3]]
		varNode := &Node{
			Type_:      "variable_declarator",
			Text_:      sourceCode[match[0]:match[1]],
			StartByte_: match[0],
			EndByte_:   match[1],
			Children_:  []*Node{},
			Properties: map[string]string{"name": varName},
		}
		
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

func (p *AdvancedJavaScriptParser) parseCallExpressions(sourceCode string, root *Node) {
	callRegex := regexp.MustCompile(`(\w+)\s*\(`)
	matches := callRegex.FindAllStringSubmatchIndex(sourceCode, -1)
	
	for _, match := range matches {
		funcName := sourceCode[match[2]:match[3]]
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

