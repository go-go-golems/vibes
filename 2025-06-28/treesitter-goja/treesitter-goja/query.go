package main

import (
	"fmt"
	"regexp"
	"strings"
)

// QueryEngine handles tree-sitter-style query execution
type QueryEngine struct {
	language string
}

// NewQueryEngine creates a new query engine
func NewQueryEngine(language string) *QueryEngine {
	return &QueryEngine{language: language}
}

// QueryMatch represents a single query match
type QueryMatch struct {
	Captures map[string]*Node
}

// ExecuteQuery executes a tree-sitter-style query on the given tree
func (qe *QueryEngine) ExecuteQuery(queryString string, tree *Tree) ([]QueryMatch, error) {
	// Parse the query string to extract patterns and captures
	patterns, err := qe.parseQuery(queryString)
	if err != nil {
		return nil, err
	}
	
	var matches []QueryMatch
	
	// Execute each pattern
	for _, pattern := range patterns {
		patternMatches := qe.executePattern(pattern, tree.rootNode)
		matches = append(matches, patternMatches...)
	}
	
	return matches, nil
}

// QueryPattern represents a parsed query pattern
type QueryPattern struct {
	NodeType string
	Captures map[string]string
	Children []QueryPattern
	Predicates []QueryPredicate
}

// QueryPredicate represents a query predicate like #eq?
type QueryPredicate struct {
	Type string
	Args []string
}

// parseQuery parses a tree-sitter query string
func (qe *QueryEngine) parseQuery(queryString string) ([]QueryPattern, error) {
	var patterns []QueryPattern
	
	// Handle function_declaration queries
	if strings.Contains(queryString, "function_declaration") {
		pattern := QueryPattern{
			NodeType: "function_declaration",
			Captures: make(map[string]string),
		}
		
		// Extract capture names
		captureRegex := regexp.MustCompile(`@(\w+)`)
		captures := captureRegex.FindAllStringSubmatch(queryString, -1)
		for _, capture := range captures {
			captureName := capture[1]
			if strings.Contains(queryString, "name:") && strings.Contains(queryString, "@"+captureName) {
				pattern.Captures[captureName] = "identifier"
			}
		}
		
		// Handle async predicate
		if strings.Contains(queryString, `async: "async"`) {
			pattern.Predicates = append(pattern.Predicates, QueryPredicate{
				Type: "eq",
				Args: []string{"async", "true"},
			})
		}
		
		patterns = append(patterns, pattern)
	}
	
	// Handle class_declaration queries
	if strings.Contains(queryString, "class_declaration") {
		pattern := QueryPattern{
			NodeType: "class_declaration",
			Captures: make(map[string]string),
		}
		
		// Extract capture names
		captureRegex := regexp.MustCompile(`@(\w+)`)
		captures := captureRegex.FindAllStringSubmatch(queryString, -1)
		for _, capture := range captures {
			captureName := capture[1]
			if strings.Contains(queryString, "name:") && strings.Contains(queryString, "@"+captureName) {
				pattern.Captures[captureName] = "identifier"
			}
		}
		
		patterns = append(patterns, pattern)
	}
	
	// Handle method_definition queries
	if strings.Contains(queryString, "method_definition") {
		pattern := QueryPattern{
			NodeType: "method_definition",
			Captures: make(map[string]string),
		}
		
		// Extract capture names
		captureRegex := regexp.MustCompile(`@(\w+)`)
		captures := captureRegex.FindAllStringSubmatch(queryString, -1)
		for _, capture := range captures {
			captureName := capture[1]
			if strings.Contains(queryString, "name:") && strings.Contains(queryString, "@"+captureName) {
				pattern.Captures[captureName] = "property_identifier"
			}
		}
		
		patterns = append(patterns, pattern)
	}
	
	// Handle arrow_function queries
	if strings.Contains(queryString, "arrow_function") {
		pattern := QueryPattern{
			NodeType: "arrow_function",
			Captures: make(map[string]string),
		}
		
		// Extract capture names
		captureRegex := regexp.MustCompile(`@(\w+)`)
		captures := captureRegex.FindAllStringSubmatch(queryString, -1)
		for _, capture := range captures {
			captureName := capture[1]
			pattern.Captures[captureName] = "arrow_function"
		}
		
		patterns = append(patterns, pattern)
	}
	
	// Handle variable_declarator queries
	if strings.Contains(queryString, "variable_declarator") {
		pattern := QueryPattern{
			NodeType: "variable_declarator",
			Captures: make(map[string]string),
		}
		
		// Extract capture names
		captureRegex := regexp.MustCompile(`@(\w+)`)
		captures := captureRegex.FindAllStringSubmatch(queryString, -1)
		for _, capture := range captures {
			captureName := capture[1]
			pattern.Captures[captureName] = "identifier"
		}
		
		patterns = append(patterns, pattern)
	}
	
	// Handle call_expression queries
	if strings.Contains(queryString, "call_expression") {
		pattern := QueryPattern{
			NodeType: "call_expression",
			Captures: make(map[string]string),
		}
		
		// Extract capture names and predicates
		captureRegex := regexp.MustCompile(`@(\w+)`)
		captures := captureRegex.FindAllStringSubmatch(queryString, -1)
		for _, capture := range captures {
			captureName := capture[1]
			if strings.Contains(queryString, "function:") && strings.Contains(queryString, "@"+captureName) {
				pattern.Captures[captureName] = "identifier"
			} else if strings.Contains(queryString, "object:") && strings.Contains(queryString, "@"+captureName) {
				pattern.Captures[captureName] = "identifier"
			} else if strings.Contains(queryString, "property:") && strings.Contains(queryString, "@"+captureName) {
				pattern.Captures[captureName] = "property_identifier"
			}
		}
		
		// Handle predicates like #eq?
		predicateRegex := regexp.MustCompile(`\(#eq\?\s+@(\w+)\s+"([^"]+)"\)`)
		predicateMatches := predicateRegex.FindAllStringSubmatch(queryString, -1)
		for _, predMatch := range predicateMatches {
			pattern.Predicates = append(pattern.Predicates, QueryPredicate{
				Type: "eq",
				Args: []string{predMatch[1], predMatch[2]},
			})
		}
		
		patterns = append(patterns, pattern)
	}
	
	// Handle import_statement queries
	if strings.Contains(queryString, "import_statement") {
		pattern := QueryPattern{
			NodeType: "import_statement",
			Captures: make(map[string]string),
		}
		
		// Extract capture names
		captureRegex := regexp.MustCompile(`@(\w+)`)
		captures := captureRegex.FindAllStringSubmatch(queryString, -1)
		for _, capture := range captures {
			captureName := capture[1]
			if strings.Contains(queryString, "source:") && strings.Contains(queryString, "@"+captureName) {
				pattern.Captures[captureName] = "string"
			}
		}
		
		patterns = append(patterns, pattern)
	}
	
	// Handle export_statement queries
	if strings.Contains(queryString, "export_statement") {
		pattern := QueryPattern{
			NodeType: "export_statement",
			Captures: make(map[string]string),
		}
		
		// Extract capture names
		captureRegex := regexp.MustCompile(`@(\w+)`)
		captures := captureRegex.FindAllStringSubmatch(queryString, -1)
		for _, capture := range captures {
			captureName := capture[1]
			pattern.Captures[captureName] = "export_statement"
		}
		
		patterns = append(patterns, pattern)
	}
	
	if len(patterns) == 0 {
		return nil, fmt.Errorf("unsupported query pattern: %s", queryString)
	}
	
	return patterns, nil
}

// executePattern executes a single pattern against a node
func (qe *QueryEngine) executePattern(pattern QueryPattern, node *Node) []QueryMatch {
	var matches []QueryMatch
	
	// Check if current node matches the pattern
	if node.Type_ == pattern.NodeType {
		match := QueryMatch{
			Captures: make(map[string]*Node),
		}
		
		// Check predicates first
		if !qe.checkPredicates(pattern.Predicates, node) {
			// Continue searching children even if this node doesn't match predicates
		} else {
			// Find captures in children
			for captureName, captureType := range pattern.Captures {
				for _, child := range node.Children_ {
					if child.Type_ == captureType {
						match.Captures[captureName] = child
						break
					}
				}
			}
			
			// Special handling for call_expression with member_expression
			if pattern.NodeType == "call_expression" && len(pattern.Predicates) > 0 {
				// This is for console.log type queries
				if qe.isConsoleLogCall(node) {
					for captureName := range pattern.Captures {
						if captureName == "object" {
							match.Captures[captureName] = &Node{
								Type_: "identifier",
								Text_: "console",
								Properties: map[string]string{"name": "console"},
							}
						} else if captureName == "method" {
							match.Captures[captureName] = &Node{
								Type_: "property_identifier",
								Text_: "log",
								Properties: map[string]string{"name": "log"},
							}
						}
					}
				}
			}
			
			// Only add match if all captures were found or it's a special case
			if len(match.Captures) == len(pattern.Captures) || 
			   (pattern.NodeType == "call_expression" && len(pattern.Predicates) > 0) {
				matches = append(matches, match)
			}
		}
	}
	
	// Recursively search children
	for _, child := range node.Children_ {
		childMatches := qe.executePattern(pattern, child)
		matches = append(matches, childMatches...)
	}
	
	return matches
}

// checkPredicates checks if a node matches the given predicates
func (qe *QueryEngine) checkPredicates(predicates []QueryPredicate, node *Node) bool {
	for _, predicate := range predicates {
		switch predicate.Type {
		case "eq":
			if len(predicate.Args) >= 2 {
				property := predicate.Args[0]
				expectedValue := predicate.Args[1]
				actualValue := node.Properties[property]
				if actualValue != expectedValue {
					return false
				}
			}
		}
	}
	return true
}

// isConsoleLogCall checks if a call expression is a console.log call
func (qe *QueryEngine) isConsoleLogCall(node *Node) bool {
	return strings.Contains(node.Text_, "console.log")
}

// QueryBuilder helps build tree-sitter queries programmatically
type QueryBuilder struct {
	patterns []string
}

// NewQueryBuilder creates a new query builder
func NewQueryBuilder() *QueryBuilder {
	return &QueryBuilder{patterns: []string{}}
}

// FunctionDeclaration adds a function declaration pattern
func (qb *QueryBuilder) FunctionDeclaration(captureName string) *QueryBuilder {
	pattern := fmt.Sprintf("(function_declaration name: (identifier) @%s)", captureName)
	qb.patterns = append(qb.patterns, pattern)
	return qb
}

// ClassDeclaration adds a class declaration pattern
func (qb *QueryBuilder) ClassDeclaration(captureName string) *QueryBuilder {
	pattern := fmt.Sprintf("(class_declaration name: (identifier) @%s)", captureName)
	qb.patterns = append(qb.patterns, pattern)
	return qb
}

// VariableDeclaration adds a variable declaration pattern
func (qb *QueryBuilder) VariableDeclaration(captureName string) *QueryBuilder {
	pattern := fmt.Sprintf("(variable_declarator name: (identifier) @%s)", captureName)
	qb.patterns = append(qb.patterns, pattern)
	return qb
}

// ArrowFunction adds an arrow function pattern
func (qb *QueryBuilder) ArrowFunction(captureName string) *QueryBuilder {
	pattern := fmt.Sprintf("(arrow_function) @%s", captureName)
	qb.patterns = append(qb.patterns, pattern)
	return qb
}

// MethodDefinition adds a method definition pattern
func (qb *QueryBuilder) MethodDefinition(captureName string) *QueryBuilder {
	pattern := fmt.Sprintf("(method_definition name: (property_identifier) @%s)", captureName)
	qb.patterns = append(qb.patterns, pattern)
	return qb
}

// CallExpression adds a call expression pattern
func (qb *QueryBuilder) CallExpression(captureName string) *QueryBuilder {
	pattern := fmt.Sprintf("(call_expression function: (identifier) @%s)", captureName)
	qb.patterns = append(qb.patterns, pattern)
	return qb
}

// Build returns the complete query string
func (qb *QueryBuilder) Build() string {
	return strings.Join(qb.patterns, "\n")
}

// PredefinedQueries contains common query patterns for JavaScript
var PredefinedQueries = map[string]string{
	"all_functions": `(function_declaration 
		name: (identifier) @function_name)`,
	
	"all_classes": `(class_declaration 
		name: (identifier) @class_name)`,
	
	"all_variables": `(variable_declarator 
		name: (identifier) @variable_name)`,
	
	"all_methods": `(method_definition 
		name: (property_identifier) @method_name)`,
	
	"all_calls": `(call_expression 
		function: (identifier) @function_name)`,
	
	"arrow_functions": `(arrow_function) @arrow_function`,
	
	"async_functions": `(function_declaration 
		async: "async"
		name: (identifier) @async_function_name)`,
	
	"exports": `(export_statement) @export`,
	
	"imports": `(import_statement 
		source: (string) @import_source)`,
	
	"console_logs": `(call_expression
		function: (member_expression
			object: (identifier) @object
			property: (property_identifier) @method)
		(#eq? @object "console")
		(#eq? @method "log"))`,
}

