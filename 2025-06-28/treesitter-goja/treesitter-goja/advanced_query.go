package main

import (
	"fmt"
	"strings"
	"time"
)

// AdvancedQueryEngine provides enhanced query capabilities
type AdvancedQueryEngine struct {
	language    string
	cache       map[string][]QueryMatch
	cacheEnabled bool
	statistics  *QueryStatistics
	astUtils    *ASTUtilities
}

// QueryStatistics tracks query performance metrics
type QueryStatistics struct {
	TotalQueries    int64
	CacheHits       int64
	CacheMisses     int64
	AverageTimeMs   float64
	TotalTimeMs     float64
}

// NewAdvancedQueryEngine creates a new advanced query engine
func NewAdvancedQueryEngine(language string) *AdvancedQueryEngine {
	return &AdvancedQueryEngine{
		language:     language,
		cache:        make(map[string][]QueryMatch),
		cacheEnabled: true,
		statistics:   &QueryStatistics{},
		astUtils:     NewASTUtilities(),
	}
}

// SetCacheEnabled enables or disables query caching
func (aqe *AdvancedQueryEngine) SetCacheEnabled(enabled bool) {
	aqe.cacheEnabled = enabled
}

// ClearCache clears the query cache
func (aqe *AdvancedQueryEngine) ClearCache() {
	aqe.cache = make(map[string][]QueryMatch)
}

// GetStatistics returns query performance statistics
func (aqe *AdvancedQueryEngine) GetStatistics() *QueryStatistics {
	return aqe.statistics
}

// ExecuteAdvancedQuery executes a query with caching and performance tracking
func (aqe *AdvancedQueryEngine) ExecuteAdvancedQuery(queryString string, tree *Tree) ([]QueryMatch, error) {
	startTime := time.Now()
	defer func() {
		duration := time.Since(startTime)
		aqe.updateStatistics(duration)
	}()
	
	// Check cache first
	cacheKey := fmt.Sprintf("%s:%s", queryString, tree.source)
	if aqe.cacheEnabled {
		if cached, exists := aqe.cache[cacheKey]; exists {
			aqe.statistics.CacheHits++
			return cached, nil
		}
		aqe.statistics.CacheMisses++
	}
	
	// Execute query
	basicEngine := NewQueryEngine(aqe.language)
	matches, err := basicEngine.ExecuteQuery(queryString, tree)
	if err != nil {
		return nil, err
	}
	
	// Cache results
	if aqe.cacheEnabled {
		aqe.cache[cacheKey] = matches
	}
	
	return matches, nil
}

// updateStatistics updates query performance statistics
func (aqe *AdvancedQueryEngine) updateStatistics(duration time.Duration) {
	aqe.statistics.TotalQueries++
	durationMs := float64(duration.Nanoseconds()) / 1e6
	aqe.statistics.TotalTimeMs += durationMs
	aqe.statistics.AverageTimeMs = aqe.statistics.TotalTimeMs / float64(aqe.statistics.TotalQueries)
}

// QueryOptimizer optimizes query patterns for better performance
type QueryOptimizer struct{}

// NewQueryOptimizer creates a new query optimizer
func NewQueryOptimizer() *QueryOptimizer {
	return &QueryOptimizer{}
}

// OptimizeQuery optimizes a query string for better performance
func (qo *QueryOptimizer) OptimizeQuery(queryString string) string {
	// Remove unnecessary whitespace
	optimized := strings.TrimSpace(queryString)
	
	// Normalize multiple spaces to single spaces
	optimized = strings.Join(strings.Fields(optimized), " ")
	
	// Add more optimization rules here
	return optimized
}

// QueryAnalyzer analyzes query patterns and provides insights
type QueryAnalyzer struct {
	astUtils *ASTUtilities
}

// NewQueryAnalyzer creates a new query analyzer
func NewQueryAnalyzer() *QueryAnalyzer {
	return &QueryAnalyzer{
		astUtils: NewASTUtilities(),
	}
}

// AnalyzeQuery analyzes a query and returns insights
func (qa *QueryAnalyzer) AnalyzeQuery(queryString string, tree *Tree) *QueryAnalysis {
	analysis := &QueryAnalysis{
		QueryString: queryString,
		Complexity:  qa.calculateComplexity(queryString),
		Patterns:    qa.extractPatterns(queryString),
		Suggestions: []string{},
	}
	
	// Add suggestions based on analysis
	if analysis.Complexity > 5 {
		analysis.Suggestions = append(analysis.Suggestions, "Consider breaking down complex query into simpler parts")
	}
	
	if strings.Contains(queryString, "*") {
		analysis.Suggestions = append(analysis.Suggestions, "Wildcard patterns may impact performance")
	}
	
	return analysis
}

// QueryAnalysis represents the result of query analysis
type QueryAnalysis struct {
	QueryString string
	Complexity  int
	Patterns    []string
	Suggestions []string
}

// calculateComplexity calculates the complexity score of a query
func (qa *QueryAnalyzer) calculateComplexity(queryString string) int {
	complexity := 0
	
	// Count parentheses (nested patterns)
	complexity += strings.Count(queryString, "(")
	
	// Count predicates
	complexity += strings.Count(queryString, "#")
	
	// Count captures
	complexity += strings.Count(queryString, "@")
	
	// Count wildcards
	complexity += strings.Count(queryString, "*") * 2
	
	return complexity
}

// extractPatterns extracts pattern types from a query string
func (qa *QueryAnalyzer) extractPatterns(queryString string) []string {
	var patterns []string
	
	commonPatterns := []string{
		"function_declaration",
		"class_declaration",
		"variable_declarator",
		"method_definition",
		"call_expression",
		"arrow_function",
		"import_statement",
		"export_statement",
	}
	
	for _, pattern := range commonPatterns {
		if strings.Contains(queryString, pattern) {
			patterns = append(patterns, pattern)
		}
	}
	
	return patterns
}

// QueryBuilder2 provides an enhanced query builder with more features
type QueryBuilder2 struct {
	patterns    []string
	predicates  []string
	captures    map[string]string
	optimizer   *QueryOptimizer
	analyzer    *QueryAnalyzer
}

// NewQueryBuilder2 creates a new enhanced query builder
func NewQueryBuilder2() *QueryBuilder2 {
	return &QueryBuilder2{
		patterns:   []string{},
		predicates: []string{},
		captures:   make(map[string]string),
		optimizer:  NewQueryOptimizer(),
		analyzer:   NewQueryAnalyzer(),
	}
}

// AddPattern adds a pattern to the query
func (qb *QueryBuilder2) AddPattern(pattern string) *QueryBuilder2 {
	qb.patterns = append(qb.patterns, pattern)
	return qb
}

// AddPredicate adds a predicate to the query
func (qb *QueryBuilder2) AddPredicate(predicate string) *QueryBuilder2 {
	qb.predicates = append(qb.predicates, predicate)
	return qb
}

// AddCapture adds a capture to the query
func (qb *QueryBuilder2) AddCapture(name, nodeType string) *QueryBuilder2 {
	qb.captures[name] = nodeType
	return qb
}

// FunctionWithName adds a function declaration pattern with name constraint
func (qb *QueryBuilder2) FunctionWithName(name, captureName string) *QueryBuilder2 {
	pattern := fmt.Sprintf(`(function_declaration 
		name: (identifier) @%s
		(#eq? @%s "%s"))`, captureName, captureName, name)
	return qb.AddPattern(pattern)
}

// ClassWithMethod adds a class with specific method pattern
func (qb *QueryBuilder2) ClassWithMethod(className, methodName, classCapture, methodCapture string) *QueryBuilder2 {
	pattern := fmt.Sprintf(`(class_declaration 
		name: (identifier) @%s
		body: (class_body
			(method_definition
				name: (property_identifier) @%s)))`, classCapture, methodCapture)
	qb.AddPredicate(fmt.Sprintf(`(#eq? @%s "%s")`, classCapture, className))
	qb.AddPredicate(fmt.Sprintf(`(#eq? @%s "%s")`, methodCapture, methodName))
	return qb.AddPattern(pattern)
}

// VariableWithValue adds a variable declaration with value constraint
func (qb *QueryBuilder2) VariableWithValue(varName, captureName string) *QueryBuilder2 {
	pattern := fmt.Sprintf(`(variable_declarator 
		name: (identifier) @%s
		(#eq? @%s "%s"))`, captureName, captureName, varName)
	return qb.AddPattern(pattern)
}

// CallToFunction adds a call expression to specific function
func (qb *QueryBuilder2) CallToFunction(funcName, captureName string) *QueryBuilder2 {
	pattern := fmt.Sprintf(`(call_expression 
		function: (identifier) @%s
		(#eq? @%s "%s"))`, captureName, captureName, funcName)
	return qb.AddPattern(pattern)
}

// Build builds the final optimized query
func (qb *QueryBuilder2) Build() string {
	query := strings.Join(qb.patterns, "\n")
	
	// Add predicates
	if len(qb.predicates) > 0 {
		query += "\n" + strings.Join(qb.predicates, "\n")
	}
	
	// Optimize the query
	return qb.optimizer.OptimizeQuery(query)
}

// Analyze analyzes the built query
func (qb *QueryBuilder2) Analyze(tree *Tree) *QueryAnalysis {
	query := qb.Build()
	return qb.analyzer.AnalyzeQuery(query, tree)
}

// QueryTemplate represents a reusable query template
type QueryTemplate struct {
	Name        string
	Description string
	Template    string
	Parameters  []string
}

// QueryTemplateLibrary manages a collection of query templates
type QueryTemplateLibrary struct {
	templates map[string]*QueryTemplate
}

// NewQueryTemplateLibrary creates a new query template library
func NewQueryTemplateLibrary() *QueryTemplateLibrary {
	lib := &QueryTemplateLibrary{
		templates: make(map[string]*QueryTemplate),
	}
	lib.loadDefaultTemplates()
	return lib
}

// loadDefaultTemplates loads default query templates
func (qtl *QueryTemplateLibrary) loadDefaultTemplates() {
	templates := []*QueryTemplate{
		{
			Name:        "function_with_params",
			Description: "Find functions with specific parameter count",
			Template:    `(function_declaration name: (identifier) @func_name parameters: (formal_parameters) @params)`,
			Parameters:  []string{"func_name"},
		},
		{
			Name:        "async_function_calls",
			Description: "Find calls to async functions",
			Template:    `(call_expression function: (identifier) @func_name (#match? @func_name ".*Async$"))`,
			Parameters:  []string{"func_name"},
		},
		{
			Name:        "error_handling",
			Description: "Find try-catch blocks",
			Template:    `(try_statement body: (statement_block) @try_body handler: (catch_clause) @catch_handler)`,
			Parameters:  []string{"try_body", "catch_handler"},
		},
		{
			Name:        "module_exports",
			Description: "Find module.exports assignments",
			Template:    `(assignment_expression left: (member_expression object: (identifier) @module property: (property_identifier) @exports) (#eq? @module "module") (#eq? @exports "exports"))`,
			Parameters:  []string{"module", "exports"},
		},
	}
	
	for _, template := range templates {
		qtl.templates[template.Name] = template
	}
}

// GetTemplate retrieves a template by name
func (qtl *QueryTemplateLibrary) GetTemplate(name string) *QueryTemplate {
	return qtl.templates[name]
}

// ListTemplates returns all available template names
func (qtl *QueryTemplateLibrary) ListTemplates() []string {
	var names []string
	for name := range qtl.templates {
		names = append(names, name)
	}
	return names
}

// AddTemplate adds a new template to the library
func (qtl *QueryTemplateLibrary) AddTemplate(template *QueryTemplate) {
	qtl.templates[template.Name] = template
}

// InstantiateTemplate creates a query from a template with parameters
func (qtl *QueryTemplateLibrary) InstantiateTemplate(name string, params map[string]string) (string, error) {
	template := qtl.GetTemplate(name)
	if template == nil {
		return "", fmt.Errorf("template not found: %s", name)
	}
	
	query := template.Template
	for param, value := range params {
		placeholder := fmt.Sprintf("{%s}", param)
		query = strings.ReplaceAll(query, placeholder, value)
	}
	
	return query, nil
}

