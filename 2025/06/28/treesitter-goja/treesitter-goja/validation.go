package main

import (
	"fmt"
	"strings"
)

// MockTesting provides a simple testing interface
type MockTesting struct {
	failed bool
	name   string
}

func (t *MockTesting) Failed() bool {
	return t.failed
}

func (t *MockTesting) Error(args ...interface{}) {
	t.failed = true
	fmt.Print("ERROR: ")
	fmt.Println(args...)
}

func (t *MockTesting) Errorf(format string, args ...interface{}) {
	t.failed = true
	fmt.Printf("ERROR: "+format+"\n", args...)
}

func (t *MockTesting) Fatal(args ...interface{}) {
	t.failed = true
	fmt.Print("FATAL: ")
	fmt.Println(args...)
}

func (t *MockTesting) Fatalf(format string, args ...interface{}) {
	t.failed = true
	fmt.Printf("FATAL: "+format+"\n", args...)
}

// TestBasicParsing tests basic JavaScript parsing functionality
func TestBasicParsing(t *MockTesting) {
	parser := NewJavaScriptParser()
	sourceCode := `
		function hello(name) {
			return "Hello, " + name;
		}
		
		var x = 42;
		var obj = { key: "value" };
	`
	
	root := parser.Parse(sourceCode)
	
	if root == nil {
		t.Fatal("Parser returned nil root node")
		return
	}
	
	if root.Type_ != "program" {
		t.Errorf("Expected root type 'program', got '%s'", root.Type_)
	}
	
	if len(root.Children_) == 0 {
		t.Error("Expected root to have children")
	}
	
	fmt.Printf("✓ Basic parsing test passed - found %d child nodes\n", len(root.Children_))
}

// TestAdvancedParsing tests advanced parsing features
func TestAdvancedParsing(t *MockTesting) {
	config := &ParserConfig{
		ParseComments:   true,
		ParseJSX:        false,
		ParseTypeScript: false,
		StrictMode:      false,
		ECMAVersion:     2023,
	}
	
	parser := NewAdvancedJavaScriptParser(config)
	sourceCode := `
		// This is a comment
		function generator() {
			return 1;
		}
		
		/* Multi-line comment */
		function MyClass() {
			this.value = 42;
		}
	`
	
	root := parser.Parse(sourceCode)
	
	if root == nil {
		t.Fatal("Advanced parser returned nil root node")
		return
	}
	
	// Check for comments
	hasComments := false
	for _, child := range root.Children_ {
		if child.Type_ == "comment" {
			hasComments = true
			break
		}
	}
	
	if !hasComments {
		t.Error("Expected to find comments in parsed tree")
	}
	
	fmt.Printf("✓ Advanced parsing test passed - found %d child nodes\n", len(root.Children_))
}

// TestQueryEngine tests the query functionality
func TestQueryEngine(t *MockTesting) {
	parser := NewJavaScriptParser()
	sourceCode := `
		function testFunc() {
			return 42;
		}
		
		function TestClass() {
			this.method = function() {};
		}
		
		var testVar = "hello";
	`
	
	root := parser.Parse(sourceCode)
	tree := &Tree{rootNode: root, source: sourceCode}
	
	engine := NewQueryEngine("javascript")
	
	// Test function query
	funcQuery := `(function_declaration name: (identifier) @function_name)`
	matches, err := engine.ExecuteQuery(funcQuery, tree)
	
	if err != nil {
		t.Fatalf("Query execution failed: %v", err)
		return
	}
	
	if len(matches) == 0 {
		t.Error("Expected to find function declarations")
	}
	
	fmt.Printf("✓ Query engine test passed - found %d function matches\n", len(matches))
}

// TestASTUtilities tests AST utility functions
func TestASTUtilities(t *MockTesting) {
	parser := NewJavaScriptParser()
	sourceCode := `
		function func1() {}
		function func2() {}
		var x = 1;
		var y = 2;
	`
	
	root := parser.Parse(sourceCode)
	utils := NewASTUtilities()
	
	// Test finding nodes by type
	functions := utils.FindNodesByType(root, "function_declaration")
	if len(functions) != 2 {
		t.Errorf("Expected 2 functions, found %d", len(functions))
	}
	
	variables := utils.FindNodesByType(root, "variable_declarator")
	if len(variables) != 2 {
		t.Errorf("Expected 2 variables, found %d", len(variables))
	}
	
	// Test tree statistics
	stats := utils.GetTreeStatistics(root)
	if stats.TotalNodes == 0 {
		t.Error("Expected non-zero total nodes")
	}
	
	fmt.Printf("✓ AST utilities test passed - %d total nodes, %d functions, %d variables\n", 
		stats.TotalNodes, len(functions), len(variables))
}

// TestAdvancedQuery tests advanced query functionality
func TestAdvancedQuery(t *MockTesting) {
	parser := NewJavaScriptParser()
	sourceCode := `
		function testFunc() {
			return 42;
		}
	`
	
	root := parser.Parse(sourceCode)
	tree := &Tree{rootNode: root, source: sourceCode}
	
	engine := NewAdvancedQueryEngine("javascript")
	
	// Test with caching
	query := `(function_declaration name: (identifier) @function_name)`
	
	// First execution (cache miss)
	matches1, err := engine.ExecuteAdvancedQuery(query, tree)
	if err != nil {
		t.Fatalf("Advanced query execution failed: %v", err)
		return
	}
	
	// Second execution (cache hit)
	matches2, err := engine.ExecuteAdvancedQuery(query, tree)
	if err != nil {
		t.Fatalf("Advanced query execution failed: %v", err)
		return
	}
	
	if len(matches1) != len(matches2) {
		t.Error("Cache should return same results")
	}
	
	stats := engine.GetStatistics()
	if stats.TotalQueries != 2 {
		t.Errorf("Expected 2 total queries, got %d", stats.TotalQueries)
	}
	
	if stats.CacheHits != 1 {
		t.Errorf("Expected 1 cache hit, got %d", stats.CacheHits)
	}
	
	fmt.Printf("✓ Advanced query test passed - %d queries, %d cache hits\n", 
		stats.TotalQueries, stats.CacheHits)
}

// TestQueryBuilder tests the query builder functionality
func TestQueryBuilder(t *MockTesting) {
	builder := NewQueryBuilder()
	
	query := builder.
		FunctionDeclaration("func_name").
		ClassDeclaration("class_name").
		Build()
	
	if query == "" {
		t.Error("Query builder should produce non-empty query")
	}
	
	expectedPatterns := []string{"function_declaration", "class_declaration"}
	for _, pattern := range expectedPatterns {
		if !strings.Contains(query, pattern) {
			t.Errorf("Query should contain pattern: %s", pattern)
		}
	}
	
	fmt.Printf("✓ Query builder test passed - generated query length: %d\n", len(query))
}

// TestQueryTemplateLibrary tests the template library
func TestQueryTemplateLibrary(t *MockTesting) {
	lib := NewQueryTemplateLibrary()
	
	templates := lib.ListTemplates()
	if len(templates) == 0 {
		t.Error("Template library should have default templates")
	}
	
	// Test getting a template
	template := lib.GetTemplate("function_with_params")
	if template == nil {
		t.Error("Should find function_with_params template")
	}
	
	fmt.Printf("✓ Template library test passed - found %d templates\n", len(templates))
}

// RunAllTests runs all validation tests
func RunAllTests() {
	fmt.Println("=== Running Tree-sitter Goja Module Validation Tests ===")
	
	tests := []struct {
		name string
		fn   func(*MockTesting)
	}{
		{"Basic Parsing", TestBasicParsing},
		{"Advanced Parsing", TestAdvancedParsing},
		{"Query Engine", TestQueryEngine},
		{"AST Utilities", TestASTUtilities},
		{"Advanced Query", TestAdvancedQuery},
		{"Query Builder", TestQueryBuilder},
		{"Template Library", TestQueryTemplateLibrary},
	}
	
	passed := 0
	for _, test := range tests {
		fmt.Printf("\nRunning test: %s\n", test.name)
		t := &MockTesting{name: test.name}
		test.fn(t)
		if !t.Failed() {
			passed++
		}
	}
	
	fmt.Printf("\n=== Test Results: %d/%d tests passed ===\n", passed, len(tests))
	
	if passed == len(tests) {
		fmt.Println("🎉 All tests passed! The Tree-sitter Goja module is working correctly.")
	} else {
		fmt.Printf("⚠️  %d tests failed. Please check the implementation.\n", len(tests)-passed)
	}
}

