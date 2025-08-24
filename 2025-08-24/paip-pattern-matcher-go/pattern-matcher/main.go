package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	fmt.Println("PAIP Pattern Matcher in Go")
	fmt.Println("==========================")
	fmt.Println()
	
	// Run examples
	runExamples()
	
	// Interactive mode
	fmt.Println("\nInteractive Mode:")
	fmt.Println("Enter patterns and inputs to test matching.")
	fmt.Println("Format: pattern | input")
	fmt.Println("Type 'quit' to exit.")
	fmt.Println()
	
	scanner := bufio.NewScanner(os.Stdin)
	
	for {
		fmt.Print("> ")
		if !scanner.Scan() {
			break
		}
		
		line := strings.TrimSpace(scanner.Text())
		if line == "quit" || line == "exit" {
			break
		}
		
		if line == "" {
			continue
		}
		
		parts := strings.Split(line, "|")
		if len(parts) != 2 {
			fmt.Println("Format: pattern | input")
			continue
		}
		
		patternStr := strings.TrimSpace(parts[0])
		inputStr := strings.TrimSpace(parts[1])
		
		testMatch(patternStr, inputStr)
		fmt.Println()
	}
}

func runExamples() {
	fmt.Println("Running Examples from PAIP Chapter 6:")
	fmt.Println()
	
	examples := []struct {
		pattern string
		input   string
		description string
	}{
		// Basic variable matching
		{"?x", "hello", "Variable matches symbol"},
		{"?x", "42", "Variable matches number"},
		
		// Exact matching
		{"hello", "hello", "Exact match"},
		{"hello", "world", "No match - different symbols"},
		
		// List patterns
		{"(a b c)", "(a b c)", "Exact list match"},
		{"(a ?x c)", "(a b c)", "List with variable"},
		{"(?x ?y ?x)", "(a b a)", "Variable consistency"},
		{"(?x ?y ?x)", "(a b c)", "Variable inconsistency"},
		
		// Predicate patterns
		{"(?is ?x numberp)", "42", "Number predicate - match"},
		{"(?is ?x numberp)", "hello", "Number predicate - no match"},
		{"(?is ?x symbolp)", "hello", "Symbol predicate - match"},
		{"(?is ?x oddp)", "3", "Odd predicate - match"},
		{"(?is ?x oddp)", "4", "Odd predicate - no match"},
		{"(?is ?x evenp)", "4", "Even predicate - match"},
		
		// Logical patterns
		{"(?and (?is ?x numberp) (?is ?x oddp))", "3", "And pattern - odd number"},
		{"(?and (?is ?x numberp) (?is ?x oddp))", "4", "And pattern - even number"},
		{"(?or < = >)", "<", "Or pattern - match first"},
		{"(?or < = >)", "=", "Or pattern - match second"},
		{"(?or < = >)", ">", "Or pattern - match third"},
		{"(?or < = >)", "+", "Or pattern - no match"},
		{"(?not hello)", "world", "Not pattern - match"},
		{"(?not hello)", "hello", "Not pattern - no match"},
		
		// Complex patterns
		{"(?x (?or < = >) ?y)", "(3 < 4)", "Relational expression"},
		{"(?x (?or < = >) ?y)", "(5 = 5)", "Equality expression"},
		{"(?x (?or < = >) ?y)", "(7 > 6)", "Greater than expression"},
		{"(?x (?or < = >) ?y)", "(3 + 4)", "Non-relational expression"},
		
		// Nested patterns
		{"(a (b ?x) d)", "(a (b c) d)", "Nested list with variable"},
		{"(a (?x ?y) d)", "(a (b c) d)", "Nested list with multiple variables"},
		
		// Advanced patterns
		{"(?and (?is ?n numberp) (?is ?n oddp))", "3", "Complex predicate combination"},
		{"(?x (?not ?x))", "(3 4)", "Negation with variables"},
		{"(?x (?not ?x))", "(3 3)", "Negation failure"},
	}
	
	for i, example := range examples {
		fmt.Printf("%d. %s\n", i+1, example.description)
		fmt.Printf("   Pattern: %s\n", example.pattern)
		fmt.Printf("   Input:   %s\n", example.input)
		
		testMatch(example.pattern, example.input)
		fmt.Println()
	}
}

func testMatch(patternStr, inputStr string) {
	pattern, err := Parse(patternStr)
	if err != nil {
		fmt.Printf("   Error parsing pattern: %v\n", err)
		return
	}
	
	input, err := Parse(inputStr)
	if err != nil {
		fmt.Printf("   Error parsing input: %v\n", err)
		return
	}
	
	result := PatMatch(pattern, input, NoBindings)
	
	if IsFail(result) {
		fmt.Printf("   Result:  NO MATCH\n")
	} else {
		fmt.Printf("   Result:  MATCH\n")
		if len(result) > 0 {
			fmt.Printf("   Bindings: %s\n", result.String())
		}
	}
}

// Additional utility functions for demonstration
func demonstrateGoSyntax() {
	fmt.Println("Go-like Syntax Examples:")
	fmt.Println("(These would be implemented in a full system)")
	fmt.Println()
	
	// Example of how Go-like syntax could be mapped to Lisp patterns
	goExamples := []struct {
		goSyntax   string
		lispPattern string
		description string
	}{
		{
			"func(x int) bool",
			"(func (?x (?is ?type numberp)) (?is ?return symbolp))",
			"Function signature pattern",
		},
		{
			"struct { Name string; Age int }",
			"(struct (Name (?is ?type symbolp)) (Age (?is ?type numberp)))",
			"Struct pattern",
		},
		{
			"if x > 0",
			"(if (?x > 0))",
			"Conditional pattern",
		},
	}
	
	for _, example := range goExamples {
		fmt.Printf("Go syntax:    %s\n", example.goSyntax)
		fmt.Printf("Lisp pattern: %s\n", example.lispPattern)
		fmt.Printf("Description:  %s\n", example.description)
		fmt.Println()
	}
}

