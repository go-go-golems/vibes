package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	_ "embed"
	"strings"
	"strconv"

	"github.com/dop251/goja"
)

//go:embed js/demo.js
var demoJS string

func main() {
	// Check for test mode
	if len(os.Args) > 1 && os.Args[1] == "--test" {
		RunAllTests()
		return
	}
	
	if len(os.Args) < 2 {
		fmt.Println("Usage: treesitter-goja <javascript-file> [--advanced]")
		fmt.Println("       treesitter-goja --test")
		fmt.Println("Example: treesitter-goja ../demo.js")
		fmt.Println("         treesitter-goja ../demo.js --advanced")
		fmt.Println("         treesitter-goja --test")
		fmt.Println()
		fmt.Println("This tool demonstrates a Tree-sitter Goja JavaScript Golang module")
		fmt.Println("that exposes Tree-sitter parsing, AST primitives, and query functionality to JavaScript.")
		fmt.Println()
		fmt.Println("Options:")
		fmt.Println("  --advanced    Use the advanced parser with more JavaScript constructs")
		fmt.Println("  --test        Run validation tests")
		os.Exit(1)
	}
	
	// Check for advanced mode
	useAdvanced := len(os.Args) > 2 && os.Args[2] == "--advanced"
	
	// Read the JavaScript file to parse
	filename := os.Args[1]
	sourceCode, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Fatalf("Error reading file: %v", err)
	}
	
	// Create Goja runtime
	runtime := goja.New()
	
	// Add console.log support
	console := runtime.NewObject()
	console.Set("log", func(args ...interface{}) {
		fmt.Println(args...)
	})
	console.Set("error", func(args ...interface{}) {
		fmt.Print("ERROR: ")
		fmt.Println(args...)
	})
	runtime.Set("console", console)
	
	// Create TreeSitter instance
	ts := NewTreeSitter()
	ts.SetRuntime(runtime)
	ts.InitializeDefaultLanguages()
	
	// Register advanced parser if requested
	if useAdvanced {
		config := &ParserConfig{
			ParseComments:   true,
			ParseJSX:        false,
			ParseTypeScript: false,
			StrictMode:      false,
			ECMAVersion:     2023,
		}
		advancedParser := NewAdvancedJavaScriptParser(config)
		ts.RegisterLanguage("javascript", advancedParser)
		ts.RegisterLanguage("js", advancedParser)
	}
	
	// Register tree-sitter module
	RegisterTreeSitterModule(runtime)
	
	// Prepare the JavaScript code by replacing placeholders
	jsCode := demoJS
	jsCode = strings.ReplaceAll(jsCode, "__USE_ADVANCED__", strconv.FormatBool(useAdvanced))
	jsCode = strings.ReplaceAll(jsCode, "__SOURCE_CODE__", "`"+string(sourceCode)+"`")
	
	// Execute the JavaScript code
	_, err = runtime.RunString(jsCode)
	if err != nil {
		log.Fatalf("Error executing JavaScript: %v", err)
	}
}

