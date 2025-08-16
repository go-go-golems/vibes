package evaluator

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/dop251/goja"
	ggjengine "github.com/go-go-golems/go-go-goja/engine"
	uhoh "github.com/go-go-golems/uhoh/pkg"
	"gopkg.in/yaml.v3"
)

// JSUhohEvaluator implements JavaScript evaluation with uhoh UI generation
type JSUhohEvaluator struct {
	runtime *goja.Runtime
	logger  *log.Logger
}

// NewJSUhohEvaluator creates a new JavaScript evaluator with uhoh integration
func NewJSUhohEvaluator() (*JSUhohEvaluator, error) {
	// Set up logging
	logFile, err := os.OpenFile("repl.log", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		return nil, fmt.Errorf("failed to open log file: %v", err)
	}
	
	logger := log.New(logFile, "[EVALUATOR] ", log.LstdFlags|log.Lshortfile)
	logger.Println("=== Starting new JSUhohEvaluator ===")

	// Create a Goja runtime with Node-style require() and native modules enabled
	logger.Println("Creating goja runtime...")
	vm, _ := ggjengine.New()
	logger.Println("Goja runtime created successfully")

	evaluator := &JSUhohEvaluator{
		runtime: vm,
		logger:  logger,
	}

	// Set up console.log
	logger.Println("Setting up console.log...")
	consoleObj := vm.NewObject()
	_ = consoleObj.Set("log", func(call goja.FunctionCall) goja.Value {
		var args []interface{}
		for _, arg := range call.Arguments {
			args = append(args, arg.Export())
		}
		fmt.Println(args...)
		logger.Printf("console.log called with: %v", args)
		return goja.Undefined()
	})
	_ = vm.Set("console", consoleObj)
	logger.Println("console.log set up successfully")

	// Set up uhoh integration functions
	logger.Println("Setting up uhoh integration functions...")
	_ = vm.Set("createUI", evaluator.createUIFunction())
	_ = vm.Set("loadFile", evaluator.loadFileFunction())
	logger.Println("Uhoh integration functions set up successfully")

	logger.Println("JSUhohEvaluator initialization complete")
	return evaluator, nil
}

// ExecuteFile executes a JavaScript file directly
func (e *JSUhohEvaluator) ExecuteFile(ctx context.Context, filePath string) (string, error) {
	e.logger.Printf("ExecuteFile called with: %s", filePath)
	
	// Resolve relative paths
	if !filepath.IsAbs(filePath) {
		wd, _ := os.Getwd()
		filePath = filepath.Join(wd, filePath)
	}
	
	e.logger.Printf("Reading file: %s", filePath)
	
	// Read the file
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		e.logger.Printf("Failed to read file: %v", err)
		return "", fmt.Errorf("failed to read file %s: %v", filePath, err)
	}
	
	e.logger.Printf("File content length: %d bytes", len(content))
	
	// Execute the file content
	e.logger.Println("Executing JavaScript code...")
	result, err := e.runtime.RunString(string(content))
	if err != nil {
		e.logger.Printf("JavaScript execution error: %v", err)
		return "", fmt.Errorf("failed to execute file %s: %v", filePath, err)
	}
	
	e.logger.Println("JavaScript execution completed successfully")
	
	// Convert result to string
	if result != nil && !goja.IsUndefined(result) {
		resultStr := result.String()
		e.logger.Printf("Execution result: %s", resultStr)
		return resultStr, nil
	}
	
	e.logger.Println("Execution result: undefined")
	return "undefined", nil
}

// createUIFunction returns a JavaScript function that creates uhoh UIs
func (e *JSUhohEvaluator) createUIFunction() func(call goja.FunctionCall) goja.Value {
	return func(call goja.FunctionCall) goja.Value {
		e.logger.Println("createUI function called")
		
		if len(call.Arguments) == 0 {
			e.logger.Println("createUI called with no arguments")
			panic(e.runtime.NewTypeError("createUI requires a form definition object"))
		}

		// Convert JavaScript object to Go interface{}
		formDef := call.Arguments[0].Export()
		e.logger.Printf("Form definition received: %+v", formDef)

		// Convert to YAML for uhoh
		yamlBytes, err := yaml.Marshal(formDef)
		if err != nil {
			e.logger.Printf("Failed to marshal to YAML: %v", err)
			panic(e.runtime.NewGoError(fmt.Errorf("Failed to convert form definition to YAML: %v", err)))
		}
		
		e.logger.Printf("YAML generated: %s", string(yamlBytes))

		// Build the BubbleTea model using uhoh
		e.logger.Println("Building BubbleTea model with uhoh...")
		form, vals, err := uhoh.BuildBubbleTeaModelFromYAML(yamlBytes)
		if err != nil {
			e.logger.Printf("Failed to build uhoh form: %v", err)
			panic(e.runtime.NewGoError(fmt.Errorf("Failed to build uhoh form: %v", err)))
		}
		
		e.logger.Printf("Form built successfully, type: %T", form)
		e.logger.Printf("Initial values: %+v", vals)

		// Run the form in a BubbleTea program
		e.logger.Println("Starting BubbleTea program...")
		p := tea.NewProgram(form, tea.WithAltScreen())
		finalModel, err := p.Run()
		if err != nil {
			e.logger.Printf("BubbleTea program error: %v", err)
			panic(e.runtime.NewGoError(fmt.Errorf("Failed to run form: %v", err)))
		}
		
		e.logger.Println("BubbleTea program completed")

		// Extract final values
		finalValues, err := uhoh.ExtractFinalValues(vals)
		if err != nil {
			e.logger.Printf("Failed to extract final values: %v", err)
			finalValues = vals // fallback to initial values
		}
		
		e.logger.Printf("Final values: %+v", finalValues)

		// Return the result
		result := map[string]interface{}{
			"success":     true,
			"message":     "UI completed successfully",
			"form_type":   fmt.Sprintf("%T", finalModel),
			"values":      finalValues,
		}
		
		e.logger.Printf("Returning result: %+v", result)
		return e.runtime.ToValue(result)
	}
}

// loadFileFunction returns a JavaScript function that loads and executes JS files
func (e *JSUhohEvaluator) loadFileFunction() func(call goja.FunctionCall) goja.Value {
	return func(call goja.FunctionCall) goja.Value {
		e.logger.Println("loadFile function called")
		
		if len(call.Arguments) == 0 {
			e.logger.Println("loadFile called with no arguments")
			panic(e.runtime.NewTypeError("loadFile requires a file path"))
		}

		filePath := call.Arguments[0].String()
		e.logger.Printf("loadFile called with path: %s", filePath)
		
		// Resolve relative paths
		if !filepath.IsAbs(filePath) {
			wd, _ := os.Getwd()
			filePath = filepath.Join(wd, filePath)
		}
		
		e.logger.Printf("Resolved file path: %s", filePath)

		// Read the file
		content, err := ioutil.ReadFile(filePath)
		if err != nil {
			e.logger.Printf("Failed to read file: %v", err)
			panic(e.runtime.NewGoError(fmt.Errorf("Failed to read file %s: %v", filePath, err)))
		}
		
		e.logger.Printf("File content length: %d bytes", len(content))

		// Execute the file content
		e.logger.Println("Executing file content...")
		_, err = e.runtime.RunString(string(content))
		if err != nil {
			e.logger.Printf("Failed to execute file: %v", err)
			panic(e.runtime.NewGoError(fmt.Errorf("Failed to execute file %s: %v", filePath, err)))
		}
		
		e.logger.Println("File executed successfully")
		return e.runtime.ToValue(fmt.Sprintf("File %s loaded successfully", filePath))
	}
}

func (e *JSUhohEvaluator) Evaluate(ctx context.Context, code string) (string, error) {
	e.logger.Printf("Evaluate called with code: %s", code)
	
	// Handle special commands
	code = strings.TrimSpace(code)
	if strings.HasPrefix(code, "/load ") {
		filePath := strings.TrimSpace(strings.TrimPrefix(code, "/load "))
		e.logger.Printf("Processing /load command for file: %s", filePath)
		return e.loadFile(filePath)
	}

	// Execute JavaScript code
	e.logger.Println("Executing JavaScript code via RunString...")
	result, err := e.runtime.RunString(code)
	if err != nil {
		e.logger.Printf("JavaScript execution error: %v", err)
		return "", err
	}
	
	e.logger.Println("JavaScript execution completed")

	// Convert result to string
	if result != nil && !goja.IsUndefined(result) {
		// If result is an object, try to format it nicely
		if obj := result.Export(); obj != nil {
			if jsonBytes, err := json.MarshalIndent(obj, "", "  "); err == nil {
				resultStr := string(jsonBytes)
				e.logger.Printf("Formatted result: %s", resultStr)
				return resultStr, nil
			}
		}
		resultStr := result.String()
		e.logger.Printf("Result: %s", resultStr)
		return resultStr, nil
	}

	e.logger.Println("Result: undefined")
	return "undefined", nil
}

func (e *JSUhohEvaluator) loadFile(filePath string) (string, error) {
	e.logger.Printf("loadFile method called with: %s", filePath)
	
	// Resolve relative paths
	if !filepath.IsAbs(filePath) {
		wd, _ := os.Getwd()
		filePath = filepath.Join(wd, filePath)
	}
	
	e.logger.Printf("Resolved path: %s", filePath)

	// Read the file
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		e.logger.Printf("Failed to read file: %v", err)
		return "", fmt.Errorf("failed to read file %s: %v", filePath, err)
	}
	
	e.logger.Printf("File content length: %d bytes", len(content))

	// Execute the file content
	e.logger.Println("Executing file content via RunString...")
	_, err = e.runtime.RunString(string(content))
	if err != nil {
		e.logger.Printf("Failed to execute file: %v", err)
		return "", fmt.Errorf("failed to execute file %s: %v", filePath, err)
	}
	
	e.logger.Println("File executed successfully")
	return fmt.Sprintf("File %s loaded successfully", filePath), nil
}

func (e *JSUhohEvaluator) GetPrompt() string {
	return "js-uhoh> "
}

func (e *JSUhohEvaluator) GetName() string {
	return "JavaScript + Uhoh"
}

func (e *JSUhohEvaluator) SupportsMultiline() bool {
	return true
}

func (e *JSUhohEvaluator) GetFileExtension() string {
	return ".js"
}

