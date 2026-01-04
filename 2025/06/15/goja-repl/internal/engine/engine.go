package engine

import (
	"fmt"
	"reflect"
	"strings"

	"github.com/dop251/goja"
)

// JSEngine represents the JavaScript engine for the REPL
type JSEngine struct {
	vm      *goja.Runtime
	history []string
}

// New creates a new JavaScript engine instance
func New() *JSEngine {
	vm := goja.New()
	
	// Add some basic global functions
	vm.Set("print", func(call goja.FunctionCall) goja.Value {
		args := make([]interface{}, len(call.Arguments))
		for i, arg := range call.Arguments {
			args[i] = arg.Export()
		}
		fmt.Println(args...)
		return goja.Undefined()
	})

	return &JSEngine{
		vm:      vm,
		history: []string{},
	}
}

// Eval evaluates JavaScript code and returns the result
func (e *JSEngine) Eval(code string) (string, error) {
	e.history = append(e.history, code)
	
	result, err := e.vm.RunString(code)
	if err != nil {
		return "", err
	}
	
	if result == nil || goja.IsUndefined(result) {
		return "undefined", nil
	}
	
	if goja.IsNull(result) {
		return "null", nil
	}
	
	return fmt.Sprintf("%v", result), nil
}

// GetHistory returns the command history
func (e *JSEngine) GetHistory() []string {
	return e.history
}

// GetVM returns the underlying Goja runtime
func (e *JSEngine) GetVM() *goja.Runtime {
	return e.vm
}

// SetGlobal sets a global variable in the JavaScript environment
func (e *JSEngine) SetGlobal(name string, value interface{}) {
	e.vm.Set(name, value)
}

// GetVariable gets a variable's value from the JavaScript environment
func (e *JSEngine) GetVariable(name string) (string, error) {
	value := e.vm.Get(name)
	if value == nil || goja.IsUndefined(value) {
		return "", fmt.Errorf("variable '%s' is not defined", name)
	}
	
	// For objects and arrays, try to JSON stringify them
	exportType := reflect.TypeOf(value.Export())
	if exportType != nil && (exportType.Kind() == reflect.Map || exportType.Kind() == reflect.Slice) {
		jsonObj, err := e.vm.RunString(fmt.Sprintf("JSON.stringify(%s, null, 2)", name))
		if err == nil && jsonObj != nil {
			return jsonObj.String(), nil
		}
	}
	
	return fmt.Sprintf("%v", value), nil
}

// SetConsoleLogHandler sets a custom handler for console.log
func (e *JSEngine) SetConsoleLogHandler(handler func(string)) {
	console := make(map[string]interface{})
	console["log"] = func(call goja.FunctionCall) goja.Value {
		args := make([]interface{}, len(call.Arguments))
		for i, arg := range call.Arguments {
			args[i] = arg.Export()
		}
		message := fmt.Sprint(args...)
		handler(message)
		return goja.Undefined()
	}
	e.vm.Set("console", console)
}

// IsSlashCommand checks if the input is a slash command
func IsSlashCommand(input string) bool {
	return strings.HasPrefix(strings.TrimSpace(input), "/")
}

// ParseSlashCommand parses a slash command and returns the command name and arguments
func ParseSlashCommand(input string) (string, string) {
	input = strings.TrimSpace(input)
	if !IsSlashCommand(input) {
		return "", ""
	}
	
	parts := strings.SplitN(input[1:], " ", 2)
	cmd := parts[0]
	
	var args string
	if len(parts) > 1 {
		args = parts[1]
	}
	
	return cmd, args
}
