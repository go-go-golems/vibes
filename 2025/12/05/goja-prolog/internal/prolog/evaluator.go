package prolog

import (
	"context"
	"embed"
	"fmt"
	"github.com/dop251/goja"
	"github.com/dop251/goja_nodejs/require"
	"github.com/go-go-golems/bobatea/pkg/repl"
	"github.com/rs/zerolog/log"
	"strings"
)

//go:embed assets/prolog-ts.js
var jsBundle embed.FS

// PrologEvaluator implements the bobatea Evaluator interface for Prolog
type PrologEvaluator struct {
	vm     *goja.Runtime
	module goja.Value // Prolog module exports
	db     goja.Value // PrologDB instance (persistent)

	// Goja function references (cached for performance)
	createDBFunc     goja.Callable
	parseClauseFunc  goja.Callable
	parseTermFunc    goja.Callable
	formatTermFunc   goja.Callable
	addClauseFunc    goja.Callable
	proveFunc        goja.Callable
	substBindingsFunc goja.Callable
	variablesInFunc   goja.Callable
}

// NewPrologEvaluator creates a new Prolog evaluator with Goja runtime
func NewPrologEvaluator() (*PrologEvaluator, error) {
	// Create Goja VM
	vm := goja.New()

	// Set up module loader
	reg := require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
		log.Debug().Str("path", path).Msg("Loading module from embedded bundle")

		// Resolve module paths to embedded bundle
		var fullPath string
		if path == "prolog-ts.js" || path == "app.js" || path == "node_modules/prolog-ts.js" {
			fullPath = "assets/prolog-ts.js"
		} else {
			return nil, fmt.Errorf("module not found: %s", path)
		}

		data, err := jsBundle.ReadFile(fullPath)
		if err != nil {
			log.Error().Err(err).Str("path", path).Str("fullPath", fullPath).Msg("Failed to load module")
			return nil, err
		}
		return data, nil
	}))

	// Enable Node.js polyfills
	reg.Enable(vm)

	// Note: Console will be set up in EvaluateStream with emit function
	// This allows console.log/error to emit events

	// Load Prolog module
	module := require.Require(vm, "prolog-ts.js")
	if module == nil {
		return nil, fmt.Errorf("failed to load prolog-ts.js module")
	}

	exports := module.ToObject(vm)
	if exports == nil {
		return nil, fmt.Errorf("failed to get module exports")
	}

	// Create PrologDB instance (persistent across evaluations)
	createDBValue := exports.Get("createPrologDB")
	createDBFunc, ok := goja.AssertFunction(createDBValue)
	if !ok {
		return nil, fmt.Errorf("createPrologDB is not a function")
	}

	db, err := createDBFunc(goja.Undefined())
	if err != nil {
		return nil, fmt.Errorf("failed to create PrologDB: %w", err)
	}

	// Cache function references
	parseClauseValue := exports.Get("parseClause")
	parseClauseFunc, ok := goja.AssertFunction(parseClauseValue)
	if !ok {
		return nil, fmt.Errorf("parseClause is not a function")
	}

	parseTermValue := exports.Get("parseTerm")
	parseTermFunc, ok := goja.AssertFunction(parseTermValue)
	if !ok {
		return nil, fmt.Errorf("parseTerm is not a function")
	}

	formatTermValue := exports.Get("formatTerm")
	formatTermFunc, ok := goja.AssertFunction(formatTermValue)
	if !ok {
		return nil, fmt.Errorf("formatTerm is not a function")
	}

	substBindingsValue := exports.Get("substBindings")
	substBindingsFunc, ok := goja.AssertFunction(substBindingsValue)
	if !ok {
		return nil, fmt.Errorf("substBindings is not a function")
	}

	variablesInValue := exports.Get("variablesIn")
	variablesInFunc, ok := goja.AssertFunction(variablesInValue)
	if !ok {
		return nil, fmt.Errorf("variablesIn is not a function")
	}

	// Get methods from PrologDB instance
	dbObj := db.ToObject(vm)
	if dbObj == nil {
		return nil, fmt.Errorf("failed to get PrologDB object")
	}

	addClauseValue := dbObj.Get("addClause")
	addClauseFunc, ok := goja.AssertFunction(addClauseValue)
	if !ok {
		return nil, fmt.Errorf("addClause is not a function")
	}

	proveValue := dbObj.Get("prove")
	proveFunc, ok := goja.AssertFunction(proveValue)
	if !ok {
		return nil, fmt.Errorf("prove is not a function")
	}

	return &PrologEvaluator{
		vm:                vm,
		module:            module,
		db:                db,
		createDBFunc:      createDBFunc,
		parseClauseFunc:   parseClauseFunc,
		parseTermFunc:     parseTermFunc,
		formatTermFunc:    formatTermFunc,
		addClauseFunc:     addClauseFunc,
		proveFunc:         proveFunc,
		substBindingsFunc: substBindingsFunc,
		variablesInFunc:   variablesInFunc,
	}, nil
}

// EvaluateStream implements the Evaluator interface
func (e *PrologEvaluator) EvaluateStream(ctx context.Context, code string, emit func(repl.Event)) error {
	code = strings.TrimSpace(code)

	// Handle empty input
	if code == "" {
		return nil
	}

	// Set up console with event emission
	e.setupConsole(emit)

	// Handle slash commands (delegated to REPL, but we can add custom ones)
	if strings.HasPrefix(code, "/") {
		return e.handleCommand(code, emit)
	}

	// Determine if input is a fact/rule or query
	isQuery := strings.HasPrefix(code, "?-") || strings.HasPrefix(code, "?")
	if isQuery {
		return e.handleQuery(ctx, code, emit)
	} else {
		return e.handleClause(ctx, code, emit)
	}
}

// handleClause processes Prolog facts and rules
func (e *PrologEvaluator) handleClause(ctx context.Context, code string, emit func(repl.Event)) error {
	// Parse clause
	clauseValue, err := e.parseClauseFunc(goja.Undefined(), e.vm.ToValue(code))
	if err != nil {
		emit(repl.Event{
			Kind: repl.EventStderr,
			Props: map[string]any{
				"text":     fmt.Sprintf("Parse error: %v", err),
				"is_error": true,
			},
		})
		return nil
	}

	clause := clauseValue.ToObject(e.vm)
	head := clause.Get("head")
	body := clause.Get("body")

	// Add to database
	_, err = e.addClauseFunc(e.db, head, body)
	if err != nil {
		emit(repl.Event{
			Kind: repl.EventStderr,
			Props: map[string]any{
				"text":     fmt.Sprintf("Error adding clause: %v", err),
				"is_error": true,
			},
		})
		return nil
	}

	// Format and emit success
	headFormatted, err := e.formatTermFunc(goja.Undefined(), head)
	if err != nil {
		emit(repl.Event{
			Kind: repl.EventResultMarkdown,
			Props: map[string]any{
				"markdown": "✓ Clause added successfully",
			},
		})
		return nil
	}

	emit(repl.Event{
		Kind: repl.EventResultMarkdown,
		Props: map[string]any{
			"markdown": fmt.Sprintf("✓ Added: %s", headFormatted.String()),
		},
	})

	return nil
}

// handleQuery processes Prolog queries
func (e *PrologEvaluator) handleQuery(ctx context.Context, code string, emit func(repl.Event)) error {
	// Remove query prefix
	queryCode := strings.TrimPrefix(strings.TrimPrefix(code, "?-"), "?")
	queryCode = strings.TrimSpace(queryCode)

	// Parse query term
	queryValue, err := e.parseTermFunc(goja.Undefined(), e.vm.ToValue(queryCode))
	if err != nil {
		emit(repl.Event{
			Kind: repl.EventStderr,
			Props: map[string]any{
				"text":     fmt.Sprintf("Parse error: %v", err),
				"is_error": true,
			},
		})
		return nil
	}

	// Create empty bindings Map
	bindingsValue, err := e.vm.RunString("new Map()")
	if err != nil {
		emit(repl.Event{
			Kind: repl.EventStderr,
			Props: map[string]any{
				"text":     fmt.Sprintf("Failed to create bindings: %v", err),
				"is_error": true,
			},
		})
		return nil
	}

	// Execute query
	solutionsValue, err := e.proveFunc(e.db, queryValue, bindingsValue)
	if err != nil {
		emit(repl.Event{
			Kind: repl.EventStderr,
			Props: map[string]any{
				"text":     fmt.Sprintf("Query error: %v", err),
				"is_error": true,
			},
		})
		return nil
	}

	// Extract solutions array
	solutionsArray := solutionsValue.ToObject(e.vm)
	if solutionsArray == nil {
		emit(repl.Event{
			Kind: repl.EventResultMarkdown,
			Props: map[string]any{
				"markdown": "**No solutions found.**",
			},
		})
		return nil
	}

	length := solutionsArray.Get("length").ToInteger()

	if length == 0 {
		emit(repl.Event{
			Kind: repl.EventResultMarkdown,
			Props: map[string]any{
				"markdown": "**No solutions found.**",
			},
		})
		return nil
	}

	// Get variables in the query for binding display
	queryVarsValue, err := e.variablesInFunc(goja.Undefined(), queryValue)
	if err != nil {
		log.Error().Err(err).Msg("Failed to get query variables")
	}

	// Format solutions
	var solutionStrs []string
	for i := int64(0); i < length; i++ {
		solutionValue := solutionsArray.Get(fmt.Sprintf("%d", i))
		if solutionValue == nil {
			log.Error().Int64("index", i).Msg("Solution value is nil")
			continue
		}

		// Substitute bindings in query to get the result with bound values
		substitutedValue, err := e.substBindingsFunc(goja.Undefined(), queryValue, solutionValue)
		if err != nil {
			log.Error().Err(err).Int64("index", i).Msg("Failed to substitute bindings")
			continue
		}

		// Format the substituted query
		formattedResult, err := e.formatTermFunc(goja.Undefined(), substitutedValue)
		if err != nil {
			log.Error().Err(err).Msg("Failed to format substituted term")
			continue
		}

		// Build solution output
		var solutionParts []string
		solutionParts = append(solutionParts, formattedResult.String())

		// Extract individual variable bindings for display
		if queryVarsValue != nil {
			bindingsStr := e.formatVariableBindings(queryVarsValue, solutionValue)
			if bindingsStr != "" {
				solutionParts = append(solutionParts, bindingsStr)
			}
		}

		solutionStrs = append(solutionStrs, fmt.Sprintf("**Solution %d:**\n%s", i+1, strings.Join(solutionParts, "\n")))
	}

	emit(repl.Event{
		Kind: repl.EventResultMarkdown,
		Props: map[string]any{
			"markdown": strings.Join(solutionStrs, "\n\n"),
		},
	})

	return nil
}

// formatVariableBindings formats the variable bindings from query variables and solution bindings
func (e *PrologEvaluator) formatVariableBindings(varsValue goja.Value, bindingsValue goja.Value) string {
	if varsValue == nil || bindingsValue == nil {
		return ""
	}

	varsArray := varsValue.ToObject(e.vm)
	if varsArray == nil {
		return ""
	}

	length := varsArray.Get("length").ToInteger()
	if length == 0 {
		return ""
	}

	var formattedLines []string
	for i := int64(0); i < length; i++ {
		varValue := varsArray.Get(fmt.Sprintf("%d", i))
		if varValue == nil {
			continue
		}

		// Variable is a Term with type='variable' and name='?x'
		varObj := varValue.ToObject(e.vm)
		if varObj == nil {
			continue
		}

		varName := varObj.Get("name")
		if varName == nil {
			continue
		}

		// Use substBindings to get the value for this variable
		substitutedVar, err := e.substBindingsFunc(goja.Undefined(), varValue, bindingsValue)
		if err != nil {
			log.Error().Err(err).Str("var", varName.String()).Msg("Failed to substitute variable")
			continue
		}

		// Format the substituted value
		formattedValue, err := e.formatTermFunc(goja.Undefined(), substitutedVar)
		if err != nil {
			log.Error().Err(err).Msg("Failed to format substituted variable")
			continue
		}

		formattedLines = append(formattedLines, fmt.Sprintf("  - %s = %s", varName.String(), formattedValue.String()))
	}

	if len(formattedLines) > 0 {
		return "Bindings:\n" + strings.Join(formattedLines, "\n")
	}

	return ""
}

// setupConsole configures console.log/error to emit events
func (e *PrologEvaluator) setupConsole(emit func(repl.Event)) {
	consoleObj := e.vm.NewObject()

	consoleObj.Set("log", func(call goja.FunctionCall) goja.Value {
		parts := make([]string, 0, len(call.Arguments))
		for _, arg := range call.Arguments {
			parts = append(parts, fmt.Sprint(arg.Export()))
		}
		message := strings.Join(parts, " ")

		emit(repl.Event{
			Kind: repl.EventLog,
			Props: map[string]any{
				"level":   "info",
				"message": message,
			},
		})
		return goja.Undefined()
	})

	consoleObj.Set("error", func(call goja.FunctionCall) goja.Value {
		parts := make([]string, 0, len(call.Arguments))
		for _, arg := range call.Arguments {
			parts = append(parts, fmt.Sprint(arg.Export()))
		}
		message := strings.Join(parts, " ")

		emit(repl.Event{
			Kind: repl.EventStderr,
			Props: map[string]any{
				"text":     message,
				"is_error": true,
			},
		})
		return goja.Undefined()
	})

	e.vm.Set("console", consoleObj)
}

// handleCommand processes custom slash commands
func (e *PrologEvaluator) handleCommand(code string, emit func(repl.Event)) error {
	// For now, let REPL handle built-in commands
	// Custom commands can be added here later
	return nil
}

// GetPrompt returns the prompt string
func (e *PrologEvaluator) GetPrompt() string {
	return "prolog> "
}

// GetName returns the evaluator name
func (e *PrologEvaluator) GetName() string {
	return "Prolog"
}

// SupportsMultiline returns true for Prolog (rules need multiple lines)
func (e *PrologEvaluator) SupportsMultiline() bool {
	return true
}

// GetFileExtension returns the file extension for external editor
func (e *PrologEvaluator) GetFileExtension() string {
	return ".pl"
}

