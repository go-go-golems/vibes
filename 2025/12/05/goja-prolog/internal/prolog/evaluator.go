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
	createDBFunc    goja.Callable
	parseClauseFunc goja.Callable
	parseTermFunc   goja.Callable
	formatTermFunc  goja.Callable
	addClauseFunc   goja.Callable
	proveFunc       goja.Callable
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
		vm:              vm,
		module:          module,
		db:              db,
		createDBFunc:    createDBFunc,
		parseClauseFunc: parseClauseFunc,
		parseTermFunc:   parseTermFunc,
		formatTermFunc:  formatTermFunc,
		addClauseFunc:   addClauseFunc,
		proveFunc:       proveFunc,
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

	// Format solutions
	var solutionStrs []string
	for i := int64(0); i < length; i++ {
		solutionValue := solutionsArray.Get(fmt.Sprintf("%d", i))
		if solutionValue == nil {
			log.Error().Int64("index", i).Msg("Solution value is nil")
			continue
		}

		solutionBindings := solutionValue.ToObject(e.vm)
		if solutionBindings == nil {
			log.Error().Int64("index", i).Msg("Solution bindings object is nil")
			continue
		}

		// Extract bindings from Map
		bindingsStr := e.formatBindings(solutionBindings, queryValue)

		// Format the query term
		queryFormatted, err := e.formatTermFunc(goja.Undefined(), queryValue)
		if err != nil {
			log.Error().Err(err).Msg("Failed to format term")
			if bindingsStr != "" {
				solutionStrs = append(solutionStrs, fmt.Sprintf("**Solution %d:**\n%s", i+1, bindingsStr))
			} else {
				solutionStrs = append(solutionStrs, fmt.Sprintf("**Solution %d:**\n(Query formatting failed)", i+1))
			}
			continue
		}

		if bindingsStr != "" {
			solutionStrs = append(solutionStrs, fmt.Sprintf("**Solution %d:**\nQuery: %s\n%s", i+1, queryFormatted.String(), bindingsStr))
		} else {
			solutionStrs = append(solutionStrs, fmt.Sprintf("**Solution %d:**\n%s", i+1, queryFormatted.String()))
		}
	}

	emit(repl.Event{
		Kind: repl.EventResultMarkdown,
		Props: map[string]any{
			"markdown": strings.Join(solutionStrs, "\n\n"),
		},
	})

	return nil
}

// formatBindings extracts and formats variable bindings from a solution Map
func (e *PrologEvaluator) formatBindings(bindingsObj *goja.Object, queryValue goja.Value) string {
	if bindingsObj == nil {
		return ""
	}

	// Use JavaScript to get Map entries as array
	// Then format each value using Goja formatTerm function
	result, err := e.vm.RunString(`
		(function(bindings) {
			if (!bindings || typeof bindings !== 'object') return [];
			var entries = [];
			if (bindings instanceof Map) {
				for (var [key, value] of bindings.entries()) {
					entries.push({key: key, value: value});
				}
			} else {
				for (var key in bindings) {
					if (bindings.hasOwnProperty(key)) {
						entries.push({key: key, value: bindings[key]});
					}
				}
			}
			return entries;
		})
	`)
	if err != nil {
		log.Error().Err(err).Msg("Failed to extract bindings")
		return ""
	}

	entriesArray := result.ToObject(e.vm)
	if entriesArray == nil {
		return ""
	}

	length := entriesArray.Get("length").ToInteger()
	if length == 0 {
		return ""
	}

	// Format each binding
	var formattedLines []string
	for i := int64(0); i < length; i++ {
		entryValue := entriesArray.Get(fmt.Sprintf("%d", i))
		if entryValue == nil {
			continue
		}

		entryObj := entryValue.ToObject(e.vm)
		if entryObj == nil {
			continue
		}

		keyValue := entryObj.Get("key")
		valueValue := entryObj.Get("value")
		
		if keyValue == nil || valueValue == nil {
			continue
		}

		// Format the value using formatTerm
		formattedValue, err := e.formatTermFunc(goja.Undefined(), valueValue)
		if err != nil {
			log.Error().Err(err).Msg("Failed to format binding value")
			// Fallback to string representation
			formattedLines = append(formattedLines, fmt.Sprintf("  - %s = %s", keyValue.String(), valueValue.String()))
			continue
		}
		
		if formattedValue == nil {
			continue
		}

		keyStr := keyValue.String()
		valueStr := formattedValue.String()
		formattedLines = append(formattedLines, fmt.Sprintf("  - %s = %s", keyStr, valueStr))
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

