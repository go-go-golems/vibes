---
Title: Prolog REPL Architecture - Integrating bobatea with Goja
Ticket: INTEGRATE-PROLOG-GOJA-REPL
Status: active
Topics:
    - go
    - typescript
    - goja
    - prolog
    - repl
DocType: design-doc
Intent: long-term
Owners: []
RelatedFiles:
    - Path: ../../../../../../../bobatea/docs/repl.md
      Note: bobatea REPL documentation explaining evaluator interface and architecture
    - Path: ../../../../../../../bobatea/examples/js-repl/main.go
      Note: JavaScript REPL example showing Goja integration pattern
    - Path: ../../../../../../../bobatea/pkg/repl/evaluator.go
      Note: Evaluator interface definition with EvaluateStream method
ExternalSources: []
Summary: Architecture guide for integrating bobatea REPL with Goja-based Prolog interpreter, explaining evaluator interface, event streaming, and implementation patterns
LastUpdated: 2025-12-04T09:43:58.033431772-05:00
---



# Prolog REPL Architecture - Integrating bobatea with Goja

## Executive Summary

This document explains how to integrate the bobatea REPL component with a Goja-based TypeScript Prolog interpreter to create an interactive Prolog REPL. The bobatea REPL provides a pluggable evaluator interface that streams structured events, enabling rich output formatting and real-time feedback. By implementing the `Evaluator` interface with Goja runtime integration, we can execute Prolog queries interactively while maintaining state across evaluations.

**Key Integration Points:**
- bobatea's `Evaluator` interface with `EvaluateStream()` method
- Goja runtime for executing TypeScript Prolog code
- Event-based output streaming for rich formatting
- State management across REPL evaluations

## Problem Statement

We have a TypeScript Prolog interpreter running in Goja and need to provide an interactive REPL interface. The challenge is bridging the gap between:
1. **bobatea REPL** - Provides UI, history, and command handling but needs an evaluator
2. **Goja Prolog Interpreter** - Executes Prolog code but needs integration with REPL UI
3. **State Management** - Prolog database must persist across multiple queries
4. **Output Formatting** - Prolog query results need structured display (bindings, solutions)

The solution requires implementing bobatea's `Evaluator` interface to execute Prolog code via Goja and stream results as structured events.

## Proposed Solution

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    bobatea REPL Model                           │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  UI Layer (Bubble Tea)                                  │  │
│  │  - Input handling                                        │  │
│  │  - History management                                    │  │
│  │  - Command parsing                                       │  │
│  └──────────────────────────────────────────────────────────┘  │
│                          │                                      │
│                          │ EvaluateStream(code, emit)          │
│                          ▼                                      │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  PrologEvaluator (implements Evaluator interface)        │  │
│  │  - Parses Prolog input                                   │  │
│  │  - Manages PrologDB state                                │  │
│  │  - Executes via Goja                                     │  │
│  │  - Emits structured events                              │  │
│  └──────────────────────────────────────────────────────────┘  │
│                          │                                      │
│                          │ vm.RunString() / module access      │
│                          ▼                                      │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  Goja Runtime                                             │  │
│  │  - Embedded TypeScript bundle                            │  │
│  │  - PrologDB instance (persistent)                        │  │
│  │  - parseClause, parseTerm, prove functions              │  │
│  └──────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### Core Components

#### 1. Evaluator Interface

The bobatea REPL uses a streaming evaluator interface that emits structured events rather than returning simple strings. This enables rich output formatting and real-time feedback.

```go
type Evaluator interface {
	EvaluateStream(ctx context.Context, code string, emit func(Event)) error
	GetPrompt() string
	GetName() string
	SupportsMultiline() bool
	GetFileExtension() string
}
```

**Key aspects:**
- **EvaluateStream**: Executes code and emits events via callback (non-blocking, streaming)
- **GetPrompt**: Returns prompt string (e.g., `"prolog> "`)
- **GetName**: Evaluator name for display
- **SupportsMultiline**: Whether multiline input is supported
- **GetFileExtension**: File extension for external editor (`.pl` for Prolog)

#### 2. Event System

Events provide structured output that the REPL can format appropriately:

```go
type EventKind string

const (
	EventInput          EventKind = "repl_input"
	EventResultMarkdown EventKind = "repl_result_markdown"
	EventStdout         EventKind = "repl_stdout"
	EventStderr         EventKind = "repl_stderr"
	EventLog            EventKind = "repl_log"
	EventStructuredLog  EventKind = "repl_structured_log"
	EventTable          EventKind = "repl_table"
	// ... more event types
)

type Event struct {
	Kind  EventKind
	Props map[string]any
}
```

**Event types for Prolog:**
- `EventResultMarkdown`: Formatted query results with bindings
- `EventStructuredLog`: Structured data (solutions as JSON/YAML)
- `EventTable`: Tabular output for multiple solutions
- `EventStderr`: Error messages

#### 3. PrologEvaluator Implementation

The evaluator bridges bobatea REPL and Goja Prolog interpreter:

```go
type PrologEvaluator struct {
	vm     *goja.Runtime
	module goja.Value  // Prolog module exports
	db     goja.Value  // PrologDB instance (persistent)
	
	// Goja function references (cached for performance)
	createDBFunc    goja.Callable
	parseClauseFunc goja.Callable
	parseTermFunc   goja.Callable
	formatTermFunc  goja.Callable
	addClauseFunc   goja.Callable
	proveFunc       goja.Callable
}

func NewPrologEvaluator() (*PrologEvaluator, error) {
	// Create Goja VM
	vm := goja.New()
	
	// Set up module loader
	reg := require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
		if path == "prolog-ts.js" {
			return jsBundle.ReadFile("assets/prolog-ts.js")
		}
		return nil, fmt.Errorf("module not found: %s", path)
	}))
	reg.Enable(vm)
	
	// Load Prolog module
	module := require.Require(vm, "prolog-ts.js")
	exports := module.ToObject(vm)
	
	// Create PrologDB instance (persistent across evaluations)
	createDBFunc, _ := goja.AssertFunction(exports.Get("createPrologDB"))
	db, _ := createDBFunc(goja.Undefined())
	
	// Cache function references
	parseClauseFunc, _ := goja.AssertFunction(exports.Get("parseClause"))
	parseTermFunc, _ := goja.AssertFunction(exports.Get("parseTerm"))
	formatTermFunc, _ := goja.AssertFunction(exports.Get("formatTerm"))
	
	dbObj := db.ToObject(vm)
	addClauseFunc, _ := goja.AssertFunction(dbObj.Get("addClause"))
	proveFunc, _ := goja.AssertFunction(dbObj.Get("prove"))
	
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
```

#### 4. EvaluateStream Implementation

The core evaluation logic handles Prolog input and emits structured events:

```go
func (e *PrologEvaluator) EvaluateStream(ctx context.Context, code string, emit func(repl.Event)) error {
	code = strings.TrimSpace(code)
	
	// Handle empty input
	if code == "" {
		return nil
	}
	
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

func (e *PrologEvaluator) handleClause(ctx context.Context, code string, emit func(repl.Event)) error {
	// Parse clause
	clauseValue, err := e.parseClauseFunc(goja.Undefined(), e.vm.ToValue(code))
	if err != nil {
		emit(repl.Event{
			Kind: repl.EventStderr,
			Props: map[string]any{
				"text": fmt.Sprintf("Parse error: %v", err),
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
				"text": fmt.Sprintf("Error adding clause: %v", err),
				"is_error": true,
			},
		})
		return nil
	}
	
	// Format and emit success
	headFormatted, _ := e.formatTermFunc(goja.Undefined(), head)
	emit(repl.Event{
		Kind: repl.EventResultMarkdown,
		Props: map[string]any{
			"markdown": fmt.Sprintf("✓ Added: %s", headFormatted.String()),
		},
	})
	
	return nil
}

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
				"text": fmt.Sprintf("Parse error: %v", err),
				"is_error": true,
			},
		})
		return nil
	}
	
	// Create empty bindings
	bindingsValue, _ := e.vm.RunString("new Map()")
	
	// Execute query
	solutionsValue, err := e.proveFunc(e.db, queryValue, bindingsValue)
	if err != nil {
		emit(repl.Event{
			Kind: repl.EventStderr,
			Props: map[string]any{
				"text": fmt.Sprintf("Query error: %v", err),
				"is_error": true,
			},
		})
		return nil
	}
	
	// Extract solutions array
	solutionsArray := solutionsValue.ToObject(e.vm)
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
		solutionObj := solutionValue.ToObject(e.vm)
		
		// Format bindings as table or markdown
		bindingsStr := e.formatBindings(solutionObj)
		solutionStrs = append(solutionStrs, fmt.Sprintf("**Solution %d:**\n%s", i+1, bindingsStr))
	}
	
	emit(repl.Event{
		Kind: repl.EventResultMarkdown,
		Props: map[string]any{
			"markdown": strings.Join(solutionStrs, "\n\n"),
		},
	})
	
	return nil
}

func (e *PrologEvaluator) formatBindings(bindingsObj *goja.Object) string {
	// Extract bindings from Map and format as table
	// This is a simplified version - full implementation would iterate Map entries
	return "  Variable bindings would be displayed here"
}
```

#### 5. Console Integration

Override console functions to stream logs as events:

```go
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
				"level": "info",
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
				"text": message,
				"is_error": true,
			},
		})
		return goja.Undefined()
	})
	
	e.vm.Set("console", consoleObj)
}
```

#### 6. Complete Main Application

Wire everything together:

```go
func main() {
	// Create evaluator
	evaluator, err := NewPrologEvaluator()
	if err != nil {
		log.Fatal(err)
	}
	
	// Configure REPL
	config := repl.DefaultConfig()
	config.Title = "Prolog REPL"
	config.Prompt = "prolog> "
	config.Placeholder = "Enter Prolog facts, rules, or queries (use ?- for queries)"
	config.EnableHistory = true
	config.EnableExternalEditor = true
	
	// Set up event bus (for timeline/structured output)
	bus, err := eventbus.NewInMemoryBus()
	if err != nil {
		log.Fatal(err)
	}
	repl.RegisterReplToTimelineTransformer(bus)
	
	// Create REPL model
	model := repl.NewModel(evaluator, config, bus.Publisher)
	
	// Create Bubble Tea program
	p := tea.NewProgram(model, tea.WithAltScreen())
	timeline.RegisterUIForwarder(bus, p)
	
	// Run event bus and UI
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	
	errs := make(chan error, 2)
	go func() { errs <- bus.Run(ctx) }()
	go func() { _, e := p.Run(); cancel(); errs <- e }()
	
	if e := <-errs; e != nil {
		log.Fatal(e)
	}
}
```

## Design Decisions

### 1. Persistent PrologDB Instance

**Decision**: Create a single `PrologDB` instance in `NewPrologEvaluator()` and reuse it across all evaluations.

**Rationale**:
- Prolog database must persist across queries
- Creating new instances would lose all facts/rules
- Matches expected REPL behavior (stateful)

**Alternative Considered**: Create new DB per evaluation
- **Rejected**: Would lose state, breaking REPL semantics

### 2. Event-Based Output

**Decision**: Use `EvaluateStream()` with event emission instead of simple string return.

**Rationale**:
- Enables rich formatting (tables, markdown, structured data)
- Supports real-time streaming for long operations
- Consistent with bobatea architecture
- Allows better error display

**Alternative Considered**: Simple string return
- **Rejected**: Less flexible, no structured output support

### 3. Function Reference Caching

**Decision**: Cache Goja function references in evaluator struct.

**Rationale**:
- Avoids repeated `Get()` and `AssertFunction()` calls
- Better performance for repeated evaluations
- Cleaner code

**Alternative Considered**: Lookup functions on each call
- **Rejected**: Performance overhead, more verbose code

### 4. Query vs Clause Detection

**Decision**: Detect queries by `?-` or `?` prefix, everything else is a clause.

**Rationale**:
- Matches Prolog convention
- Simple heuristic
- Can be extended with better parsing

**Alternative Considered**: Always try clause first, then query
- **Rejected**: Ambiguous, could lead to confusing errors

## Implementation Plan

### Phase 1: Basic Evaluator
- [ ] Create `PrologEvaluator` struct
- [ ] Implement `NewPrologEvaluator()` with Goja setup
- [ ] Implement basic `EvaluateStream()` (clause handling only)
- [ ] Test adding facts

### Phase 2: Query Support
- [ ] Implement query parsing and execution
- [ ] Format solutions as markdown
- [ ] Handle empty solutions
- [ ] Test queries with variables

### Phase 3: Output Formatting
- [ ] Format bindings as tables
- [ ] Pretty-print Prolog terms
- [ ] Add structured log events
- [ ] Improve error messages

### Phase 4: Advanced Features
- [ ] Custom commands (`/clear`, `/list`, `/help`)
- [ ] Multiline support for rules
- [ ] External editor integration
- [ ] History persistence

### Phase 5: Polish
- [ ] Performance optimization
- [ ] Better error handling
- [ ] Documentation
- [ ] Examples

## Key Integration Patterns

### Pattern 1: Goja Function Calling

```go
// Get function reference
funcValue := exports.Get("functionName")
func, ok := goja.AssertFunction(funcValue)
if !ok {
	return fmt.Errorf("functionName is not a function")
}

// Call function
result, err := func(goja.Undefined(), arg1, arg2)
```

### Pattern 2: Object Method Calling

```go
// Get object method
obj := instanceValue.ToObject(vm)
methodValue := obj.Get("methodName")
method, _ := goja.AssertFunction(methodValue)

// Call method (first arg is receiver)
result, err := method(instanceValue, arg1, arg2)
```

### Pattern 3: Array Access

```go
// Get array length
array := value.ToObject(vm)
length := array.Get("length").ToInteger()

// Access element
element := array.Get(fmt.Sprintf("%d", index))
```

### Pattern 4: Map Creation and Access

```go
// Create Map
bindingsValue, _ := vm.RunString("new Map()")

// Access in JavaScript (via RunString)
result, _ := vm.RunString(fmt.Sprintf(`
	bindings.set("%s", %s);
`, varName, value))
```

## Troubleshooting

### Issue: PrologDB loses state between evaluations

**Cause**: Creating new DB instance for each evaluation

**Solution**: Create DB once in `NewPrologEvaluator()` and reuse

### Issue: Functions not found

**Cause**: Module not loaded or exports not accessible

**Solution**: Verify module loading, check export names match

### Issue: Events not displaying

**Cause**: Event bus not set up or events not emitted

**Solution**: Ensure `bus.Publisher` passed to `NewModel()`, verify `emit()` calls

### Issue: Multiline input not working

**Cause**: `SupportsMultiline()` returns false

**Solution**: Return `true` for Prolog (rules need multiple lines)

## References

- **bobatea REPL Documentation**: `bobatea/docs/repl.md`
- **js-repl Example**: `bobatea/examples/js-repl/main.go`
- **Evaluator Interface**: `bobatea/pkg/repl/evaluator.go`
- **Goja Integration Guide**: `vibes/2025/12/05/goja-prolog/pkg/doc/topics/building-typescript-goja-applications.md`
