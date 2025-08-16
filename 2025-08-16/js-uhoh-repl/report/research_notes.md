# Research Notes: JavaScript REPL Project

## go-go-goja Module Creation

### Key Concepts:
- Native modules bridge Go capabilities with JavaScript accessibility
- Modules implement the `NativeModule` interface
- Each module becomes a Node.js-style package that JavaScript can import via `require()`
- Automatic registration through `init()` functions

### Module Structure:
```go
type m struct{}
var _ modules.NativeModule = (*m)(nil)

func (m) Name() string { return "moduleName" }
func (m) Loader(vm *goja.Runtime, moduleObj *goja.Object) {
    exports := moduleObj.Get("exports").(*goja.Object)
    // Export functions to JavaScript
}
func init() { modules.Register(&m{}) }
```

### Type Conversion:
- Go to JS: string→String, int/float64→Number, bool→Boolean, map[string]interface{}→Object, []interface{}→Array, nil→null
- JS to Go: String→string, Number→int/int64/float64, Boolean→bool, Object→map[string]interface{}, Array→[]interface{}, null/undefined→nil

### Error Handling:
- Return Go errors directly from exported functions
- Runtime automatically converts them to JavaScript Error objects

### Module Registration:
- Add module to import list in `engine/runtime.go`
- Use blank import to ensure `init()` function runs

### Examples Seen:
- File System Module (fs): readFileSync, writeFileSync, existsSync
- HTTP Client Module: get method returning status, body, headers



## bobatea REPL Framework

### Key Features:
- Generic evaluator interface - Works with any language or custom evaluator
- Configurable behavior - Customizable prompts, themes, and settings
- Command history - Navigation through previous commands with persistence
- Multiline support - Optional multiline input mode for complex expressions
- External editor integration - Open $EDITOR for complex input
- Slash commands - Built-in commands plus custom command support
- Multiple themes - Built-in themes (default, dark, light) and custom styling
- Embeddable design - Clean message-based API for integration
- Keyboard shortcuts - Comprehensive keyboard navigation
- Real-time evaluation - Non-blocking evaluation with loading states

### Architecture Components:
- **Model** (UI State) - Manages UI state and coordination
- **Evaluator** (Interface) - Defines contract for language evaluators
- **History** (Command Log) - Command storage and navigation
- **Styles** (Theming) - Lipgloss-based styling configuration

### Evaluator Interface:
```go
type Evaluator interface {
    Evaluate(ctx context.Context, code string) (string, error)
    GetPrompt() string
    GetName() string
    SupportsMultiline() bool
    GetFileExtension() string
}
```

### Basic Usage Pattern:
```go
// Create evaluator and configuration
evaluator := &MyEvaluator{}
config := repl.DefaultConfig()
config.Title = "My REPL"

// Create and run the REPL
model := repl.NewModel(evaluator, config)
p := tea.NewProgram(model, tea.WithAltScreen())
```

### Message System:
- EvaluationCompleteMsg - Evaluation completed
- QuitMsg - REPL should quit
- ClearHistoryMsg - Clear history
- ExternalEditorCompleteMsg - External editor complete


## uhoh Integration Example (from user)

### Key API Usage:
```go
// Build huh.Form as a tea.Model and send to UI
form, vals, err := uhohdsl.BuildBubbleTeaModelFromYAML([]byte(req.DslYAML))
if err != nil {
    return GenerativeUIResponse{}, errors.Wrap(err, "build uhoh form")
}
replyCh := make(chan ToolUIReply, 1)
toolReqCh <- ToolUIRequest{Form: form, Values: vals, ReplyCh: replyCh}
```

### Integration Pattern:
- Use `uhohdsl.BuildBubbleTeaModelFromYAML()` to build BubbleTea models from YAML/JSON
- Returns form (tea.Model) and values
- Communication through channels (ToolUIRequest/ToolUIReply)
- Forms are built as huh.Form models


## uhoh DSL Structure

### Top-Level Structure:
```yaml
name: string  # Optional name for the form
theme: string  # Optional theme (Charm, Dracula, Catppuccin, Base16, Default)
groups:
  - name: string  # Optional group name
    fields:
      # List of fields
```

### Field Types:
1. **input**: Single-line text input
2. **text**: Multi-line text input  
3. **select**: Single-selection from options
4. **multiselect**: Multiple-selection from options
5. **confirm**: Yes/No confirmation
6. **note**: Informational field
7. **filepicker**: File selection field

### Common Field Properties:
```yaml
type: string  # Required: field type
key: string   # Required: unique identifier
title: string # Optional: title/prompt
description: string # Optional: description
value: any    # Optional: default value
validation:   # Optional: validation rules
  - condition: string
    error: string
```

### Key API Function:
- `uhohdsl.BuildBubbleTeaModelFromYAML([]byte(yamlData))` returns (form tea.Model, vals map[string]interface{}, error)

## JavaScript REPL Example Analysis

### Key Components:
- JSEvaluator struct with goja.Runtime
- Implements repl.Evaluator interface
- Uses ggjengine.New() to create runtime with modules
- Custom console.log implementation
- Supports multiline input

### Integration Pattern:
```go
type JSEvaluator struct {
    runtime *goja.Runtime
}

func (e *JSEvaluator) Evaluate(ctx context.Context, code string) (string, error) {
    result, err := e.runtime.RunString(code)
    // Handle result conversion
}
```

