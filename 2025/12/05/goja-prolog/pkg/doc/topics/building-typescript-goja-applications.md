---
Title: Building TypeScript + Goja Applications
Slug: building-typescript-goja-applications
Short: Complete playbook for integrating TypeScript code into Go binaries using Goja runtime
Topics:
- typescript
- goja
- go
- build-pipeline
- integration
IsTemplate: false
IsTopLevel: true
ShowPerDefault: true
SectionType: GeneralTopic
---

# Building TypeScript + Goja Applications

## Overview

Goja provides a pure Go JavaScript runtime that enables you to run TypeScript code within Go applications without requiring Node.js. This integration pattern allows you to leverage TypeScript's type safety and modern JavaScript features while maintaining a single, self-contained Go binary. By bundling TypeScript into CommonJS format and embedding it in your binary, you can create applications that combine Go's performance with TypeScript's developer experience.

This playbook walks through the complete process of setting up a TypeScript + Goja project, from initial structure to running your first integrated application.

## Project Structure

When integrating TypeScript with Goja, you need to carefully organize your project to accommodate both the TypeScript build process and Go's embedding requirements. The structure must support a workflow where TypeScript code is compiled, bundled, and then embedded into the Go binary at compile time. This means separating concerns: TypeScript source lives in one directory, build artifacts are staged in another, and the final embedded bundle must be in a location that Go's `go:embed` directive can access (which has strict path requirements). Understanding this structure upfront prevents common issues with module loading, embedding paths, and build pipeline configuration.

**📁 Directory Layout:**

```
your-project/
├── cmd/
│   └── your-app/
│       ├── assets/          # Embedded JS bundle (generated)
│       └── main.go          # Go entry point
├── web/                     # TypeScript source
│   ├── app.ts              # Entry point wrapper
│   ├── your-module.ts      # Your TypeScript code
│   ├── tsconfig.json
│   └── package.json
├── build/
│   └── generate.go         # Build pipeline
├── assets/                 # Temporary build output (gitignored)
└── go.mod
```

**🔑 Key Directories:**

| Directory | Purpose | Notes |
|-----------|---------|-------|
| `web/` | TypeScript source | All `.ts` files, configs, and `node_modules` |
| `cmd/your-app/assets/` | Embedded bundle | Must be relative to `main.go` for `go:embed` |
| `build/` | Build pipeline | `generate.go` orchestrates TypeScript → bundle → copy |
| `assets/` | Staging area | Temporary output before copying to embed location |

## Setting Up TypeScript

The TypeScript compiler configuration is critical because Goja's module system expects CommonJS format, not ES modules. This means your `tsconfig.json` must explicitly set `"module": "CommonJS"` even though modern TypeScript projects often default to ES modules. Additionally, you need to balance modern TypeScript features (like strict type checking) with Goja's JavaScript runtime capabilities, which don't support the absolute latest ECMAScript features. The target version (`ES2019`) is chosen specifically because it provides a good balance: modern enough for most use cases, but compatible with Goja's runtime. Getting this configuration wrong will result in modules that can't be loaded by Goja's `require()` system.

**⚙️ Configuration Requirements:**

Create `web/tsconfig.json`:

```json
{
  "compilerOptions": {
    "target": "ES2019",
    "module": "CommonJS",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "outDir": "./dist",
    "declaration": true
  },
  "include": ["*.ts", "**/*.ts"],
  "exclude": ["node_modules", "dist"]
}
```

**🔧 Critical Settings:**

| Setting | Value | Why |
|---------|-------|-----|
| `module` | `"CommonJS"` | ✅ Required for `goja_nodejs` module loader |
| `target` | `"ES2019"` | ✅ Balances modern features with Goja compatibility |
| `strict` | `true` | ✅ Enables full type checking (catch errors early) |
| `esModuleInterop` | `true` | ✅ Allows importing CommonJS modules |
| `skipLibCheck` | `true` | ⚡ Faster compilation (skips `.d.ts` checking) |

Create `web/package.json`:

```json
{
  "name": "your-app-web",
  "version": "1.0.0",
  "devDependencies": {
    "esbuild": "^0.25.5",
    "typescript": "^5.8.3"
  }
}
```

Install dependencies:

```bash
cd web && npm install
```

## Creating the TypeScript Entry Point

The entry point file (`app.ts`) serves as a bridge between your TypeScript code and Goja's module system. While you could directly export classes and functions from your modules, Goja has limitations when it comes to instantiating ES6 classes directly from Go code. The factory function pattern solves this by providing a simple function that Goja can call to create instances. Additionally, re-exporting everything from a single entry point makes it clear what's available to Go code and simplifies module loading. This wrapper doesn't change your core TypeScript logic—it just provides a Go-friendly interface layer that makes integration seamless.

**🔌 Entry Point Pattern:**

Create `web/app.ts`:

```typescript
// Import your TypeScript modules
import { YourClass, yourFunction } from './your-module';

// Re-export everything for Go access
export { YourClass, yourFunction };

// Factory function for creating instances
// Goja can't directly instantiate ES6 classes, so use a factory
export function createYourClass(): YourClass {
  return new YourClass();
}
```

**💡 Why This Pattern:**

- ✅ **Factory functions**: Goja can't directly instantiate ES6 classes → factory functions solve this
- ✅ **Single entry point**: All exports accessible via one module (`require("app.js")`)
- ✅ **Separation of concerns**: Core logic unchanged, wrapper provides Go integration layer

## Building the JavaScript Bundle

The build pipeline is a multi-step process that transforms TypeScript source code into a single JavaScript bundle that can be embedded in your Go binary. First, TypeScript type-checks your code (without emitting JavaScript) to catch errors early. Then esbuild bundles all your TypeScript files and their dependencies into a single CommonJS file, resolving imports and inlining code. Finally, the bundle is copied to a location where Go's `go:embed` can access it—this must be a subdirectory relative to your `main.go` file, as `go:embed` doesn't support parent directory paths (`../`). This pipeline is typically automated using `go:generate` directives, which run shell commands as part of the build process.

**🔨 Build Pipeline Steps:**

Create `build/generate.go`:

```go
//go:build generate
// +build generate

package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// 1️⃣ Type-check (no JS emitted)
//go:generate npx tsc --project ../web/tsconfig.json --noEmit
// 2️⃣ Produce CommonJS bundle that Goja can load
//go:generate npx esbuild ../web/app.ts --bundle --format=cjs --platform=node --target=es2019 --outfile=../assets/app.js --sourcemap=inline
// 3️⃣ Copy bundle to embeddable location
//go:generate sh -c "mkdir -p ../cmd/your-app/assets && cp ../assets/app.js ../cmd/your-app/assets/app.js"

func main() {
	if len(os.Args) > 1 && os.Args[1] == "copy-bundle" {
		// Alternative: Use Go code for copying if shell commands aren't available
		buildDir, _ := os.Getwd()
		source := filepath.Join(buildDir, "../assets/app.js")
		dest := filepath.Join(buildDir, "../cmd/your-app/assets/app.js")
		os.MkdirAll(filepath.Dir(dest), 0755)
		// Copy logic here...
		fmt.Printf("Copied bundle to %s\n", dest)
	}
}
```

**📋 Build Steps:**

| Step | Command | Purpose |
|------|---------|---------|
| 1️⃣ **Type-check** | `tsc --noEmit` | ✅ Validates TypeScript (catches errors before bundling) |
| 2️⃣ **Bundle** | `esbuild --bundle --format=cjs` | ✅ Compiles TS → JS, bundles dependencies, outputs CommonJS |
| 3️⃣ **Copy** | `cp assets/app.js cmd/app/assets/` | ✅ Moves bundle to `go:embed`-accessible location |

Run the build:

```bash
cd build && go generate generate.go
```

Or manually:

```bash
cd web
npx tsc --project tsconfig.json --noEmit
npx esbuild app.ts --bundle --format=cjs --platform=node --target=es2019 --outfile=../assets/app.js --sourcemap=inline
mkdir -p ../cmd/your-app/assets
cp ../assets/app.js ../cmd/your-app/assets/app.js
```

## Implementing the Go Application

The Go application is where everything comes together: it creates a Goja JavaScript runtime, configures a custom module loader that intercepts `require()` calls and resolves them to your embedded bundle, and then executes your TypeScript code. The custom loader is crucial because Goja's default module system doesn't know about your embedded files—it needs a function that maps module paths (like `"app.js"`) to the actual embedded file data. Once the module is loaded, you access its exports using Goja's object model: `ToObject()` converts the module to an object, `Get()` retrieves exported values, and `AssertFunction()` converts values to callable functions. This pattern allows you to call TypeScript functions, create instances via factory functions, and interact with your TypeScript code as if it were native Go code.

**🚀 Go Application Components:**

Create `cmd/your-app/main.go`:

```go
package main

import (
	"embed"
	"fmt"
	"github.com/dop251/goja"
	"github.com/dop251/goja_nodejs/require"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"os"
)

//go:embed assets/app.js
var jsBundle embed.FS

func main() {
	setupLogger()
	log.Info().Msg("Starting application")

	// Create Goja VM
	vm := goja.New()

	// Set up module loader
	reg := require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
		// Resolve module paths to embedded bundle
		var fullPath string
		if path == "app.js" || path == "node_modules/app.js" {
			fullPath = "assets/app.js"
		} else {
			return nil, fmt.Errorf("module not found: %s", path)
		}

		data, err := jsBundle.ReadFile(fullPath)
		if err != nil {
			log.Error().Err(err).Str("path", path).Msg("Failed to load module")
			return nil, err
		}
		return data, nil
	}))

	// Enable Node.js polyfills
	reg.Enable(vm)

	// Set up console
	setupConsole(vm)

	// Load your TypeScript module
	module := require.Require(vm, "app.js")
	if module == nil {
		log.Fatal().Msg("Failed to load module")
	}

	// Access module exports
	exports := module.ToObject(vm)
	
	// Get factory function
	createFuncValue := exports.Get("createYourClass")
	createFunc, ok := goja.AssertFunction(createFuncValue)
	if !ok {
		log.Fatal().Msg("createYourClass is not a function")
	}

	// Create instance
	instanceValue, err := createFunc(goja.Undefined())
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create instance")
	}

	// Use the instance
	instance := instanceValue.ToObject(vm)
	methodValue := instance.Get("yourMethod")
	methodFunc, _ := goja.AssertFunction(methodValue)
	result, _ := methodFunc(instanceValue, vm.ToValue("argument"))

	log.Info().Interface("result", result).Msg("Execution completed")
}

func setupLogger() {
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr}).
		With().
		Caller().
		Logger()
	zerolog.SetGlobalLevel(zerolog.DebugLevel)
}

func setupConsole(vm *goja.Runtime) {
	console := vm.NewObject()
	console.Set("log", func(call goja.FunctionCall) goja.Value {
		args := make([]interface{}, len(call.Arguments))
		for i, arg := range call.Arguments {
			args[i] = arg.Export()
		}
		fmt.Println(args...)
		return goja.Undefined()
	})
	console.Set("error", func(call goja.FunctionCall) goja.Value {
		args := make([]interface{}, len(call.Arguments))
		for i, arg := range call.Arguments {
			args[i] = arg.Export()
		}
		fmt.Fprintln(os.Stderr, "ERROR:", args)
		return goja.Undefined()
	})
	vm.Set("console", console)
}
```

**🔑 Key Components:**

| Component | Purpose | Example |
|-----------|--------|---------|
| **`go:embed`** | Embeds JS bundle at compile time | `//go:embed assets/app.js` |
| **Custom loader** | Maps `require()` paths → embedded files | `require.WithLoader(func(path) ([]byte, error))` |
| **Module access** | Gets exports from loaded module | `module.ToObject(vm).Get("exportName")` |
| **Function calls** | Invokes TypeScript functions | `func.Call(receiver, args...)` |

## Installing Dependencies

Add the required Go dependencies to your project:

```bash
go get github.com/dop251/goja@latest
go get github.com/dop251/goja_nodejs/require@latest
go get github.com/rs/zerolog@latest
```

## Building and Running

Build the TypeScript bundle and Go binary:

```bash
# Build TypeScript bundle
cd build && go generate generate.go

# Build Go binary
go build -o bin/your-app ./cmd/your-app

# Run
./bin/your-app
```

## Common Patterns

These patterns represent the most common ways you'll interact with TypeScript code from Go. Understanding these will cover 90% of integration scenarios.

### Calling TypeScript Functions from Go

When you export a function from TypeScript, accessing it from Go follows a consistent pattern: get the module exports, retrieve the function value, assert it's a function, then call it with arguments. Goja automatically converts Go values to JavaScript values using `ToValue()`, and converts JavaScript return values back to Go using `Export()` or type-specific methods like `ToString()`.

**📞 Function Call Pattern:**

Access exported functions directly from module exports:

```go
exports := module.ToObject(vm)
funcValue := exports.Get("yourFunction")
func, _ := goja.AssertFunction(funcValue)
result, _ := func(goja.Undefined(), vm.ToValue("arg1"), vm.ToValue("arg2"))
```

### Creating and Using Class Instances

Since Goja can't directly instantiate ES6 classes, you use factory functions exported from your TypeScript entry point. The factory function creates and returns an instance, which you then use to call methods. Methods are accessed via `Get()` on the instance object, and called with the instance as the first argument (the `this` context in JavaScript).

**🏭 Instance Creation Pattern:**

```go
// Create instance
createFunc, _ := goja.AssertFunction(exports.Get("createYourClass"))
instance, _ := createFunc(goja.Undefined())

// Call method
instanceObj := instance.ToObject(vm)
method, _ := goja.AssertFunction(instanceObj.Get("methodName"))
result, _ := method(instance, vm.ToValue("argument"))
```

### Passing Data Between Go and TypeScript

Goja provides automatic type conversion between Go and JavaScript, but understanding the conversion rules helps avoid surprises. Go maps become JavaScript objects, Go slices become JavaScript arrays, and Go structs become JavaScript objects with matching properties. When returning values from JavaScript, `Export()` converts to `interface{}`, while type-specific methods like `ToInteger()` or `ToString()` provide direct conversions.

**🔄 Type Conversion:**

```go
// Go → TypeScript
vm.ToValue(map[string]interface{}{"key": "value"})
vm.ToValue([]string{"a", "b", "c"})

// TypeScript → Go
jsValue.Export()  // Converts to Go interface{}
jsValue.ToInteger()
jsValue.ToString()
```

### Error Handling

JavaScript errors in Goja can manifest as returned errors from function calls or as panics if unhandled. Wrapping Goja operations in recover blocks prevents panics from crashing your Go application. Additionally, JavaScript `throw` statements become Go errors that you can check and handle appropriately.

**⚠️ Error Handling Pattern:**

```go
func() {
	defer func() {
		if r := recover(); r != nil {
			log.Error().Interface("panic", r).Msg("JavaScript execution failed")
		}
	}()
	
	result, err := vm.RunString("yourJavaScriptCode()")
	if err != nil {
		log.Error().Err(err).Msg("JavaScript error")
	}
}()
```

## Troubleshooting

Common issues and their solutions based on real-world integration experience.

### Module Not Found Errors

**Problem**: `require()` fails to find your module

**🔍 Diagnosis & Solutions:**

| Issue | Check | Fix |
|-------|-------|-----|
| Bundle missing | Verify `cmd/your-app/assets/app.js` exists | Run build pipeline |
| Path mismatch | Check loader resolves `"app.js"` → `"assets/app.js"` | Update loader logic |
| `go:embed` path | Must be relative to source file (no `../`) | Copy bundle to subdirectory |

### Type Errors in TypeScript

**Problem**: TypeScript compilation fails

**🔍 Diagnosis & Solutions:**

| Issue | Check | Fix |
|-------|-------|-----|
| Type errors | Run `tsc --noEmit` for details | Fix type issues in source |
| Import resolution | Verify all imports resolve | Check `include` in `tsconfig.json` |
| Missing types | Check `node_modules/@types` | Install missing type definitions |

### Runtime Errors in Goja

**Problem**: JavaScript code executes but throws errors

**🔍 Diagnosis & Solutions:**

| Issue | Check | Fix |
|-------|-------|-----|
| Unclear errors | Enable `--sourcemap=inline` | Better stack traces |
| No console output | Call `setupConsole()` | Console logs visible |
| Missing exports | Verify `app.ts` exports functions | Add missing exports |
| Nil values | Check `ToObject()` returns non-nil | Add null checks |

### go:embed Path Issues

**Problem**: `go:embed` cannot find files

**🔍 Diagnosis & Solutions:**

| Issue | Check | Fix |
|-------|-------|-----|
| Invalid path | No `../` in embed path | Use subdirectory relative to source |
| File missing | Bundle exists at build time? | Run build pipeline before `go build` |
| Wrong location | Path relative to `main.go`? | Copy to `cmd/app/assets/` |

## Best Practices

These practices come from real-world experience building TypeScript + Goja integrations. Following them will save you significant debugging time.

**✅ Essential Practices:**

| Practice | Why | Impact |
|----------|-----|--------|
| **Keep bundles small** | Faster load times, smaller binaries | ⚡ Performance |
| **Use factory functions** | Goja can't instantiate ES6 classes directly | 🔧 Compatibility |
| **Enable sourcemaps** | Better error messages and debugging | 🐛 Debugging |
| **Type-check first** | Catch errors before bundling | ⚠️ Error prevention |
| **Test incrementally** | Verify each build step works | 🔍 Early detection |
| **Document exports** | Clear API for Go code | 📚 Maintainability |
| **Cache function references** | Avoid repeated `Get()`/`AssertFunction()` calls | ⚡ Performance |
| **Add null checks** | Prevent nil pointer panics | 🛡️ Robustness |

## Adding bobatea REPL Support

The [bobatea](https://github.com/go-go-golems/bobatea) framework provides a powerful REPL (Read-Eval-Print Loop) component that transforms your Goja-based interpreter into an interactive terminal application. Unlike simple command-line tools, a REPL provides a persistent session where users can enter code, see results, and build up state incrementally. bobatea's REPL adds professional features like syntax highlighting, command history, multiline input support, external editor integration, and rich output formatting (markdown, tables, structured logs). The key architectural pattern is the `Evaluator` interface: you implement this interface to provide language-specific evaluation logic, while bobatea handles all the UI, input management, and event routing. This separation means you focus on executing code and emitting results, while bobatea handles the complex terminal UI interactions.

### Overview

bobatea's REPL uses an `Evaluator` interface that you implement to provide language evaluation logic. For a Goja-based interpreter, you'll:

1. Create an evaluator struct that wraps your Goja runtime
2. Implement the `EvaluateStream` method to execute code and emit events
3. Wire up the REPL with event bus and Bubble Tea UI
4. Handle console integration for TypeScript `console.log`/`console.error`

### Project Structure for REPL

Add these components to your existing project:

```
your-project/
├── cmd/
│   └── your-app/
│       ├── assets/
│       └── main.go          # REPL main application
├── internal/
│   └── evaluator/
│       └── evaluator.go    # Evaluator implementation
└── web/
    └── app.ts              # TypeScript entry point
```

### Installing bobatea Dependencies

Add bobatea and Bubble Tea to your project:

```bash
go get github.com/go-go-golems/bobatea/pkg/repl@latest
go get github.com/go-go-golems/bobatea/pkg/eventbus@latest
go get github.com/charmbracelet/bubbletea@latest
```

### Creating the Evaluator

The evaluator implements the `repl.Evaluator` interface and manages your Goja runtime:

Create `internal/evaluator/evaluator.go`:

```go
package evaluator

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

//go:embed ../../cmd/your-app/assets/app.js
var jsBundle embed.FS

// YourEvaluator implements the bobatea Evaluator interface
type YourEvaluator struct {
	vm     *goja.Runtime
	module goja.Value // Module exports
	db     goja.Value // Persistent state (if needed)

	// Cached function references for performance
	createDBFunc     goja.Callable
	parseFunc        goja.Callable
	formatFunc       goja.Callable
	evaluateFunc     goja.Callable
}

// NewYourEvaluator creates a new evaluator with Goja runtime
func NewYourEvaluator() (*YourEvaluator, error) {
	vm := goja.New()

	// Set up module loader (same as main.go example)
	reg := require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
		var fullPath string
		if path == "app.js" || path == "node_modules/app.js" {
			fullPath = "../../cmd/your-app/assets/app.js"
		} else {
			return nil, fmt.Errorf("module not found: %s", path)
		}
		return jsBundle.ReadFile(fullPath)
	}))

	reg.Enable(vm)

	// Load module
	module := require.Require(vm, "app.js")
	if module == nil {
		return nil, fmt.Errorf("failed to load module")
	}

	exports := module.ToObject(vm)

	// Cache function references
	createDBValue := exports.Get("createDB")
	createDBFunc, ok := goja.AssertFunction(createDBValue)
	if !ok {
		return nil, fmt.Errorf("createDB is not a function")
	}

	db, err := createDBFunc(goja.Undefined())
	if err != nil {
		return nil, fmt.Errorf("failed to create DB: %w", err)
	}

	// Cache other functions...
	parseValue := exports.Get("parse")
	parseFunc, _ := goja.AssertFunction(parseValue)

	formatValue := exports.Get("format")
	formatFunc, _ := goja.AssertFunction(formatValue)

	return &YourEvaluator{
		vm:          vm,
		module:      module,
		db:          db,
		createDBFunc: createDBFunc,
		parseFunc:   parseFunc,
		formatFunc:  formatFunc,
	}, nil
}

// EvaluateStream implements the Evaluator interface
func (e *YourEvaluator) EvaluateStream(ctx context.Context, code string, emit func(repl.Event)) error {
	code = strings.TrimSpace(code)

	// Handle empty input
	if code == "" {
		return nil
	}

	// Set up console to emit events
	e.setupConsole(emit)

	// Handle slash commands
	if strings.HasPrefix(code, "/") {
		return e.handleCommand(code, emit)
	}

	// Execute code and emit results
	return e.executeCode(code, emit)
}

// executeCode runs the code and emits results as events
func (e *YourEvaluator) executeCode(code string, emit func(repl.Event)) error {
	// Parse input
	parsedValue, err := e.parseFunc(goja.Undefined(), e.vm.ToValue(code))
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

	// Execute (example - adjust to your needs)
	resultValue, err := e.evaluateFunc(e.db, parsedValue)
	if err != nil {
		emit(repl.Event{
			Kind: repl.EventStderr,
			Props: map[string]any{
				"text":     fmt.Sprintf("Execution error: %v", err),
				"is_error": true,
			},
		})
		return nil
	}

	// Format and emit result
	formattedValue, err := e.formatFunc(goja.Undefined(), resultValue)
	if err != nil {
		emit(repl.Event{
			Kind: repl.EventResultMarkdown,
			Props: map[string]any{
				"markdown": fmt.Sprintf("Result: %v", resultValue.Export()),
			},
		})
		return nil
	}

	emit(repl.Event{
		Kind: repl.EventResultMarkdown,
		Props: map[string]any{
			"markdown": formattedValue.String(),
		},
	})

	return nil
}

// setupConsole configures console.log/error to emit events
func (e *YourEvaluator) setupConsole(emit func(repl.Event)) {
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
func (e *YourEvaluator) handleCommand(code string, emit func(repl.Event)) error {
	// Custom commands like /clear, /help, etc.
	switch code {
	case "/clear":
		// Clear state
		return nil
	case "/help":
		emit(repl.Event{
			Kind: repl.EventResultMarkdown,
			Props: map[string]any{
				"markdown": "**Available commands:**\n- `/clear` - Clear state\n- `/help` - Show this help",
			},
		})
		return nil
	}
	return nil
}

// GetPrompt returns the prompt string
func (e *YourEvaluator) GetPrompt() string {
	return "your-lang> "
}

// GetName returns the evaluator name
func (e *YourEvaluator) GetName() string {
	return "YourLanguage"
}

// SupportsMultiline returns whether multiline input is supported
func (e *YourEvaluator) SupportsMultiline() bool {
	return true
}

// GetFileExtension returns the file extension for external editor
func (e *YourEvaluator) GetFileExtension() string {
	return ".yourlang"
}
```

### Wiring Up the Main Application

Create `cmd/your-app/main.go`:

```go
package main

import (
	"context"
	"flag"
	"log"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/go-go-golems/bobatea/pkg/eventbus"
	"github.com/go-go-golems/bobatea/pkg/logutil"
	"github.com/go-go-golems/bobatea/pkg/repl"
	"github.com/go-go-golems/bobatea/pkg/timeline"
	"github.com/rs/zerolog"
	"github.com/your-org/your-project/internal/evaluator"
)

func parseLevel(s string) zerolog.Level {
	switch s {
	case "trace": return zerolog.TraceLevel
	case "debug": return zerolog.DebugLevel
	case "info": return zerolog.InfoLevel
	case "warn", "warning": return zerolog.WarnLevel
	case "error", "err": return zerolog.ErrorLevel
	default: return zerolog.ErrorLevel
	}
}

func main() {
	// CLI flags for logging
	ll := flag.String("log-level", "error", "log level: trace, debug, info, warn, error")
	lf := flag.String("log-file", "", "log file path (optional)")
	flag.Parse()

	level := parseLevel(*ll)
	if *lf != "" {
		logutil.InitTUILoggingToFile(level, *lf)
	} else {
		logutil.InitTUILoggingToDiscard(level)
	}

	// Create evaluator
	eval, err := evaluator.NewYourEvaluator()
	if err != nil {
		log.Fatal(err)
	}

	// Configure REPL
	config := repl.DefaultConfig()
	config.Title = "Your Language REPL"
	config.Prompt = "your-lang> "
	config.Placeholder = "Enter code here..."
	config.EnableHistory = true
	config.EnableExternalEditor = true

	// Set up event bus
	bus, err := eventbus.NewInMemoryBus()
	if err != nil {
		log.Fatal(err)
	}
	repl.RegisterReplToTimelineTransformer(bus)

	// Create REPL model
	model := repl.NewModel(eval, config, bus.Publisher)

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

### Event Types

bobatea REPL uses structured events for output:

- **`EventResultMarkdown`**: Formatted markdown output
- **`EventStderr`**: Error messages
- **`EventLog`**: Log messages
- **`EventStructuredLog`**: Structured data (YAML/JSON)
- **`EventTable`**: Tabular data

Example event emission:

```go
emit(repl.Event{
	Kind: repl.EventResultMarkdown,
	Props: map[string]any{
		"markdown": "**Result:**\n```\nvalue\n```",
	},
})
```

### Handling Queries with Variables

For languages like Prolog that support queries with variables, use `substBindings` pattern:

```go
// Get variables in query
queryVarsValue, _ := e.variablesInFunc(goja.Undefined(), queryValue)

// For each solution
for i := int64(0); i < length; i++ {
	solutionValue := solutionsArray.Get(fmt.Sprintf("%d", i))
	
	// Substitute bindings in query
	substitutedValue, _ := e.substBindingsFunc(goja.Undefined(), queryValue, solutionValue)
	formattedResult, _ := e.formatTermFunc(goja.Undefined(), substitutedValue)
	
	// Format individual bindings
	bindingsStr := e.formatVariableBindings(queryVarsValue, solutionValue)
	
	emit(repl.Event{
		Kind: repl.EventResultMarkdown,
		Props: map[string]any{
			"markdown": fmt.Sprintf("**Solution %d:**\n%s\n%s", i+1, formattedResult.String(), bindingsStr),
		},
	})
}
```

### Testing TUI Applications with tmux

TUI applications require a terminal environment. Use tmux for automated testing:

**Basic setup:**

```bash
# Create test session
tmux new-session -d -s test-repl -x 120 -y 40

# Start application
tmux send-keys -t test-repl "./bin/your-app" Enter
sleep 2

# Send input
tmux send-keys -t test-repl "your code here" Enter
sleep 1

# Capture output (full pane, no tail/head)
tmux capture-pane -t test-repl -p > output.txt

# Cleanup
tmux send-keys -t test-repl "C-c"
tmux kill-session -t test-repl
```

**Key points:**
- Always capture full pane (don't use `tail`/`head`) to see complete UI state
- Use appropriate delays between commands
- Save captures to files for analysis
- Test keyboard shortcuts: `Up`/`Down` (history), `Tab` (focus), `C-c` (quit)

### Common Patterns

**Persistent State:**

```go
// Create state once in NewEvaluator
db, _ := createDBFunc(goja.Undefined())

// Reuse across evaluations
e.db = db
```

**Function Caching:**

```go
// Cache in NewEvaluator for performance
parseFunc, _ := goja.AssertFunction(exports.Get("parse"))
formatFunc, _ := goja.AssertFunction(exports.Get("format"))

// Reuse in EvaluateStream
result, _ := e.parseFunc(goja.Undefined(), e.vm.ToValue(code))
```

**Error Handling:**

```go
if err != nil {
	emit(repl.Event{
		Kind: repl.EventStderr,
		Props: map[string]any{
			"text":     fmt.Sprintf("Error: %v", err),
			"is_error": true,
		},
	})
	return nil // Don't return error, emit event instead
}
```

### Troubleshooting REPL Integration

**Issue**: REPL doesn't start or crashes immediately

**Solutions**:
- Check that evaluator is created successfully
- Verify event bus is set up correctly
- Ensure Bubble Tea program is created with `tea.WithAltScreen()`
- Check for nil pointer panics in evaluator

**Issue**: Events not displaying

**Solutions**:
- Verify `bus.Publisher` is passed to `NewModel()`
- Check that `emit()` is being called
- Ensure `repl.RegisterReplToTimelineTransformer(bus)` is called
- Verify event bus is running (`bus.Run(ctx)`)

**Issue**: Console.log not working

**Solutions**:
- Call `setupConsole(emit)` at the start of `EvaluateStream()`
- Verify console object is set on VM: `e.vm.Set("console", consoleObj)`
- Check that emit function is capturing events

**Issue**: Multiline input not working

**Solutions**:
- Return `true` from `SupportsMultiline()`
- Verify REPL config has multiline enabled (default: true)
- Test with external editor integration (`C-e`)

### Best Practices for REPL Integration

- **Cache function references**: Store Goja functions in evaluator struct for performance
- **Persistent state**: Create stateful objects (like databases) once in `NewEvaluator()`
- **Event-based output**: Always use `emit()` instead of returning strings
- **Console integration**: Set up console in `EvaluateStream()` to capture TypeScript logs
- **Error handling**: Emit error events, don't return errors from `EvaluateStream()`
- **Full pane capture**: When testing with tmux, capture entire pane to see complete UI state
- **Null checks**: Always check for nil values when accessing Goja objects
- **Function caching**: Cache all utility functions (`substBindings`, `formatTerm`, etc.) upfront

## Next Steps

Once you have a working integration, consider:

- Adding development mode with hot reload
- Implementing bidirectional communication (Go → TypeScript and TypeScript → Go)
- Creating helper utilities for common patterns
- Adding comprehensive error handling
- Performance profiling and optimization
- Adding REPL support with bobatea (see section above)
- Implementing custom commands (`/clear`, `/help`, etc.)
- Adding syntax highlighting
- Implementing history persistence

