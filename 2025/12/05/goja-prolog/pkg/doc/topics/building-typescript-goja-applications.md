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

A well-organized project structure separates TypeScript source code, build artifacts, and Go application code. This separation keeps concerns clear and makes the build pipeline straightforward.

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

**Key directories:**
- `web/` contains all TypeScript source files
- `cmd/your-app/assets/` holds the embedded JavaScript bundle (must be relative to main.go for go:embed)
- `build/` contains the build pipeline configuration
- `assets/` is a temporary staging area for the bundle before copying

## Setting Up TypeScript

TypeScript configuration must target CommonJS module format, which is required by goja_nodejs's module loader. The configuration balances modern TypeScript features with compatibility requirements.

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

**Critical settings:**
- `"module": "CommonJS"` - Required for goja_nodejs compatibility
- `"target": "ES2019"` - Balances modern features with Goja support
- `"strict": true` - Enables full type checking

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

The entry point wrapper imports your TypeScript modules and re-exports them in a way that Goja can easily access. This pattern provides a clean interface between Go and TypeScript code.

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

**Why this pattern:**
- Factory functions provide a clean way to create instances from Go
- Re-exports make all functionality accessible via a single module
- Keeps your original TypeScript code unchanged and reusable

## Building the JavaScript Bundle

The build pipeline compiles TypeScript, bundles it into a single CommonJS file, and copies it to an embeddable location. This process ensures type safety while producing a bundle that Goja can load.

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

**Build steps explained:**
1. **Type-check**: Validates TypeScript without emitting JavaScript
2. **Bundle**: esbuild compiles and bundles into single CommonJS file
3. **Copy**: Moves bundle to location where go:embed can access it

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

The Go application creates a Goja runtime, sets up module loading, and executes your TypeScript code. The custom module loader intercepts `require()` calls and resolves them to your embedded bundle.

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

**Key components:**
- **go:embed**: Embeds the JavaScript bundle at compile time
- **Custom loader**: Resolves `require()` calls to embedded files
- **Module access**: Uses `ToObject()` and `Get()` to access exports
- **Function calls**: Uses `AssertFunction()` and `Call()` to invoke methods

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

### Calling TypeScript Functions from Go

Access exported functions directly from module exports:

```go
exports := module.ToObject(vm)
funcValue := exports.Get("yourFunction")
func, _ := goja.AssertFunction(funcValue)
result, _ := func(goja.Undefined(), vm.ToValue("arg1"), vm.ToValue("arg2"))
```

### Creating and Using Class Instances

Use factory functions to create instances, then call methods:

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

Goja automatically converts between Go and JavaScript types:

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

Handle JavaScript errors and Goja panics:

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

### Module Not Found Errors

**Problem**: `require()` fails to find your module

**Solutions**:
- Verify the bundle exists at `cmd/your-app/assets/app.js`
- Check the module loader path resolution logic
- Ensure `go:embed` path matches actual file location (must be relative to source file)

### Type Errors in TypeScript

**Problem**: TypeScript compilation fails

**Solutions**:
- Run `npx tsc --project web/tsconfig.json --noEmit` to see detailed errors
- Check that all imports resolve correctly
- Verify `tsconfig.json` includes all necessary files

### Runtime Errors in Goja

**Problem**: JavaScript code executes but throws errors

**Solutions**:
- Enable sourcemaps (`--sourcemap=inline`) for better error messages
- Check console output (ensure `setupConsole()` is called)
- Verify all required functions are exported from `app.ts`

### go:embed Path Issues

**Problem**: `go:embed` cannot find files

**Solutions**:
- Files must be in subdirectories relative to the source file (no `../`)
- Copy bundle to `cmd/your-app/assets/` before building
- Verify file exists at build time (go:embed requires files at compile time)

## Best Practices

- **Keep bundles small**: Only include code you actually use
- **Use factory functions**: They provide cleaner APIs than direct class instantiation
- **Enable sourcemaps**: They make debugging much easier
- **Type-check first**: Catch errors before bundling
- **Test incrementally**: Verify each step (type-check, bundle, embed, run)
- **Document exports**: Make it clear what's available to Go code

## Next Steps

Once you have a working integration, consider:

- Adding development mode with hot reload
- Implementing bidirectional communication (Go → TypeScript and TypeScript → Go)
- Creating helper utilities for common patterns
- Adding comprehensive error handling
- Performance profiling and optimization

