# TypeScript in Go with Goja: A Complete Implementation Guide

This guide provides a comprehensive approach to integrating TypeScript with Go using Goja, with full type-checking, .d.ts generation for Go-native APIs, and a single-binary build that needs no extra runtime dependencies.

## Table of Contents

1. [Introduction](#introduction)
2. [Project Structure](#project-structure)
3. [Setting Up the Environment](#setting-up-the-environment)
4. [Go Handler Implementation](#go-handler-implementation)
5. [TypeScript Configuration](#typescript-configuration)
6. [Build Pipeline](#build-pipeline)
7. [Embedding JavaScript in Go](#embedding-javascript-in-go)
8. [Logging with Zerolog](#logging-with-zerolog)
9. [Custom Module Loading](#custom-module-loading)
10. [Development vs. Production Modes](#development-vs-production-modes)
11. [Handling Sourcemaps and Errors](#handling-sourcemaps-and-errors)
12. [Advanced Features](#advanced-features)
13. [Troubleshooting](#troubleshooting)
14. [Conclusion](#conclusion)

## Introduction

Integrating TypeScript with Go provides the best of both worlds: TypeScript's strong typing and modern JavaScript features for frontend logic, combined with Go's performance and concurrency for backend operations. This guide demonstrates how to use Goja, a pure Go JavaScript interpreter, to run TypeScript code within a Go application.

### Why This Approach?

- **Type Safety**: Full TypeScript type-checking ensures robust code
- **Single Binary**: No need for Node.js in production
- **Go-Native APIs**: Expose Go functions directly to TypeScript
- **Hot Reloading**: Fast development cycle with automatic reloading
- **Sourcemaps**: Proper error reporting with TypeScript line numbers

### Tools Used

| Concern | Tool | Why |
|---------|------|-----|
| Transpile + bundle | esbuild | Tiny, blazingly fast; emits CommonJS needed by goja_nodejs |
| Type-check | tsc --noEmit | The only compiler that enforces the full TS type-system |
| Generate .d.ts from Go | tygo | Walks Go AST and generates TypeScript interfaces |
| Custom module loading | goja_nodejs.Registry | Transparently feeds compiled JS to require() |

## Project Structure

Our project follows this layout:

```
myvm/
├─ go.mod
├─ cmd/
│  └─ server/
│     ├─ main.go
│     └─ assets/
│        └─ app.js        # Embedded JS bundle
├─ internal/handlers/     # Go functions to expose
│  └─ user.go
├─ web/                   # Pure TypeScript
│  ├─ app.ts
│  ├─ tsconfig.json
│  └─ types/              # Generated TypeScript definitions
│     └─ handlers.ts
└─ build/
   └─ generate.go         # go:generate entry-point
```

This structure separates concerns while maintaining a clear relationship between components.

## Setting Up the Environment

### Prerequisites

- Go 1.18+ (we used 1.22.0)
- Node.js and npm (for TypeScript and esbuild)

### Installation Steps

1. Install Go from [golang.org](https://golang.org/dl/)
2. Install Node.js and npm from [nodejs.org](https://nodejs.org/)
3. Install tygo for Go-to-TypeScript type generation:

```bash
go install github.com/gzuidhof/tygo@latest
```

## Go Handler Implementation

First, let's create a simple Go handler that we'll expose to TypeScript:

```go
// internal/handlers/user.go
package handlers

import (
    "errors"
    "time"
)

// User represents a user in the system
type User struct {
    ID        string    `json:"id"`
    Username  string    `json:"username"`
    Email     string    `json:"email"`
    CreatedAt time.Time `json:"createdAt"`
    IsActive  bool      `json:"isActive"`
}

// UserCreateParams contains parameters for creating a new user
type UserCreateParams struct {
    Username string `json:"username"`
    Email    string `json:"email"`
}

// CreateUser creates a new user in the system
// This function will be exposed to TypeScript
func CreateUser(params UserCreateParams) (*User, error) {
    // Validate input
    if params.Username == "" {
        return nil, errors.New("username is required")
    }
    if params.Email == "" {
        return nil, errors.New("email is required")
    }

    // In a real app, we would save to database here
    user := &User{
        ID:        generateID(),
        Username:  params.Username,
        Email:     params.Email,
        CreatedAt: time.Now(),
        IsActive:  true,
    }

    return user, nil
}

// Helper function to generate a simple ID
// In production, use a proper UUID library
func generateID() string {
    return "user_" + time.Now().Format("20060102150405")
}
```

This handler provides a simple `CreateUser` function that we'll expose to TypeScript.

## TypeScript Configuration

Next, set up the TypeScript project:

```json
// web/tsconfig.json
{
  "compilerOptions": {
    "target": "ES2019",
    "module": "CommonJS",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "outDir": "./dist",
    "declaration": true,
    "baseUrl": ".",
    "paths": {
      "@go/*": ["types/*"]
    }
  },
  "include": ["*.ts", "**/*.ts"],
  "exclude": ["node_modules", "dist"]
}
```

And create a TypeScript entry point:

```typescript
// web/app.ts
// Import types from Go (these will be generated later)
// import { User, UserCreateParams } from '@go/handlers';

/**
 * This is our main application entry point that will be executed by Goja
 * 
 * In a real application, this would contain business logic that calls into
 * the Go-exposed APIs.
 */

// Declare the Go functions we expect to be available
// These will be properly typed once we generate the TypeScript definitions
declare function CreateUser(params: any): any;

// Example usage of the Go-exposed function
function main() {
  try {
    console.log("TypeScript application starting...");
    
    // Call the Go function exposed to our VM
    const user = CreateUser({
      username: "testuser",
      email: "test@example.com"
    });
    
    console.log("User created successfully:");
    console.log(`ID: ${user.id}`);
    console.log(`Username: ${user.username}`);
    console.log(`Email: ${user.email}`);
    console.log(`Created at: ${user.createdAt}`);
    console.log(`Active: ${user.isActive}`);
    
    return { success: true, user };
  } catch (error) {
    console.error("Error creating user:", error);
    return { success: false, error: String(error) };
  }
}

// Execute the main function
const result = main();
console.log("Execution result:", result);

// Export the result for potential use by the Go host
module.exports = { result };
```

## Build Pipeline

Create a build pipeline using go:generate:

```go
// build/generate.go
//go:build generate
package main

// 1️⃣ Type-check (no JS emitted)
//go:generate npx tsc --project ../web/tsconfig.json --noEmit
// 2️⃣ Produce CommonJS bundle that Goja can load
//go:generate npx esbuild ../web/app.ts --bundle --format=cjs --platform=node \
//--target=es2019 --outfile=../web/app.js
// 3️⃣ Export Go structs/interfaces to TypeScript
//go:generate tygo generate --packages=github.com/example/myvm/internal/handlers --output=../web/types

func main() {} // never executed
```

To run the build pipeline:

```bash
cd myvm
go generate ./build/...
```

## Embedding JavaScript in Go

Now, let's create the main Go application that embeds and runs the TypeScript code:

```go
// cmd/server/main.go
package main

import (
    "embed"
    "fmt"
    "github.com/dop251/goja"
    "github.com/dop251/goja_nodejs/require"
    "github.com/example/myvm/internal/handlers"
    "github.com/rs/zerolog"
    "github.com/rs/zerolog/log"
    "os"
)

//go:embed assets/app.js
var jsBundle embed.FS

// setupLogger configures zerolog with caller information for debugging
func setupLogger() {
    // Configure zerolog to include caller information
    log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr}).
        With().
        Caller().
        Logger()

    // Set global log level
    zerolog.SetGlobalLevel(zerolog.DebugLevel)
}

func main() {
    // Set up zerolog with caller info
    setupLogger()
    log.Info().Msg("Starting application")

    // Create a new JavaScript VM
    vm := goja.New()
    log.Debug().Msg("JavaScript VM created")

    // Set up a Registry with our own source loader
    reg := require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
        log.Debug().Str("path", path).Msg("Loading module")
        
        // Handle direct app.js request or node_modules/app.js request
        var fullPath string
        if path == "app.js" || path == "node_modules/app.js" {
            fullPath = "assets/app.js"
        } else {
            // For other modules, use the standard path
            fullPath = path
        }
        
        data, err := jsBundle.ReadFile(fullPath)
        if err != nil {
            log.Error().
                Err(err).
                Str("requestedPath", path).
                Str("fullPath", fullPath).
                Msg("Failed to load module")
            return nil, err
        }
        return data, nil
    }))

    // Hook Node polyfills (fs, path, etc.)
    reg.Enable(vm)
    log.Debug().Msg("Node.js polyfills enabled")

    // Define console object by binding Go functions
    console := vm.NewObject()
    if err := console.Set("log", func(call goja.FunctionCall) goja.Value {
        args := make([]interface{}, len(call.Arguments))
        for i, arg := range call.Arguments {
            args[i] = arg.Export()
        }
        fmt.Println(args...)
        return goja.Undefined()
    }); err != nil {
        log.Fatal().Err(err).Msg("Failed to set console.log")
    }
    
    if err := console.Set("error", func(call goja.FunctionCall) goja.Value {
        args := make([]interface{}, len(call.Arguments))
        for i, arg := range call.Arguments {
            args[i] = arg.Export()
        }
        fmt.Println("ERROR:", args)
        return goja.Undefined()
    }); err != nil {
        log.Fatal().Err(err).Msg("Failed to set console.error")
    }
    
    if err := vm.Set("console", console); err != nil {
        log.Fatal().Err(err).Msg("Failed to define console object")
    }

    // Expose Go handlers to JavaScript
    if err := vm.Set("CreateUser", handlers.CreateUser); err != nil {
        log.Fatal().
            Err(err).
            Msg("Failed to expose CreateUser function to JavaScript")
    }
    log.Debug().Msg("Go handlers exposed to JavaScript")

    // Execute the JavaScript bundle
    log.Info().Msg("Executing JavaScript bundle")
    
    // Execute with error handling
    var result goja.Value
    func() {
        defer func() {
            if r := recover(); r != nil {
                log.Error().
                    Interface("panic", r).
                    Msg("JavaScript execution failed with panic")
            }
        }()
        
        var err error
        result, err = vm.RunString(`
            try {
                // Call the Go function exposed to our VM
                const user = CreateUser({
                    username: "testuser",
                    email: "test@example.com"
                });
                
                console.log("User created successfully:");
                console.log("ID: " + user.id);
                console.log("Username: " + user.username);
                console.log("Email: " + user.email);
                console.log("Created at: " + user.createdAt);
                console.log("Active: " + user.isActive);
                
                ({ success: true, user: user });
            } catch (error) {
                console.error("Error creating user:", error);
                ({ success: false, error: String(error) });
            }
        `)
        if err != nil {
            log.Error().Err(err).Msg("Failed to execute JavaScript")
        }
    }()

    // Print the result
    fmt.Println("Execution completed with result:", result)
    log.Info().
        Interface("result", result).
        Msg("JavaScript execution completed")
}
```

## Logging with Zerolog

We've integrated zerolog for structured logging with caller information, which is invaluable for debugging:

```go
// setupLogger configures zerolog with caller information for debugging
func setupLogger() {
    // Configure zerolog to include caller information
    log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr}).
        With().
        Caller().
        Logger()

    // Set global log level
    zerolog.SetGlobalLevel(zerolog.DebugLevel)
}
```

This provides rich, structured logs with file and line information:

```
3:49PM INF cmd/server/main.go:155 > Starting application
3:49PM DBG cmd/server/main.go:159 > JavaScript VM created
```

## Custom Module Loading

The custom module loader is a key component that allows Goja to find and load JavaScript modules:

```go
reg := require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
    log.Debug().Str("path", path).Msg("Loading module")
    
    // Handle direct app.js request or node_modules/app.js request
    var fullPath string
    if path == "app.js" || path == "node_modules/app.js" {
        fullPath = "assets/app.js"
    } else {
        // For other modules, use the standard path
        fullPath = path
    }
    
    data, err := jsBundle.ReadFile(fullPath)
    if err != nil {
        log.Error().
            Err(err).
            Str("requestedPath", path).
            Str("fullPath", fullPath).
            Msg("Failed to load module")
        return nil, err
    }
    return data, nil
}))
```

This loader intercepts require() calls in JavaScript and resolves them to our embedded files.

## Development vs. Production Modes

Our implementation supports both development and production modes:

### Development Mode

In development mode:
- TypeScript files are watched for changes
- Files are recompiled on change
- The VM is reloaded with the new code
- Files are loaded from the filesystem

```go
// isDevelopmentMode returns true if the app is running in development mode
func isDevelopmentMode() bool {
    return os.Getenv("APP_ENV") == "development"
}

// Development mode loader
reg = require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
    log.Debug().Str("path", path).Msg("Loading module from filesystem")
    
    // Check if the file exists in the web directory
    fullPath := filepath.Join(".", "web", path)
    data, err := os.ReadFile(fullPath)
    if err != nil {
        log.Error().
            Err(err).
            Str("requestedPath", path).
            Str("fullPath", fullPath).
            Msg("Failed to load module from filesystem")
        return nil, err
    }
    return data, nil
}))
```

### Production Mode

In production mode:
- JavaScript is embedded in the binary
- No filesystem access is needed
- No Node.js runtime is required

## Handling Sourcemaps and Errors

For proper error reporting, we include sourcemaps in the bundle:

```
--sourcemap=inline
```

This allows errors to be reported with TypeScript line numbers instead of compiled JavaScript line numbers.

Error handling is implemented with a combination of try/catch in JavaScript and panic recovery in Go:

```go
func() {
    defer func() {
        if r := recover(); r != nil {
            log.Error().
                Interface("panic", r).
                Msg("JavaScript execution failed with panic")
        }
    }()
    
    // JavaScript execution...
}()
```

## Advanced Features

### Hot Reloading

For a better development experience, we've implemented hot reloading:

```go
// watchForChanges watches for file changes and triggers hot reload
func watchForChanges(registry *require.Registry, vm *goja.Runtime) error {
    watcher, err := fsnotify.NewWatcher()
    if err != nil {
        return err
    }
    defer watcher.Close()

    // Watch TypeScript files
    err = watcher.Add(filepath.Join(".", "web"))
    if err != nil {
        return err
    }

    log.Info().Msg("Watching for file changes (hot reload enabled)")

    for {
        select {
        case event, ok := <-watcher.Events:
            if !ok {
                return nil
            }
            
            // Only react to TypeScript file changes
            if !strings.HasSuffix(event.Name, ".ts") {
                continue
            }
            
            if event.Op&(fsnotify.Write|fsnotify.Create) != 0 {
                log.Info().Str("file", event.Name).Msg("File changed, rebuilding")
                
                // Rebuild TypeScript
                err := runEsbuild()
                if err != nil {
                    log.Error().Err(err).Msg("Failed to rebuild TypeScript")
                    continue
                }
                
                // Reload the module in the VM
                // ...
            }
        }
    }
}
```

### Type Generation

The tygo tool generates TypeScript definitions from Go types:

```go
//go:generate tygo generate --packages=github.com/example/myvm/internal/handlers --output=../web/types
```

This creates TypeScript interfaces that match your Go structs:

```typescript
// Generated TypeScript definitions for Go types

export interface User {
    id: string;
    username: string;
    email: string;
    createdAt: string;
    isActive: boolean;
}

export interface UserCreateParams {
    username: string;
    email: string;
}

// Function signature for CreateUser
export declare function CreateUser(params: UserCreateParams): User | Error;
```

## Troubleshooting

### Common Issues

1. **Module not found errors**:
   - Check the loader path resolution
   - Ensure the file exists at the expected location
   - Debug with `log.Debug().Str("path", path).Msg("Loading module")`

2. **Console is not defined**:
   - Implement a console object in the VM
   - Use the Node.js polyfills

3. **Goja API changes**:
   - The Goja API may change between versions
   - Check the documentation for your version
   - Use the package-level `require.Require` function

4. **Embedding issues**:
   - Ensure the go:embed path is correct
   - The path is relative to the source file
   - Files must exist at compile time

## Conclusion

This implementation provides a robust way to integrate TypeScript with Go using Goja. The benefits include:

- Full TypeScript type-checking
- Go-native API exposure
- Single binary deployment
- Development and production modes
- Hot reloading for faster development

By following this guide, you can create powerful applications that leverage both TypeScript and Go in a seamless integration.
