---
Title: Go Binary Architecture for Prolog Interpreter
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
    - Path: 2025-06-22/goja-ts-integration/myvm/build/generate.go
      Note: Build pipeline pattern using esbuild to compile TypeScript to CommonJS
    - Path: 2025-06-22/goja-ts-integration/myvm/cmd/server/main.go
      Note: Goja integration pattern showing how to load TypeScript in Go binary
    - Path: 2025-06-22/goja-ts-integration/myvm/web/app.ts
      Note: TypeScript entry point pattern for Goja integration
    - Path: 2025/12/03/prolog-webapp/server/prolog-ts.ts
      Note: TypeScript Prolog interpreter implementation - core logic to be integrated
ExternalSources: []
Summary: Analyzes architecture for integrating TypeScript Prolog interpreter into Go binary using Goja runtime, following proven goja-ts-integration pattern
LastUpdated: 2025-12-03T19:46:23.593011933-05:00
---



# Go Binary Architecture for Prolog Interpreter

## Executive Summary

This design document analyzes how to integrate the TypeScript Prolog interpreter (`prolog-ts.ts`) into a Go binary using Goja. The goal is to create a single, self-contained Go binary that can execute the Prolog interpreter without requiring Node.js or external runtime dependencies. This leverages the existing `goja-ts-integration` pattern which demonstrates TypeScript-to-Go integration using esbuild for bundling and Goja's JavaScript runtime.

**Key Approach:**
- Compile TypeScript Prolog interpreter to CommonJS bundle using esbuild
- Embed the bundled JavaScript in the Go binary using `go:embed`
- Load and execute via Goja runtime with custom module loader
- Expose Go functions to TypeScript for I/O and REPL interaction (future)

## Problem Statement

The Prolog interpreter is currently implemented in TypeScript (`prolog-ts.ts`) and designed to run in a web environment. To create a standalone REPL binary, we need to:

1. **Runtime Independence**: Run TypeScript code without Node.js runtime
2. **Single Binary**: Package everything into one Go executable
3. **Module Loading**: Handle TypeScript imports/exports within Goja
4. **Type Safety**: Maintain TypeScript type-checking during development
5. **Build Pipeline**: Automate TypeScript compilation and embedding

The existing `goja-ts-integration` project provides a proven pattern for this integration, but we need to adapt it specifically for the Prolog interpreter's structure and requirements.

## Proposed Solution

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Go Binary (Single Executable)            │
├─────────────────────────────────────────────────────────────┤
│  ┌──────────────────────────────────────────────────────┐  │
│  │           Goja JavaScript Runtime (VM)                │  │
│  ├──────────────────────────────────────────────────────┤  │
│  │  ┌────────────────────────────────────────────────┐  │  │
│  │  │  Embedded JS Bundle (prolog-ts.js)             │  │  │
│  │  │  - Compiled from prolog-ts.ts via esbuild      │  │  │
│  │  │  - CommonJS format for goja_nodejs             │  │  │
│  │  │  - Contains: PrologDB, parseTerm, unify, etc. │  │  │
│  │  └────────────────────────────────────────────────┘  │  │
│  │                                                       │  │
│  │  ┌────────────────────────────────────────────────┐  │  │
│  │  │  Custom Module Loader                           │  │  │
│  │  │  - Resolves require() calls                     │  │  │
│  │  │  - Loads from embedded FS                      │  │  │
│  │  └────────────────────────────────────────────────┘  │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Go Main Application                                 │  │
│  │  - Initializes Goja VM                              │  │
│  │  - Sets up module loader                             │  │
│  │  - Exposes Go functions (future: REPL I/O)           │  │
│  │  - Executes Prolog interpreter entry point          │  │
│  └──────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### Component Analysis

#### 1. TypeScript Prolog Interpreter (`prolog-ts.ts`)

**Current Structure:**
- **Exports**: `PrologDB` class, `parseTerm`, `parseClause`, `unify`, `formatTerm`, `substBindings`, `variablesIn`
- **Dependencies**: Pure TypeScript, no external npm packages
- **Entry Point**: Module exports, no default execution
- **Types**: Well-defined interfaces (`Term`, `Atom`, `Variable`, `Compound`, `ListTerm`, `Bindings`, `Clause`)

**Key Observations:**
- Self-contained implementation (no external dependencies)
- Uses ES6 classes and Map data structures (compatible with Goja)
- Exports are explicit and well-structured
- No side effects at module load time

#### 2. Goja Integration Pattern (from `goja-ts-integration`)

**Build Pipeline:**
```go
// build/generate.go
//go:generate npx tsc --project ../web/tsconfig.json --noEmit  // Type-check
//go:generate npx esbuild ../web/app.ts --bundle --format=cjs --platform=node --target=es2019 --outfile=../web/app.js
```

**Key Components:**
- **esbuild**: Transpiles TypeScript → JavaScript, bundles into single file
- **CommonJS format**: Required by `goja_nodejs/require` module loader
- **go:embed**: Embeds compiled JS bundle into Go binary
- **Custom Loader**: Intercepts `require()` calls, resolves from embedded FS

**Module Loading Pattern:**
```go
reg := require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
    // Resolve path to embedded bundle
    data, err := jsBundle.ReadFile("assets/prolog-ts.js")
    return data, nil
}))
reg.Enable(vm)  // Enable Node.js polyfills
```

### Adaptation Strategy

#### Step 1: Project Structure

```
prolog-goja-binary/
├── cmd/
│   └── prolog-repl/
│       └── main.go              # Go entry point
├── internal/
│   └── prolog/                  # Future: Go bindings for REPL
├── web/                         # TypeScript source
│   ├── prolog-ts.ts            # Copy of prolog-ts.ts
│   ├── app.ts                  # Entry point that exports PrologDB
│   ├── tsconfig.json
│   └── package.json
├── build/
│   └── generate.go             # Build pipeline
└── assets/                      # Generated (gitignored)
    └── prolog-ts.js            # Compiled bundle
```

#### Step 2: TypeScript Entry Point (`web/app.ts`)

Create a thin wrapper that:
- Imports all exports from `prolog-ts.ts`
- Re-exports them for Go access
- Provides a factory function for creating PrologDB instances

```typescript
// web/app.ts
import {
  PrologDB,
  parseTerm,
  parseClause,
  formatTerm,
  unify,
  substBindings,
  variablesIn,
  type Term,
  type Bindings,
  type Clause
} from './prolog-ts';

// Export everything for Go access
export {
  PrologDB,
  parseTerm,
  parseClause,
  formatTerm,
  unify,
  substBindings,
  variablesIn
};

export type { Term, Bindings, Clause };

// Factory function for creating new database instances
export function createPrologDB(): PrologDB {
  return new PrologDB();
}
```

#### Step 3: Build Configuration

**tsconfig.json:**
```json
{
  "compilerOptions": {
    "target": "ES2019",
    "module": "CommonJS",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "outDir": "./dist",
    "declaration": true
  },
  "include": ["*.ts"],
  "exclude": ["node_modules", "dist"]
}
```

**build/generate.go:**
```go
//go:build generate
package main

// Type-check TypeScript
//go:generate npx tsc --project ../web/tsconfig.json --noEmit

// Bundle TypeScript to CommonJS
//go:generate npx esbuild ../web/app.ts --bundle --format=cjs --platform=node --target=es2019 --outfile=../assets/prolog-ts.js --sourcemap=inline

func main() {}
```

#### Step 4: Go Main Application

**cmd/prolog-repl/main.go:**
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

//go:embed assets/prolog-ts.js
var jsBundle embed.FS

func main() {
    setupLogger()
    
    // Create Goja VM
    vm := goja.New()
    
    // Set up module loader
    reg := require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
        if path == "prolog-ts.js" || path == "app.js" {
            return jsBundle.ReadFile("assets/prolog-ts.js")
        }
        return nil, fmt.Errorf("module not found: %s", path)
    }))
    reg.Enable(vm)
    
    // Set up console
    setupConsole(vm)
    
    // Load and execute the Prolog module
    result, err := vm.RunString(`
        const prolog = require('prolog-ts.js');
        const db = prolog.createPrologDB();
        
        // Test: Add a fact
        const fact = prolog.parseClause('(likes alice bob)');
        db.addClause(fact.head, fact.body);
        
        // Test: Query
        const query = prolog.parseTerm('(likes alice ?x)');
        const solutions = db.prove(query, new Map());
        
        console.log('Solutions:', solutions.length);
        return { success: true, solutions: solutions.length };
    `)
    
    if err != nil {
        log.Fatal().Err(err).Msg("Failed to execute Prolog interpreter")
    }
    
    fmt.Println("Result:", result)
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
    vm.Set("console", console)
}
```

### Key Integration Points

#### 1. Module Export Access

Goja can access JavaScript exports via:
- `vm.Get("prolog")` after `require('prolog-ts.js')`
- Function calls: `prologCreateDB.Call(goja.Undefined())`
- Object property access for classes

**Pattern for accessing PrologDB:**
```go
// Get the module exports
prologModule := require.Require(vm, "prolog-ts.js")
prologObj := prologModule.ToObject(vm)

// Get factory function
createDB := prologObj.Get("createPrologDB")
createDBFunc, ok := goja.AssertFunction(createDB)
if !ok {
    log.Fatal().Msg("createPrologDB is not a function")
}

// Create instance
dbValue, err := createDBFunc(goja.Undefined())
if err != nil {
    log.Fatal().Err(err).Msg("Failed to create PrologDB")
}
```

#### 2. Data Structure Mapping

**TypeScript → Goja:**
- `Map<string, Term>` (Bindings) → Goja Object
- `Term` interfaces → Goja Objects with type properties
- Arrays → Goja Arrays

**Go → TypeScript:**
- Go structs → JavaScript objects (via `vm.ToValue()`)
- Go maps → JavaScript objects
- Go slices → JavaScript arrays

#### 3. Error Handling

- TypeScript errors → Goja panics → Recover in Go
- Prolog unification failures → Return empty solutions array
- Parse errors → Throw JavaScript Error → Catch in Go

## Design Decisions

### 1. CommonJS Bundle Format
**Decision**: Use esbuild with `--format=cjs` and `--platform=node`
**Rationale**: 
- Required by `goja_nodejs/require` module loader
- Single-file bundle simplifies embedding
- No external module resolution needed

### 2. Thin Entry Point Wrapper
**Decision**: Create `app.ts` that imports and re-exports from `prolog-ts.ts`
**Rationale**:
- Keeps `prolog-ts.ts` unchanged (can be used elsewhere)
- Provides clean export interface for Go
- Allows adding Go-specific bindings later

### 3. Factory Function Pattern
**Decision**: Export `createPrologDB()` factory function
**Rationale**:
- Goja can't directly instantiate ES6 classes via `new`
- Factory function provides clean API
- Allows future initialization parameters

### 4. Embedded Bundle Location
**Decision**: Store compiled JS in `assets/` directory, embed via `go:embed`
**Rationale**:
- Standard Go pattern for embedded resources
- Path resolution is straightforward
- Build-time embedding ensures single binary

### 5. Development vs Production Modes
**Decision**: Start with production-only mode (embedded bundle)
**Rationale**:
- Simpler initial implementation
- REPL focus doesn't need hot-reload
- Can add dev mode later if needed

## Alternatives Considered

### 1. Direct JavaScript Execution (No Bundle)
**Rejected**: Would require handling TypeScript imports manually
**Reason**: More complex module resolution, defeats purpose of bundling

### 2. WebAssembly Compilation
**Rejected**: Overkill for this use case, adds complexity
**Reason**: Goja provides sufficient performance, simpler integration

### 3. Separate Node.js Process
**Rejected**: Defeats goal of single binary
**Reason**: Requires Node.js runtime, adds process management overhead

### 4. Rewrite in Go
**Rejected**: Loses TypeScript implementation, significant effort
**Reason**: Existing TypeScript implementation is complete and tested

## Implementation Plan

### Phase 1: Basic Integration (Current Focus)
- [ ] Set up project structure
- [ ] Copy `prolog-ts.ts` to `web/` directory
- [ ] Create `web/app.ts` entry point wrapper
- [ ] Configure `tsconfig.json` and `package.json`
- [ ] Create `build/generate.go` with esbuild pipeline
- [ ] Implement `cmd/prolog-repl/main.go` with basic VM setup
- [ ] Test: Load module and create PrologDB instance
- [ ] Test: Add fact and query (basic functionality)

### Phase 2: Go Bindings (Future)
- [ ] Expose Go functions for REPL I/O (readline, print)
- [ ] Implement command-line argument parsing
- [ ] Add file loading capability
- [ ] Error handling and reporting

### Phase 3: REPL Implementation (Future)
- [ ] Interactive loop with readline
- [ ] Command parsing (facts, rules, queries)
- [ ] Result formatting and display
- [ ] History and completion

## Open Questions

1. **Performance**: How does Goja performance compare to native Go for Prolog execution?
   - *Investigation needed*: Benchmark simple queries

2. **Memory Management**: How to handle large Prolog databases in Goja?
   - *Consideration*: Goja GC vs Go GC interaction

3. **Error Reporting**: How to map TypeScript stack traces to source locations?
   - *Solution*: Use inline sourcemaps from esbuild

4. **Type Safety**: Should we generate Go types from TypeScript interfaces?
   - *Future*: Consider tygo for bidirectional type generation

5. **Module Splitting**: Should we split PrologDB and parsing into separate modules?
   - *Decision*: Start monolithic, split if needed

## References

- **Prolog Implementation**: `vibes/2025/12/03/prolog-webapp/server/prolog-ts.ts`
- **Goja Integration Pattern**: `vibes/2025-06-22/goja-ts-integration/`
- **Goja Documentation**: https://github.com/dop251/goja
- **esbuild Documentation**: https://esbuild.github.io/
- **goja_nodejs**: https://github.com/dop251/goja_nodejs
