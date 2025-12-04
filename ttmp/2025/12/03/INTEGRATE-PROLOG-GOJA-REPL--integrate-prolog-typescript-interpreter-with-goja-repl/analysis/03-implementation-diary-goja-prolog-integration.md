---
Title: Implementation Diary - Goja Prolog Integration
Ticket: INTEGRATE-PROLOG-GOJA-REPL
Status: active
Topics:
    - go
    - typescript
    - goja
    - prolog
    - repl
DocType: analysis
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2025/12/05/goja-prolog
      Note: Implementation project directory
ExternalSources: []
Summary: Diary of implementation process, learnings, and next steps
LastUpdated: 2025-12-04T09:37:00-05:00
---


# Implementation Diary - Goja Prolog Integration

## 2025-12-04 - Initial Implementation

### What I Did

1. **Created project structure** (`2025/12/05/goja-prolog/`)
   - Set up Go module: `github.com/wesen/goja-prolog`
   - Created directories: `cmd/prolog-repl/`, `internal/prolog/`, `web/`, `build/`, `assets/`
   - Added to workspace: `go work use ./vibes/2025/12/05/goja-prolog`

2. **Copied Prolog TypeScript implementation**
   - Copied `prolog-ts.ts` from `2025/12/03/prolog-webapp/server/` to `web/prolog-ts.ts`
   - Created `web/app.ts` wrapper that imports and re-exports all Prolog functions
   - Added factory function `createPrologDB()` for creating instances

3. **Set up TypeScript configuration**
   - Created `web/tsconfig.json` with CommonJS module format (required by goja_nodejs)
   - Created `web/package.json` with esbuild and TypeScript dev dependencies
   - Installed npm dependencies

4. **Created build pipeline** (`build/generate.go`)
   - Type-check step: `tsc --noEmit`
   - Bundle step: `esbuild` with CommonJS format
   - Copy step: Copy bundle to embeddable location (`cmd/prolog-repl/assets/`)

5. **Implemented Go main application** (`cmd/prolog-repl/main.go`)
   - Set up Goja VM with custom module loader
   - Embedded JavaScript bundle using `go:embed`
   - Loaded Prolog module and accessed exports
   - Created PrologDB instance using factory function
   - Tested: Added fact `(likes alice bob)` and queried `(likes alice ?x)`

### What Worked

✅ **Module loading**: The custom loader successfully resolves `require('prolog-ts.js')` from embedded bundle  
✅ **Factory function pattern**: `createPrologDB()` works perfectly for creating instances from Go  
✅ **Function access**: Successfully accessed `parseClause`, `parseTerm`, `formatTerm`, `addClause`, `prove` methods  
✅ **Basic Prolog functionality**: Added fact and queried successfully - got 1 solution  
✅ **Build pipeline**: esbuild creates clean CommonJS bundle (41.2kb)  
✅ **Embedding**: `go:embed` works when files are in subdirectory relative to source file  

### What Didn't Work / Challenges

❌ **go:embed path restrictions**: 
   - Initially tried `../../assets/prolog-ts.js` - Go doesn't allow `../` in embed paths
   - **Solution**: Copy bundle to `cmd/prolog-repl/assets/` directory (same level as main.go)
   - **Learning**: `go:embed` requires files to be in subdirectories relative to the source file, not parent directories

❌ **go:generate with workspace modules**:
   - `go generate ./build/...` failed: "directory prefix build does not contain modules"
   - **Workaround**: Run commands directly (`npx tsc`, `npx esbuild`)
   - **Future**: Need to investigate proper go:generate setup for workspace modules

❌ **Map creation in Goja**:
   - Initially tried complex Map constructor approach
   - **Solution**: Simple `vm.RunString("new Map()")` works perfectly
   - **Learning**: Goja can execute JavaScript directly, no need for complex Go wrappers

### What I Learned

1. **go:embed constraints**: 
   - Must embed files from subdirectories relative to source file
   - Cannot use `../` paths
   - Common pattern: Copy build artifacts to `cmd/<binary>/assets/` for embedding

2. **Goja module loading**:
   - `goja_nodejs/require` provides CommonJS `require()` support
   - Custom loader function intercepts `require()` calls
   - Path resolution can be customized (e.g., `node_modules/prolog-ts.js` → `assets/prolog-ts.js`)

3. **TypeScript → Goja integration**:
   - esbuild bundles TypeScript to single CommonJS file
   - Factory functions are cleaner than trying to instantiate ES6 classes directly
   - Function calls work naturally: `goja.AssertFunction()` + `func.Call()`

4. **Build pipeline pattern**:
   - Type-check first (`tsc --noEmit`)
   - Bundle second (`esbuild`)
   - Copy to embeddable location third
   - Embed in Go binary fourth

### Current Status

✅ **Working**: Basic Prolog interpreter runs successfully in Go binary  
✅ **Tested**: Can add facts and query them  
⏳ **Next**: Need to test more complex queries, rules, and variable bindings  

### What to Do Next Time

1. **Fix go:generate**:
   - Investigate why `go generate ./build/...` doesn't work with workspace modules
   - Consider using Makefile or shell script for build pipeline
   - Or create a separate build tool that can be run directly

2. **Improve error handling**:
   - Better error messages when module loading fails
   - Validate that required functions exist before calling
   - Handle JavaScript errors more gracefully

3. **Test more Prolog features**:
   - Rules (not just facts)
   - Multiple solutions
   - Variable bindings in solutions
   - List operations
   - Complex queries

4. **Add REPL functionality**:
   - Read-eval-print loop
   - Command parsing
   - Result formatting
   - History

5. **Performance testing**:
   - Benchmark Goja vs native Go
   - Test with larger databases
   - Memory profiling

6. **Documentation**:
   - Add README with build instructions
   - Document the architecture
   - Add examples

### Build Commands Reference

**Manual build:**
```bash
# Type-check
cd web && npx tsc --project tsconfig.json --noEmit

# Bundle
cd web && npx esbuild app.ts --bundle --format=cjs --platform=node --target=es2019 --outfile=../assets/prolog-ts.js --sourcemap=inline

# Copy to embeddable location
cp assets/prolog-ts.js cmd/prolog-repl/assets/prolog-ts.js

# Build Go binary
go build -o bin/prolog-repl ./cmd/prolog-repl

# Run
./bin/prolog-repl
```

**Using go:generate (from build/ directory):**
```bash
cd build && go generate generate.go
```

Note: The `go:generate` directives use shell commands for the copy step since `go run` on the same file creates circular dependency issues.

### Files Created/Modified

- `2025/12/05/goja-prolog/go.mod` - Go module definition
- `2025/12/05/goja-prolog/web/prolog-ts.ts` - Copied Prolog implementation
- `2025/12/05/goja-prolog/web/app.ts` - Entry point wrapper
- `2025/12/05/goja-prolog/web/tsconfig.json` - TypeScript config
- `2025/12/05/goja-prolog/web/package.json` - npm dependencies
- `2025/12/05/goja-prolog/build/generate.go` - Build pipeline
- `2025/12/05/goja-prolog/cmd/prolog-repl/main.go` - Main application
- `2025/12/05/goja-prolog/.gitignore` - Ignore build artifacts
