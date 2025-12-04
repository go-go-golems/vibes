---
Title: Bug Report - Query bindings not displaying variable values
Ticket: INTEGRATE-PROLOG-GOJA-REPL
Status: resolved
Topics:
    - go
    - typescript
    - goja
    - prolog
    - repl
    - bug
DocType: analysis
Intent: long-term
Owners: []
RelatedFiles:
    - vibes/2025/12/05/goja-prolog/internal/prolog/evaluator.go
ExternalSources: []
Summary: Query results show unsubstituted variables instead of bound values - FIXED
LastUpdated: 2025-12-04T10:00:00-05:00
---

# Bug Report: Query bindings not displaying variable values

## Summary

When executing a Prolog query with variables, the REPL displays the original query with unsubstituted variables instead of showing the actual bound values.

## Severity

**Medium** - Core functionality works (queries execute correctly), but output is misleading.

## Steps to Reproduce

1. Start the Prolog REPL: `./bin/prolog-repl`
2. Add a fact: `(likes alice bob)`
3. Query with a variable: `?- (likes alice ?x)`

## Expected Behavior

```
Solution 1:
Query: (likes alice bob)
Bindings:
  - ?x = bob
```

Or at minimum:
```
Solution 1:
  - ?x = bob
```

## Actual Behavior

```
Solution 1: (likes alice ?x)
```

The variable `?x` is not substituted with its bound value `bob`.

## Root Cause Analysis

The issue is in `formatBindings()` in `internal/prolog/evaluator.go`. The function:

1. Creates a JavaScript function to extract Map entries
2. Calls it with the bindings object
3. But the result is a **function**, not the result of calling the function with the bindings

**Problem Code** (line ~327-348):
```go
result, err := e.vm.RunString(`
    (function(bindings) {
        // ... extract entries ...
        return entries;
    })
`)
```

This returns the function itself, not the result of calling it. The function needs to be invoked with the bindings object.

**Additionally**, the bindings returned by `prove()` are JavaScript `Map` objects, and the extraction logic may need adjustment.

## Proposed Fix

1. **Option A**: Call the function with the bindings object:
```go
extractorFunc, _ := goja.AssertFunction(result)
entriesResult, _ := extractorFunc(goja.Undefined(), bindingsObj)
entriesArray := entriesResult.ToObject(e.vm)
```

2. **Option B**: Use `substBindings` from prolog-ts to substitute variables in the query term before formatting:
```go
// Export substBindings from app.ts
substBindingsFunc := exports.Get("substBindings")
substitutedTerm, _ := substBindingsFunc(goja.Undefined(), queryValue, bindingsObj)
formattedResult, _ := formatTermFunc(goja.Undefined(), substitutedTerm)
```

Option B is cleaner and matches how the original `prolog-executor-ts.ts` handles formatting.

## Files Affected

- `vibes/2025/12/05/goja-prolog/internal/prolog/evaluator.go` - `formatBindings()` function
- `vibes/2025/12/05/goja-prolog/web/app.ts` - May need to export `substBindings`

## Test Case

After fix, verify:
```
prolog> (likes alice bob)
✓ Added: (likes alice bob)

prolog> (likes bob charlie)
✓ Added: (likes bob charlie)

prolog> ?- (likes ?who ?whom)
Solution 1:
  - ?who = alice
  - ?whom = bob

Solution 2:
  - ?who = bob
  - ?whom = charlie
```

## Related Issues

- Nil pointer panic was fixed in previous commit (added null checks)
- Console integration added for debugging

## Priority

High - This is a core usability issue that makes the REPL hard to use effectively.

## Resolution

**Status**: ✅ FIXED

**Fix Date**: 2025-12-04

**Implementation**: Implemented Option B - using `substBindings` from prolog-ts

**Changes Made**:
1. Added `substBindingsFunc` and `variablesInFunc` to `PrologEvaluator` struct
2. Cached these functions in `NewPrologEvaluator()` from module exports
3. Replaced `formatBindings()` with `formatVariableBindings()` that:
   - Uses `variablesIn()` to get query variables
   - Uses `substBindings()` to substitute each variable with its bound value
   - Formats each binding pair using `formatTerm()`
4. Updated `handleQuery()` to:
   - Substitute bindings in the entire query term using `substBindings()`
   - Format the substituted query (shows `(likes alice bob)` instead of `(likes alice ?x)`)
   - Display individual variable bindings separately

**Code Changes**:
- `internal/prolog/evaluator.go`: 
  - Added `substBindingsFunc` and `variablesInFunc` fields
  - Added function caching in `NewPrologEvaluator()`
  - Replaced `formatBindings()` with `formatVariableBindings()`
  - Updated `handleQuery()` to use substitution approach

**Test Results**:
```
prolog> (likes alice bob)
✓ Added: (likes alice bob)

prolog> ?- (likes alice ?x)
Solution 1: (likes alice bob) Bindings:

  • ?x = bob
```

✅ **Verified**: Query now shows substituted query term AND individual variable bindings.

**Lessons Learned**:
- `substBindings()` is the correct approach - it matches the original TypeScript implementation
- Need to cache all utility functions from module exports
- Formatting should show both the substituted query (for readability) and individual bindings (for clarity)
