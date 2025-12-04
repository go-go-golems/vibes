---
Title: Prolog Implementation Details
Slug: prolog-implementation
Short: Detailed explanation of the Prolog interpreter implementation, architecture, and design decisions
Topics:
- prolog
- implementation
- architecture
- goja
- typescript
IsTemplate: false
IsTopLevel: true
ShowPerDefault: true
SectionType: GeneralTopic
---

# Prolog Implementation Details

The Prolog REPL is built using a TypeScript Prolog interpreter that runs inside a Go binary via Goja. This architecture provides the benefits of TypeScript's type safety and modern JavaScript features while delivering a self-contained Go application. The core interpreter logic is based on the Prolog implementation from PAIP (Paradigms of Artificial Intelligence Programming) Chapter 11, specifically the `prolog1.lisp` example. This section explains the implementation architecture, key algorithms, and design decisions that make the interpreter work.

## Architecture Overview

The implementation consists of three main layers:

1. **TypeScript Interpreter** (`web/prolog-ts.ts`): Core Prolog logic written in TypeScript
2. **Goja Runtime** (`internal/prolog/evaluator.go`): Go wrapper that executes TypeScript code
3. **REPL Interface** (`cmd/prolog-repl/main.go`): Terminal UI using bobatea framework

**Data Flow:**

```
User Input → REPL → Evaluator → Goja VM → TypeScript Interpreter → Results → Events → REPL → Display
```

**Key Components:**

| Component | Location | Purpose |
|-----------|----------|---------|
| `PrologDB` | `web/prolog-ts.ts` | Knowledge base storing facts and rules |
| `unify()` | `web/prolog-ts.ts` | Unification algorithm for pattern matching |
| `prove()` | `web/prolog-ts.ts` | Backtracking search for solutions |
| `PrologEvaluator` | `internal/prolog/evaluator.go` | Goja integration and REPL interface |
| `bobatea REPL` | `cmd/prolog-repl/main.go` | Terminal UI and event handling |

## Core Algorithms

### Unification

Unification is the process of making two terms identical by finding variable bindings. It's the fundamental operation that makes Prolog's pattern matching work.

**Algorithm** (from `unify()` function):

```typescript
function unify(x: Term, y: Term, bindings: Bindings): Bindings | Fail {
  // Dereference variables to their bound values
  x = deref(x, bindings);
  y = deref(y, bindings);
  
  // Same term - unification succeeds
  if (termsEqual(x, y)) return bindings;
  
  // Variable unification - bind variable to value
  if (x.type === 'variable') return bindVariable(x.name, y, bindings);
  if (y.type === 'variable') return bindVariable(y.name, x, bindings);
  
  // Compound term unification - unify functor and arguments
  if (x.type === 'compound' && y.type === 'compound') {
    if (x.functor !== y.functor || x.args.length !== y.args.length) {
      return FAIL;
    }
    // Recursively unify each argument
    let newBindings = bindings;
    for (let i = 0; i < x.args.length; i++) {
      newBindings = unify(x.args[i], y.args[i], newBindings);
      if (newBindings === FAIL) return FAIL;
    }
    return newBindings;
  }
  
  return FAIL;
}
```

**Key features:**

- **Dereferencing**: Variables are dereferenced to their bound values before comparison
- **Occur check**: Prevents infinite structures (e.g., `?x = (?x)`)
- **Recursive**: Handles nested compound terms and lists
- **Immutable**: Returns new bindings map, doesn't modify input

**Example:**

Unifying `(likes alice ?x)` with `(likes alice bob)`:
1. Functors match: `likes` = `likes` ✅
2. First args match: `alice` = `alice` ✅
3. Second args: `?x` (variable) unified with `bob` → bind `?x = bob`
4. Result: `{?x: bob}`

### Backtracking Search

The `prove()` function implements backtracking search to find all solutions to a query.

**Algorithm** (from `prove()` method):

```typescript
prove(goal: Term, bindings: Bindings): Bindings[] {
  const predicate = this.getPredicate(goal);
  const clauses = this.getClauses(predicate);
  const solutions: Bindings[] = [];
  
  for (const clause of clauses) {
    // Rename variables to avoid conflicts
    const renamedClause = this.renameVariables(clause);
    
    // Try to unify goal with clause head
    const newBindings = unify(goal, renamedClause.head, bindings);
    
    if (newBindings !== FAIL) {
      // If unification succeeds, prove all goals in body
      const bodySolutions = this.proveAll(renamedClause.body, newBindings);
      solutions.push(...bodySolutions);
    }
  }
  
  return solutions;
}
```

**Key features:**

- **Clause selection**: Tries each clause for the predicate in order
- **Variable renaming**: Renames variables in clauses to avoid conflicts
- **Body evaluation**: If head unifies, recursively proves body goals
- **Multiple solutions**: Collects all successful bindings

**Example:**

Query: `?- (likes alice ?x)`

Facts:
- `(likes alice bob)`
- `(likes alice charlie)`

Process:
1. Try first clause: `(likes alice bob)`
   - Unify `(likes alice ?x)` with `(likes alice bob)` → `{?x: bob}` ✅
   - Body is empty (fact) → solution found
2. Try second clause: `(likes alice charlie)`
   - Unify `(likes alice ?x)` with `(likes alice charlie)` → `{?x: charlie}` ✅
   - Body is empty → solution found
3. Result: `[{?x: bob}, {?x: charlie}]`

### Variable Renaming

Variables in clauses are renamed to avoid conflicts between different uses of the same variable name.

**Why it's needed:**

Without renaming, this rule would fail incorrectly:

```
(grandparent ?gp ?gc) :- (parent ?gp ?p) (parent ?p ?gc)
```

If we query `?- (grandparent alice ?gc)` and `?p` in the rule conflicts with `?p` from another part of the query, unification fails.

**Algorithm** (from `renameVariables()` method):

```typescript
private renameVariables(clause: Clause): Clause {
  const vars = this.variablesIn([clause.head, ...clause.body]);
  const renaming = new Map<string, string>();
  
  // Generate unique names: ?_0, ?_1, ?_2, ...
  for (const varName of Array.from(vars)) {
    renaming.set(varName, `?_${this.varCounter++}`);
  }
  
  // Apply renaming to head and body
  return {
    head: this.renameInTerm(clause.head, renaming),
    body: clause.body.map(goal => this.renameInTerm(goal, renaming)),
  };
}
```

**Example:**

Original clause: `(grandparent ?gp ?gc) :- (parent ?gp ?p) (parent ?p ?gc)`

After renaming: `(grandparent ?_0 ?_1) :- (parent ?_0 ?_2) (parent ?_2 ?_1)`

Now `?_0`, `?_1`, `?_2` are unique and won't conflict with query variables.

## Data Structures

### Term Types

Prolog terms are represented as TypeScript discriminated unions:

```typescript
type Term = Atom | Variable | Compound | ListTerm;

interface Atom {
  type: 'atom';
  name: string;
}

interface Variable {
  type: 'variable';
  name: string;
}

interface Compound {
  type: 'compound';
  functor: string;
  args: Term[];
}

interface ListTerm {
  type: 'list';
  elements: Term[];
}
```

**Design decisions:**

- **Discriminated union**: `type` field enables type-safe pattern matching
- **Immutable**: Terms are never modified, new terms created for transformations
- **Recursive**: `args` and `elements` contain `Term[]`, enabling nested structures

### Bindings

Variable bindings are stored in a `Map<string, Term>`:

```typescript
type Bindings = Map<string, Term>;
```

**Why Map instead of object:**

- **Type safety**: Map preserves `Term` type, object would require `any`
- **Performance**: Map operations are O(1) average case
- **Clarity**: Explicit key-value relationship

**Example bindings:**

```typescript
const bindings = new Map([
  ['?x', { type: 'atom', name: 'bob' }],
  ['?y', { type: 'atom', name: 'charlie' }]
]);
```

### Knowledge Base

The `PrologDB` class stores facts and rules:

```typescript
class PrologDB {
  private predicates: Map<string, Clause[]> = new Map();
  private varCounter = 0;
}
```

**Structure:**

- **`predicates`**: Maps predicate name → list of clauses
  - Example: `'likes'` → `[{head: ..., body: []}, {head: ..., body: []}]`
- **`varCounter`**: Counter for generating unique variable names during renaming

**Why this structure:**

- **Fast lookup**: O(1) access to clauses for a predicate
- **Multiple clauses**: Supports multiple facts/rules with same predicate
- **Efficient**: Only stores clauses, not all possible queries

## Parsing

The parser converts string input into `Term` data structures.

**Tokenization** (from `tokenize()` function):

```typescript
function tokenize(input: string): string[] {
  // Add spaces around parentheses and :- for easier tokenization
  const spaced = input
    .replace(/\(/g, ' ( ')
    .replace(/\)/g, ' ) ')
    .replace(/:-/g, ' :- ');
  
  return spaced.trim().split(/\s+/).filter(t => t.length > 0);
}
```

**Parsing** (from `parseTokens()` function):

Recursive descent parser that handles:
- Atoms: `alice`, `bob123`
- Variables: `?x`, `?Var`
- Compounds: `(likes alice bob)`
- Lists: `(a b c)`
- Rules: `(head) :- (body1) (body2)`

**Example parse tree:**

Input: `(likes alice bob)`

```
Compound {
  functor: 'likes',
  args: [
    Atom { name: 'alice' },
    Atom { name: 'bob' }
  ]
}
```

## Goja Integration

The TypeScript interpreter runs inside Go via Goja runtime.

**Module Loading** (from `NewPrologEvaluator()`):

```go
reg := require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
    var fullPath string
    if path == "prolog-ts.js" {
        fullPath = "assets/prolog-ts.js"
    }
    return jsBundle.ReadFile(fullPath)
}))
reg.Enable(vm)
```

**Function Caching**:

Functions are cached in the evaluator struct for performance:

```go
type PrologEvaluator struct {
    parseClauseFunc  goja.Callable
    parseTermFunc    goja.Callable
    formatTermFunc   goja.Callable
    substBindingsFunc goja.Callable
    // ...
}
```

**Why cache:**

- **Performance**: Avoids repeated `Get()` and `AssertFunction()` calls
- **Type safety**: `AssertFunction()` only called once, errors caught early
- **Cleaner code**: Direct function calls instead of object property access

## REPL Integration

The `PrologEvaluator` implements bobatea's `Evaluator` interface:

```go
type Evaluator interface {
    EvaluateStream(ctx context.Context, code string, emit func(Event)) error
    GetPrompt() string
    GetName() string
    SupportsMultiline() bool
    GetFileExtension() string
}
```

**Event-Based Output**:

Results are emitted as events rather than returned strings:

```go
emit(repl.Event{
    Kind: repl.EventResultMarkdown,
    Props: map[string]any{
        "markdown": formattedResult,
    },
})
```

**Why events:**

- **Rich formatting**: Supports markdown, tables, structured logs
- **Streaming**: Can emit multiple events for long operations
- **Consistent**: Matches bobatea architecture

## Design Decisions

### Why TypeScript?

- **Type safety**: Catches errors at compile time
- **Modern features**: Classes, maps, arrow functions
- **Familiar**: JavaScript ecosystem knowledge applies
- **Portable**: Runs anywhere Goja runs (any Go platform)

### Why Goja?

- **Pure Go**: No external dependencies (no Node.js required)
- **Fast**: Efficient JavaScript runtime
- **Embeddable**: Can embed JavaScript bundle in Go binary
- **Compatible**: Supports CommonJS modules via `goja_nodejs`

### Why bobatea?

- **Professional UI**: Syntax highlighting, history, multiline
- **Event-based**: Clean separation of evaluation and display
- **Extensible**: Easy to add custom commands and features
- **Terminal-native**: Built for terminal applications

## Performance Considerations

**Function caching**: Reduces Goja overhead by caching function references

**Variable renaming**: Only renames when needed (during clause matching)

**Immutable data**: Terms and bindings are never mutated, enabling safe sharing

**Early termination**: Unification fails fast when terms don't match

## Limitations and Future Work

**Current limitations:**

- No built-in predicates (`=`, `is`, `write`)
- No cut operator (`!`)
- No negation as failure (`\+`)
- No arithmetic operations
- Simple list representation (not `[a,b,c]` syntax)

**Potential improvements:**

- Add built-in predicates
- Improve error messages with source locations
- Add syntax highlighting for Prolog
- Support traditional Prolog list syntax
- Add debugging features (trace mode)

## References

- **PAIP Chapter 11**: Original Prolog implementation in Lisp
  - Book: "Paradigms of Artificial Intelligence Programming" by Peter Norvig
  - Chapter covers Prolog interpreter implementation
- **Original TypeScript implementation**: `vibes/2025/12/03/prolog-webapp/server/prolog-ts.ts`
- **Goja documentation**: https://github.com/dop251/goja
- **bobatea REPL**: `bobatea/docs/repl.md`

## See Also

- Getting started: `glaze help getting-started`
- Prolog reference: `glaze help prolog-reference`
- Building TypeScript + Goja apps: `glaze help building-typescript-goja-applications`


