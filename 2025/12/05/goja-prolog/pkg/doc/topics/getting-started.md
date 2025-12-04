---
Title: Getting Started with Prolog REPL
Slug: getting-started
Short: Quick start guide for using the Prolog REPL - installation, basic usage, and first steps
Topics:
- prolog
- repl
- getting-started
- tutorial
IsTemplate: false
IsTopLevel: true
ShowPerDefault: true
SectionType: GeneralTopic
---

# Getting Started with Prolog REPL

The Prolog REPL is an interactive terminal application that lets you write and execute Prolog code in real-time. Unlike traditional Prolog implementations that require separate compilation steps, this REPL provides immediate feedback as you add facts, define rules, and query your knowledge base. It's built using Goja to run a TypeScript Prolog interpreter, giving you a self-contained binary that works anywhere Go runs, without requiring Node.js or external Prolog installations.

## Installation

The Prolog REPL is distributed as a single Go binary. You can build it from source or download a pre-built binary for your platform.

**Building from source:**

```bash
cd vibes/2025/12/05/goja-prolog
go generate ./build
go build -o bin/prolog-repl ./cmd/prolog-repl
```

**Running:**

```bash
./bin/prolog-repl
```

You should see the REPL interface with a prompt:

```
prolog> Enter Prolog facts, rules, or queries (use ?- for queries)
TAB: switch focus | Enter: submit | Up/Down: history/selection | c: copy code | y: copy text | Ctrl+C: quit
```

## Your First Prolog Session

Prolog programs consist of **facts** (statements that are always true) and **rules** (conditional statements). You add facts and rules to build a knowledge base, then query it to find answers.

**Step 1: Add a fact**

Type a fact and press Enter:

```
prolog> (likes alice bob)
```

You'll see confirmation:

```
✓ Added: (likes alice bob)
```

**Step 2: Add more facts**

```
prolog> (likes bob charlie)
✓ Added: (likes bob charlie)

prolog> (likes charlie david)
✓ Added: (likes charlie david)
```

**Step 3: Query the knowledge base**

Use `?-` prefix to ask questions:

```
prolog> ?- (likes alice ?x)
```

The REPL finds all solutions:

```
Solution 1: (likes alice bob) Bindings:

  • ?x = bob
```

**Step 4: Find multiple solutions**

```
prolog> ?- (likes ?who ?whom)
```

This finds all pairs where someone likes someone:

```
Solution 1: (likes alice bob) Bindings:

  • ?who = alice
  • ?whom = bob

Solution 2: (likes bob charlie) Bindings:

  • ?who = bob
  • ?whom = charlie

Solution 3: (likes charlie david) Bindings:

  • ?who = charlie
  • ?whom = david
```

## Understanding the Output

When you query with variables, the REPL shows:

1. **Substituted query**: The query with variables replaced by their bound values
   - Example: `(likes alice bob)` shows `?x` was bound to `bob`

2. **Individual bindings**: Each variable and its value
   - Example: `?x = bob` shows the explicit binding

This dual display helps you understand both the complete answer and the individual variable assignments.

## Basic Prolog Concepts

**Facts** are statements that are always true:

```
(likes alice bob)        # Alice likes Bob
(parent alice charlie)   # Alice is parent of Charlie
```

**Rules** define relationships using `:-` (read as "if"):

```
(grandparent ?gp ?gc) :- (parent ?gp ?p) (parent ?p ?gc)
```

This means: `?gp` is a grandparent of `?gc` if `?gp` is parent of `?p` AND `?p` is parent of `?gc`.

**Queries** ask questions:

```
?- (likes alice ?x)      # Who does Alice like?
?- (parent ?x charlie)   # Who is Charlie's parent?
?- (grandparent ?gp ?gc) # Find all grandparent-grandchild pairs
```

## Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `Enter` | Submit current input |
| `Up`/`Down` | Navigate command history |
| `Tab` | Switch focus between input and output |
| `c` | Copy code block |
| `y` | Copy text |
| `Ctrl+C` | Quit REPL |
| `Ctrl+E` | Open external editor (for multiline input) |

## Next Steps

- Learn Prolog syntax: See `glaze help prolog-reference`
- Understand the implementation: See `glaze help prolog-implementation`
- Build TypeScript + Goja apps: See `glaze help building-typescript-goja-applications`

## Common First Steps

**1. Family relationships:**

```
prolog> (parent alice bob)
prolog> (parent bob charlie)
prolog> (grandparent ?gp ?gc) :- (parent ?gp ?p) (parent ?p ?gc)
prolog> ?- (grandparent alice ?gc)
```

**2. Simple rules:**

```
prolog> (sibling ?x ?y) :- (parent ?p ?x) (parent ?p ?y)
prolog> (parent alice bob)
prolog> (parent alice charlie)
prolog> ?- (sibling bob ?y)
```

**3. Lists:**

```
prolog> (member ?x (?x . ?rest))
prolog> (member ?x (?y . ?rest)) :- (member ?x ?rest)
prolog> ?- (member ?x (a b c))
```

## Troubleshooting

**Problem**: Query returns no solutions

**Solution**: Check that facts are added correctly. Use the exact syntax: `(predicate arg1 arg2)`

**Problem**: Variables not showing in results

**Solution**: Ensure variables start with `?` (e.g., `?x`, not `x`)

**Problem**: Parse errors

**Solution**: Check parentheses are balanced and syntax matches Prolog format

For more help, see `glaze help prolog-reference` for syntax details.


