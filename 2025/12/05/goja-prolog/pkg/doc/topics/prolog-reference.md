---
Title: Prolog Reference
Slug: prolog-reference
Short: Complete reference for Prolog syntax, terms, facts, rules, queries, and examples
Topics:
- prolog
- syntax
- reference
- examples
IsTemplate: false
IsTopLevel: true
ShowPerDefault: true
SectionType: GeneralTopic
---

# Prolog Reference

This reference covers the Prolog syntax and features supported by the REPL. The implementation follows the core Prolog concepts from PAIP (Paradigms of Artificial Intelligence Programming) Chapter 11, providing a subset of Prolog that's sufficient for learning and many practical applications. Understanding these syntax rules and patterns will help you write effective Prolog programs.

## Terms

Prolog programs are built from **terms**, which represent data structures. There are four types of terms:

### Atoms

Atoms are constants that represent names or identifiers. They start with a lowercase letter or are quoted strings.

**Syntax:**

```
atom
foo
bar123
'alice'
```

**Examples:**

```
prolog> (likes alice bob)
prolog> (parent 'alice' 'bob')
```

### Variables

Variables represent unknown values that Prolog will try to find. They start with `?` followed by a name.

**Syntax:**

```
?x
?Var
?person
?X123
```

**Examples:**

```
prolog> ?- (likes alice ?x)        # Find who Alice likes
prolog> ?- (parent ?x charlie)     # Find Charlie's parent
prolog> ?- (likes ?who ?whom)      # Find all like relationships
```

**Important**: Variables are scoped to a single query. Each query starts with fresh variables.

### Compound Terms

Compound terms represent structured data with a functor (name) and arguments.

**Syntax:**

```
(functor arg1 arg2 ... argN)
```

**Examples:**

```
(likes alice bob)              # likes(alice, bob)
(parent alice charlie)        # parent(alice, charlie)
(append (a b) (c d) ?result)  # append([a,b], [c,d], Result)
```

**Structure:**

- Functor: The name of the relationship/predicate (e.g., `likes`, `parent`)
- Arguments: Zero or more terms (atoms, variables, compounds, lists)

### Lists

Lists are sequences of terms, represented using parentheses.

**Syntax:**

```
(a b c)           # List with elements a, b, c
(a (b c) d)       # Nested list
()                # Empty list
```

**Examples:**

```
prolog> (member ?x (a b c))
prolog> (append (a b) (c d) ?result)
prolog> ?- (member ?x (1 2 3))
```

**Note**: Lists are represented as `(element1 element2 ...)` rather than traditional Prolog `[element1, element2, ...]` syntax for parsing simplicity.

## Facts

Facts are statements that are always true. They define relationships or properties.

**Syntax:**

```
(predicate arg1 arg2 ... argN)
```

**Examples:**

```
prolog> (likes alice bob)
✓ Added: (likes alice bob)

prolog> (parent alice charlie)
✓ Added: (parent alice charlie)

prolog> (color apple red)
✓ Added: (color apple red)
```

**Adding facts:**

- Type the fact without any prefix
- Press Enter to add it to the knowledge base
- The REPL confirms with `✓ Added: ...`

**Multiple facts with same predicate:**

You can add multiple facts with the same predicate name:

```
prolog> (likes alice bob)
prolog> (likes alice charlie)
prolog> (likes bob david)
```

All are stored and can be queried.

## Rules

Rules define conditional relationships using `:-` (read as "if"). The head (left side) is true if all goals in the body (right side) are true.

**Syntax:**

```
(head) :- (goal1) (goal2) ... (goalN)
```

**Examples:**

```
prolog> (grandparent ?gp ?gc) :- (parent ?gp ?p) (parent ?p ?gc)
✓ Added: (grandparent ?gp ?gc) :- (parent ?gp ?p) (parent ?p ?gc)

prolog> (sibling ?x ?y) :- (parent ?p ?x) (parent ?p ?y)
✓ Added: (sibling ?x ?y) :- (parent ?p ?x) (parent ?p ?y)

prolog> (ancestor ?a ?d) :- (parent ?a ?d)
prolog> (ancestor ?a ?d) :- (parent ?a ?x) (ancestor ?x ?d)
```

**Reading rules:**

- `(grandparent ?gp ?gc) :- (parent ?gp ?p) (parent ?p ?gc)` means:
  - `?gp` is a grandparent of `?gc` IF
  - `?gp` is a parent of `?p` AND `?p` is a parent of `?gc`

**Multiple rules for same predicate:**

You can define multiple rules for the same predicate (disjunction):

```
prolog> (ancestor ?a ?d) :- (parent ?a ?d)
prolog> (ancestor ?a ?d) :- (parent ?a ?x) (ancestor ?x ?d)
```

This means: `?a` is an ancestor of `?d` if `?a` is a direct parent OR if `?a` is a parent of some `?x` who is an ancestor of `?d`.

## Queries

Queries ask questions about your knowledge base. Use `?-` or `?` prefix.

**Syntax:**

```
?- (predicate arg1 arg2 ...)
? (predicate arg1 arg2 ...)
```

**Examples:**

```
prolog> ?- (likes alice ?x)
Solution 1: (likes alice bob) Bindings:
  • ?x = bob

prolog> ?- (parent ?x charlie)
Solution 1: (likes alice charlie) Bindings:
  • ?x = alice

prolog> ?- (grandparent ?gp ?gc)
Solution 1: (grandparent alice charlie) Bindings:
  • ?gp = alice
  • ?gc = charlie
```

**Understanding results:**

- **Substituted query**: Shows the query with variables replaced by values
- **Bindings**: Shows each variable and its bound value
- **Multiple solutions**: All matching solutions are shown

**No solutions:**

If no solutions exist:

```
prolog> ?- (likes nobody ?x)
**No solutions found.**
```

## Common Patterns

### Family Relationships

```
prolog> (parent alice bob)
prolog> (parent bob charlie)
prolog> (parent charlie david)

prolog> (grandparent ?gp ?gc) :- (parent ?gp ?p) (parent ?p ?gc)
prolog> (ancestor ?a ?d) :- (parent ?a ?d)
prolog> (ancestor ?a ?d) :- (parent ?a ?x) (ancestor ?x ?d)

prolog> ?- (grandparent alice ?gc)
prolog> ?- (ancestor alice ?d)
```

### List Operations

```
prolog> (member ?x (?x . ?rest))
prolog> (member ?x (?y . ?rest)) :- (member ?x ?rest)

prolog> (append () ?ys ?ys)
prolog> (append (?x . ?xs) ?ys (?x . ?zs)) :- (append ?xs ?ys ?zs)

prolog> ?- (member ?x (a b c))
prolog> ?- (append (a b) (c d) ?result)
```

### Transitive Relationships

```
prolog> (likes alice bob)
prolog> (likes bob charlie)
prolog> (likes charlie david)

prolog> (likes_transitive ?x ?y) :- (likes ?x ?y)
prolog> (likes_transitive ?x ?y) :- (likes ?x ?z) (likes_transitive ?z ?y)

prolog> ?- (likes_transitive alice ?y)
```

### Conditional Facts

```
prolog> (parent alice bob)
prolog> (parent bob charlie)
prolog> (sibling ?x ?y) :- (parent ?p ?x) (parent ?p ?y) (not_equal ?x ?y)

prolog> ?- (sibling bob ?y)
```

## Limitations

This implementation provides a core Prolog subset:

**Supported:**
- ✅ Facts and rules
- ✅ Variables and unification
- ✅ Backtracking and multiple solutions
- ✅ Recursive rules
- ✅ Lists and compound terms

**Not supported:**
- ❌ Built-in predicates (`=`, `is`, `write`, etc.)
- ❌ Cut operator (`!`)
- ❌ Negation as failure (`\+`)
- ❌ Arithmetic operations
- ❌ String operations
- ❌ File I/O

For a full Prolog implementation, consider SWI-Prolog or GNU Prolog.

## Examples

### Example 1: Family Tree

```
prolog> (parent alice bob)
prolog> (parent alice charlie)
prolog> (parent bob david)
prolog> (parent bob eve)

prolog> (grandparent ?gp ?gc) :- (parent ?gp ?p) (parent ?p ?gc)
prolog> (sibling ?x ?y) :- (parent ?p ?x) (parent ?p ?y)

prolog> ?- (grandparent alice ?gc)
Solution 1: (grandparent alice david) Bindings:
  • ?gp = alice
  • ?gc = david
Solution 2: (grandparent alice eve) Bindings:
  • ?gp = alice
  • ?gc = eve

prolog> ?- (sibling bob ?y)
Solution 1: (sibling bob charlie) Bindings:
  • ?x = bob
  • ?y = charlie
```

### Example 2: List Membership

```
prolog> (member ?x (?x . ?rest))
prolog> (member ?x (?y . ?rest)) :- (member ?x ?rest)

prolog> ?- (member ?x (a b c))
Solution 1: (member a (a b c)) Bindings:
  • ?x = a
Solution 2: (member b (a b c)) Bindings:
  • ?x = b
Solution 3: (member c (a b c)) Bindings:
  • ?x = c
```

### Example 3: Path Finding

```
prolog> (edge a b)
prolog> (edge b c)
prolog> (edge c d)
prolog> (edge a d)

prolog> (path ?x ?y) :- (edge ?x ?y)
prolog> (path ?x ?y) :- (edge ?x ?z) (path ?z ?y)

prolog> ?- (path a ?y)
Solution 1: (path a b) Bindings:
  • ?x = a
  • ?y = b
Solution 2: (path a c) Bindings:
  • ?x = a
  • ?y = c
Solution 3: (path a d) Bindings:
  • ?x = a
  • ?y = d
```

## See Also

- Getting started: `glaze help getting-started`
- Implementation details: `glaze help prolog-implementation`
- Building TypeScript + Goja apps: `glaze help building-typescript-goja-applications`

