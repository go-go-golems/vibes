# PAIP Chapter 6 Pattern Matcher Research

## Source
- URL: https://github.com/norvig/paip-lisp/blob/main/docs/chapter6.md
- Chapter: "Building Software Tools"

## Key Findings So Far

### Core Pattern Matching Function (from initial view)
```lisp
(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
    ((variable-p pattern)
      (match-variable pattern input bindings))
    ((eql pattern input) bindings)
    ((segment-pattern-p pattern)
      (segment-matcher pattern input bindings))
    ((single-pattern-p pattern) ; ***
      (single-matcher pattern input bindings)) ; ***
    ((and (consp pattern) (consp input))
      (pat-match (rest pattern) (rest input)
            (pat-match (first pattern) (first input)
                bindings)))
    t fail))
```

### Key Components Identified
1. `pat-match` - Main pattern matching function
2. `variable-p` - Check if pattern is a variable
3. `match-variable` - Match a variable pattern
4. `segment-pattern-p` - Check if pattern is a segment pattern
5. `segment-matcher` - Match segment patterns
6. `single-pattern-p` - Check if pattern is a single pattern
7. `single-matcher` - Match single patterns
8. `no-bindings` and `fail` - Special binding values

### Pattern Types
- Variables (e.g., ?x)
- Segment patterns (e.g., ?*x)
- Single patterns
- Literal matches
- Compound patterns (lists)

## Next Steps
- Find the implementation of helper functions
- Understand the binding mechanism
- Find examples and test cases
- Understand segment matching algorithm



## Pattern Grammar Table

| Pattern Type | Syntax | Description |
|--------------|--------|-------------|
| pat=> | var | match any one expression |
| | constant | match just this atom |
| | segment-pat | match something against a sequence |
| | single-pat | match something against one expression |
| | (pat . pat) | match the first and the rest |
| single-pat=> | (?is var predicate) | test predicate on one expression |
| | (?or pat...) | match any pattern on one expression |
| | (?and pat...) | match every pattern on one expression |
| | (?not pat...) | succeed if pattern(s) do not match |
| segment-pat=> | ((?* var)...) | match zero or more expressions |
| | ((?+ var) ... ) | match one or more expressions |
| | ((?? var) ... ) | match zero or one expression |

### Pattern Examples Found
- `(?is ?n numberp)` - match numbers
- `(?or < = >)` - match relational operators
- `(?and (?is ?n numberp) (?is ?n oddp))` - match odd numbers
- `(?not ?x)` - ensure two parts are not equal
- `(?if (> ?x ?y))` - test relationships between variables
- `(?* ?y)` - match zero or more expressions
- `(?+ var)` - match one or more expressions
- `(?? var)` - match zero or one expression

