# Micro-PLANNER Common Lisp Implementation Guide

## Introduction

Micro-PLANNER is a historically significant programming language developed in the late 1960s and early 1970s at MIT. It was designed as a subset of Carl Hewitt's PLANNER language and was implemented by Gerry Sussman, Eugene Charniak, and Terry Winograd. Micro-PLANNER was notably used in Winograd's natural language understanding program SHRDLU, which demonstrated remarkable capabilities for its time in understanding and reasoning about a blocks world.

This document provides a comprehensive guide to a modern Common Lisp implementation of Micro-PLANNER, which we call MICROPLANNER-CL. This implementation captures the essential features of the original Micro-PLANNER, including pattern-directed invocation, assertions, goal-directed execution, and backtracking.

## Historical Context

Micro-PLANNER was part of a paradigm shift in artificial intelligence programming. It combined procedural and logical approaches to knowledge representation and reasoning, offering a more flexible alternative to pure resolution-based theorem proving systems that were popular at the time.

Key innovations in Micro-PLANNER included:

1. **Pattern-directed invocation**: Procedures could be triggered by patterns in data
2. **Assertions and goals**: Knowledge could be represented as assertions, and problems as goals to be achieved
3. **Backtracking**: Automatic exploration of alternative solutions
4. **Procedural embedding of knowledge**: Knowledge could be represented procedurally rather than just declaratively

These features made Micro-PLANNER particularly well-suited for natural language understanding, planning, and other AI tasks that required flexible reasoning.

## Core Concepts

### Pattern Matching

Pattern matching is at the heart of Micro-PLANNER. Patterns can contain variables (denoted by symbols starting with '?') that can match any value. For example, the pattern `(on ?x table)` can match assertions like `(on block-a table)`, binding the variable `?x` to `block-a`.

### Assertions and the Database

Knowledge in Micro-PLANNER is represented as assertions stored in a database. Assertions are simply Lisp expressions that represent facts about the world. For example:

```lisp
(planner-assert '(block a))
(planner-assert '(on a table))
(planner-assert '(color a red))
```

### Goals and Theorem Proving

Problems are represented as goals to be achieved. A goal is a pattern that Micro-PLANNER tries to match against assertions in the database or prove using theorems. For example:

```lisp
(goal '(on ?x table))  ; Find something that's on the table
```

### Theorems

Theorems in Micro-PLANNER are procedures that can be used to prove goals that don't directly match assertions in the database. They can be thought of as rules of inference. For example:

```lisp
(define-theorem 'grandparent-theorem
  '(grandparent ?gp ?gc)
  '(try-one
    (goal '(father ?gp ?p))
    (goal '(father ?p ?gc))))
```

This theorem states that to prove someone is a grandparent, we need to prove they are the father of someone who is in turn the father of the grandchild.

### Backtracking

Micro-PLANNER uses backtracking to explore alternative solutions. If a particular path of reasoning fails, Micro-PLANNER automatically backtracks to try other alternatives. This is handled through the `try-all` and `try-one` constructs and the `fail` and `succeed` functions.

## Installation

### Prerequisites

- SBCL (Steel Bank Common Lisp)
- Quicklisp (optional, for dependency management)

### Installation Steps

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/microplanner-cl.git
   cd microplanner-cl
   ```

2. Load the system in SBCL:
   ```bash
   sbcl --load system.lisp
   ```

3. Run the examples:
   ```bash
   sbcl --script run-simplified-examples.lisp
   ```

## Basic Usage

### Starting a REPL

To start a Micro-PLANNER REPL:

```lisp
(microplanner:planner-repl)
```

### Making Assertions

To add facts to the database:

```lisp
(microplanner:planner-assert '(block a))
(microplanner:planner-assert '(on a table))
(microplanner:planner-assert '(color a red))
```

### Creating Goals

To find something that matches a pattern:

```lisp
(microplanner:goal '(on ?x table))
```

This will return bindings for the variable `?x` if a match is found.

### Defining Theorems

To define a rule of inference:

```lisp
(microplanner:define-theorem 'clear-theorem
  '(clear ?x)
  '(try-one
    (microplanner:goal '(block ?x))
    (let ((results (microplanner:try-all (microplanner:goal '(on ?y ?x)))))
      (if results
          (microplanner:fail)
          (microplanner:succeed t))))
  "A block is clear if nothing is on it")
```

### Pattern Variables

Pattern variables are created using the `?` function:

```lisp
(let ((x-var (microplanner:? 'x)))
  (microplanner:goal `(block ,x-var)))
```

## Advanced Usage

### Backtracking and Control Flow

Micro-PLANNER provides several constructs for controlling the flow of execution and backtracking:

- `try-all`: Try all possible solutions
- `try-one`: Try to find one solution
- `fail`: Explicitly fail and trigger backtracking
- `succeed`: Explicitly succeed with a value
- `backtrack`: Trigger backtracking

Example:

```lisp
(microplanner:try-all
  (microplanner:goal '(block ?x))
  (format t "Found block: ~S~%" (cdr (assoc '?x result))))
```

### Forward and Backward Chaining

Micro-PLANNER supports both forward and backward chaining through the `consequent` and `antecedent` functions:

```lisp
;; Forward chaining: If we assert a father relationship, also assert a parent relationship
(microplanner:consequent
  '(father ?f ?c)
  (lambda (bindings proof)
    (microplanner:planner-assert `(parent ,(cdr (assoc '?f bindings)) ,(cdr (assoc '?c bindings))))))

;; Backward chaining: To prove someone is a parent, try to prove they are a father
(microplanner:antecedent
  '(parent ?p ?c)
  '(father ?p ?c))
```

## API Reference

### Core Functions

- `planner-assert`: Add an assertion to the database
- `goal`: Establish a goal to prove a pattern
- `consequent`: Define a forward-chaining rule
- `antecedent`: Define a backward-chaining rule
- `erase`: Remove assertions matching a pattern from the database
- `planner-repl`: Start a Micro-PLANNER REPL

### Pattern Matching

- `pattern-match`: Match a pattern against data
- `?`: Create a pattern variable
- `??`: Create a segment pattern variable
- `constant`: Declare a value as a constant (not to be matched)
- `variable-p`: Check if a value is a pattern variable

### Database Management

- `find-assertions`: Find all assertions matching a pattern
- `clear-database`: Clear the database

### Theorem Proving

- `prove`: Try to prove a goal
- `define-theorem`: Define a new theorem

### Control Flow

- `try-all`: Try all possible solutions
- `try-one`: Try to find one solution
- `fail`: Explicitly fail and trigger backtracking
- `succeed`: Explicitly succeed with a value
- `backtrack`: Trigger backtracking

### Debugging

- `enable-debug`: Enable debug mode
- `disable-debug`: Disable debug mode

## Examples

### Example 1: Basic Assertions and Goals

```lisp
;; Make some assertions
(microplanner:planner-assert '(block a))
(microplanner:planner-assert '(block b))
(microplanner:planner-assert '(block c))
(microplanner:planner-assert '(on a table))
(microplanner:planner-assert '(on b a))
(microplanner:planner-assert '(on c b))

;; Find what's on the table
(let* ((x-var (microplanner:? 'x))
       (result (microplanner:goal `(on ,x-var table))))
  (format t "Found on table: ~S~%" (cdr (assoc x-var result))))
```

### Example 2: Theorem Proving

```lisp
;; Make some assertions about family relationships
(microplanner:planner-assert '(father john mary))
(microplanner:planner-assert '(father mike john))

;; Define a theorem for grandparent relationship
(let ((gp-var (microplanner:? 'gp))
      (gc-var (microplanner:? 'gc))
      (p-var (microplanner:? 'p)))
  (microplanner:define-theorem 'grandparent-theorem
    `(grandparent ,gp-var ,gc-var)
    `(microplanner:try-one
      (microplanner:goal '(father ,gp-var ,p-var))
      (microplanner:goal '(father ,p-var ,gc-var)))
    "A theorem to determine if someone is a grandparent through the father's side"))

;; Find grandparents
(let* ((gp-var (microplanner:? 'gp))
       (gc-var (microplanner:? 'gc))
       (results (microplanner:try-all (microplanner:goal `(grandparent ,gp-var ,gc-var)))))
  (dolist (result results)
    (format t "~S is a grandparent of ~S~%" 
            (cdr (assoc gp-var result))
            (cdr (assoc gc-var result)))))
```

## Limitations and Future Work

This implementation of Micro-PLANNER in Common Lisp has some limitations compared to the original:

1. **Performance**: The implementation is not optimized for performance and may encounter stack overflow errors with deeply recursive theorems.
2. **Segment Variables**: Support for segment variables (matching multiple elements in a list) is limited.
3. **Integration**: The original Micro-PLANNER was tightly integrated with Maclisp; this implementation is more standalone.

Future work could include:

1. Optimizing the implementation to handle more complex examples without stack overflow
2. Improving the pattern matching system to better support segment variables
3. Adding more advanced features from the original Micro-PLANNER
4. Developing more comprehensive examples and applications

## Conclusion

MICROPLANNER-CL provides a modern Common Lisp implementation of the historically significant Micro-PLANNER language. While it has some limitations compared to the original, it captures the essential features that made Micro-PLANNER an important tool in early AI research.

By studying and using this implementation, you can gain insights into the foundations of AI programming and explore alternative approaches to knowledge representation and reasoning that complement modern techniques.

## References

1. Hewitt, C. (1969). "PLANNER: A Language for Manipulating Models and Proving Theorems in Robots"
2. Sussman, G., Charniak, E., & Winograd, T. (1970). "Micro-PLANNER Reference Manual"
3. Winograd, T. (1972). "Understanding Natural Language"
4. Hewitt, C. (1973). "Actor Semantics of PLANNER-73"
5. Wikipedia. "Planner (programming language)". https://en.wikipedia.org/wiki/Planner_(programming_language)
