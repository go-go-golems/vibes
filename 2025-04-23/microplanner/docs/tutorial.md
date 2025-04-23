# Micro-PLANNER Tutorial

## Introduction to Micro-PLANNER

Micro-PLANNER is a historically significant AI programming language that pioneered pattern-directed invocation and goal-oriented problem solving. This tutorial will guide you through using our Common Lisp implementation of Micro-PLANNER, helping you understand its core concepts and how to apply them to solve problems.

## Getting Started

### Installation

First, ensure you have SBCL (Steel Bank Common Lisp) installed on your system. Then follow these steps:

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/microplanner-cl.git
   cd microplanner-cl
   ```

2. Load the system in SBCL:
   ```bash
   sbcl --load system.lisp
   ```

### Your First Micro-PLANNER Program

Let's start with a simple example to understand the basics:

```lisp
;; Load the Micro-PLANNER system
(load "system.lisp")

;; Clear any existing database
(microplanner:clear-database)

;; Make some assertions about blocks
(microplanner:planner-assert '(block a))
(microplanner:planner-assert '(block b))
(microplanner:planner-assert '(on a table))
(microplanner:planner-assert '(on b a))

;; Find what's on the table
(let* ((x-var (microplanner:? 'x))
       (result (microplanner:goal `(on ,x-var table))))
  (format t "Found on table: ~S~%" (cdr (assoc x-var result))))
```

Save this as `first-example.lisp` and run it with:

```bash
sbcl --load first-example.lisp
```

You should see output indicating that block 'a' is on the table.

## Understanding Pattern Matching

Pattern matching is fundamental to Micro-PLANNER. Let's explore it in more detail:

```lisp
;; Create some pattern variables
(defvar *x-var* (microplanner:? 'x))
(defvar *y-var* (microplanner:? 'y))

;; Match a pattern against data
(let ((result (microplanner:pattern-match 
                `(father ,*x-var* ,*y-var*)
                '(father john mary))))
  (format t "x = ~S, y = ~S~%" 
          (cdr (assoc *x-var* result))
          (cdr (assoc *y-var* result))))
```

This will output: `x = JOHN, y = MARY`

## Working with the Database

Micro-PLANNER stores knowledge as assertions in a database:

```lisp
;; Clear the database
(microplanner:clear-database)

;; Add some assertions
(microplanner:planner-assert '(person john))
(microplanner:planner-assert '(person mary))
(microplanner:planner-assert '(likes john pizza))
(microplanner:planner-assert '(likes mary ice-cream))

;; Find all people
(let* ((person-var (microplanner:? 'person))
       (results (microplanner:try-all 
                  (microplanner:goal `(person ,person-var)))))
  (format t "People in the database:~%")
  (dolist (result results)
    (format t "  ~S~%" (cdr (assoc person-var result)))))

;; Find what John likes
(let* ((likes-var (microplanner:? 'likes))
       (result (microplanner:goal `(likes john ,likes-var))))
  (format t "John likes: ~S~%" (cdr (assoc likes-var result))))

;; Remove an assertion
(microplanner:erase '(likes john pizza))

;; Check if John still likes pizza
(let* ((likes-var (microplanner:? 'likes))
       (result (microplanner:goal `(likes john ,likes-var))))
  (if result
      (format t "John still likes: ~S~%" (cdr (assoc likes-var result)))
      (format t "John doesn't like anything anymore.~%")))
```

## Defining and Using Theorems

Theorems allow you to define rules for deriving new knowledge:

```lisp
;; Clear the database
(microplanner:clear-database)

;; Add family relationships
(microplanner:planner-assert '(father john mary))
(microplanner:planner-assert '(father bob john))

;; Define a theorem for grandparent relationship
(let ((gp-var (microplanner:? 'gp))
      (gc-var (microplanner:? 'gc))
      (p-var (microplanner:? 'p)))
  (microplanner:define-theorem 'grandparent-theorem
    `(grandparent ,gp-var ,gc-var)
    `(microplanner:try-one
      (microplanner:goal '(father ,gp-var ,p-var))
      (microplanner:goal '(father ,p-var ,gc-var)))
    "A theorem to determine if someone is a grandparent"))

;; Find who is Mary's grandparent
(let* ((gp-var (microplanner:? 'gp))
       (result (microplanner:goal `(grandparent ,gp-var mary))))
  (if result
      (format t "Mary's grandparent is: ~S~%" (cdr (assoc gp-var result)))
      (format t "Mary has no grandparent in the database.~%")))
```

## Backtracking and Control Flow

Micro-PLANNER uses backtracking to explore alternative solutions:

```lisp
;; Clear the database
(microplanner:clear-database)

;; Add some cities and connections
(microplanner:planner-assert '(city new-york))
(microplanner:planner-assert '(city boston))
(microplanner:planner-assert '(city philadelphia))
(microplanner:planner-assert '(connection new-york boston))
(microplanner:planner-assert '(connection boston philadelphia))

;; Define a theorem for finding paths
(let ((from-var (microplanner:? 'from))
      (to-var (microplanner:? 'to))
      (via-var (microplanner:? 'via)))
  (microplanner:define-theorem 'path-direct
    `(path ,from-var ,to-var)
    `(microplanner:try-one
      (microplanner:goal '(connection ,from-var ,to-var)))
    "Direct path between cities")
  
  (microplanner:define-theorem 'path-indirect
    `(path ,from-var ,to-var)
    `(microplanner:try-one
      (microplanner:goal '(connection ,from-var ,via-var))
      (microplanner:goal '(path ,via-var ,to-var)))
    "Indirect path between cities"))

;; Find if there's a path from New York to Philadelphia
(let* ((result (microplanner:goal '(path new-york philadelphia))))
  (if result
      (format t "There is a path from New York to Philadelphia.~%")
      (format t "There is no path from New York to Philadelphia.~%")))

;; Find all paths starting from New York
(let* ((to-var (microplanner:? 'to))
       (results (microplanner:try-all 
                  (microplanner:goal `(path new-york ,to-var)))))
  (format t "Paths from New York:~%")
  (dolist (result results)
    (format t "  To: ~S~%" (cdr (assoc to-var result)))))
```

## Building a Simple Expert System

Let's build a simple medical diagnosis expert system:

```lisp
;; Clear the database
(microplanner:clear-database)

;; Add symptoms and diseases
(microplanner:planner-assert '(symptom fever))
(microplanner:planner-assert '(symptom cough))
(microplanner:planner-assert '(symptom headache))
(microplanner:planner-assert '(symptom sore-throat))

(microplanner:planner-assert '(disease cold (symptoms cough sore-throat)))
(microplanner:planner-assert '(disease flu (symptoms fever headache cough)))

;; Define a theorem for diagnosis
(let ((disease-var (microplanner:? 'disease))
      (symptoms-var (microplanner:? 'symptoms))
      (patient-symptoms-var (microplanner:? 'patient-symptoms)))
  (microplanner:define-theorem 'diagnose
    `(diagnose ,patient-symptoms-var ,disease-var)
    `(microplanner:try-one
      (microplanner:goal '(disease ,disease-var (symptoms ,symptoms-var)))
      ;; Check if all disease symptoms are in patient symptoms
      (let ((all-present t))
        (dolist (symptom ,symptoms-var)
          (unless (member symptom ,patient-symptoms-var)
            (setf all-present nil)))
        (if all-present
            (microplanner:succeed t)
            (microplanner:fail))))
    "Diagnose a disease based on symptoms"))

;; Diagnose a patient with fever and cough
(let* ((disease-var (microplanner:? 'disease))
       (patient-symptoms '(fever cough))
       (results (microplanner:try-all 
                  (microplanner:goal `(diagnose ,patient-symptoms ,disease-var)))))
  (format t "Possible diagnoses:~%")
  (dolist (result results)
    (format t "  ~S~%" (cdr (assoc disease-var result)))))
```

## Advanced Topics

### Forward and Backward Chaining

Micro-PLANNER supports both forward and backward chaining:

```lisp
;; Forward chaining example
(microplanner:clear-database)

;; Define a consequent: if X is a parent of Y, then Y is a child of X
(let ((parent-var (microplanner:? 'parent))
      (child-var (microplanner:? 'child)))
  (microplanner:consequent
    `(parent ,parent-var ,child-var)
    (lambda (bindings proof)
      (let ((parent (cdr (assoc parent-var bindings)))
            (child (cdr (assoc child-var bindings))))
        (microplanner:planner-assert `(child ,child ,parent))))))

;; Add a parent relationship
(microplanner:planner-assert '(parent john mary))

;; Check if the child relationship was automatically added
(let* ((parent-var (microplanner:? 'parent))
       (result (microplanner:goal `(child mary ,parent-var))))
  (if result
      (format t "Mary is a child of: ~S~%" (cdr (assoc parent-var result)))
      (format t "No child relationship found.~%")))
```

### Debugging

Micro-PLANNER provides debugging tools to help understand the execution flow:

```lisp
;; Enable debug mode
(microplanner:enable-debug)

;; Run a goal with debugging output
(let* ((x-var (microplanner:? 'x))
       (result (microplanner:goal `(block ,x-var))))
  (format t "Result: ~S~%" result))

;; Disable debug mode
(microplanner:disable-debug)
```

## Conclusion

This tutorial has introduced you to the basics of Micro-PLANNER in Common Lisp. You've learned about pattern matching, assertions, goals, theorems, and backtracking. With these tools, you can build knowledge-based systems that can reason about complex problems.

For more detailed information, refer to the comprehensive guide and API documentation included with this implementation.

## Next Steps

- Study the examples in the `examples.lisp` file
- Try building your own knowledge-based system
- Explore the original Micro-PLANNER papers to understand its historical context
- Experiment with more complex theorems and pattern matching
