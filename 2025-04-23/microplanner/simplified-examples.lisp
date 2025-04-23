(defpackage :microplanner-examples
  (:use :cl :microplanner)
  (:export :run-all-examples))

(in-package :microplanner-examples)

;;;; Example 1: Basic Assertions and Goals
;;;; This example demonstrates the basic functionality of assertions and goals

(defun example-basic ()
  (format t "~%~%=== Example 1: Basic Assertions and Goals ===~%")
  (clear-database)
  
  ;; Make some assertions
  (format t "~%Making assertions about blocks...~%")
  (planner-assert '(block a))
  (planner-assert '(block b))
  (planner-assert '(block c))
  (planner-assert '(on a table))
  (planner-assert '(on b a))
  (planner-assert '(on c b))
  (planner-assert '(color a red))
  (planner-assert '(color b green))
  (planner-assert '(color c blue))
  
  ;; Show the database
  (format t "~%Database contents:~%")
  (dolist (assertion microplanner:*database*)
    (format t "  ~S~%" assertion))
  
  ;; Simple goal - find a block
  (format t "~%Finding a block:~%")
  (let* ((x-var (? 'x))
         (result (goal `(block ,x-var))))
    (format t "  Found block: ~S~%" (cdr (assoc x-var result))))
  
  ;; Find all blocks
  (format t "~%Finding all blocks:~%")
  (let* ((x-var (? 'x))
         (results (try-all (goal `(block ,x-var)))))
    (dolist (result results)
      (format t "  Found block: ~S~%" (cdr (assoc x-var result)))))
  
  ;; Find what's on the table
  (format t "~%Finding what's on the table:~%")
  (let* ((x-var (? 'x))
         (result (goal `(on ,x-var table))))
    (format t "  Found on table: ~S~%" (cdr (assoc x-var result))))
  
  ;; Find the color of block 'b'
  (format t "~%Finding color of block 'b':~%")
  (let* ((c-var (? 'c))
         (result (goal `(color b ,c-var))))
    (format t "  Color of b: ~S~%" (cdr (assoc c-var result))))
  
  t)

;;;; Example 2: Pattern Matching
;;;; This example demonstrates the pattern matching capabilities

(defun example-pattern-matching ()
  (format t "~%~%=== Example 2: Pattern Matching ===~%")
  (clear-database)
  
  ;; Make some assertions with complex structures
  (format t "~%Making assertions with complex structures...~%")
  (planner-assert '(person (name john doe) (age 30) (occupation programmer)))
  (planner-assert '(person (name jane smith) (age 28) (occupation doctor)))
  (planner-assert '(parent john alice))
  (planner-assert '(parent jane alice))
  
  ;; Show the database
  (format t "~%Database contents:~%")
  (dolist (assertion microplanner:*database*)
    (format t "  ~S~%" assertion))
  
  ;; Find a person by name
  (format t "~%Finding a person with name 'john doe':~%")
  (let* ((rest-var (? 'rest))
         (result (goal `(person (name john doe) ,rest-var))))
    (format t "  Found: ~S~%" (cdr (assoc rest-var result))))
  
  ;; Find parents of alice
  (format t "~%Finding parents of alice:~%")
  (let* ((parent-var (? 'parent))
         (results (try-all (goal `(parent ,parent-var alice)))))
    (dolist (result results)
      (format t "  ~S is a parent of alice~%" 
              (cdr (assoc parent-var result)))))
  
  t)

;;;; Example 3: Theorem Proving
;;;; This example demonstrates the theorem proving capabilities

(defun example-theorem-proving ()
  (format t "~%~%=== Example 3: Theorem Proving ===~%")
  (clear-database)
  
  ;; Make some assertions about family relationships
  (format t "~%Making assertions about family relationships...~%")
  (planner-assert '(father john mary))
  (planner-assert '(father mike john))
  (planner-assert '(mother sarah john))
  
  ;; Define a theorem for grandparent relationship
  (format t "~%Defining theorem for grandparent relationship...~%")
  (let ((gp-var (? 'gp))
        (gc-var (? 'gc))
        (p-var (? 'p)))
    (define-theorem 'grandparent-theorem
      `(grandparent ,gp-var ,gc-var)
      `(try-one
        (goal '(father ,gp-var ,p-var))
        (goal '(father ,p-var ,gc-var)))
      "A theorem to determine if someone is a grandparent through the father's side"))
  
  ;; Show the database
  (format t "~%Database contents:~%")
  (dolist (assertion microplanner:*database*)
    (format t "  ~S~%" assertion))
  
  ;; Find grandparents
  (format t "~%Finding grandparents:~%")
  (let* ((gp-var (? 'gp))
         (gc-var (? 'gc))
         (results (try-all (goal `(grandparent ,gp-var ,gc-var)))))
    (dolist (result results)
      (format t "  ~S is a grandparent of ~S~%" 
              (cdr (assoc gp-var result))
              (cdr (assoc gc-var result)))))
  
  t)

;;;; Run all examples

(defun run-all-examples ()
  (format t "~%~%=== Running All Micro-PLANNER Examples ===~%")
  (example-basic)
  (example-pattern-matching)
  (example-theorem-proving)
  (format t "~%~%=== All Examples Completed ===~%")
  t)
