;;;; MICROPLANNER-CL - A Common Lisp implementation of Micro-PLANNER
;;;; Based on the original Micro-PLANNER for PDP-10 Maclisp
;;;; Author: Manus AI, 2025

(defpackage :microplanner
  (:use :cl)
  (:export
   ;; Core functions
   :planner-assert
   :goal
   :consequent
   :antecedent
   :erase
   :planner-repl
   
   ;; Pattern matching
   :pattern-match
   :?
   :??
   :constant
   :variable-p
   
   ;; Database
   :database
   :find-assertions
   :clear-database
   
   ;; Theorem proving
   :prove
   :theorem
   :define-theorem
   
   ;; Control flow
   :try-all
   :try-one
   :fail
   :succeed
   :backtrack
   
   ;; Variables
   :*database*
   :*theorems*
   :*trail*
   :*depth*
   :*debug-mode*
   
   ;; Debugging
   :enable-debug
   :disable-debug))

(in-package :microplanner)

;;; ======================================================================
;;; Global variables and data structures
;;; ======================================================================

(defvar *database* nil "The assertion database")
(defvar *theorems* nil "The theorem database")
(defvar *trail* nil "The backtracking trail")
(defvar *depth* 0 "Current recursion depth")
(defvar *debug-mode* nil "Debug mode flag")

;;; ======================================================================
;;; Pattern matching system
;;; ======================================================================

(defun variable-p (x)
  "Check if X is a pattern variable"
  (and (symbolp x)
       (char= (char (symbol-name x) 0) #\?)))

(defun constant (x)
  "Declare X as a constant (not to be matched)"
  (list 'constant x))

(defun constant-p (x)
  "Check if X is a declared constant"
  (and (listp x) (eq (car x) 'constant)))

(defun pattern-match (pattern data &optional (bindings nil))
  "Match PATTERN against DATA with BINDINGS"
  (cond
    ;; If we have a successful match with bindings
    ((eq bindings 'fail) 'fail)
    
    ;; If pattern is a variable, try to bind it
    ((variable-p pattern)
     (match-variable pattern data bindings))
    
    ;; If pattern is a constant declaration, match the constant
    ((constant-p pattern)
     (if (equal (second pattern) data)
         bindings
         'fail))
    
    ;; If both are atoms, they must be equal
    ((and (atom pattern) (atom data))
     (if (equal pattern data) bindings 'fail))
    
    ;; If one is atom and other is list, fail
    ((or (atom pattern) (atom data)) 'fail)
    
    ;; If both are lists, match recursively
    (t (pattern-match (cdr pattern) (cdr data)
                      (pattern-match (car pattern) (car data) bindings)))))

(defun match-variable (var data bindings)
  "Match a variable against data with current bindings"
  (let ((binding (assoc var bindings)))
    (cond
      ;; If variable is already bound, match against its value
      (binding
       (if (equal (cdr binding) data)
           bindings
           'fail))
      
      ;; Otherwise, add new binding
      (t (cons (cons var data) bindings)))))

(defun instantiate (pattern bindings)
  "Instantiate a pattern with variable bindings"
  (cond
    ((null bindings) pattern)
    ((eq bindings 'fail) 'fail)
    ((variable-p pattern)
     (let ((binding (assoc pattern bindings)))
       (if binding
           (cdr binding)
           pattern)))
    ((constant-p pattern) (second pattern))
    ((atom pattern) pattern)
    (t (cons (instantiate (car pattern) bindings)
             (instantiate (cdr pattern) bindings)))))

;;; ======================================================================
;;; Database management
;;; ======================================================================

(defun planner-assert (assertion &optional (db '*database*))
  "Add an assertion to the database"
  (when *debug-mode*
    (format t "~&Asserting: ~S~%" assertion))
  (let ((db-val (if (symbolp db) (symbol-value db) db)))
    (push assertion db-val)
    (if (symbolp db)
        (set db db-val)
        (setf db db-val))
    assertion))

(defun erase (pattern &optional (db '*database*))
  "Remove assertions matching pattern from database"
  (when *debug-mode*
    (format t "~&Erasing: ~S~%" pattern))
  (let ((db-val (if (symbolp db) (symbol-value db) db))
        (removed nil))
    (setf db-val
          (remove-if (lambda (assertion)
                       (let ((match (pattern-match pattern assertion)))
                         (when (not (eq match 'fail))
                           (push assertion removed)
                           t)))
                     db-val))
    (if (symbolp db)
        (set db db-val)
        (setf db db-val))
    removed))

(defun find-assertions (pattern &optional (db '*database*))
  "Find all assertions matching pattern in database"
  (let ((db-val (if (symbolp db) (symbol-value db) db))
        (matches nil))
    (dolist (assertion db-val)
      (let ((bindings (pattern-match pattern assertion)))
        (unless (eq bindings 'fail)
          (push (cons assertion bindings) matches))))
    (reverse matches)))

(defun clear-database (&optional (db '*database*))
  "Clear the database"
  (if (symbolp db)
      (set db nil)
      (setf db nil)))

;;; ======================================================================
;;; Theorem proving system
;;; ======================================================================

(defstruct theorem
  name
  pattern
  body
  documentation)

(defun define-theorem (name pattern body &optional documentation)
  "Define a new theorem"
  (let ((thm (make-theorem :name name
                          :pattern pattern
                          :body body
                          :documentation documentation)))
    (push thm *theorems*)
    thm))

(defun find-theorems (pattern)
  "Find theorems that might apply to pattern"
  (let ((matches nil))
    (dolist (thm *theorems*)
      (let ((bindings (pattern-match (theorem-pattern thm) pattern)))
        (unless (eq bindings 'fail)
          (push (cons thm bindings) matches))))
    (reverse matches)))

(defun prove (goal &optional (depth 0))
  "Try to prove a goal"
  (let ((*depth* depth))
    (when *debug-mode*
      (format t "~&~vTTrying to prove: ~S~%" (* depth 2) goal))
    
    ;; First try direct matches in the database
    (let ((direct-matches (find-assertions goal)))
      (when direct-matches
        (when *debug-mode*
          (format t "~&~vTFound direct match: ~S~%" (* depth 2) 
                  (caar direct-matches)))
        (return-from prove (values t (cdar direct-matches) (caar direct-matches)))))
    
    ;; Then try applicable theorems
    (let ((theorems (find-theorems goal)))
      (dolist (thm-match theorems)
        (let* ((thm (car thm-match))
               (bindings (cdr thm-match))
               (instantiated-body (instantiate (theorem-body thm) bindings)))
          (when *debug-mode*
            (format t "~&~vTTrying theorem: ~S~%" (* depth 2) (theorem-name thm)))
          
          (multiple-value-bind (success new-bindings proof)
              (eval `(let ((*depth* ,(1+ depth)))
                       ,instantiated-body))
            (when success
              (when *debug-mode*
                (format t "~&~vTTheorem succeeded: ~S~%" (* depth 2) 
                        (theorem-name thm)))
              (return-from prove (values t 
                                         (append bindings new-bindings)
                                         (list 'by-theorem 
                                               (theorem-name thm) 
                                               proof))))))))
    
    ;; If we get here, we failed
    (when *debug-mode*
      (format t "~&~vTFailed to prove: ~S~%" (* depth 2) goal))
    (values nil nil nil)))

;;; ======================================================================
;;; Control flow and backtracking
;;; ======================================================================

(defmacro try-all (form &body body)
  "Try all possible solutions, executing BODY for each"
  `(let ((results nil))
     (handler-case
         (loop
           (let ((result ,form))
             (push (progn ,@body) results)
             (fail)))
       (backtrack-condition () nil))
     (nreverse results)))

(defmacro try-one (form &body body)
  "Try to find one solution, executing BODY if found"
  `(handler-case
       (let ((result ,form))
         ,@body)
     (backtrack-condition () nil)))

(define-condition backtrack-condition () ())

(defun fail ()
  "Explicitly fail and trigger backtracking"
  (signal 'backtrack-condition))

(defun succeed (&optional value)
  "Explicitly succeed with a value"
  value)

(defun backtrack ()
  "Trigger backtracking"
  (fail))

;;; ======================================================================
;;; Goal-directed execution
;;; ======================================================================

(defun goal (pattern &optional (action nil action-provided-p))
  "Establish a goal to prove pattern"
  (multiple-value-bind (success bindings proof)
      (prove pattern)
    (if success
        (if action-provided-p
            (funcall action bindings proof)
            (values bindings proof))
        nil)))

(defun consequent (pattern &optional (action nil action-provided-p))
  "Define a consequent (forward-chaining rule)"
  (define-theorem (gensym "CONSEQUENT")
    pattern
    `(progn
       ,(if action-provided-p
            `(funcall ',action bindings proof)
            `(values bindings proof))
       t)
    "Auto-generated consequent"))

(defun antecedent (pattern condition &optional (action nil action-provided-p))
  "Define an antecedent (backward-chaining rule)"
  (define-theorem (gensym "ANTECEDENT")
    pattern
    `(let ((bindings (prove ',condition (1+ *depth*))))
       (if bindings
           ,(if action-provided-p
                `(funcall ',action bindings)
                `(values t bindings))
           (values nil nil)))
    "Auto-generated antecedent"))

;;; ======================================================================
;;; REPL and utilities
;;; ======================================================================

(defun planner-repl ()
  "Start a Micro-PLANNER REPL"
  (format t "~&Micro-PLANNER REPL (type 'quit' to exit)~%")
  (loop
    (format t "~&PLANNER> ")
    (finish-output)
    (let ((input (read)))
      (when (eq input 'quit)
        (return-from planner-repl))
      (handler-case
          (let ((result (eval input)))
            (format t "~&=> ~S~%" result))
        (error (e)
          (format t "~&Error: ~A~%" e))))))

(defun ? (var)
  "Create a pattern variable"
  (intern (format nil "?~A" var)))

(defun ?? (var)
  "Create a segment pattern variable"
  (intern (format nil "??~A" var)))

(defun enable-debug ()
  "Enable debug mode"
  (setf *debug-mode* t))

(defun disable-debug ()
  "Disable debug mode"
  (setf *debug-mode* nil))

;;; ======================================================================
;;; Package initialization
;;; ======================================================================

(defun initialize-microplanner ()
  "Initialize the Micro-PLANNER system"
  (setf *database* nil)
  (setf *theorems* nil)
  (setf *trail* nil)
  (setf *depth* 0)
  (setf *debug-mode* nil)
  t)

(initialize-microplanner)
