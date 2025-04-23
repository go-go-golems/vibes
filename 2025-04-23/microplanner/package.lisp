(defpackage :microplanner-cl
  (:use :cl :microplanner)
  (:nicknames :mplanner)
  (:import-from :microplanner
               #:planner-assert
               #:goal
               #:consequent
               #:antecedent
               #:erase
               #:planner-repl
               #:pattern-match
               #:?
               #:??
               #:constant
               #:variable-p
               #:database
               #:find-assertions
               #:clear-database
               #:prove
               #:theorem
               #:define-theorem
               #:try-all
               #:try-one
               #:fail
               #:succeed
               #:backtrack
               #:enable-debug
               #:disable-debug
               #:*database*
               #:*theorems*
               #:*debug-mode*)
  (:export
   #:planner-assert
   #:goal
   #:consequent
   #:antecedent
   #:erase
   #:planner-repl
   #:pattern-match
   #:?
   #:??
   #:constant
   #:variable-p
   #:database
   #:find-assertions
   #:clear-database
   #:prove
   #:theorem
   #:define-theorem
   #:try-all
   #:try-one
   #:fail
   #:succeed
   #:backtrack
   #:enable-debug
   #:disable-debug
   #:*database*
   #:*theorems*
   #:*debug-mode*))

(in-package :microplanner-cl)
