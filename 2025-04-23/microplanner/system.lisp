(defpackage :microplanner-system
  (:use :cl)
  (:export :load-microplanner))

(in-package :microplanner-system)

(defun load-microplanner ()
  "Load the microplanner system"
  (load "/home/ubuntu/microplanner-cl/microplanner.lisp")
  (load "/home/ubuntu/microplanner-cl/package.lisp")
  (load "/home/ubuntu/microplanner-cl/examples.lisp")
  (format t "~%Microplanner system loaded successfully.~%")
  (format t "To run all examples: (microplanner-examples:run-all-examples)~%")
  (format t "To start the REPL: (microplanner:planner-repl)~%")
  t)
