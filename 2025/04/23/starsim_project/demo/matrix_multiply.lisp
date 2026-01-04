;;;; Matrix Multiplication Demo for STARSIM
;;;; This demonstrates using the *Lisp simulator to perform matrix multiplication

(require :starsim "/home/ubuntu/starsim_project/simulator/starsim/starsim")

(defpackage :matrix-demo
  (:use :common-lisp :starsim)
  (:export :run-demo))

(in-package :matrix-demo)

(defun create-test-matrix (rows cols &optional (pattern :sequential))
  "Create a test matrix with the specified pattern:
   :sequential - Sequential numbers
   :identity - Identity matrix (requires square matrix)
   :random - Random values between 0 and 9"
  (let ((matrix (starsim:*make-array (list rows cols) :initial-element 0)))
    (dotimes (i rows)
      (dotimes (j cols)
        (let ((value (case pattern
                       (:sequential (+ j (* i cols)))
                       (:identity (if (= i j) 1 0))
                       (:random (random 10))
                       (otherwise 0)))
              (index (+ j (* i cols))))
          (setf (aref (starsim::pvar-data matrix) index) value))))
    matrix))

(defun matrix-multiply (matrix-a matrix-b rows-a cols-a cols-b)
  "Multiply matrix-a (rows-a x cols-a) by matrix-b (cols-a x cols-b)"
  (let ((result (starsim:*make-array (list rows-a cols-b) :initial-element 0)))
    ;; For each element in the result matrix
    (dotimes (i rows-a)
      (dotimes (j cols-b)
        (let ((sum 0))
          ;; Compute the dot product of row i from A and column j from B
          (dotimes (k cols-a)
            (let ((a-index (+ k (* i cols-a)))
                  (b-index (+ j (* k cols-b))))
              (incf sum (* (aref (starsim::pvar-data matrix-a) a-index)
                           (aref (starsim::pvar-data matrix-b) b-index)))))
          ;; Store the result
          (setf (aref (starsim::pvar-data result) (+ j (* i cols-b))) sum))))
    result))

(defun print-matrix (matrix rows cols)
  "Print a matrix in a readable format"
  (format t "~%Matrix (~D x ~D):~%" rows cols)
  (dotimes (i rows)
    (format t "  ")
    (dotimes (j cols)
      (format t "~4D " (aref (starsim::pvar-data matrix) (+ j (* i cols)))))
    (format t "~%")))

(defun run-demo ()
  "Run the matrix multiplication demo"
  (format t "~%*Lisp Matrix Multiplication Demo~%")
  (format t "===================================~%")
  
  ;; Set up a 4x4 grid for our simulation
  (starsim:with-simulator (:dimensions '(4 4))
    ;; Create test matrices
    (format t "~%Creating test matrices...~%")
    (let* ((rows-a 3)
           (cols-a 4)
           (rows-b cols-a)
           (cols-b 2)
           (matrix-a (create-test-matrix rows-a cols-a :sequential))
           (matrix-b (create-test-matrix rows-b cols-b :random)))
      
      ;; Print the input matrices
      (format t "~%Matrix A:")
      (print-matrix matrix-a rows-a cols-a)
      
      (format t "~%Matrix B:")
      (print-matrix matrix-b rows-b cols-b)
      
      ;; Perform the multiplication
      (format t "~%Multiplying matrices...~%")
      (let ((result (matrix-multiply matrix-a matrix-b rows-a cols-a cols-b)))
        
        ;; Print the result
        (format t "~%Result Matrix C = A × B:")
        (print-matrix result rows-a cols-b)
        
        ;; Demonstrate some *Lisp operations on the result
        (format t "~%Demonstrating *Lisp operations on the result matrix:~%")
        (format t "Sum of all elements: ~A~%" (starsim:*sum result))
        (format t "Maximum value: ~A~%" (starsim:*max result))
        (format t "Minimum value: ~A~%" (starsim:*min result))
        
        ;; Return the result
        result))))

;; Run the demo when this file is loaded
(format t "~%Matrix multiplication demo loaded.~%")
(format t "Run (matrix-demo:run-demo) to execute the demo.~%")
