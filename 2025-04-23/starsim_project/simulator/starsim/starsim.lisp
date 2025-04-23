;;;; STARSIM - A simplified *Lisp (StarLisp) simulator
;;;; Based on documentation from Thinking Machines Corporation
;;;; This implementation provides core functionality of *Lisp for educational purposes

(defpackage :starsim
  (:use :common-lisp)
  (:export 
   ;; Core functions
   :*defun :*let :*setf :*when :*if :*progn
   ;; Parallel array operations
   :pvar :pvarp :*make-array :*aref :*setf :*map
   ;; Communication operations
   :*pset :*pset-grid :*news
   ;; Reduction operations
   :*sum :*max :*min :*and :*or
   ;; Utility functions
   :*print :*describe :initialize-simulator :with-simulator
   ;; Configuration
   :*set-grid-size :*current-grid-size))

(in-package :starsim)

;;;; Simulator Configuration

(defvar *grid-dimensions* '(8 8))
(defvar *current-vp-set* nil)
(defvar *simulator-initialized* nil)

(defun *set-grid-size (dimensions)
  "Set the dimensions of the processor grid. DIMENSIONS should be a list of integers."
  (setf *grid-dimensions* dimensions)
  (when *simulator-initialized*
    (initialize-simulator))
  dimensions)

(defun *current-grid-size ()
  "Return the current dimensions of the processor grid."
  *grid-dimensions*)

;;;; Parallel Variable (PVAR) Implementation

(defstruct pvar
  "Structure representing a parallel variable across all processors."
  (name nil)
  (data nil)
  (type t))

(defun *make-array (dimensions &key (element-type t) (initial-element nil))
  "Create a parallel array with the given DIMENSIONS."
  (let* ((total-size (apply #'* *grid-dimensions*))
         (data (make-array total-size :element-type element-type 
                          :initial-element initial-element)))
    (make-pvar :name 'unnamed :data data :type element-type)))

(defun *aref (pvar &rest indices)
  "Access an element of a parallel array."
  (let ((index (apply #'grid-to-linear indices)))
    (aref (pvar-data pvar) index)))

(defun grid-to-linear (indices)
  "Convert grid coordinates to linear index."
  (let ((dimensions *grid-dimensions*))
    (loop for index in indices
          for dim in dimensions
          for scale = 1 then (* scale dim)
          sum (* index scale))))

(defun linear-to-grid (linear-index)
  "Convert linear index to grid coordinates."
  (let ((dimensions *grid-dimensions*))
    (loop with index = linear-index
          for dim in dimensions
          collect (mod index dim)
          do (setf index (floor index dim)))))

;;;; Core Parallel Operations

(defmacro *defun (name lambda-list &body body)
  "Define a *Lisp function."
  `(defun ,name ,lambda-list
     ,@body))

(defmacro *let (bindings &body body)
  "Parallel let binding."
  `(let ,bindings
     ,@body))

(defmacro *when (test &body body)
  "Parallel conditional execution."
  `(when ,test
     ,@body))

(defmacro *if (test then &optional else)
  "Parallel conditional branching."
  `(if ,test ,then ,else))

(defmacro *progn (&body body)
  "Parallel sequential execution."
  `(progn ,@body))

(defun *setf (pvar value &rest indices)
  "Set a value in a parallel array."
  (if indices
      (let ((index (apply #'grid-to-linear indices)))
        (setf (aref (pvar-data pvar) index) value))
      (error "Not implemented yet: general *setf")))

;;;; Communication Operations

(defun *pset (source-pvar dest-pvar &key (source-indices nil) (dest-indices nil))
  "Copy values from source-pvar to dest-pvar, optionally using the provided indices."
  (if (and source-indices dest-indices)
      (let ((source-index (apply #'grid-to-linear source-indices))
            (dest-index (apply #'grid-to-linear dest-indices)))
        (setf (aref (pvar-data dest-pvar) dest-index)
              (aref (pvar-data source-pvar) source-index)))
      (loop for i below (length (pvar-data source-pvar))
            do (setf (aref (pvar-data dest-pvar) i)
                     (aref (pvar-data source-pvar) i)))))

(defun *pset-grid (source-pvar dest-pvar source-grid dest-grid)
  "Copy a value from source-pvar at source-grid to dest-pvar at dest-grid."
  (let ((source-index (apply #'grid-to-linear source-grid))
        (dest-index (apply #'grid-to-linear dest-grid)))
    (setf (aref (pvar-data dest-pvar) dest-index)
          (aref (pvar-data source-pvar) source-index))))

(defun *news (pvar direction &optional (distance 1))
  "Shift values in a pvar in the specified direction by the given distance."
  (let* ((dimensions *grid-dimensions*)
         (result (make-pvar :name 'news-result 
                           :data (copy-seq (pvar-data pvar))
                           :type (pvar-type pvar))))
    (loop for i below (length (pvar-data pvar))
          for coords = (linear-to-grid i)
          for new-coords = (mapcar (lambda (coord dim dir dist)
                                    (mod (+ coord (* dir dist)) dim))
                                  coords dimensions direction (make-list (length dimensions) 
                                                                        :initial-element distance))
          for new-index = (apply #'grid-to-linear new-coords)
          do (setf (aref (pvar-data result) new-index) (aref (pvar-data pvar) i)))
    result))

;;;; Reduction Operations

(defun *sum (pvar)
  "Sum all values in a pvar."
  (loop for i below (length (pvar-data pvar))
        sum (aref (pvar-data pvar) i)))

(defun *max (pvar)
  "Find the maximum value in a pvar."
  (loop for i below (length (pvar-data pvar))
        maximize (aref (pvar-data pvar) i)))

(defun *min (pvar)
  "Find the minimum value in a pvar."
  (loop for i below (length (pvar-data pvar))
        minimize (aref (pvar-data pvar) i)))

(defun *and (pvar)
  "Logical AND of all values in a pvar."
  (loop for i below (length (pvar-data pvar))
        always (aref (pvar-data pvar) i)))

(defun *or (pvar)
  "Logical OR of all values in a pvar."
  (loop for i below (length (pvar-data pvar))
        thereis (aref (pvar-data pvar) i)))

;;;; Mapping Operations

(defun *map (function pvar &rest more-pvars)
  "Apply FUNCTION element-wise to PVAR and MORE-PVARS."
  (let* ((result (make-pvar :name 'map-result 
                           :data (make-array (length (pvar-data pvar)))
                           :type (pvar-type pvar))))
    (loop for i below (length (pvar-data pvar))
          do (setf (aref (pvar-data result) i)
                   (apply function 
                          (cons (aref (pvar-data pvar) i)
                                (mapcar (lambda (p) (aref (pvar-data p) i)) more-pvars)))))
    result))

;;;; Utility Functions

(defun *print (pvar &optional (stream t))
  "Print a representation of a pvar."
  (format stream "~&PVAR: ~A~%" (pvar-name pvar))
  (format stream "Type: ~A~%" (pvar-type pvar))
  (format stream "Dimensions: ~A~%" *grid-dimensions*)
  (format stream "Data:~%")
  (print-grid pvar stream)
  pvar)

(defun print-grid (pvar &optional (stream t))
  "Print a pvar as a grid."
  (let ((dimensions *grid-dimensions*))
    (case (length dimensions)
      (1 (print-1d-grid pvar dimensions stream))
      (2 (print-2d-grid pvar dimensions stream))
      (otherwise (format stream "~A~%" (pvar-data pvar)))))
  pvar)

(defun print-1d-grid (pvar dimensions stream)
  (let ((width (first dimensions)))
    (format stream "[")
    (dotimes (x width)
      (format stream "~A" (aref (pvar-data pvar) x))
      (when (< x (1- width))
        (format stream " ")))
    (format stream "]~%")))

(defun print-2d-grid (pvar dimensions stream)
  (let ((width (first dimensions))
        (height (second dimensions)))
    (dotimes (y height)
      (format stream "[")
      (dotimes (x width)
        (format stream "~A" (aref (pvar-data pvar) (+ x (* y width))))
        (when (< x (1- width))
          (format stream " ")))
      (format stream "]~%"))))

(defun *describe (pvar &optional (stream t))
  "Print detailed information about a pvar."
  (format stream "~&PVAR Details:~%")
  (format stream "  Name: ~A~%" (pvar-name pvar))
  (format stream "  Type: ~A~%" (pvar-type pvar))
  (format stream "  Grid Dimensions: ~A~%" *grid-dimensions*)
  (format stream "  Total Elements: ~A~%" (length (pvar-data pvar)))
  (format stream "  Data: ~A~%" (pvar-data pvar))
  pvar)

;;;; Simulator Initialization and Control

(defun initialize-simulator ()
  "Initialize the *Lisp simulator."
  (format t "~&Initializing *Lisp simulator with grid dimensions: ~A~%" *grid-dimensions*)
  (setf *simulator-initialized* t)
  t)

(defmacro with-simulator ((&key (dimensions '(8 8))) &body body)
  "Execute BODY with the simulator initialized with the given DIMENSIONS."
  `(progn
     (*set-grid-size ,dimensions)
     (initialize-simulator)
     ,@body))

;; Initialize the simulator with default settings
(initialize-simulator)

(format t "~&STARSIM: *Lisp simulator loaded successfully.~%")
(format t "Default grid size: ~A~%" *grid-dimensions*)
(format t "Use (starsim:with-simulator (:dimensions '(x y))) to change grid size.~%")
