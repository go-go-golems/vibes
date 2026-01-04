;; Script to run simplified examples and save output to a file
(load "/home/ubuntu/microplanner-cl/microplanner.lisp")
(load "/home/ubuntu/microplanner-cl/package.lisp")
(load "/home/ubuntu/microplanner-cl/simplified-examples.lisp")

;; Redirect output to a file
(let ((output-file (open "/home/ubuntu/microplanner-cl/examples-output.txt" 
                         :direction :output 
                         :if-exists :supersede)))
  (let ((*standard-output* output-file))
    (format t "MICROPLANNER-CL EXAMPLES OUTPUT~%")
    (format t "============================~%~%")
    (format t "Running simplified examples at ~A~%~%" (get-universal-time))
    (microplanner-examples:run-all-examples))
  (close output-file))

(format t "~%Examples completed and output saved to examples-output.txt~%")
(quit)
