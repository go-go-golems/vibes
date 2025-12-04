(load "prolog-standalone.lisp")

(format t "~%Testing Prolog interpreter...~%~%")

;; Add some facts
(<- (likes kim robin))
(<- (likes sandy lee))
(<- (likes robin cats))

;; Add a rule
(<- (likes sandy ?x) (likes ?x cats))

;; Test query
(format t "Query: Who does Sandy like?~%")
(?- (likes sandy ?who))

(format t "~%~%Test complete!~%")
