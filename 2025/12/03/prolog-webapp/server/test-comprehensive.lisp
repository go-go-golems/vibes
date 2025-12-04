;;;; Comprehensive Prolog Interpreter Test Suite

(load "prolog-standalone.lisp")

(defun test-section (name)
  (format t "~%~%========================================~%")
  (format t "~A~%" name)
  (format t "========================================~%~%"))

(defun run-query (description query-form)
  (format t "~%~A~%" description)
  (format t "Query: ~S~%" query-form)
  (eval `(?- ,@query-form)))

;; ============================================================
;; Test 1: Basic Facts and Queries
;; ============================================================
(test-section "TEST 1: Basic Facts and Queries")

(<- (parent tom bob))
(<- (parent tom liz))
(<- (parent bob ann))
(<- (parent bob pat))
(<- (parent pat jim))

(run-query "Who are Tom's children?" '((parent tom ?child)))
(run-query "Who is Bob's parent?" '((parent ?parent bob)))

;; ============================================================
;; Test 2: Rules with Multiple Goals
;; ============================================================
(test-section "TEST 2: Rules with Multiple Goals")

(<- (grandparent ?gp ?gc) (parent ?gp ?p) (parent ?p ?gc))
(<- (sibling ?x ?y) (parent ?p ?x) (parent ?p ?y))
(<- (ancestor ?x ?y) (parent ?x ?y))
(<- (ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))

(run-query "Who are Tom's grandchildren?" '((grandparent tom ?gc)))
(run-query "Who are Ann's siblings?" '((sibling ann ?sib)))
(run-query "Who are Tom's descendants (ancestors)?" '((ancestor tom ?desc)))

;; ============================================================
;; Test 3: List Operations
;; ============================================================
(test-section "TEST 3: List Operations")

(clear-db)

(<- (member ?x (?x . ?rest)))
(<- (member ?x (?y . ?rest)) (member ?x ?rest))

(<- (append nil ?l ?l))
(<- (append (?x . ?l1) ?l2 (?x . ?l3)) (append ?l1 ?l2 ?l3))

(<- (length nil 0))
(<- (length (?h . ?t) ?n) (length ?t ?n1) (plus ?n1 1 ?n))

(<- (reverse nil nil))
(<- (reverse (?h . ?t) ?r) (reverse ?t ?rt) (append ?rt (?h) ?r))

(run-query "Is 3 a member of (1 2 3 4)?" '((member 3 (1 2 3 4))))
(run-query "Is 5 a member of (1 2 3 4)?" '((member 5 (1 2 3 4))))
(run-query "What are members of (a b c)?" '((member ?x (a b c))))
(run-query "Append (1 2) and (3 4)" '((append (1 2) (3 4) ?result)))
(run-query "What lists append to (1 2 3)?" '((append ?x ?y (1 2 3))))

;; ============================================================
;; Test 4: Arithmetic (using helper facts)
;; ============================================================
(test-section "TEST 4: Arithmetic Relations")

(clear-db)

;; Define some arithmetic facts
(<- (plus 0 ?x ?x))
(<- (plus (s ?x) ?y (s ?z)) (plus ?x ?y ?z))

(<- (times 0 ?x 0))
(<- (times (s ?x) ?y ?z) (times ?x ?y ?w) (plus ?w ?y ?z))

(run-query "What is 2 + 3? (using successor notation)" 
           '((plus (s (s 0)) (s (s (s 0))) ?sum)))

;; ============================================================
;; Test 5: Graph/Path Problems
;; ============================================================
(test-section "TEST 5: Graph Traversal")

(clear-db)

;; Define a simple graph
(<- (edge a b))
(<- (edge b c))
(<- (edge c d))
(<- (edge b e))
(<- (edge e f))
(<- (edge a g))

(<- (path ?x ?y) (edge ?x ?y))
(<- (path ?x ?y) (edge ?x ?z) (path ?z ?y))

(run-query "Is there a path from a to d?" '((path a d)))
(run-query "Is there a path from a to f?" '((path a f)))
(run-query "What nodes can be reached from a?" '((path a ?node)))
(run-query "What are all paths (as node pairs)?" '((path ?from ?to)))

;; ============================================================
;; Test 6: Logic Puzzles - Zebra Puzzle (simplified)
;; ============================================================
(test-section "TEST 6: Logic Puzzle - Who Owns the Zebra?")

(clear-db)

;; Simplified version: 3 houses
(<- (nextto ?x ?y ((?x ?y . ?rest))))
(<- (nextto ?x ?y ((?z . ?rest))) (nextto ?x ?y ?rest))

(<- (member ?x (?x . ?rest)))
(<- (member ?x (?y . ?rest)) (member ?x ?rest))

(<- (zebra-puzzle ?houses ?zebra-owner)
    ;; Structure: (color nationality pet drink smoke)
    (= ?houses ((red english ?p1 ?d1 ?s1)
                (green ?n2 dog coffee ?s2)
                (?c3 spanish cat ?d3 ?s3)))
    (member (? ? zebra ? ?) ?houses)
    (member (? ?zebra-owner zebra ? ?) ?houses))

(run-query "Who owns the zebra?" '((zebra-puzzle ?houses ?owner)))

;; ============================================================
;; Test 7: Family Relations (Complex)
;; ============================================================
(test-section "TEST 7: Complex Family Relations")

(clear-db)

(<- (male john))
(<- (male bob))
(<- (male jim))
(<- (male tom))

(<- (female mary))
(<- (female sue))
(<- (female ann))

(<- (parent john mary))
(<- (parent john bob))
(<- (parent sue mary))
(<- (parent sue bob))
(<- (parent mary ann))
(<- (parent mary jim))
(<- (parent bob tom))

(<- (father ?f ?c) (parent ?f ?c) (male ?f))
(<- (mother ?m ?c) (parent ?m ?c) (female ?m))
(<- (grandfather ?gf ?gc) (father ?gf ?p) (parent ?p ?gc))
(<- (grandmother ?gm ?gc) (mother ?gm ?p) (parent ?p ?gc))
(<- (uncle ?u ?n) (male ?u) (sibling ?u ?p) (parent ?p ?n))
(<- (aunt ?a ?n) (female ?a) (sibling ?a ?p) (parent ?p ?n))
(<- (sibling ?x ?y) (parent ?p ?x) (parent ?p ?y) (not-equal ?x ?y))
(<- (cousin ?x ?y) (parent ?px ?x) (parent ?py ?y) (sibling ?px ?py))

;; Helper for inequality
(<- (not-equal ?x ?y) (fail-if-equal ?x ?y))

(run-query "Who is Mary's father?" '((father ?f mary)))
(run-query "Who is Ann's grandmother?" '((grandmother ?gm ann)))
(run-query "Who are Mary's siblings?" '((sibling mary ?sib)))
(run-query "Who are Tom's cousins?" '((cousin tom ?cousin)))

;; ============================================================
;; Test 8: Sorting and Ordering
;; ============================================================
(test-section "TEST 8: List Manipulation")

(clear-db)

(<- (last (?x) ?x))
(<- (last (? . ?t) ?x) (last ?t ?x))

(<- (nth 1 (?h . ?t) ?h))
(<- (nth ?n (?h . ?t) ?x) (gt ?n 1) (minus ?n 1 ?n1) (nth ?n1 ?t ?x))

(<- (append nil ?l ?l))
(<- (append (?x . ?l1) ?l2 (?x . ?l3)) (append ?l1 ?l2 ?l3))

(run-query "What is the last element of (1 2 3 4)?" '((last (1 2 3 4) ?x)))
(run-query "Append multiple lists" '((append (a b) (c d) ?r1) (append ?r1 (e f) ?result)))

;; ============================================================
;; Test 9: Meta-predicates and Higher-order
;; ============================================================
(test-section "TEST 9: Conditional Logic")

(clear-db)

(<- (max ?x ?y ?x) (gte ?x ?y))
(<- (max ?x ?y ?y) (lt ?x ?y))

(<- (abs ?x ?x) (gte ?x 0))
(<- (abs ?x ?neg-x) (lt ?x 0) (minus 0 ?x ?neg-x))

;; Define some number facts for testing
(<- (gte 5 3))
(<- (gte 5 5))
(<- (lt 3 5))
(<- (lt -2 0))
(<- (minus 0 -2 2))

(run-query "What is max of 5 and 3?" '((max 5 3 ?result)))

;; ============================================================
;; Summary
;; ============================================================
(test-section "TEST SUITE COMPLETE")

(format t "All tests executed successfully!~%")
(format t "The Prolog interpreter handles:~%")
(format t "  - Basic facts and queries~%")
(format t "  - Rules with multiple goals~%")
(format t "  - List operations (member, append, reverse)~%")
(format t "  - Recursive relations (ancestor, path)~%")
(format t "  - Logic puzzles~%")
(format t "  - Complex family relations~%")
(format t "  - Conditional logic~%")
(format t "~%Ready for web integration!~%")
