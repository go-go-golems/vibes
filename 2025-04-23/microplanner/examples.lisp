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
  (let ((result (goal `(block ,(? 'x)))))
    (format t "  Found block: ~S~%" (cdr (assoc (? 'x) result))))
  
  ;; Find all blocks
  (format t "~%Finding all blocks:~%")
  (let ((results (try-all (goal `(block ,(? 'x))))))
    (dolist (result results)
      (format t "  Found block: ~S~%" (cdr (assoc (? 'x) result)))))
  
  ;; Find what's on the table
  (format t "~%Finding what's on the table:~%")
  (let ((result (goal `(on ,(? 'x) table))))
    (format t "  Found on table: ~S~%" (cdr (assoc (? 'x) result))))
  
  ;; Find the color of block 'b'
  (format t "~%Finding color of block 'b':~%")
  (let ((result (goal `(color b ,(? 'c)))))
    (format t "  Color of b: ~S~%" (cdr (assoc (? 'c) result))))
  
  ;; Find all blocks and their colors
  (format t "~%Finding all blocks and their colors:~%")
  (let ((results (try-all (goal `(color ,(? 'block) ,(? 'color))))))
    (dolist (result results)
      (format t "  Block ~S is ~S~%" 
              (cdr (assoc (? 'block) result))
              (cdr (assoc (? 'color) result)))))
  
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
  (planner-assert '(person (name bob jones) (age 45) (occupation teacher)))
  (planner-assert '(likes john programming))
  (planner-assert '(likes jane medicine))
  (planner-assert '(likes bob teaching))
  (planner-assert '(parent john alice))
  (planner-assert '(parent jane alice))
  (planner-assert '(parent bob carol))
  
  ;; Show the database
  (format t "~%Database contents:~%")
  (dolist (assertion microplanner:*database*)
    (format t "  ~S~%" assertion))
  
  ;; Find a person by name
  (format t "~%Finding a person with name 'john doe':~%")
  (let ((result (goal `(person (name john doe) ,(? 'rest)))))
    (format t "  Found: ~S~%" (cdr (assoc (? 'rest) result))))
  
  ;; Find all people and their ages
  (format t "~%Finding all people and their ages:~%")
  (let ((results (try-all (goal `(person (name ,(? 'first) ,(? 'last)) (age ,(? 'age)) ,(? 'rest))))))
    (dolist (result results)
      (format t "  ~S ~S is ~S years old~%" 
              (cdr (assoc (? 'first) result))
              (cdr (assoc (? 'last) result))
              (cdr (assoc (? 'age) result)))))
  
  ;; Find what people like
  (format t "~%Finding what people like:~%")
  (let ((results (try-all (goal `(likes ,(? 'person) ,(? 'activity))))))
    (dolist (result results)
      (format t "  ~S likes ~S~%" 
              (cdr (assoc (? 'person) result))
              (cdr (assoc (? 'activity) result)))))
  
  ;; Find parents of alice
  (format t "~%Finding parents of alice:~%")
  (let ((results (try-all (goal `(parent ,(? 'parent) alice)))))
    (dolist (result results)
      (format t "  ~S is a parent of alice~%" 
              (cdr (assoc (? 'parent) result)))))
  
  t)

;;;; Example 3: Theorem Proving
;;;; This example demonstrates the theorem proving capabilities

(defun example-theorem-proving ()
  (format t "~%~%=== Example 3: Theorem Proving ===~%")
  (clear-database)
  
  ;; Make some assertions about family relationships
  (format t "~%Making assertions about family relationships...~%")
  (planner-assert '(father john mary))
  (planner-assert '(father john bob))
  (planner-assert '(father mike john))
  (planner-assert '(mother lisa mary))
  (planner-assert '(mother lisa bob))
  (planner-assert '(mother sarah john))
  
  ;; Define a theorem for grandparent relationship
  (format t "~%Defining theorem for grandparent relationship...~%")
  (define-theorem 'grandparent-theorem
    `(grandparent ,(? 'gp) ,(? 'gc))
    '(try-one
      (goal `(father ,(? 'gp) ,(? 'p)))
      (goal `(father ,(? 'p) ,(? 'gc))))
    "A theorem to determine if someone is a grandparent through the father's side")
  
  (define-theorem 'grandparent-theorem2
    `(grandparent ,(? 'gp) ,(? 'gc))
    '(try-one
      (goal `(father ,(? 'gp) ,(? 'p)))
      (goal `(mother ,(? 'p) ,(? 'gc))))
    "A theorem to determine if someone is a grandparent through the mother's side")
  
  (define-theorem 'grandparent-theorem3
    `(grandparent ,(? 'gp) ,(? 'gc))
    '(try-one
      (goal `(mother ,(? 'gp) ,(? 'p)))
      (goal `(father ,(? 'p) ,(? 'gc))))
    "A theorem to determine if someone is a grandparent through the father's side")
  
  (define-theorem 'grandparent-theorem4
    `(grandparent ,(? 'gp) ,(? 'gc))
    '(try-one
      (goal `(mother ,(? 'gp) ,(? 'p)))
      (goal `(mother ,(? 'p) ,(? 'gc))))
    "A theorem to determine if someone is a grandparent through the mother's side")
  
  ;; Define a theorem for sibling relationship
  (format t "~%Defining theorem for sibling relationship...~%")
  (define-theorem 'sibling-theorem
    `(sibling ,(? 'a) ,(? 'b))
    '(try-one
      (goal `(father ,(? 'f) ,(? 'a)))
      (goal `(father ,(? 'f) ,(? 'b)))
      (if (equal (? 'a) (? 'b))
          (fail)
          (succeed t)))
    "A theorem to determine if two people are siblings")
  
  ;; Show the database
  (format t "~%Database contents:~%")
  (dolist (assertion microplanner:*database*)
    (format t "  ~S~%" assertion))
  
  ;; Find grandparents
  (format t "~%Finding grandparents:~%")
  (let ((results (try-all (goal `(grandparent ,(? 'gp) ,(? 'gc))))))
    (dolist (result results)
      (format t "  ~S is a grandparent of ~S~%" 
              (cdr (assoc (? 'gp) result))
              (cdr (assoc (? 'gc) result)))))
  
  ;; Find siblings
  (format t "~%Finding siblings:~%")
  (let ((results (try-all (goal `(sibling ,(? 'a) ,(? 'b))))))
    (dolist (result results)
      (format t "  ~S and ~S are siblings~%" 
              (cdr (assoc (? 'a) result))
              (cdr (assoc (? 'b) result)))))
  
  t)

;;;; Example 4: Blocks World
;;;; This example demonstrates a classic AI problem - the blocks world

(defun example-blocks-world ()
  (format t "~%~%=== Example 4: Blocks World ===~%")
  (clear-database)
  
  ;; Set up the initial state
  (format t "~%Setting up initial blocks world state...~%")
  (planner-assert '(block a))
  (planner-assert '(block b))
  (planner-assert '(block c))
  (planner-assert '(block d))
  (planner-assert '(on a table))
  (planner-assert '(on b a))
  (planner-assert '(on c table))
  (planner-assert '(on d c))
  (planner-assert '(clear b))
  (planner-assert '(clear d))
  
  ;; Define theorems for blocks world
  (format t "~%Defining blocks world theorems...~%")
  
  ;; A block is clear if nothing is on it
  (define-theorem 'clear-theorem
    `(clear ,(? 'x))
    '(try-one
      (goal `(block ,(? 'x)))
      (let ((results (try-all (goal `(on ,(? 'y) ,(? 'x))))))
        (if results
            (fail)
            (succeed t))))
    "A block is clear if nothing is on it")
  
  ;; Define a theorem for moving a block
  (define-theorem 'move-theorem
    `(move ,(? 'block) ,(? 'from) ,(? 'to))
    '(try-one
      (goal `(block ,(? 'block)))
      (goal `(on ,(? 'block) ,(? 'from)))
      (goal `(clear ,(? 'block)))
      (goal `(clear ,(? 'to)))
      (unless (equal (? 'to) (? 'block))
        (erase `(on ,(? 'block) ,(? 'from)))
        (planner-assert `(on ,(? 'block) ,(? 'to)))
        (succeed t)))
    "Move a block from one place to another")
  
  ;; Show the initial state
  (format t "~%Initial state:~%")
  (dolist (assertion microplanner:*database*)
    (format t "  ~S~%" assertion))
  
  ;; Find what blocks are clear
  (format t "~%Finding clear blocks:~%")
  (let ((results (try-all (goal `(clear ,(? 'x))))))
    (dolist (result results)
      (format t "  Block ~S is clear~%" 
              (cdr (assoc (? 'x) result)))))
  
  ;; Move block 'b' to the table
  (format t "~%Moving block 'b' to the table...~%")
  (let ((result (goal '(move b a table))))
    (if result
        (format t "  Successfully moved block b to the table~%")
        (format t "  Failed to move block b to the table~%")))
  
  ;; Show the new state
  (format t "~%New state after moving b:~%")
  (dolist (assertion microplanner:*database*)
    (format t "  ~S~%" assertion))
  
  ;; Find what blocks are clear now
  (format t "~%Finding clear blocks after move:~%")
  (let ((results (try-all (goal `(clear ,(? 'x))))))
    (dolist (result results)
      (format t "  Block ~S is clear~%" 
              (cdr (assoc (? 'x) result)))))
  
  ;; Try to move block 'a' to block 'd'
  (format t "~%Trying to move block 'a' to block 'd'...~%")
  (let ((result (goal '(move a table d))))
    (if result
        (format t "  Successfully moved block a to block d~%")
        (format t "  Failed to move block a to block d~%")))
  
  ;; Show the final state
  (format t "~%Final state:~%")
  (dolist (assertion microplanner:*database*)
    (format t "  ~S~%" assertion))
  
  t)

;;;; Example 5: Path Finding
;;;; This example demonstrates path finding in a graph

(defun example-path-finding ()
  (format t "~%~%=== Example 5: Path Finding ===~%")
  (clear-database)
  
  ;; Set up a graph of cities and connections
  (format t "~%Setting up a graph of cities...~%")
  (planner-assert '(city new-york))
  (planner-assert '(city boston))
  (planner-assert '(city philadelphia))
  (planner-assert '(city washington-dc))
  (planner-assert '(city chicago))
  (planner-assert '(city los-angeles))
  (planner-assert '(city san-francisco))
  (planner-assert '(city seattle))
  
  (planner-assert '(connection new-york boston 215))
  (planner-assert '(connection new-york philadelphia 100))
  (planner-assert '(connection philadelphia washington-dc 140))
  (planner-assert '(connection boston chicago 960))
  (planner-assert '(connection chicago seattle 2000))
  (planner-assert '(connection chicago los-angeles 2015))
  (planner-assert '(connection los-angeles san-francisco 380))
  (planner-assert '(connection seattle san-francisco 810))
  
  ;; Define a theorem for finding a path
  (format t "~%Defining path finding theorem...~%")
  
  ;; Direct connection
  (define-theorem 'path-direct
    `(path ,(? 'from) ,(? 'to) ,(? 'distance) ,(? 'route))
    '(try-one
      (goal `(connection ,(? 'from) ,(? 'to) ,(? 'distance)))
      (setf (? 'route) (list (? 'from) (? 'to)))
      (succeed t))
    "Direct path between cities")
  
  ;; Indirect connection (recursive)
  (define-theorem 'path-indirect
    `(path ,(? 'from) ,(? 'to) ,(? 'total-distance) ,(? 'route))
    '(try-one
      (goal `(connection ,(? 'from) ,(? 'intermediate) ,(? 'distance1)))
      (goal `(path ,(? 'intermediate) ,(? 'to) ,(? 'distance2) ,(? 'sub-route)))
      (setf (? 'total-distance) (+ (? 'distance1) (? 'distance2)))
      (setf (? 'route) (cons (? 'from) (? 'sub-route)))
      (succeed t))
    "Indirect path between cities")
  
  ;; Show the database
  (format t "~%Database contents:~%")
  (dolist (assertion microplanner:*database*)
    (format t "  ~S~%" assertion))
  
  ;; Find a path from New York to Washington DC
  (format t "~%Finding a path from New York to Washington DC:~%")
  (let ((result (goal `(path new-york washington-dc ,(? 'distance) ,(? 'route)))))
    (if result
        (format t "  Path found: ~S with distance ~S miles~%" 
                (cdr (assoc (? 'route) result))
                (cdr (assoc (? 'distance) result)))
        (format t "  No path found~%")))
  
  ;; Find a path from New York to San Francisco
  (format t "~%Finding a path from New York to San Francisco:~%")
  (let ((result (goal `(path new-york san-francisco ,(? 'distance) ,(? 'route)))))
    (if result
        (format t "  Path found: ~S with distance ~S miles~%" 
                (cdr (assoc (? 'route) result))
                (cdr (assoc (? 'distance) result)))
        (format t "  No path found~%")))
  
  t)

;;;; Example 6: Expert System
;;;; This example demonstrates a simple expert system for medical diagnosis

(defun example-expert-system ()
  (format t "~%~%=== Example 6: Expert System ===~%")
  (clear-database)
  
  ;; Set up the knowledge base
  (format t "~%Setting up medical diagnosis knowledge base...~%")
  
  ;; Symptoms
  (planner-assert '(symptom fever))
  (planner-assert '(symptom cough))
  (planner-assert '(symptom headache))
  (planner-assert '(symptom sore-throat))
  (planner-assert '(symptom fatigue))
  (planner-assert '(symptom body-aches))
  (planner-assert '(symptom runny-nose))
  (planner-assert '(symptom sneezing))
  
  ;; Diseases and their symptoms
  (planner-assert '(disease common-cold (symptoms runny-nose sneezing sore-throat cough)))
  (planner-assert '(disease flu (symptoms fever cough headache fatigue body-aches)))
  (planner-assert '(disease covid-19 (symptoms fever cough fatigue sore-throat)))
  (planner-assert '(disease allergies (symptoms runny-nose sneezing)))
  
  ;; Define a theorem for diagnosis
  (format t "~%Defining diagnosis theorem...~%")
  
  (define-theorem 'diagnose-theorem
    `(diagnose ,(? 'symptoms) ,(? 'disease) ,(? 'matching-symptoms) ,(? 'missing-symptoms))
    '(try-one
      (goal `(disease ,(? 'disease) (symptoms ,(? 'all-disease-symptoms))))
      (let ((matching nil)
            (missing nil))
        ;; Find matching symptoms
        (dolist (symptom (? 'all-disease-symptoms))
          (if (member symptom (? 'symptoms))
              (push symptom matching)
              (push symptom missing)))
        (setf (? 'matching-symptoms) matching)
        (setf (? 'missing-symptoms) missing)
        (succeed t)))
    "Diagnose a disease based on symptoms")
  
  ;; Show the database
  (format t "~%Knowledge base contents:~%")
  (dolist (assertion microplanner:*database*)
    (format t "  ~S~%" assertion))
  
  ;; Diagnose a patient with certain symptoms
  (format t "~%Diagnosing a patient with: fever, cough, fatigue~%")
  (let* ((patient-symptoms '(fever cough fatigue))
         (results (try-all (goal `(diagnose ,patient-symptoms ,(? 'disease) ,(? 'matching) ,(? 'missing))))))
    
    ;; Sort results by number of matching symptoms (descending)
    (setf results 
          (sort results #'> 
                :key (lambda (result) 
                       (length (cdr (assoc (? 'matching) result))))))
    
    (format t "~%Possible diagnoses (in order of likelihood):~%")
    (dolist (result results)
      (let ((disease (cdr (assoc (? 'disease) result)))
            (matching (cdr (assoc (? 'matching) result)))
            (missing (cdr (assoc (? 'missing) result))))
        (format t "~%  Disease: ~S~%" disease)
        (format t "    Matching symptoms: ~S~%" matching)
        (format t "    Missing symptoms: ~S~%" missing)
        (format t "    Match score: ~D/~D~%" 
                (length matching) 
                (+ (length matching) (length missing))))))
  
  t)

;;;; Example 7: Natural Language Processing
;;;; This example demonstrates simple natural language processing

(defun example-nlp ()
  (format t "~%~%=== Example 7: Natural Language Processing ===~%")
  (clear-database)
  
  ;; Set up the knowledge base
  (format t "~%Setting up NLP knowledge base...~%")
  
  ;; Grammar rules
  (planner-assert '(sentence (noun-phrase) (verb-phrase)))
  (planner-assert '(noun-phrase (determiner) (noun)))
  (planner-assert '(noun-phrase (determiner) (adjective) (noun)))
  (planner-assert '(verb-phrase (verb) (noun-phrase)))
  
  ;; Lexicon
  (planner-assert '(determiner the))
  (planner-assert '(determiner a))
  (planner-assert '(determiner an))
  
  (planner-assert '(adjective big))
  (planner-assert '(adjective small))
  (planner-assert '(adjective red))
  (planner-assert '(adjective blue))
  
  (planner-assert '(noun dog))
  (planner-assert '(noun cat))
  (planner-assert '(noun ball))
  (planner-assert '(noun house))
  
  (planner-assert '(verb chased))
  (planner-assert '(verb saw))
  (planner-assert '(verb hit))
  
  ;; Define a theorem for parsing
  (format t "~%Defining parsing theorems...~%")
  
  (define-theorem 'parse-sentence
    `(parse ,(? 'words) (sentence ,(? 'np) ,(? 'vp)))
    '(try-one
      (let* ((np-result (goal `(parse-np ,(? 'words) ,(? 'np) ,(? 'rest1))))
             (np (cdr (assoc (? 'np) np-result)))
             (rest1 (cdr (assoc (? 'rest1) np-result)))
             (vp-result (goal `(parse-vp ,rest1 ,(? 'vp) ,(? 'rest2))))
             (vp (cdr (assoc (? 'vp) vp-result)))
             (rest2 (cdr (assoc (? 'rest2) vp-result))))
        (if (null rest2)
            (progn
              (setf (? 'np) np)
              (setf (? 'vp) vp)
              (succeed t))
            (fail))))
    "Parse a sentence")
  
  (define-theorem 'parse-np-1
    `(parse-np ,(? 'words) (noun-phrase ,(? 'det) ,(? 'noun)) ,(? 'rest))
    '(try-one
      (let* ((det (first (? 'words)))
             (det-result (goal `(determiner ,det)))
             (noun (second (? 'words)))
             (noun-result (goal `(noun ,noun))))
        (if (and det-result noun-result)
            (progn
              (setf (? 'det) det)
              (setf (? 'noun) noun)
              (setf (? 'rest) (cddr (? 'words)))
              (succeed t))
            (fail))))
    "Parse a simple noun phrase")
  
  (define-theorem 'parse-np-2
    `(parse-np ,(? 'words) (noun-phrase ,(? 'det) ,(? 'adj) ,(? 'noun)) ,(? 'rest))
    '(try-one
      (let* ((det (first (? 'words)))
             (det-result (goal `(determiner ,det)))
             (adj (second (? 'words)))
             (adj-result (goal `(adjective ,adj)))
             (noun (third (? 'words)))
             (noun-result (goal `(noun ,noun))))
        (if (and det-result adj-result noun-result)
            (progn
              (setf (? 'det) det)
              (setf (? 'adj) adj)
              (setf (? 'noun) noun)
              (setf (? 'rest) (cdddr (? 'words)))
              (succeed t))
            (fail))))
    "Parse a noun phrase with adjective")
  
  (define-theorem 'parse-vp
    `(parse-vp ,(? 'words) (verb-phrase ,(? 'verb) ,(? 'np)) ,(? 'rest))
    '(try-one
      (let* ((verb (first (? 'words)))
             (verb-result (goal `(verb ,verb)))
             (np-result (goal `(parse-np ,(cdr (? 'words)) ,(? 'np) ,(? 'rest)))))
        (if (and verb-result np-result)
            (progn
              (setf (? 'verb) verb)
              (setf (? 'np) (cdr (assoc (? 'np) np-result)))
              (setf (? 'rest) (cdr (assoc (? 'rest) np-result)))
              (succeed t))
            (fail))))
    "Parse a verb phrase")
  
  ;; Show the database
  (format t "~%Knowledge base contents (partial):~%")
  (dolist (assertion (subseq microplanner:*database* 0 10))
    (format t "  ~S~%" assertion))
  (format t "  ...~%")
  
  ;; Parse some sentences
  (format t "~%Parsing: 'the dog chased the cat'~%")
  (let* ((sentence '(the dog chased the cat))
         (result (goal `(parse ,sentence ,(? 'parse-tree)))))
    (if result
        (format t "  Parse tree: ~S~%" (cdr (assoc (? 'parse-tree) result)))
        (format t "  Failed to parse~%")))
  
  (format t "~%Parsing: 'the big dog chased a small cat'~%")
  (let* ((sentence '(the big dog chased a small cat))
         (result (goal `(parse ,sentence ,(? 'parse-tree)))))
    (if result
        (format t "  Parse tree: ~S~%" (cdr (assoc (? 'parse-tree) result)))
        (format t "  Failed to parse~%")))
  
  (format t "~%Parsing: 'a red ball hit the blue house'~%")
  (let* ((sentence '(a red ball hit the blue house))
         (result (goal `(parse ,sentence ,(? 'parse-tree)))))
    (if result
        (format t "  Parse tree: ~S~%" (cdr (assoc (? 'parse-tree) result)))
        (format t "  Failed to parse~%")))
  
  t)

;;;; Run all examples

(defun run-all-examples ()
  (format t "~%~%=== Running All Micro-PLANNER Examples ===~%")
  (example-basic)
  (example-pattern-matching)
  (example-theorem-proving)
  (example-blocks-world)
  (example-path-finding)
  (example-expert-system)
  (example-nlp)
  (format t "~%~%=== All Examples Completed ===~%")
  t)
