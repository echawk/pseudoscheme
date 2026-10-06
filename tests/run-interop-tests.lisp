; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;;; Tests for the Scheme <-> Common Lisp bridge (src/interop.lisp,
;;;; src/api.lisp, src/asdf.lisp; docs/interop.md).
;;;;
;;;;   sbcl --dynamic-space-size 4GB --control-stack-size 500MB \
;;;;        --script tests/run-interop-tests.lisp [-v]
;;;;
;;;; Self-contained: the Lisp library Scheme imports is defined here, the
;;;; autoloading test uses a stand-in system loader, and the ASDF test
;;;; loads examples/mixed-system.  No network, no Quicklisp.

(require :asdf)

;; The dependencies (float-features, cl-unicode, ...) come from
;; Quicklisp when it's installed; QUICKLOAD fetches any that are missing.
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))

(defun load-system (system)
  (if (find-package "QL")
      (uiop:symbol-call "QL" "QUICKLOAD" system :silent t)
      (asdf:load-system system)))

(defvar *root*
  (let ((here (make-pathname :name nil :type nil
			     :defaults (or *load-truename* *load-pathname*))))
    (truename (merge-pathnames (make-pathname :directory '(:relative :up)) here))))
(pushnew *root* asdf:*central-registry* :test #'equal)
(pushnew (merge-pathnames "examples/mixed-system/" *root*) asdf:*central-registry* :test #'equal)

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (load-system :r7rs)))

(ps:disable-float-traps)

(defparameter *verbose* (member "-v" (uiop:command-line-arguments) :test #'string=))

;;; ------------------------------------------------------------------
;;; A Lisp library for Scheme to use

(defpackage "INTEROP-TEST-LIB"
  (:use "COMMON-LISP")
  (:export "ADD" "SMALLP" "BIG-P" "NOTHING" "CALL-WITH" "SCALE" "*COUNTER*"
	   "+ANSWER+" "DIVIDE" "AREA" "SQUARE-SHAPE" "MAKE-SQUARE-SHAPE"
	   "BOX" "MAKE-BOX" "BOX-VALUE" "FAIL" "COUNT-TRUE" "A-MACRO"))

(in-package "INTEROP-TEST-LIB")

(defun add (a b) (+ a b))
(defun smallp (x) (< x 10))
(defun big-p (x) (> x 100))
(defun nothing () nil)				; NIL meaning ()
(defun call-with (f x) (funcall f x))
(defun scale (x &key (by 2)) (* x by))
(defvar *counter* 0)
(defconstant +answer+ 42)
(defun divide (a b) (floor a b))		; two values
(defclass square-shape () ((side :initarg :side)))
(defun make-square-shape (side) (make-instance 'square-shape :side side))
(defgeneric area (shape))
(defmethod area ((s square-shape)) (* (slot-value s 'side) (slot-value s 'side)))
(defstruct box value)
(defun fail () (error "failed on purpose"))
(defun count-true (f list) (count-if f list))	; F's NIL must mean false
(defmacro a-macro (x) x)

(in-package "CL-USER")

;;; ------------------------------------------------------------------
;;; The harness

(defvar *passed* 0)
(defvar *failed* '())

(defun check (name expected thunk &key (test #'equal))
  (let ((actual (handler-case (multiple-value-list (funcall thunk))
		  (serious-condition (e) (list :error (remove #\Newline (princ-to-string e)))))))
    (if (funcall test (if (and (consp expected) (eq (car expected) :values)) (cdr expected) (list expected))
		 actual)
	(progn (incf *passed*) (when *verbose* (format t "~&ok   ~A~%" name)))
	(progn (push name *failed*)
	       (format t "~&FAIL ~A~%     expected ~S~%     got      ~S~%" name expected actual)))))

(defmacro test (name expected &body body)
  `(check ,name ',expected (lambda () ,@body)))

(defun scheme (text)
  "Run an R7RS program; its value written as Scheme would."
  (r7rs:write-to-string (r7rs:eval text)))

(defmacro stest (name expected text)
  "TEXT is a program importing (cl interop-test-lib) etc.; EXPECTED the
written form of its value."
  `(check ,name ,expected (lambda () (scheme ,text))))

(defparameter *prelude*
  "(import (scheme base) (prefix (cl interop-test-lib) t:) (prefix (cl common-lisp) cl:) (pseudoscheme lisp)) ")

(defun p (body) (concatenate 'string *prelude* body))

;;; ------------------------------------------------------------------
;;; Scheme calling Lisp

(stest "call a Lisp function" "5" (p "(t:add 2 3)"))
(stest "predicate FOOP: NIL -> #f" "(#t #f)" (p "(list (t:smallp 1) (t:smallp 50))"))
(stest "predicate FOO-P: NIL -> #f" "(#f #t)" (p "(list (t:big-p 1) (t:big-p 500))"))
(stest "non-predicate NIL is ()" "()" (p "(t:nothing)"))
(stest "ANSI predicates without P" "(#f #t #f)" (p "(list (cl:equal 1 2) (cl:string= \"a\" \"a\") (cl:member 3 '(1 2)))"))
(stest "member returns the tail" "(2 3)" (p "(cl:member 2 '(1 2 3))"))
(stest "Scheme #f argument is NIL" "#t" (p "(cl:null #f)"))
(stest "Scheme predicate's #f reaches Lisp as NIL" "1"
       (p "(t:count-true (lambda (x) (> x 2)) '(1 2 3))"))
(stest "Scheme procedure called from Lisp" "11" (p "(t:call-with (lambda (x) (+ x 1)) 10)"))
;; continuations through Lisp code: escaping works; re-entering a
;; continuation captured under Lisp code is an error, not a wrong answer
(stest "escape from a Scheme procedure called from Lisp" "escaped"
       (p "(call/cc (lambda (k) (cl:mapcar (lambda (x) (if (= x 2) (k 'escaped) x)) '(1 2 3))))"))
(stest "re-entry through Lisp code is an error" "barrier"
       (p "(let ((k #f) (n 0)) (guard (e (#t 'barrier)) (cl:mapcar (lambda (x) (call/cc (lambda (c) (if (= x 2) (set! k c)) x))) '(1 2 3)) (set! n (+ n 1)) (if (< n 2) (k 0) n)))"))
(stest "re-entry around Lisp code works" "(2 (1 2 3))"
       (p "(let ((k #f) (n 0)) (let ((r (call/cc (lambda (c) (set! k c) 0)))) (set! n (+ n 1)) (let ((l (cl:mapcar (lambda (x) (+ x r)) '(1 2 3)))) (if (< n 2) (k 0) (list n l)))))"))
(stest "keyword arguments" "(6 30)" (p "(list (t:scale 3) (t:scale 3 #:by 10))"))
(stest "keyword reads, writes, is self-evaluating" "(#:by #:by #t)" (p "(list #:by '#:by (eq? #:by (lisp-keyword \"by\")))"))
(stest "multiple values" "(3 2)" (p "(call-with-values (lambda () (t:divide 17 5)) list)"))
(stest "special variable read" "0" (p "t:*counter*"))
(stest "special variable set!" "7" (p "(set! t:*counter* 7) t:*counter*"))
(test "special variable set! is visible in Lisp" 7 interop-test-lib:*counter*)
(stest "set! a special to #f gives NIL" "()" (p "(set! t:*counter* #f) t:*counter*"))
(setf interop-test-lib:*counter* 0)
(stest "lisp-let binds dynamically" "(5 0)"
       (p "(list (lisp-let ((t:*counter* 5)) t:*counter*) t:*counter*)"))
(stest "lisp-let and Lisp code" "\"ff\"" (p "(lisp-let ((cl:*print-base* 16)) (cl:string-downcase (cl:princ-to-string 255)))"))
(stest "constant" "42" (p "t:+answer+"))
(stest "lisp-symbol-of" "INTEROP-TEST-LIB:*COUNTER*" (p "(lisp-symbol-of t:*counter*)"))
(stest "generic function and CLOS instance" "49" (p "(t:area (t:make-square-shape 7))"))
(stest "struct accessor and lisp-set!" "(1 2)"
       (p "(let ((b (t:make-box #:value 1))) (let ((before (t:box-value b))) (lisp-set! (t:box-value b) 2) (list before (t:box-value b))))"))
(stest "lisp-set! of gethash, :test cl:equal" "(1 #t)"
       (p "(let ((h (cl:make-hash-table #:test cl:equal))) (lisp-set! (cl:gethash \"k\" h) 1) (list (cl:gethash \"k\" h) (eq? (cl:hash-table-test h) (lisp-symbol \"equal\" \"cl\"))))"))
(stest "Lisp error is a Scheme condition" "\"failed on purpose\""
       (p "(guard (e ((error-object? e) (error-object-message e))) (t:fail))"))
(test "macros, functions and variables are all exported" (:syntax :procedure :syntax)
  (let ((exports (r7rs:library-exports '(cl interop-test-lib))))
    (flet ((kind (name) (cdr (assoc name exports :key #'ps:scheme-symbol-name :test #'string=))))
      (list (kind "a-macro") (kind "add") (kind "*counter*")))))
(stest "sort with a Lisp predicate" "(1 2 3)" (p "(cl:sort (list 3 1 2) cl:<)"))
(stest "sort with a Scheme predicate and #:key" "((1 . a) (2 . b))"
       (p "(cl:sort (list '(2 . b) '(1 . a)) (lambda (x y) (< x y)) #:key cl:car)"))
(stest "lisp-true? / lisp-false?" "(#f #f #t #t)" (p "(list (lisp-true? '()) (lisp-true? #f) (lisp-true? 0) (lisp-false? '()))"))
(stest "lisp-function is raw" "()" (p "((lisp-function \"evenp\") 3)"))
(stest "lisp-funcall" "6" (p "(lisp-funcall (lisp-function \"+\") 1 2 3)"))
(stest "lisp-eval-string" "(1 4 9)" (p "(lisp-eval-string \"(mapcar (lambda (x) (* x x)) '(1 2 3))\")"))
(stest "only/rename on a (cl ...) library" "3"
       "(import (scheme base) (rename (only (cl interop-test-lib) add) (add plus))) (plus 1 2)")
(stest "nested package name (cl a b) is A/B" "\"A/B\""
       (progn (unless (find-package "A/B") (make-package "A/B" :use '()))
	      (let ((s (intern "WHO" "A/B"))) (export s "A/B") (setf (fdefinition s) (lambda () "A/B")))
	      "(import (scheme base) (cl a b)) (who)"))

;; Autoloading: a stand-in loader that "loads" a system by making its package.
(defvar *loaded-systems* '())
(let ((pseudoscheme-interop:*lisp-system-loader*
	(lambda (system)
	  (push system *loaded-systems*)
	  (when (string= system "autoloaded-pkg")
	    (let ((pkg (or (find-package "AUTOLOADED-PKG") (make-package "AUTOLOADED-PKG" :use '()))))
	      (let ((s (intern "HELLO" pkg))) (export s pkg) (setf (fdefinition s) (lambda () "hi"))))))))
  (stest "autoload: missing package loads its system" "\"hi\""
	 "(import (scheme base) (cl autoloaded-pkg)) (hello)")
  (test "autoload: the loader got the system name" ("autoloaded-pkg") *loaded-systems*)
  (test "autoload: no system -> a clear error" t
    (handler-case (progn (r7rs:eval "(import (cl no-such-system-zz))") nil)
      (error (e) (and (search "no-such-system-zz" (princ-to-string e)) t)))))

;;; Lisp macros and special operators from Scheme
(stest "loop over a Scheme list, Scheme procedures inside" "(102 104 106)"
       (p "(define (f x) (+ x 100)) (define xs '(1 2 3 4 5 6)) (cl:loop for x in xs when (even? x) collect (f x))"))
(stest "loop arithmetic" "5050" (p "(cl:loop for i from 1 to 100 sum i)"))
(stest "loop destructuring" "(3 7)" (p "(cl:loop for (a b) in '((1 2) (3 4)) collect (+ a b))"))
(stest "loop binder shadows a Scheme variable" "(a b)" (p "(let ((x 5)) (cl:loop for x in '(a b) collect x))"))
(stest "nested Lisp macros: when, return" "2" (p "(cl:loop for x in '(1 2 3) do (cl:when (> x 1) (cl:return x)))"))
(stest "destructuring-bind with &key" "(1 2 3 4)" (p "(cl:destructuring-bind (a (b c) &key d) '(1 (2 3) #:d 4) (list a b c d))"))
(stest "multiple-value-bind" "(3 2)" (p "(cl:multiple-value-bind (q r) (cl:floor 17 5) (list q r))"))
(stest "handler-case with a condition type" "bad" (p "(cl:handler-case (cl:parse-integer \"x\") (cl:parse-error () 'bad))"))
(stest "with-output-to-string" "\"x=42\"" (p "(cl:with-output-to-string (s) (cl:format s \"x=~a\" 42))"))
(stest "special operator let binding a special" "\"101\"" (p "(cl:let ((cl:*print-base* 2)) (cl:princ-to-string 5))"))
(stest "incf on a Lisp variable" "12" (p "(cl:let ((y 2)) (cl:incf y 10) y)"))
(stest "#f inside a Lisp form is NIL" "no" (p "(cl:if #f 'yes 'no)"))
(stest "type names are symbols" "(#t #f)" (p "(list (cl:typep 3 cl:integer) (cl:typep \"x\" cl:integer))"))
(stest "CLOS defined from Scheme" "(12 27)"
       (p "(cl:defclass circle () ((radius #:initarg #:radius #:reader radius))) (cl:defgeneric area (shape)) (cl:defmethod area ((c circle)) (* 3 (* (radius c) (radius c)))) (list ((lisp-function 'area) (cl:make-instance 'circle #:radius 2)) (lisp (area (make-instance 'circle :radius 3))))"))
(stest "a macro from another package: test lib" "3" (p "(lisp (t:add 1 2))"))
(test "Scheme macro inside a Lisp form is an error" t
  (handler-case (progn (r7rs:eval "(import (scheme base) (prefix (cl common-lisp) cl:)) (cl:progn (let-values (((a) 1)) a))") nil)
    (error (e) (and (search "inside a Lisp macro call" (princ-to-string e)) t))))

;;; ------------------------------------------------------------------
;;; Lisp calling Scheme

(test "use-library makes a package" "SRFI-1" (package-name (r7rs:use-library '(srfi 1))))
(test "procedure as function" 10 (funcall 'srfi-1::fold #'+ 0 '(1 2 3 4)))
(test "Lisp predicate passed to Scheme" (2 4) (funcall 'srfi-1::filter #'evenp '(1 2 3 4)))
(test "#f result comes back NIL" nil (funcall 'srfi-1::any #'stringp '(1 2)))
(test "multiple values come back" (:values (2 4) (1 3)) (funcall 'srfi-1::partition #'evenp '(1 2 3 4)))
(test "Scheme procedure round-trips unwrapped" (1 4 9)
  (funcall 'srfi-1::map (r7rs:scheme (lambda (x) (* x x))) '(1 2 3)))
(test "verbatim callback may return ()" (1 1 3 3)
  (funcall 'srfi-1::append-map (r7rs:verbatim (lambda (x) (if (evenp x) nil (list x x)))) '(1 2 3)))
(test "use-library with :package and a string name" "000042"
  (progn (r7rs:use-library "(srfi 13)" :package "S13-TEST") (uiop:symbol-call "S13-TEST" "STRING-PAD" "42" 6 #\0)))
(test "use-library :convert nil gives the procedures themselves" ps:false
  (progn (r7rs:use-library '(srfi 1) :package "SRFI-1-RAW" :convert nil)
	 (uiop:symbol-call "SRFI-1-RAW" "ANY" (r7rs:eval "(lambda (x) (string? x))") '(1 2))))
(test "library-exports" (:procedure :syntax)
  (list (cdr (assoc "fold" (r7rs:library-exports '(srfi 1)) :key #'ps:scheme-symbol-name :test #'equal))
	(cdr (assoc "cut" (r7rs:library-exports '(srfi 26)) :key #'ps:scheme-symbol-name :test #'equal))))
(test "variables become symbol macros" 3
  (progn (r7rs:eval "(define-library (itest vars) (export counter bump!) (import (scheme base)) (begin (define counter 1) (define (bump!) (set! counter (+ counter 1)))))")
	 (r7rs:use-library '(itest vars) :package "ITEST-VARS")
	 (uiop:symbol-call "ITEST-VARS" "BUMP!") (uiop:symbol-call "ITEST-VARS" "BUMP!")
	 (eval (intern "COUNTER" "ITEST-VARS"))))
(test "redefining a library replaces it" 20
  (progn (r7rs:eval "(define-library (itest redef) (export f) (import (scheme base)) (begin (define (f x) x)))")
	 (r7rs:eval "(define-library (itest redef) (export f) (import (scheme base)) (begin (define (f x) (* 2 x))))")
	 (funcall (r7rs:procedure "f" :library '(itest redef)) 10)))
(test "r7rs:scheme" 120 (r7rs:scheme (define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)))
(test "r7rs:scheme imports at the REPL" (3 2 1) (r7rs:scheme (import (srfi 1)) (fold cons '() '(1 2 3))))
(test "r7rs:scheme converts #f" nil (r7rs:scheme (eq? 'a 'b)))
(test "r7rs:eval keeps #f" ps:false (r7rs:eval "(eq? 'a 'b)"))
(test "r7rs:true-p" (nil t) (list (r7rs:true-p r7rs:false) (r7rs:true-p nil)))
(test "r7rs:procedure from the REPL" 720 (funcall (r7rs:procedure "fact") 6))
(test "r7rs:procedure from a library" 2 (funcall (r7rs:procedure "string-index" :library '(srfi 13)) "hello" #\l))
(test "r7rs:expand gives core Scheme" t (consp (r7rs:expand '(let loop ((i 0)) (if (< i 3) (loop (+ i 1)) i)))))
(test "r7rs:translate gives Lisp" t (consp (r7rs:translate '(lambda (x) (* x x)))))
(test "r7rs:read-from-string / write-to-string" "(#t #f #\\a #(1 2) #:key Hi)"
  (r7rs:write-to-string (r7rs:read-from-string "(#t #f #\\a #(1 2) #:key Hi)")))
(test "r7rs:load" t (progn (r7rs:load (merge-pathnames "examples/scheme-uses-lisp.scm" *root*)) t)
  ) ; (prints the example's output; checked by eye under -v)
(test "uncaught Scheme error is a Lisp error" t
  (handler-case (progn (r7rs:eval "(error \"boom\")") nil) (error () t)))
(test "add-library-directory" t
  (let ((dir (namestring (merge-pathnames "examples/mixed-system/lib/" *root*))))
    (r7rs:add-library-directory dir)
    (and (member dir r7rs:*library-path* :test #'string=) t)))

;; Scheme macros from Lisp, import sets into the current package, define
(defpackage "INTEROP-IMPORT-TEST" (:use "COMMON-LISP"))
(let ((*package* (find-package "INTEROP-IMPORT-TEST")))
  (eval (read-from-string "(r7rs:import (only (srfi 1) fold iota) (rename (only (srfi 1) delete-duplicates) (delete-duplicates dedup)) (prefix (srfi 13) str-) (srfi 26) (srfi 2) (srfi 8))")))
(defmacro in-import-test (string)
  `(let ((*package* (find-package "INTEROP-IMPORT-TEST")))
     (eval (read-from-string ,string))))
(test "import: only" 6 (in-import-test "(fold #'+ 0 '(1 2 3))"))
(test "import: rename" (1 2 3) (in-import-test "(dedup '(1 2 1 3))"))
(test "import: prefix" "007" (in-import-test "(str-string-pad \"7\" 3 #\\0)"))
(test "import: a name CL has is an error" t
  (handler-case (progn (in-import-test "(r7rs:import (only (srfi 1) delete-duplicates))") nil)
    (error (e) (and (search "DELETE-DUPLICATES" (princ-to-string e)) t))))
(test "Scheme macro from Lisp: cut" (2 4 6) (in-import-test "(mapcar (cut * 2 <>) '(1 2 3))"))
(test "Scheme macro sees a Lisp lexical variable" (11 12) (in-import-test "(let ((n 10)) (mapcar (cut + n <>) '(1 2)))"))
(test "Scheme macro uses a Lisp function" "hi!" (in-import-test "(funcall (cut format nil \"~a!\" <>) \"hi\")"))
(test "Scheme macro, quoted Lisp data, and-let*" 500
  (in-import-test "(let ((table '((pear . 5)))) (and-let* ((e (assoc 'pear table)) (n (cdr e))) (* n 100)))"))
(test "Scheme macro, multiple values from a Lisp function: receive" (3 2)
  (in-import-test "(receive (q r) (floor 17 5) (list q r))"))
(test "Scheme macro hygiene: the macro's own temporaries don't capture" (1 2)
  (in-import-test "(let ((x 1) (y 2)) (and-let* ((x x) (z y)) (list x z)))"))
(test "r7rs:define binds a Lisp function" (1 2 6 24)
  (progn (in-import-test "(r7rs:define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))")
	 (in-import-test "(mapcar #'fact '(1 2 3 4))")))
(test "r7rs:define of a variable" 100 (progn (in-import-test "(r7rs:define limit 100)") (in-import-test "limit")))
(test "r7rs:define over a built-in name shadows it at the REPL" (t nil)
  (progn (in-import-test "(r7rs:define (positive? x) (> x 0))") (in-import-test "(list (positive? 1) (positive? -1))")))
(test "r7rs:define-library, then import" 5
  (progn (in-import-test "(r7rs:define-library (itest geometry) (export distance) (import (scheme base) (scheme inexact)) (begin (define (distance x y) (sqrt (+ (* x x) (* y y))))))")
	 (in-import-test "(r7rs:import (itest geometry))")
	 (in-import-test "(distance 3 4)")))
(test "r7rs:false in Scheme written as Lisp" nil (r7rs:scheme (if r7rs:false 'yes r7rs:false)))

;; R6RS and R5RS have the same API.
(test "r6rs:scheme" (1 2 3) (r6rs:scheme (import (rnrs sorting)) (list-sort < '(3 1 2))))
(test "r6rs:eval" 6 (r6rs:eval "(import (rnrs)) (fold-left + 0 '(1 2 3))"))
(test "r6rs:use-library" (1 2 3)
  (progn (r6rs:use-library '(rnrs sorting) :package "RNRS-SORTING-TEST")
	 (uiop:symbol-call "RNRS-SORTING-TEST" "LIST-SORT" #'< '(3 2 1))))
(test "r6rs:expand / translate" (t t) (list (consp (r6rs:expand '(lambda (x) x))) (consp (r6rs:translate '(lambda (x) x)))))
(test "r5rs:scheme" 63 (r5rs:scheme (define (twice f) (lambda (x) (f (f x)))) ((twice (lambda (x) (* x 3))) 7)))
(test "r5rs:eval folds case" 3 (r5rs:eval "(DEFINE (Add A B) (+ a b)) (add 1 2)"))
(test "r5rs:procedure" 9 (funcall (r5rs:procedure "add") 4 5))
(test "r5rs:expand / translate" (t t) (list (consp (r5rs:expand '(let ((x 1)) x))) (consp (r5rs:translate '(lambda (x) x)))))
(test "r5rs:read-from-string folds case" "hello" (ps:scheme-symbol-name (r5rs:read-from-string "HELLO")))

;;; ------------------------------------------------------------------
;;; ASDF

(test "ASDF system with a Scheme library component" (:mean . 5)
  (let ((*standard-output* (make-broadcast-stream)))
    (handler-bind ((warning #'muffle-warning)) (asdf:load-system :mixed-demo))
    (let ((summary (uiop:symbol-call "MIXED-DEMO" "REPORT" '(2 4 4 4 5 5 7 9))))
      (cons :mean (cdr (first summary))))))
(test "reloading the system (library redefinition)" t
  (let ((*standard-output* (make-broadcast-stream)))
    (handler-bind ((warning #'muffle-warning)) (asdf:load-system :mixed-demo :force '(:mixed-demo)))
    t))
(test "the r5rs/r6rs/r7rs shorthand systems exist" (t t t)
  (mapcar (lambda (s) (and (asdf:find-system s nil) t)) '(:r5rs :r6rs :r7rs)))

;;; ------------------------------------------------------------------

(format t "~&~%Interop: ~D of ~D tests passed.~%" *passed* (+ *passed* (length *failed*)))
(when *failed*
  (format t "Failed: ~{~A~^; ~}~%" (reverse *failed*)))
