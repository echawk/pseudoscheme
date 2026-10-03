; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;;; Smoke tests for macro expansion (hygiene, syntax-case proper,
;;;; derived forms), run through both expanders: the old vendored
;;;; Dybvig/Hieb 1992 syntax-case (vendor/syntax-case, SC:SC-EVAL) and
;;;; psyntax, the R6RS front end (R6RS:EVAL, at the REPL top level).
;;;;
;;;; Usage:  sbcl --control-stack-size 500MB --script tests/run-syntax-case-tests.lisp

(require :asdf)

(let* ((here (make-pathname :name nil :type nil
			    :defaults (or *load-truename* *load-pathname*)))
       (root (merge-pathnames (make-pathname :directory '(:relative :up))
			       here)))
  (pushnew (truename root) asdf:*central-registry* :test #'equal))

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (asdf:load-system :pseudoscheme/syntax-case)
    (asdf:load-system :pseudoscheme/api)))

(defun rd (string)
  (with-input-from-string (s string) (funcall ps:*scheme-read* s)))

(defvar *run* 0)
(defvar *passed* 0)

;;; Each test is a list of setup forms followed by the form whose value
;;; is checked.  Expected values are written as Scheme and compared with
;;; EQUAL after reading, so lists/symbols/numbers/strings all work.

(defvar *evaluator* nil "Function of one form.")

(defun check (name forms expected)
  (incf *run*)
  (let ((result (handler-case
		    (let ((v nil))
		      (dolist (f forms v)
			(setq v (funcall *evaluator* (rd f)))))
		  (error (e) (list :error (princ-to-string e)))))
	(want (rd expected)))
    (cond ((equal result want) (incf *passed*))
	  (t (format t "~&FAIL ~A~%  got:  ~S~%  want: ~S~%" name result want)))))

(defmacro deftest (name forms expected)
  `(check ,name ',forms ,expected))

(defvar *datum->syntax* "implicit-identifier")
(defvar *syntax->datum* "syntax-object->datum")

(defmacro deftest* (name forms expected)
  `(check ,name (list ,@forms) ,expected))

(defun run-tests ()
;; syntax-rules, hygiene in both directions
(deftest* "syntax-rules defines"
  ("(define-syntax swap! (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))"
   "(define x 1)"
   "(define tmp 2)"
   "(swap! x tmp)"
   "(list x tmp)")
  "(2 1)")
(deftest* "introduced binding doesn't capture user variable"
  ("(define-syntax my-or2 (syntax-rules () ((_ a b) (let ((t a)) (if t t b)))))"
   "(let ((t 7)) (my-or2 #f t))")
  "7")
(deftest* "introduced reference isn't captured by user binding"
  ("(define-syntax m (syntax-rules () ((_ e) (if e 'then 'else))))"
   "(let ((if list)) (m #t))")
  "then")
(deftest* "recursive macro with ellipsis"
  ("(define-syntax my-let* (syntax-rules () ((_ () b ...) (let () b ...)) ((_ ((x v) r ...) b ...) (let ((x v)) (my-let* (r ...) b ...)))))"
   "(my-let* ((a 1) (b (+ a 1)) (c (* b 3))) (list a b c))")
  "(1 2 6)")
(deftest* "nested ellipsis (not R7RS-style a ... ...)"
  ("(define-syntax flat (syntax-rules () ((_ (a ...) ...) '((a ...) ...))))"
   "(flat (1 2) (3) () (4 5))")
  "((1 2) (3) () (4 5))")
(deftest* "literals"
  ("(define-syntax arrow (syntax-rules (=>) ((_ a => b) (cons a b)) ((_ a b c) 'no)))"
   "(list (arrow 1 => 2) (arrow 1 2 3))")
  "((1 . 2) no)")

;; syntax-case proper
(deftest* "syntax-case with fender"
  ((format nil "(define-syntax kind (lambda (x) (syntax-case x () ((_ n) (number? (~A (syntax n))) (syntax 'num)) ((_ n) (syntax 'other)))))" *syntax->datum*)
   "(list (kind 3) (kind a))")
  "(num other)")
(deftest* "with-syntax and generate-temporaries"
  ("(define-syntax my-let (lambda (x) (syntax-case x () ((_ ((n v) ...) b ...) (with-syntax (((t ...) (generate-temporaries (syntax (n ...))))) (syntax ((lambda (t ...) (let ((n t) ...) b ...)) v ...)))))))"
   "(my-let ((a 1) (b 2)) (+ a b))")
  "3")
(deftest* "implicit-identifier / datum->syntax"
  ((format nil "(define-syntax with-it (lambda (x) (syntax-case x () ((k body) (with-syntax ((it (~A (syntax k) 'it))) (syntax (let ((it 42)) body)))))))" *datum->syntax*)
   "(with-it (+ it 1))")
  "43")
(deftest* "local let-syntax"
  ("(let-syntax ((double (syntax-rules () ((_ e) (* 2 e))))) (double 21))")
  "42")

;; macro-defs.ss derived forms
(deftest* "cond =>" ("(cond ((assv 2 '((1 . a) (2 . b))) => cdr) (else 'none))") "b")
(deftest* "case" ("(case 3 ((1 2) 'low) ((3 4) 'mid) (else 'hi))") "mid")
(deftest* "do" ("(do ((i 0 (+ i 1)) (acc '() (cons i acc))) ((= i 3) acc))") "(2 1 0)")
(deftest* "named let" ("(let loop ((i 0) (s 0)) (if (< i 5) (loop (+ i 1) (+ s i)) s))") "10")
(deftest* "quasiquote" ("`(1 ,(+ 1 1) ,@(list 3 4) 5)") "(1 2 3 4 5)")
(deftest* "delay/force" ("(force (delay (+ 1 2)))") "3")
(deftest* "when/unless" ("(list (when #t 1 2) (unless #f 3))") "(2 3)")

)

(defun run-with (label evaluator datum->syntax)
  (setq *run* 0 *passed* 0)
  (let ((*evaluator* evaluator) (*datum->syntax* datum->syntax)
	(*syntax->datum* (if (string= datum->syntax "datum->syntax") "syntax->datum" "syntax-object->datum")))
    (run-tests))
  (format t "~&~A: ~A of ~A tests passed.~%" label *passed* *run*)
  (= *passed* *run*))

;; psyntax first: the old expander leaves macro definitions on the
;; plists of shared SCHEME symbols, which confuses a later psyntax run in
;; the same image.  (The two are never meant to coexist.)
(let* ((new (run-with "psyntax (R6RS front end)" #'r6rs:eval "datum->syntax"))
       (old (run-with "syntax-case (1992, vendor/syntax-case)" #'sc:sc-eval "implicit-identifier")))
  (uiop:quit (if (and old new) 0 1)))
