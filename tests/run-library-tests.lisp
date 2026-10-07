; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;;; Tests for the library layer (src/library.lisp) and the R7RS / R6RS
;;;; front ends built on it: define-library / library, import sets,
;;;; cond-expand, top-level programs, and a sampling of what each
;;;; standard library provides.
;;;;
;;;; Usage:  sbcl --script tests/run-library-tests.lisp

(require :asdf)

;; The dependencies (float-features, cl-unicode, ...) come from
;; Quicklisp when it's installed; QUICKLOAD fetches any that are missing.
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))

(defun load-system (system)
  (if (find-package "QL")
      (uiop:symbol-call "QL" "QUICKLOAD" system :silent t)
      (asdf:load-system system)))

(let* ((here (make-pathname :name nil :type nil
			    :defaults (or *load-truename* *load-pathname*)))
       (root (merge-pathnames (make-pathname :directory '(:relative :up)) here)))
  (pushnew (truename root) asdf:*central-registry* :test #'equal))

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (load-system :pseudoscheme/api)))

(defvar *run* 0)
(defvar *passed* 0)

(defun read-all-from-string (string)
  (with-input-from-string (in string)
    (loop for form = (funcall ps:*scheme-read* in)
	  until (eq form ps:eof-object)
	  collect form)))

(defun check (name thunk expected)
  "THUNK returns a Lisp value; EXPECTED is Scheme text compared with EQUAL?
(or the keyword :ERROR to expect any error)."
  (incf *run*)
  (let ((got (handler-case (funcall thunk)
	       (error (e) (list :error (remove #\Newline (princ-to-string e)))))))
    (cond ((eq expected :error)
	   (if (and (consp got) (eq (car got) :error))
	       (incf *passed*)
	       (format t "~&FAIL ~A~%  expected an error, got ~S~%" name got)))
	  (t
	   (let ((want (car (read-all-from-string expected))))
	     (if (ps::scheme-equal-p got want)
		 (incf *passed*)
		 (format t "~&FAIL ~A~%  got:  ~S~%  want: ~S~%" name got want)))))))

(defun r7 (source)
  "Run SOURCE (any define-library forms, then an R7RS program) on psyntax
and return the program's last value."
  (r7rs:eval source))

(defun r6 (source)
  "Install any (library ...) forms in SOURCE, then run the (import ...)
program after them, through psyntax; return the program's last value."
  (unless psx:*host* (psx::boot))
  (psx:eval-forms (read-all-from-string source)))

(defmacro deftest (name (kind) source expected)
  `(check ,name (lambda () (,kind ,source)) ,expected))

;;; ------------------------------------------------------------------
;;; The library layer itself, R7RS syntax

(deftest "define-library, export rename, import prefix/only" (r7)
  "(define-library (test counter)
     (export make-counter (rename counter-value value))
     (import (scheme base))
     (begin
       (define (make-counter) (list 0))
       (define (counter-value c) (car c))))
   (import (only (scheme base) define +) (prefix (test counter) c:))
   (c:value (c:make-counter))"
  "0")

(deftest "exported variable is shared, not copied" (r7)
  "(define-library (test shared)
     (export bump! get)
     (import (scheme base))
     (begin (define n 0) (define (bump!) (set! n (+ n 1))) (define (get) n)))
   (import (scheme base) (test shared))
   (bump!) (bump!)
   (get)"
  "2")

(deftest "exported macro" (r7)
  "(define-library (test macros)
     (export swap!)
     (import (scheme base))
     (begin (define-syntax swap!
              (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))))
   (import (scheme base) (test macros))
   (define x 1) (define y 2) (swap! x y) (list x y)"
  "(2 1)")

(deftest "rename import" (r7)
  "(import (rename (only (scheme base) car quote) (car first)))
   (first '(1 2))"
  "1")

(deftest "except import leaves the name unbound" (r7)
  "(import (except (scheme base) car))
   (car '(1 2))"
  :error)

(deftest "import of an unexported name is an error" (r7)
  "(import (only (scheme base) no-such-thing))"
  :error)

(deftest "unknown library is an error" (r7)
  "(import (no such library))"
  :error)

(deftest "cond-expand in library declarations" (r7)
  "(define-library (test ce)
     (export which)
     (import (scheme base))
     (cond-expand
       (no-such-feature (begin (define (which) 'wrong)))
       ((library (scheme base)) (begin (define (which) 'right)))
       (else (begin (define (which) 'else)))))
   (import (scheme base) (test ce))
   (which)"
  "right")

(deftest "cond-expand as an expression" (r7)
  "(import (scheme base))
   (cond-expand (r7rs 'yes) (else 'no))"
  "yes")

(deftest "cond-expand and/or/not" (r7)
  "(import (scheme base))
   (list (cond-expand ((and r7rs ratios) 1) (else 2))
         (cond-expand ((or nonsense full-unicode) 1) (else 2))
         (cond-expand ((not r7rs) 1) (else 2)))"
  "(1 1 2)")

(deftest "a library must define what it exports" (r7)
  "(define-library (test bad)
     (export missing)
     (import (scheme base)))"
  :error)

;;; ------------------------------------------------------------------
;;; R7RS: derived forms and procedures

(deftest "when / unless" (r7)
  "(import (scheme base)) (list (when #t 1 2) (unless #f 3))" "(2 3)")

(deftest "let-values / let*-values" (r7)
  "(import (scheme base))
   (list (let-values (((a b) (values 1 2)) ((c) (values 3))) (list a b c))
         (let*-values (((a b) (values 1 2)) ((c) (values (+ a b)))) c))"
  "((1 2 3) 3)")

(deftest "define-values" (r7)
  "(import (scheme base))
   (define-values (q r) (floor/ 17 5))
   (define-values (a . rest) (values 1 2 3))
   (list q r a rest)"
  "(3 2 1 (2 3))")

(deftest "parameterize" (r7)
  "(import (scheme base))
   (define p (make-parameter 10 (lambda (x) (* x 2))))
   (list (p) (parameterize ((p 3)) (p)) (p))"
  "(20 6 20)")

(deftest "case-lambda" (r7)
  "(import (scheme base) (scheme case-lambda))
   (define f (case-lambda (() 'none) ((a) (list 'one a)) ((a b) (list 'two a b)) ((a . rest) (list 'many a rest))))
   (list (f) (f 1) (f 1 2) (f 1 2 3))"
  "(none (one 1) (two 1 2) (many 1 (2 3)))")

(deftest "define-record-type" (r7)
  "(import (scheme base))
   (define-record-type point (make-point x y) point? (x point-x set-point-x!) (y point-y))
   (define p (make-point 1 2))
   (set-point-x! p 10)
   (list (point? p) (point? 5) (point-x p) (point-y p))"
  "(#t #f 10 2)")

(deftest "guard with else" (r7)
  "(import (scheme base))
   (guard (e (#t (list 'caught (error-object-message e) (error-object-irritants e))))
     (error \"boom\" 1 2))"
  "(caught \"boom\" (1 2))")

(deftest "guard with => and re-raise to an outer guard" (r7)
  "(import (scheme base))
   (guard (outer (#t (list 'outer outer)))
     (list (guard (inner ((assq 'a inner) => cdr) ((assq 'b inner)))
             (raise (list (cons 'a 42))))
           (guard (inner ((string? inner) 'string))
             (raise 'not-a-string))))"
  "(outer not-a-string)")

(deftest "guard catches errors from the Lisp underneath" (r7)
  "(import (scheme base))
   (guard (e ((error-object? e) 'caught)) (car 5))"
  "caught")

(deftest "with-exception-handler and raise-continuable" (r7)
  "(import (scheme base))
   (with-exception-handler
     (lambda (c) 42)
     (lambda () (+ (raise-continuable 'oops) 1)))"
  "43")

(deftest "dynamic-wind runs the after thunk when a guard unwinds" (r7)
  "(import (scheme base))
   (define log '())
   (guard (e (#t (set! log (cons 'handler log))))
     (dynamic-wind (lambda () (set! log (cons 'in log)))
                   (lambda () (raise 'x))
                   (lambda () (set! log (cons 'out log)))))
   (reverse log)"
  "(in out handler)")

(deftest "delay / force / delay-force / make-promise" (r7)
  "(import (scheme base) (scheme lazy))
   (define count 0)
   (define p (delay (begin (set! count (+ count 1)) 'v)))
   (force p) (force p)
   (define (loop n) (if (= n 0) (delay 'done) (delay-force (loop (- n 1)))))
   (list count (force (loop 1000)) (force (make-promise 7)) (promise? p) (promise? 7))"
  "(1 done 7 #t #f)")

(deftest "bytevectors" (r7)
  "(import (scheme base))
   (define bv (bytevector 1 2 3))
   (bytevector-u8-set! bv 0 255)
   (list (bytevector-u8-ref bv 0) (bytevector-length bv)
         (bytevector-length (bytevector-append bv (make-bytevector 2 7)))
         (bytevector-u8-ref (bytevector-copy bv 1) 0)
         (bytevector? bv) (bytevector? (vector 1)))"
  "(255 3 5 2 #t #f)")

(deftest "utf8 round trip" (r7)
  "(import (scheme base))
   (let ((bv (string->utf8 \"aλ€\")))
     (list (bytevector-length bv) (utf8->string bv)))"
  "(6 \"aλ€\")")

(deftest "floor/ truncate/ values" (r7)
  "(import (scheme base))
   (let-values (((q r) (floor/ -5 2)) ((tq tr) (truncate/ -5 2)) ((s rem) (exact-integer-sqrt 17)))
     (list q r tq tr s rem))"
  "(-3 1 -2 -1 4 1)")

(deftest "string-map / vector-map / for-each with several sequences" (r7)
  "(import (scheme base) (scheme char))
   (list (string-map char-upcase \"abc\")
         (vector-map + #(1 2 3) #(10 20 30))
         (let ((acc '())) (string-for-each (lambda (a b) (set! acc (cons (list a b) acc))) \"ab\" \"xy\") acc))"
  "(\"ABC\" #(11 22 33) ((#\\b #\\y) (#\\a #\\x)))")

(deftest "vector and string ranges" (r7)
  "(import (scheme base))
   (let ((v (vector 1 2 3 4 5)))
     (list (vector-copy v 1 3) (vector->list v 2) (vector-append #(1) #(2 3))
           (begin (vector-fill! v 0 3) v)
           (string-copy \"hello\" 1 3)))"
  "(#(2 3) (3 4 5) #(1 2 3) #(1 2 3 0 0) \"el\")")

(deftest "n-ary string and char comparison" (r7)
  "(import (scheme base))
   (list (string<? \"a\" \"b\" \"c\") (string=? \"a\" \"a\" \"b\") (char<? #\\a #\\b #\\c))"
  "(#t #f #t)")

(deftest "member / assoc with a predicate" (r7)
  "(import (scheme base))
   (list (member 2.0 '(1 2 3) =) (assoc 2.0 '((1 . a) (2 . b)) =))"
  "((2 3) (2 . b))")

(deftest "exact / inexact / square / exact-integer?" (r7)
  "(import (scheme base) (scheme inexact))
   (list (exact 2.0) (inexact 1/4) (square 5) (exact-integer? 5) (exact-integer? 5.0)
         (nan? 1.0) (finite? 1.0) (infinite? 1.0))"
  "(2 0.25 25 #t #f #f #t #f)")

(deftest "(scheme process-context), (scheme time), features" (r7)
  "(import (scheme base) (scheme time) (scheme process-context))
   (list (list? (command-line)) (exact? (current-jiffy)) (and (memq 'r7rs (features)) #t))"
  "(#t #t #t)")

(deftest "(scheme eval) with environment" (r7)
  "(import (scheme base) (scheme eval))
   (eval '(* 6 7) (environment '(scheme base)))"
  "42")

(deftest "stubbed procedure signals a clear error" (r7)
  "(import (scheme base)) (read-u8 (current-input-port))"
  :error)

;;; ------------------------------------------------------------------
;;; R6RS

(deftest "R6RS library form with version, rename export, program" (r6)
  "(library (acme stack (1 0))
     (export make push! pop! (rename (stack-size size)))
     (import (rnrs base))
     (define (make) (vector '()))
     (define (push! s v) (vector-set! s 0 (cons v (vector-ref s 0))))
     (define (pop! s) (let ((v (car (vector-ref s 0)))) (vector-set! s 0 (cdr (vector-ref s 0))) v))
     (define (stack-size s) (length (vector-ref s 0))))
   (import (rnrs base) (prefix (only (acme stack (1)) make push! size) s:))
   (let ((s (s:make))) (s:push! s 1) (s:push! s 2) (s:size s))"
  "2")

(deftest "R6RS version references" (r6)
  "(library (acme versioned (2 3)) (export v) (import (rnrs base)) (define v 'two-three))
   (import (rnrs base) (acme versioned (or (1) (2 (>= 1)))))
   v"
  "two-three")

(deftest "R6RS version mismatch" (r6)
  "(library (acme versioned2 (2 3)) (export v) (import (rnrs base)) (define v 1))
   (import (rnrs base) (acme versioned2 (3)))
   v"
  :error)

(deftest "R6RS library exporting a syntax-case macro" (r6)
  "(library (acme macros)
     (export my-or swap!)
     (import (rnrs base) (rnrs syntax-case))
     (define-syntax my-or
       (syntax-rules () ((_) #f) ((_ e) e) ((_ e r ...) (let ((t e)) (if t t (my-or r ...))))))
     (define-syntax swap!
       (lambda (x)
         (syntax-case x ()
           ((_ a b) (identifier? (syntax a)) (syntax (let ((tmp a)) (set! a b) (set! b tmp))))))))
   (import (rnrs base) (acme macros))
   (let ((tmp 1) (other 2)) (swap! tmp other) (list tmp other (let ((t 5)) (my-or #f t))))"
  "(2 1 5)")

(deftest "R6RS (rnrs syntax-case): datum->syntax and with-syntax" (r6)
  "(import (rnrs base) (rnrs syntax-case))
   (define-syntax with-it
     (lambda (x)
       (syntax-case x ()
         ((k body) (with-syntax ((it (datum->syntax (syntax k) 'it)))
                     (syntax (let ((it 42)) body)))))))
   (with-it (+ it 1))"
  "43")

(deftest "R6RS div/mod/div0/mod0 against the report's table" (r6)
  "(import (rnrs base))
   (list (div 123 10) (mod 123 10) (div 123 -10) (mod 123 -10)
         (div -123 10) (mod -123 10) (div -123 -10) (mod -123 -10)
         (div0 123 10) (mod0 123 10) (div0 123 -10) (mod0 123 -10)
         (div0 -123 10) (mod0 -123 10) (div0 -123 -10) (mod0 -123 -10))"
  "(12 3 -12 3 -13 7 13 7 12 3 -12 3 -12 -3 12 -3)")

(deftest "R6RS error has a who; assertion-violation; assert" (r6)
  "(import (rnrs base) (rnrs exceptions) (rnrs conditions))
   (define (try thunk)
     (call-with-current-continuation
       (lambda (k)
         (with-exception-handler
           (lambda (c) (k (list (condition-who c) (condition-message c) (condition-irritants c)
                                (assertion-violation? c))))
           thunk))))
   (list (try (lambda () (error 'f \"bad thing\" 1 2)))
         (try (lambda () (assertion-violation 'g \"nope\" 'x)))
         (try (lambda () (assert (= 1 2)))))"
  "((f \"bad thing\" (1 2) #f) (g \"nope\" (x) #t) (assert \"assertion failed\" ((= 1 2)) #t))")

(deftest "R6RS let-values, letrec*, named let, case, do" (r6)
  "(import (rnrs base) (rnrs control))
   (list (let-values (((a b) (values 1 2))) (+ a b))
         (letrec* ((a 1) (b (+ a 1))) (list a b))
         (let loop ((i 0) (acc '())) (if (= i 3) acc (loop (+ i 1) (cons i acc))))
         (case 3 ((1 2) 'low) ((3 4) 'mid) (else 'high))
         (do ((i 0 (+ i 1)) (s 0 (+ s i))) ((= i 4) s)))"
  "(3 (1 2) (2 1 0) mid 6)")

(deftest "R6RS: things (rnrs base) doesn't export are not visible" (r6)
  "(import (rnrs base)) (set-cdr! (list 1) 2)"
  :error)

(deftest "R6RS guard" (r6)
  "(import (rnrs base) (rnrs exceptions) (rnrs conditions))
   (guard (e ((assertion-violation? e) (list 'assertion (condition-message e)))
             ((error? e) (list 'error (condition-who e))))
     (error 'me \"oops\"))"
  "(error me)")

(deftest "SRFI 0: each shipped SRFI is a feature" (r7)
  "(import (scheme base))
   (list (cond-expand (srfi-1 1) (else 2))
         (cond-expand ((and srfi-0 srfi-9) 1) (else 2))
         (cond-expand (srfi-9999 1) (else 2)))"
  "(1 1 2)")

(deftest "SRFI 0: it is an error for no clause to apply" (r7)
  "(import (scheme base)) (cond-expand (no-such-feature 1))"
  :error)

(deftest "SRFI 0 from R6RS, as (srfi :0)" (r6)
  "(import (rnrs) (srfi :0)) (cond-expand ((and r6rs srfi-0) 'yes) (else 'no))"
  "yes")

(deftest "R6RS (define x) with no expression" (r6)
  "(import (rnrs)) (define x) (set! x 5) x"
  "5")

(deftest "with-syntax's body may begin with definitions" (r6)
  "(import (rnrs))
   (define-syntax m
     (lambda (x) (with-syntax ((a 1)) (define b #'2) #`(+ a #,b))))
   (m)"
  "3")

(deftest "(ikarus)" (r6)
  "(import (rnrs) (ikarus))
   (list (add1 1) (fxsub1 3) (port-closed? (current-output-port))
         (environment-symbols (environment '(only (rnrs) car))))"
  "(2 2 #f (car))")

(deftest "(chezscheme) machine-type names the platform" (r6)
  "(import (rnrs) (chezscheme))
   (let ((m (symbol->string (machine-type))))
     (and (char=? (string-ref m 0) #\\t) (> (string-length m) 3)))"
  "#t")

(deftest "(chezscheme) with (rnrs): the shared procedures take Chez's arguments" (r6)
  "(import (rnrs) (rnrs eval) (chezscheme))
   (let ((log '()))
     (dynamic-wind #t (lambda () (set! log (cons 'in log))) (lambda () 'body)
                   (lambda () (set! log (cons 'out log))))
     (list (reverse log) (eval '(+ 1 2) (environment '(rnrs)))))"
  "((in out) 3)")

(deftest "(chezscheme) reader syntax: boxes, fxvectors, length-prefixed vectors" (r6)
  "(import (chezscheme))
   (list (unbox '#&5) (fxvector-ref '#vfx(1 2 3) 2) '#3(a b) (box? (box 1)))"
  "(5 3 #(a b b) #t)")

(deftest "(chezscheme) parameters are set by calling them, per thread" (r6)
  "(import (chezscheme))
   (define p (make-parameter 1))
   (p 2)
   (define seen #f)
   (thread-join (fork-thread (lambda () (let ((before (p))) (p 9) (set! seen (list before (p)))))))
   (list seen (p) (parameterize ((p 3)) (p)))"
  "((2 9) 2 3)")

(deftest "(chezscheme) a body imports a library" (r6)
  "(import (chezscheme))
   (let () (import (only (rnrs lists) fold-left)) (fold-left + 0 '(1 2 3)))"
  "6")

(deftest "(guile): catch and throw, prompts, hash tables, strings" (r6)
  "(import (guile))
   (define h (make-hash-table))
   (hash-set! h \"k\" 1)
   (define-syntax-rule (twice e) (list e e))
   (list (catch 'oops (lambda () (throw 'oops 1 2)) (lambda (key . args) (cons key args)))
         (false-if-exception (error 'x \"bad\"))
         (hash-ref h \"k\") (string-split \"a,b\" #\\,) (twice (1+ 1))
         (% (+ 1 (abort-to-prompt (default-prompt-tag) (lambda (k) (k (k 10)))))))"
  "((oops 1 2) #f 1 (\"a\" \"b\") (2 2) 12)")

(deftest "(chezscheme) format, paths, sort, 1+" (r6)
  "(import (chezscheme))
   (list (format \"~a-~s ~d\" \"x\" \"y\" 42) (path-parent \"a/b/c.ss\") (path-extension \"c.ss\")
         (sort < '(3 1 2)) (1+ 4) (logand 12 10))"
  "(\"x-\\\"y\\\" 42\" \"a/b\" \"ss\" (1 2 3) 5 8)")

(deftest "SRFI 4: homogeneous vectors, literals and write" (r7)
  "(import (scheme base) (scheme write) (srfi 4))
   (let ((p (open-output-string))
         (v '#s16(1 -2 3)))
     (write (list v #f64(0.5) (f32vector 1.5) (u8vector 1 2) (bytevector 7) #c64() #f #t) p)
     (list (get-output-string p)
           (s16vector-ref v 1) (s16vector? v) (vector? v) (u8vector? (bytevector 1))
           (equal? v (s16vector 1 -2 3)) (equal? v (s32vector 1 -2 3))))"
  "(\"(#s16(1 -2 3) #f64(0.5) #f32(1.5) #u8(1 2) #u8(7) #c64() #f #t)\" -2 #t #f #t #t #f)")

(deftest "SRFI 4: elements are checked" (r7)
  "(import (scheme base) (srfi 4)) (s8vector 200)"
  :error)

(deftest "SRFI 160" (r7)
  "(import (scheme base) (srfi 160 u16) (srfi 160 base))
   (list (u16vector->list (u16vector-map (lambda (x) (* x 2)) (u16vector 1 2 3)))
         (u16vector-fold + 0 #u16(1 2 3))
         (u16? 70000)
         (c128vector-ref (c128vector 1+2i) 0))"
  "((2 4 6) 6 #f 1.0+2.0i)")

;; Akku escapes characters in file names: (lib let-optionals*) is
;; lib/let-optionals%2a.sls.
(let ((dir (uiop:ensure-directory-pathname
	    (merge-pathnames (format nil "pseudoscheme-lib-test-~D/" (random 1000000))
			     (uiop:temporary-directory)))))
  (ensure-directories-exist (merge-pathnames "lib/" dir))
  (with-open-file (out (merge-pathnames "lib/let-optionals%2a.sls" dir) :direction :output)
    (write-string "(library (lib let-optionals*) (export lo) (import (rnrs)) (define lo 'found))" out))
  (push (namestring dir) psx:*library-path*)
  (unwind-protect
       (deftest "%xx escapes in library file names" (r6)
	 "(import (rnrs) (lib let-optionals*)) lo"
	 "found")
    (pop psx:*library-path*)
    (uiop:delete-directory-tree dir :validate t)))

(format t "~&library layer / R7RS / R6RS: ~A of ~A tests passed.~%" *passed* *run*)
(uiop:quit (if (= *passed* *run*) 0 1))
