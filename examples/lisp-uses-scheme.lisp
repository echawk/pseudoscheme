;;; Common Lisp using Scheme.
;;;
;;;   sbcl --dynamic-space-size 4GB --control-stack-size 500MB \
;;;        --load examples/lisp-uses-scheme.lisp
;;;
;;; (asdf:load-system :r7rs) -- or :r6rs, :r5rs, all the same system,
;;; pseudoscheme/api -- gives the packages R7RS, R6RS and R5RS.

(require :asdf)
(asdf:load-system :r7rs)

(defpackage "LISP-USES-SCHEME" (:use "COMMON-LISP"))
(in-package "LISP-USES-SCHEME")

(defmacro show (form)
  `(format t "~&~S~%  => ~{~S~^, ~}~%" ',form (multiple-value-list ,form)))

;;; 1. Scheme libraries as Lisp packages.  USE-LIBRARY makes a package
;;; (named after the library unless :PACKAGE says otherwise) whose
;;; functions are the library's procedures.  Booleans are converted:
;;; #f comes back as NIL, and a Lisp function passed in is treated as
;;; a predicate (its NIL means #f) -- so #'evenp just works.

(r7rs:use-library '(srfi 1))                       ; package SRFI-1
(show (srfi-1:fold #'+ 0 '(1 2 3 4)))
(show (srfi-1:filter #'evenp '(1 2 3 4 5 6)))
(show (srfi-1:partition #'evenp '(1 2 3 4)))      ; two values
(show (srfi-1:delete-duplicates '(a b a c b)))
(show (srfi-1:any #'stringp '(1 2 3)))             ; NIL, not #f

(r7rs:use-library '(srfi 13) :package "STR")
(show (str:string-pad "42" 6 #\0))
(show (str:string-join '("a" "b" "c") ", "))

;;; 2. Your own Scheme library, defined in Scheme and used from Lisp.

(r7rs:eval "
(define-library (demo geometry)
  (export make-point point-x point-y distance)
  (import (scheme base) (scheme inexact))
  (begin
    (define-record-type point (make-point x y) point? (x point-x) (y point-y))
    (define (distance p q)
      (sqrt (+ (square (- (point-x p) (point-x q)))
               (square (- (point-y p) (point-y q))))))))")

(r7rs:use-library '(demo geometry) :package "GEOMETRY")
(show (geometry:distance (geometry:make-point 0 0) (geometry:make-point 3 4)))

;;; 3. Evaluating Scheme.  R7RS:SCHEME takes Scheme written as Lisp data
;;; (CL's FOO is Scheme's foo) and returns Lisp-style values;
;;; R7RS:EVAL takes text (for #f, #\a, ... exactly) and returns Scheme
;;; values as they are.

(show (r7rs:scheme (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
                   (fact 20)))
(show (r7rs:eval "(let-values (((q r) (floor/ 17 5))) (list q r))"))
(show (r7rs:true-p (r7rs:eval "(string=? \"a\" \"b\")")))

;;; 4. Scheme procedures as Lisp functions.

(let ((fact (r7rs:procedure "fact")))
  (show (mapcar fact '(1 2 3 4 5))))
(show (funcall (r7rs:procedure "string-index" :library '(srfi 13)) "hello" #\l))

;;; 5. Looking inside: what Scheme expands to, and the Lisp it becomes.

(show (r7rs:expand '(let loop ((i 0)) (if (< i 10) (loop (+ i 1)) i))))
(show (r7rs:translate '(lambda (x) (* x x))))

;;; 6. Errors: an uncaught Scheme raise is a Lisp error.

(show (handler-case (r7rs:eval "(error \"something broke\" 42)")
        (error (e) (format nil "caught: ~A" e))))

;;; 7. The other standards.

(show (r6rs:scheme (import (rnrs sorting)) (list-sort < '(3 1 2))))
(show (r5rs:scheme (define (twice f) (lambda (x) (f (f x))))
                   ((twice (lambda (x) (* x 3))) 7)))
