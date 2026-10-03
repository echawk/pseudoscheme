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

;;; 1. Import Scheme libraries into this package, with R7RS import sets.
;;; Procedures become functions, syntax becomes Lisp macros.  Names that
;;; would redefine a Common Lisp symbol (SRFI 1's FIND, REMOVE,
;;; DELETE-DUPLICATES...) are an error, so pick with ONLY, or PREFIX or
;;; RENAME them.  Booleans are converted: #f comes back as NIL, and a
;;; Lisp function passed in is treated as a predicate -- #'evenp works.

(r7rs:import (only (srfi 1) fold filter partition iota)
             (rename (only (srfi 1) delete-duplicates) (delete-duplicates dedup))
             (prefix (srfi 13) str-)
             (srfi 26)                  ; cut and cute: Scheme macros
             (srfi 2))                  ; and-let*

(show (fold #'+ 0 '(1 2 3 4)))
(show (filter #'evenp (iota 10)))
(show (partition #'evenp '(1 2 3 4)))            ; two values
(show (dedup '(a b a c b)))
(show (str-string-pad "42" 6 #\0))

;;; 2. Scheme's hygienic macros, used from Lisp.  Their arguments are
;;; Lisp code: Lisp variables, functions and quoted data mean what they
;;; mean in Lisp.  The Scheme macro expands (hygienically) and becomes
;;; ordinary Lisp code, compiled with the rest of the file.

(show (mapcar (cut * 2 <>) '(1 2 3)))
(let ((n 10))
  (show (mapcar (cut + n <>) '(1 2 3))))          ; a Lisp lexical variable
(show (funcall (cut format nil "~a and ~a" <> <>) "this" "that"))
(let ((table '((apple . 3) (pear . 5))))
  (show (and-let* ((entry (assoc 'pear table))
                   (count (cdr entry)))
          (* count 100))))
(show (macroexpand-1 '(cut list 1 <>)))

;;; 3. Define in Scheme, call from Lisp.  R7RS:DEFINE defines at the
;;; Scheme REPL and binds the Lisp function of the same name.

(r7rs:define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
(show (fact 20))
(show (mapcar #'fact '(1 2 3 4 5)))

(r7rs:define (collatz-steps n)
  (let loop ((n n) (steps 0))
    (cond ((= n 1) steps)
          ((even? n) (loop (quotient n 2) (+ steps 1)))
          (else (loop (+ (* 3 n) 1) (+ steps 1))))))
(show (loop for i from 1 to 10 collect (collatz-steps i)))

;;; 4. A Scheme library written in this file, imported like any other.

(r7rs:define-library (demo geometry)
  (export make-point point-x point-y distance)
  (import (scheme base) (scheme inexact))
  (begin
    (define-record-type point (make-point x y) point? (x point-x) (y point-y))
    (define (distance p q)
      (sqrt (+ (square (- (point-x p) (point-x q)))
               (square (- (point-y p) (point-y q))))))))

(r7rs:import (demo geometry))
(show (distance (make-point 0 0) (make-point 3 4)))

;;; 5. Expressions.  R7RS:SCHEME evaluates Scheme written as Lisp data
;;; (R7RS:FALSE is #f); R7RS:EVAL takes text when you need Scheme's
;;; reader (#\a, #(...), exact #f), and returns values unconverted.

(show (r7rs:scheme (let-values (((q r) (floor/ 17 5))) (list q r))))
(show (r7rs:scheme (if r7rs:false 'yes 'no)))
(show (r7rs:eval "(vector-map char-upcase #(#\\a #\\b))"))

;;; 6. Looking inside: what Scheme expands to, and the Lisp it becomes.

(show (r7rs:expand '(let loop ((i 0)) (if (< i 10) (loop (+ i 1)) i))))
(show (r7rs:translate '(lambda (x) (* x x))))

;;; 7. Errors: an uncaught Scheme raise is a Lisp error.

(show (handler-case (r7rs:eval "(error \"something broke\" 42)")
        (error (e) (format nil "caught: ~A" e))))

;;; 8. The other standards have the same interface.

(r6rs:import (rename (only (rnrs sorting) list-sort) (list-sort r6rs-sort)))
(show (r6rs-sort #'< '(3 1 2)))
(r5rs:define (twice f) (lambda (x) (f (f x))))
(show (funcall (twice (lambda (x) (* x 3))) 7))
