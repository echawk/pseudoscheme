;;; Tests for SRFI 110: examples of the SRFI's, read with sweet-read.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 110))
(define (sweet s) (sweet-read (open-input-string s)))
(test-begin "srfi-110")

(test-equal '(define (fibfast n) (if (< n 2) n (fibup n 2 1 0)))
  (sweet "define fibfast(n)\n  if {n < 2}\n    n\n    fibup n 2 1 0\n\n"))
(test-equal '(define (fac x) (if (<= x 1) 1 (* x (fac (- x 1)))))
  (sweet "define fac(x)\n  if {x <= 1} 1 {x * fac{x - 1}}\n\n"))
;; a line with one datum is that datum; a blank line ends the expression
(test-equal 'x (sweet "x\n\n"))
(test-equal '(a b c) (sweet "a b c\n\n"))
;; \\ splits a line; $ makes the rest a sublist
(test-equal '(let ((x 1)) (* x 2))
  (sweet "let\n  \\\\\n    x 1\n  * x 2\n\n"))
(test-equal '(a (b c)) (sweet "a $ b c\n\n"))
;; curly infix and neoteric expressions
(test-equal '(+ 1 2) (sweet "{1 + 2}\n\n"))
(test-equal '(f (g x)) (sweet "f g(x)\n\n"))
(test-equal '(f x) (neoteric-read (open-input-string "f(x)")))
(test-equal '(* a (+ b c)) (curly-infix-read (open-input-string "{a * {b + c}}")))
;; a traditional s-expression reads as itself
(test-equal '(define (g y) (* y y)) (sweet "(define (g y) (* y y))\n"))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-110")
  (exit (if (zero? failures) 0 1)))
