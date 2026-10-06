;;; Tests for SRFI 49: the SRFI's examples, read with i-expression-read.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 49))
(define (i s) (i-expression-read (open-input-string s)))
(test-begin "srfi-49")

(test-equal '(define (fac x) (if (= x 0) 1 (* x (fac (- x 1)))))
  (i "define\n fac x\n if\n  = x 0\n  1\n  * x\n    fac\n     - x 1\n"))
(test-equal '(let ((foo (+ 1 2)) (bar (+ 3 4))) (+ foo bar))
  (i "let\n group\n  foo\n   + 1 2\n  bar\n   + 3 4\n + foo bar\n"))
(test-equal '(define (fac x) (if (= x 0) 1 (* x (fac (- x 1)))))
  (i "define (fac x)\n if (= x 0) 1\n  * x\n   fac (- x 1)\n"))
(test-equal '(let ((foo (+ 1 2)) (bar (+ 3 4))) (+ foo bar))
  (i "let\n group\n  foo (+ 1 2)\n  bar (+ 3 4)\n + foo bar\n"))
(test-equal '(quote (a b)) (i "' a b\n"))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-49")
  (exit (if (zero? failures) 0 1)))
