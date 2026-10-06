;;; Tests for SRFI 149: the SRFI's examples, and more.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 149))
(test-begin "srfi-149")

(test-equal '(1 2 3 4 5 6)
  (let-syntax
      ((my-append
        (syntax-rules ()
          ((my-append (a ...) ...) '(a ... ...)))))
    (my-append (1 2 3) (4 5 6))))

(test-equal '(((bar 1) (bar 2)) ((baz 3) (baz 4)))
  (let-syntax
      ((foo
        (syntax-rules ()
          ((foo (a b ...) ...) '(((a b) ...) ...)))))
    (foo (bar 1 2) (baz 3 4))))

;; a lifted rule: the transformer applied to each element
(define-syntax pairs
  (syntax-rules ()
    ((_ (k v ...) ...) '((k . v) ... ...))))
(test-equal '((a . 1) (a . 2) (b . 3)) (pairs (a 1 2) (b 3)))

;; a variable of depth 0 under ellipses is repeated (R7RS already)
(define-syntax tag-all
  (syntax-rules ()
    ((_ t (x ...) ...) '((t x ...) ...))))
(test-equal '((z 1 2) (z 3)) (tag-all z (1 2) (3)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-149")
  (exit (if (zero? failures) 0 1)))
