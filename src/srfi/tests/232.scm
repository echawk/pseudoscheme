;;; Tests for SRFI 232: the sample implementation's test suite
;;; (reference/srfi-232/test-body.scm), included unmodified, plus a few
;;; more.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 232))

(test-begin "srfi-232")
(include "../reference/srfi-232/test-body.scm")

(define-curried (add3 a b c) (+ a b c))
(test-group "define-curried"
  (test-eqv 6 (add3 1 2 3))
  (test-eqv 6 (((add3 1) 2) 3))
  (test-eqv 6 ((add3 1 2) 3))
  (test-eq add3 (add3)))
(test-group "Plain formals"
  (test-equal '(1 2) ((curried args args) 1 2))
  (test-equal "nullary: the body itself" 'x (curried () 'x)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-232")
  (exit (if (zero? failures) 0 1)))
