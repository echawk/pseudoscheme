;;; SRFI 161 tests: the SRFI's own (srfi 161 test), inlined.
;;; Copyright (C) Marc Nieper-Wißkirchen (2018); MIT licence.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 161))

(test-begin "srfi-161")

(define a (ubox 'a))
(define b (ubox 'b))
(define c (ubox 'c))
(define d (ubox 'd))
(define e (ubox 'e))
(define f (ubox 'f))

(test-assert (ubox? (ubox 'g)))
(test-assert (not (ubox? (vector 'h))))
(test-assert (not (ubox=? a b)))
(test-eq 'a (ubox-ref a))

(ubox-link! a b)
(ubox-union! a c)
(ubox-unify! cons d e)
(ubox-link! b f)

(test-assert (ubox=? a b))
(test-assert (ubox=? b c))
(test-assert (ubox=? c f))
(test-assert (ubox=? a f))
(test-assert (ubox=? d e))
(test-assert (not (ubox=? a e)))

(test-eq (ubox-ref a) 'f)
(test-equal (ubox-ref d) '(d . e))

(ubox-set! b 'i)
(test-eq (ubox-ref a) 'i)

(ubox-link! a e)
(test-assert (ubox=? c e))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-161")
  (exit (if (zero? failures) 0 1)))
