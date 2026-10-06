;;; Tests for SRFI 162: the sample implementation's shared tests
;;; (reference/srfi-162/shared-tests.scm, written for (chibi test)),
;;; which cover SRFI 128 as well, run under SRFI 64.
(import (scheme base) (scheme char) (scheme inexact)
        (scheme process-context)
        (srfi 64) (srfi 162))

;; (chibi test) compatibility for shared-tests.scm
(define-syntax test
  (syntax-rules ()
    ((_ expected expr) (test-equal expected expr))
    ((_ name expected expr) (test-equal name expected expr))))
(define (test-exit) #f)
(define exact->inexact inexact)

(test-begin "srfi-162")
(include "../reference/srfi-162/shared-tests.scm")

;; pair-comparator, from the SRFI document
(test-assert (<? pair-comparator '(1 . 2) '(1 . 3)))
(test-assert (=? pair-comparator '(a . "b") '(a . "b")))
(test-equal "c" (comparator-max string-comparator "a" "c" "b"))
(test-equal #\a (comparator-min char-ci-comparator #\B #\a))
(test-assert (<? boolean-comparator #f #t))
(test-assert (<? list-comparator '(1 2) '(1 3)))
(test-assert (<? vector-comparator '#(1 2) '#(1 2 3)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-162")
  (exit (if (zero? failures) 0 1)))
