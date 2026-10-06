;;; Tests for SRFI 112.  The SRFI standardizes no values, only that each
;;; procedure returns a string or #f; the implementation's name and
;;; version are Pseudoscheme's.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 112))

(define (string-or-false? x) (or (string? x) (not x)))

(test-begin "srfi-112")
(test-equal "pseudoscheme" (implementation-name))
(test-equal "3.0" (implementation-version))
(test-assert (string-or-false? (cpu-architecture)))
(test-assert (string-or-false? (machine-name)))
(test-assert (string-or-false? (os-name)))
(test-assert (string-or-false? (os-version)))
;; SBCL answers all four on the platforms it runs on.
(test-assert (string? (cpu-architecture)))
(test-assert (string? (os-name)))
(test-assert (> (string-length (os-name)) 0))
(test-equal (os-name) (os-name))
(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-112")
  (exit (if (zero? failures) 0 1)))
