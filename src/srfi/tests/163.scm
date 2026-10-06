;;; Tests for SRFI 163 and SRFI 58's notation: the SRFI's examples.
(import (scheme base) (scheme read) (scheme process-context) (srfi 64)
        (srfi 163) (srfi 164))
(define (r s) (read (open-input-string s)))
(test-begin "srfi-163")

(define m #2a((11 12 13) (21 22 23)))
(test-assert (array? m))
(test-eqv 2 (array-rank m))
(test-eqv 12 (array-ref m 0 1))
(test-eqv 23 (array-ref m 1 2))
(test-equal "#2a((11 12 13) (21 22 23))" (format-array m))

(define u #2u32@2@3((1 2) (2 3)))
(test-eqv 2 (array-start u 0))
(test-eqv 3 (array-start u 1))
(test-eqv 3 (array-ref u 3 4))
(test-equal "#2a@2:2@3:2((1 2) (2 3))" (format-array u))

(define z (r "#0a sym"))
(test-eqv 0 (array-rank z))
(test-eq 'sym (array-ref z))
(test-equal "#0a sym" (format-array z))
(test-eqv 237.0 (array-ref (r "#0f32 237.0")))

(test-eqv 0 (array-end (r "#2a:0:2()") 0))
(test-eqv 2 (array-end (r "#2a:0:2()") 1))
(test-eqv 0 (array-end (r "#2a:2:0(() ())") 1))
(test-eqv 3 (array-end (r "#3a:2:0:3(() ())") 2))
(test-equal "#2a((11 12 13) (21 22 23))" (format-array (r "#2a:2:3((11 12 13) (21 22 23))")))
(test-eqv 4 (array-ref (r "#2a@1:2@1:2((1 2) (3 4))") 2 2))

;; SRFI 58's notation
(test-eqv 4 (array-ref (r "#2A((1 2) (3 4))") 1 1))
(test-eqv 2.5 (array-ref (r "#1A:floR64b(1.5 2.5)") 1))

(test-error (r "#2a((1 2) (3))"))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-163")
  (exit (if (zero? failures) 0 1)))
