;;; Tests for SRFI 10, from the SRFI's examples.  A constructor applies to
;;; what is read after it is defined, so the data are read with read.
(import (scheme base) (scheme read) (scheme process-context) (srfi 64) (srfi 10))
(define (r s) (read (open-input-string s)))
(test-begin "srfi-10")

(define-reader-ctor 'list list)
(test-equal '(1 2) (r "#,(list 1 2)"))
(define-reader-ctor 'ratio /)
(test-eqv 3/4 (r "#,(ratio 3 4)"))
(define-reader-ctor 'my-vector (lambda args (apply vector (cons 'my-vector-tag args))))
(test-equal #(my-vector-tag 0 1 2) (r "#,(my-vector 0 1 2)"))
(define-reader-ctor 'list->vector list->vector)
(test-equal '(a #(1 2)) (r "(a #,(list->vector (1 2)))"))
(test-equal "a tag with no constructor is R6RS's unsyntax"
  '(unsyntax (no-such-tag 1)) (r "#,(no-such-tag 1)"))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-10")
  (exit (if (zero? failures) 0 1)))
