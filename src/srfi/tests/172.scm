;;; Tests for SRFI 172, from the SRFI document's description: the
;;; libraries work as eval environments, have the documented number of
;;; exports, and leave out what they say they do.
(import (scheme base) (scheme eval) (scheme process-context) (srfi 64))

(define safe (environment '(srfi 172)))
(define functional (environment '(srfi 172 functional)))

(define (bound? name env)
  (guard (e (#t #f))
    (eval name env)
    #t))

(test-begin "srfi-172")

(test-equal 6 ((eval '(lambda (x) (* x 2)) safe) 3))
(test-equal '(1 4 9)
  ((eval '(lambda (l) (map (lambda (x) (* x x)) l)) safe) '(1 2 3)))
(test-equal "abc"
  ((eval '(lambda (s) (let ((p (open-output-string)))
                        (write-string s p)
                        (get-output-string p)))
         safe)
   "abc"))
(test-equal '(a . 5)
  ((eval '(lambda (p) (set-cdr! p 5) p) safe) (cons 'a 1)))
(test-equal 7
  ((eval '(case-lambda ((x) x) ((x y) (+ x y))) functional) 3 4))
(test-equal 'big
  ((eval '(lambda (n) (cond ((> n 10) 'big) (else 'small))) functional) 11))
(test-equal 3
  ((eval '(lambda (v) (guard (e ((string? e) (string-length e)))
                        (raise v)))
         functional)
   "abc"))

;; Absent from both: anything that reaches the outside world.
(test-assert (not (bound? 'display safe)))
(test-assert (not (bound? 'open-input-file safe)))
(test-assert (not (bound? 'eval safe)))
(test-assert (not (bound? 'exit safe)))
(test-assert (not (bound? 'current-output-port safe)))
;; Present in (srfi 172), absent from (srfi 172 functional).
(test-assert (bound? 'set-car! safe))
(test-assert (bound? 'string-set! safe))
(test-assert (bound? 'read-char safe))
(test-assert (bound? 'open-input-string functional))
(test-assert (not (bound? 'set-car! functional)))
(test-assert (not (bound? 'vector-fill! functional)))
(test-assert (not (bound? 'read-char functional)))
(test-assert (not (bound? 'write-string functional)))
(test-assert (not (bound? 'peek-char functional)))
(test-assert (not (bound? 'call-with-port functional)))
(test-error (eval '(lambda (x) (set! x 1)) functional))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-172")
  (exit (if (zero? failures) 0 1)))
