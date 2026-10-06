;;; Tests for SRFI 7, from the SRFI document's grammar and semantics.
;;; A program runs in a top level of its own, so the tests look at the
;;; value of its last form.
(import (scheme base) (scheme file) (scheme write) (scheme process-context)
        (srfi 64) (srfi 7))

(test-begin "srfi-7")

;; process-program, the reference procedure
(test-equal '((define x 1) (display x))
  (process-program '(program (requires srfi-1) (code (define x 1) (display x)))
                   '(srfi-1)))
(test-equal #f
  (process-program '(program (requires srfi-1 srfi-2) (code 1)) '(srfi-1)))
(test-equal '(b)
  (process-program '(program (feature-cond (srfi-99 (code a))
                                           ((or srfi-99 srfi-1) (code b))
                                           (else (code c))))
                   '(srfi-1)))
(test-equal '(c)
  (process-program '(program (feature-cond ((and srfi-1 (not srfi-2)) (code a))
                                           (else (code c))))
                   '(srfi-1 srfi-2)))
(test-equal '(a)
  (process-program '(program (feature-cond ((and) (code a)))) '()))
(test-equal #f
  (process-program '(program (feature-cond ((or) (code a)))) '()))

;; program: code runs, in order, with interleaved definitions
(test-equal 3
  (program (code (define x 1))
           (code (define y (+ x 1)))
           (code (+ x y))))
;; requires makes the SRFI's library available, without an import
(test-equal '(0 1 2)
  (program (requires srfi-1)
           (code (iota 3))))
(test-equal '(1 2)
  (program (requires srfi-8 srfi-1)
           (code (receive (a . b) (apply values (iota 3)) b))))
;; and its bindings win over R7RS's: SRFI 13's string-map
(test-equal "BC"
  (program (requires srfi-13)
           (code (string-map char-upcase "abcd" 1 3))))
;; an unsatisfiable requirement is an error
(test-error (program (requires srfi-99999) (code 1)))
(test-error (program (feature-cond (srfi-99999 (code 1)))))
;; feature-cond chooses the first satisfied clause, and its features'
;; libraries are imported
(test-equal 'r7rs
  (program (feature-cond (no-such-feature (code 'none))
                         ((and r7rs (not no-such-feature)) (code 'r7rs))
                         (else (code 'else)))))
(test-equal 'else
  (program (feature-cond (no-such-feature (code 'none))
                         (else (code 'else)))))
(test-equal 15
  (program (feature-cond (srfi-1 (code (fold + 0 '(1 2 3 4 5))))
                         (else (code 'no-srfi-1)))))
;; macros defined in the code
(test-equal '(2 1)
  (program (code (define-syntax swap
                   (syntax-rules () ((_ a b) (list b a)))))
           (code (swap 1 2))))
;; files, read when the program runs
(define file "srfi-7-test-file.scm")
(call-with-output-file file
  (lambda (out) (write '(define z 40) out) (write '(define w 2) out)))
(test-equal 42
  (program (files "srfi-7-test-file.scm")
           (code (+ z w))))
;; load-program
(define prog-file "srfi-7-test-program.scm")
(call-with-output-file prog-file
  (lambda (out)
    (write '(program (requires srfi-1)
                     (files "srfi-7-test-file.scm")
                     (code (last (list z w))))
           out)))
(test-equal 2 (load-program prog-file))
(delete-file file)
(delete-file prog-file)

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-7")
  (exit (if (zero? failures) 0 1)))
