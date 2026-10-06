;;; Tests for SRFI 251: the SRFI's examples, the output captured.
(import (scheme base) (scheme write) (scheme process-context) (srfi 64) (srfi 251))
(define-syntax output-of
  (syntax-rules ()
    ((_ body ...) (parameterize ((current-output-port (open-output-string)))
                    body ... (get-output-string (current-output-port))))))
(test-begin "srfi-251")

(test-equal "the result is: 42"
  (output-of
   (let ((x 0))
     (display "the result is")
     (define (foo) x)
     (define x 42)
     (display ": ")
     (display (foo)))))
(test-equal "the result is: 0"
  (output-of
   (let ((x 0))
     (display "the result is")
     (define (foo) x)
     (display ": ")
     (define xx 42)
     (display (foo)))))
(test-equal "the result is: 0"
  (output-of
   (let ((x 0))
     (define-syntax define-thunk
       (syntax-rules ()
         ((_ i v) (define (i) v))))
     (display "the result is")
     (display ": ")
     (define xx 42)
     (define-thunk foo x)
     (display (foo)))))

(define (double-square x)
  (unless (number? x)
    (error "double-square: not a number" x))
  (define y (* x x))
  (* 2 y))
(test-equal 18 (double-square 3))
(test-error (double-square 'a))
(test-equal '(1 2 3)
  (let ()
    (define a 1)
    (set! a (+ a 0))
    (define b (+ a 1))
    (begin (define c (+ b 1)))
    (list a b c)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-251")
  (exit (if (zero? failures) 0 1)))
