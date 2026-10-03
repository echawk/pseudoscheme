;;; (srfi 64 compat): the little of SRFI 35 and SRFI 48 that the SRFI
;;; 64 implementation uses, written for Pseudoscheme so that SRFI 64
;;; depends on nothing else.
;;;
;;; - format: SRFI 48's (format dest control arg ...) for the
;;;   directives the test runner uses, ~a ~s ~% ~~ (and ~n).  DEST is
;;;   #t (current output), #f (return a string) or a port.
;;; - condition-type?, condition?, condition-has-type?: SRFI 35's
;;;   condition types, used by test-error when its error-type argument
;;;   is a condition type.  There are no SRFI 35 condition types here,
;;;   so condition-type? is always false; test-error still accepts #t
;;;   or a predicate (e.g. error-object? or R6RS assertion-violation?).
;;; - settable-parameter: the implementation sets test-runner-current and
;;;   test-runner-factory by calling them with an argument, as Guile,
;;;   Chibi and others allow (SRFI 39 leaves it open); R7RS parameters
;;;   here do not.  settable-parameter makes a settable cell: called
;;;   with no argument it returns the value, with one it sets it.  The
;;;   implementation never parameterizes them, so it needs no more.
(define-library (srfi 64 compat)
  (export format condition-type? condition? condition-has-type?
          settable-parameter)
  (import (except (scheme base) make-parameter) (scheme write))
  (begin
    (define (format dest control . args)
      (define (emit port)
        (let ((n (string-length control)))
          (let loop ((i 0) (args args))
            (cond ((>= i n) #f)
                  ((and (char=? (string-ref control i) #\~) (< (+ i 1) n))
                   (let ((c (string-ref control (+ i 1))))
                     (case c
                       ((#\a #\A) (display (car args) port) (loop (+ i 2) (cdr args)))
                       ((#\s #\S) (write (car args) port) (loop (+ i 2) (cdr args)))
                       ((#\% #\n #\N) (newline port) (loop (+ i 2) args))
                       ((#\~) (write-char #\~ port) (loop (+ i 2) args))
                       (else (error "format: unsupported directive" c control)))))
                  (else (write-char (string-ref control i) port)
                        (loop (+ i 1) args))))))
      (cond ((not dest)
             (let ((p (open-output-string))) (emit p) (get-output-string p)))
            ((eq? dest #t) (emit (current-output-port)))
            (else (emit dest))))
    (define (settable-parameter value . converter)
      (let ((convert (if (pair? converter) (car converter) (lambda (x) x))))
        (set! value (convert value))
        (lambda args
          (if (null? args)
              value
              (set! value (convert (car args)))))))
    (define (condition-type? x) #f)
    (define (condition? x) #f)
    (define (condition-has-type? c type) #f)))
