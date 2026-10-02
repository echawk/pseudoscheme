;;; A small stand-in for chibi's (chibi test), just enough for
;;; tests/chibi/r7rs-tests.scm: TEST (with or without a name), TEST-VALUES,
;;; TEST-ASSERT, TEST-ERROR, TEST-BEGIN/TEST-END.  Failures are printed;
;;; (TEST-RESULTS) returns (run . passed) for the runner.

(define-library (chibi test)
  (export test test-values test-assert test-error test-begin test-end
          test-results)
  (import (scheme base) (scheme write))
  (begin
    (define *run* 0)
    (define *passed* 0)

    (define (test-results) (cons *run* *passed*))

    (define (test-begin . name) #t)
    (define (test-end . name) #t)

    (define (close-enough? a b)
      (or (equal? a b)
          (and (real? a) (real? b) (inexact? a) (inexact? b)
               (< (abs (- a b)) (* 1e-8 (max 1 (abs a) (abs b)))))
          (and (pair? a) (pair? b)
               (close-enough? (car a) (car b))
               (close-enough? (cdr a) (cdr b)))))

    (define (record! ok? name expected got expr)
      (set! *run* (+ *run* 1))
      (if ok?
          (set! *passed* (+ *passed* 1))
          (begin
            (display "FAIL: ")
            (write expr)
            (display ": expected ")
            (write expected)
            (display " but got ")
            (write got)
            (newline))))

    (define (run-test name expected thunk expr)
      (let ((got (guard (e (#t (list 'exception
                                     (if (error-object? e) (error-object-message e) e))))
                   (thunk))))
        (record! (close-enough? expected got) name expected got expr)))

    (define-syntax test
      (syntax-rules ()
        ((_ name expected expr)
         (run-test name expected (lambda () expr) 'expr))
        ((_ expected expr)
         (run-test #f expected (lambda () expr) 'expr))))

    (define-syntax test-values
      (syntax-rules ()
        ((_ expected expr)
         (run-test #f (call-with-values (lambda () expected) list)
                   (lambda () (call-with-values (lambda () expr) list))
                   'expr))))

    (define-syntax test-assert
      (syntax-rules ()
        ((_ expr) (run-test #f #t (lambda () (if expr #t #f)) 'expr))
        ((_ name expr) (run-test name #t (lambda () (if expr #t #f)) 'expr))))

    (define-syntax test-error
      (syntax-rules ()
        ((_ expr)
         (record! (guard (e (#t #t)) expr #f) #f #t #f 'expr))
        ((_ name expr)
         (record! (guard (e (#t #t)) expr #f) name #t #f 'expr))))))
