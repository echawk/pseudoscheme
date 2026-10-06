;;; Tests for SRFI 173: the sample implementation's tests
;;; (reference/srfi-173/tests.scm, for (chibi test)), included with a
;;; chibi-style test macro, plus a few from the SRFI document.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 173))

(define-syntax test
  (syntax-rules ()
    ((_ expected expr) (test-equal expected expr))))
;; tests.scm starts with its own import form; skip it.
(define-syntax import
  (syntax-rules () ((_ . _) (begin))))

(test-begin "srfi-173")
(include "../reference/srfi-173/tests.scm")

(test-assert (hook? (make-hook 1)))
(test-assert (not (hook? (lambda () #f))))
;; The order in which hook-run calls the procedures is unspecified.
(test-equal 3
  (let* ((acc '())
         (hook (make-hook 1)))
    (hook-add! hook (lambda (x) (set! acc (cons x acc))))
    (hook-add! hook (lambda (x) (set! acc (cons (+ x 1) acc))))
    (hook-run hook 1)
    (apply + acc)))
(test-error #t (hook-run (make-hook 1)))
(test-equal 1 (let ((hook (make-hook 0)) (p (lambda () 1)) (q (lambda () 2)))
                (hook-add! hook p)
                (hook-add! hook q)
                (hook-delete! hook q)
                (length (hook->list hook))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-173")
  (exit (if (zero? failures) 0 1)))
