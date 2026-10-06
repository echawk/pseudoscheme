;;; (srfi private srfi-201-syntax): syntax->list, for the fenders of
;;; (srfi 201)'s macros.  The sample implementation applies SRFI 1's
;;; every to syntax objects for lists, assuming they are lists; psyntax
;;; may wrap them, as R6RS allows.
(define-library (srfi private srfi-201-syntax)
  (export syntax->list)
  (import (scheme base)
          (only (rnrs syntax-case) syntax-case syntax))
  (begin
    (define (syntax->list s)
      (syntax-case s ()
        ((a . b) (cons #'a (syntax->list #'b)))
        (_ '())))))
