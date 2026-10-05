;;; SRFI 227: optional arguments.  Daphne Preston-Kendal's R7RS sample
;;; implementation (on case-lambda), unmodified
;;; (reference/srfi-227/lambda-optional.scm; MIT licence, per the SRFI
;;; document, in reference/srfi-227/LICENSE).  The library form follows
;;; the shipped lib/srfi/227.sld.  define-optionals and define-optionals*
;;; are in (srfi 227 definitions), as the SRFI specifies.
(define-library (srfi 227)
  (export (rename lambda-optional opt-lambda)
          (rename lambda-optional* opt*-lambda)
          let-optionals
          let-optionals*)
  (import (scheme base)
          (scheme case-lambda))
  (include "reference/srfi-227/lambda-optional.scm")
  (begin
    (define-syntax let-optionals
      (syntax-rules ()
        ((_ expr opt-formals body1 ... body2)
         (apply (lambda-optional opt-formals body1 ... body2) expr))))
    (define-syntax let-optionals*
      (syntax-rules ()
        ((_ expr opt-formals body1 ... body2)
         (apply (lambda-optional* opt-formals body1 ... body2) expr))))))
