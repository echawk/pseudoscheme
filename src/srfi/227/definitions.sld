;;; SRFI 227's (srfi 227 definitions): define-optionals and
;;; define-optionals*, as in the SRFI's R6RS library of that name.
(define-library (srfi 227 definitions)
  (export define-optionals define-optionals*)
  (import (scheme base) (srfi 227))
  (begin
    (define-syntax define-optionals
      (syntax-rules ()
        ((_ (name . opt-formals) body1 ... body2)
         (define name (opt-lambda opt-formals body1 ... body2)))))
    (define-syntax define-optionals*
      (syntax-rules ()
        ((_ (name . opt-formals) body1 ... body2)
         (define name (opt*-lambda opt-formals body1 ... body2)))))))
