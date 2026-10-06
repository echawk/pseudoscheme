;;; SRFI 211: old-style Lisp macros.  Written for Pseudoscheme (the SRFI's implementation is
;;; necessarily per-implementation); see 211.sld.
;;; A Lisp transformer gets the datum of the macro use and its result is
;;; injected (datum->syntax with the use's keyword): not hygienic.
(define-library (srfi 211 define-macro)
  (export define-macro lisp-transformer)
  (import (scheme base)
          (srfi private srfi-211-transformers))
  (begin
    (define-syntax define-macro
      (syntax-rules ()
        ((_ (keyword . formals) body1 body2 ...)
         (define-syntax keyword
           (lisp-transformer
            (lambda (exp)
              (apply (lambda formals body1 body2 ...) (cdr exp))))))
        ((_ keyword transformer)
         (define-syntax keyword (lisp-transformer transformer)))))))
