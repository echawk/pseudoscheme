;;; SRFI 145: assume.
(define-library (srfi 145)
  (export assume)
  (import (scheme base))
  (begin
    (define-syntax assume
      (syntax-rules ()
        ((_ expression message ...)
         (or expression (error "invalid assumption" 'expression message ...)))))))
