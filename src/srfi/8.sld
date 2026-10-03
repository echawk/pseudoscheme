;;; SRFI 8: receive.
(define-library (srfi 8)
  (export receive)
  (import (scheme base))
  (begin
    (define-syntax receive
      (syntax-rules ()
        ((_ formals expr body1 body2 ...)
         (call-with-values (lambda () expr) (lambda formals body1 body2 ...)))))))
