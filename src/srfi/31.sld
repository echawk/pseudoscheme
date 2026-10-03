;;; SRFI 31: rec.
(define-library (srfi 31)
  (export rec)
  (import (scheme base))
  (begin
    (define-syntax rec
      (syntax-rules ()
        ((_ (name . variables) . body)
         (letrec ((name (lambda variables . body))) name))
        ((_ name expression)
         (letrec ((name expression)) name))))))
