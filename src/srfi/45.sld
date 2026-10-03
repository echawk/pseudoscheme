;;; SRFI 45: lazy evaluation: lazy is R7RS's delay-force, eager its
;;; make-promise.
(define-library (srfi 45)
  (export delay lazy force eager)
  (import (scheme base) (scheme lazy))
  (begin
    (define-syntax lazy
      (syntax-rules () ((_ e) (delay-force e))))
    (define (eager x) (make-promise x))))
