;;; SRFI 61: cond with guards, (test guard => receiver).  Written for
;;; Pseudoscheme.
(define-library (srfi 61)
  (export cond)
  (import (except (scheme base) cond) (prefix (only (scheme base) cond) r7:))
  (begin
    (define-syntax cond
      (syntax-rules (else =>)
        ((_ (else e1 e2 ...)) (begin e1 e2 ...))
        ((_ (test guard => receiver) clause ...)
         (call-with-values (lambda () test)
           (lambda args
             (if (apply guard args)
                 (apply receiver args)
                 (cond clause ...)))))
        ((_ (test => receiver) clause ...)
         (let ((t test)) (if t (receiver t) (cond clause ...))))
        ((_ (test) clause ...)
         (or test (cond clause ...)))
        ((_ (test e1 e2 ...) clause ...)
         (if test (begin e1 e2 ...) (cond clause ...)))
        ((_) (if #f #f))))))
