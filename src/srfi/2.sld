;;; SRFI 2: and-let*.  Written for Pseudoscheme.
(define-library (srfi 2)
  (export and-let*)
  (import (scheme base))
  (begin
    (define-syntax and-let*
      (syntax-rules ()
        ((_ ()) #t)
        ((_ () body1 body2 ...) (let () body1 body2 ...))
        ((_ ((var expr) . rest) . body)
         (let ((var expr))
           (if var (and-let* rest . body) #f)))
        ((_ ((expr) . rest) . body)
         (if expr (and-let* rest . body) #f))
        ((_ (var . rest) . body)          ; a bare identifier: a boolean test
         (if var (and-let* rest . body) #f))))))
