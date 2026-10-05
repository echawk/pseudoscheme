;;; SRFI 197: pipeline operators.  Adam R. Nelson's sample
;;; implementation, unmodified (reference/srfi-197/srfi-197.scm; MIT
;;; licence, per the SRFI document, in reference/srfi-197/LICENSE).  The
;;; library form follows the shipped srfi-197.sld, plus (srfi 2), which
;;; chain-and needs.
;;;
;;; The file's inner helper macros take the placeholder _ as a literal
;;; and also start their own patterns with _.  R7RS ignores the first
;;; element of a syntax-rules pattern, but psyntax's syntax-rules
;;; matches it, as a literal when it is one.  So the syntax-rules this
;;; file sees is a wrapper that renames each pattern's first element to
;;; a fresh identifier first.
;;;
;;; Limitation: the _ ... placeholder (a step's trailing multiple
;;; values) does not work, because psyntax's custom-ellipsis
;;; syntax-rules cannot take ... itself as a literal.
(define-library (srfi 197)
  (export chain chain-and chain-when chain-lambda nest nest-reverse)
  (import (except (scheme base) syntax-rules)
          (rename (only (scheme base) syntax-rules) (syntax-rules %syntax-rules))
          (only (rnrs syntax-case) syntax-case syntax with-syntax identifier?)
          (srfi 2))
  (begin
    (define-syntax syntax-rules
      (lambda (x)
        (define (rename-head clause)
          (syntax-case clause ()
            (((head . pattern) template) #'((ignored . pattern) template))
            (other #'other)))
        (syntax-case x ()
          ((k ellipsis (literal ...) clause ...)
           (identifier? #'ellipsis)
           (with-syntax (((c ...) (map rename-head #'(clause ...))))
             #'(%syntax-rules ellipsis (literal ...) c ...)))
          ((k (literal ...) clause ...)
           (with-syntax (((c ...) (map rename-head #'(clause ...))))
             #'(%syntax-rules (literal ...) c ...)))))))
  (include "reference/srfi-197/srfi-197.scm"))
