;;; SRFI 147's sample (srfi 147 er-macro-transformer) library: an
;;; explicit-renaming transformer that works as a custom macro
;;; transformer under SRFI 147's define-syntax, let-syntax and
;;; letrec-syntax.  See 147.sld and (srfi private
;;; srfi-147-implementation).  It needs SRFI 147's define-syntax: with
;;; (scheme base)'s, use SRFI 211's er-macro-transformer.
(define-library (srfi 147 er-macro-transformer)
  (export er-macro-transformer)
  (import (srfi private srfi-147-implementation)))
