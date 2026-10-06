;;; SRFI 251: Mixing groups of definitions with expressions within
;;; bodies.  Built into psyntax's bodies (vendor/psyntax, chi-internal): a
;;; group of definitions after an expression begins a nested body, in the
;;; scope of the earlier ones.  This library exports nothing.
(define-library (srfi 251)
  (export)
  (import (scheme base)))
