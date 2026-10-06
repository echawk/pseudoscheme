;;; SRFI 149: Basic syntax-rules template extensions.  Built into
;;; psyntax's syntax templates (vendor/psyntax, gen-syntax): a
;;; subtemplate may be followed by several ellipses, and a pattern
;;; variable by more ellipses than in its pattern, the innermost excess
;;; ones repeating it.  As the SRFI asks, the library exports
;;; syntax-rules, the binding of (scheme base).
(define-library (srfi 149)
  (export syntax-rules)
  (import (only (scheme base) syntax-rules)))
