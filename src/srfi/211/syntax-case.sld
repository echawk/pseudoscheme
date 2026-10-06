;;; SRFI 211: the syntax-case system.  Written for Pseudoscheme (the SRFI's implementation is
;;; necessarily per-implementation); see 211.sld.
;;; R6RS's syntax-case, as psyntax provides it; it already accepts ...
;;; and _ as literals, as SRFI 211 requires.
(define-library (srfi 211 syntax-case)
  (export syntax-case syntax identifier? bound-identifier=? free-identifier=?
          syntax->datum datum->syntax generate-temporaries with-syntax
          quasisyntax unsyntax unsyntax-splicing syntax-violation)
  (import (rnrs syntax-case)))
