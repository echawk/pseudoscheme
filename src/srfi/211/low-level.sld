;;; SRFI 211: the low-level macro facility of the R4RS.  Written for Pseudoscheme (the SRFI's implementation is
;;; necessarily per-implementation); see 211.sld.
(define-library (srfi 211 low-level)
  (export syntax unwrap-syntax identifier? free-identifier=? bound-identifier=?
          identifier->symbol generate-identifier construct-identifier)
  (import (only (rnrs syntax-case) syntax identifier? free-identifier=?
                bound-identifier=?)
          (srfi private srfi-211-transformers)))
