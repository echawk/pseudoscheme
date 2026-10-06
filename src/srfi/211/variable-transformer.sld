;;; SRFI 211: variable transformers.  Written for Pseudoscheme (the SRFI's implementation is
;;; necessarily per-implementation); see 211.sld.
;;; R6RS's make-variable-transformer; explicit-renaming and the other
;;; transformers here are transformation procedures, so it takes them too.
(define-library (srfi 211 variable-transformer)
  (export make-variable-transformer)
  (import (only (rnrs syntax-case) make-variable-transformer)))
