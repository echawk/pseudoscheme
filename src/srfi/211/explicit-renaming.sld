;;; SRFI 211: explicit-renaming macros.  Written for Pseudoscheme (the SRFI's implementation is
;;; necessarily per-implementation); see 211.sld.
;;; er-macro-transformer is a macro (it captures the place it's used,
;;; to rename symbols there); as a variable reference it's a procedure.
(define-library (srfi 211 explicit-renaming)
  (export er-macro-transformer (rename preidentifier? identifier?))
  (import (srfi private srfi-211-transformers)))
