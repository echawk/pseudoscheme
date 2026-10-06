;;; SRFI 211: implicitly renaming macros.  Written for Pseudoscheme (the SRFI's implementation is
;;; necessarily per-implementation); see 211.sld.
(define-library (srfi 211 implicit-renaming)
  (export ir-macro-transformer)
  (import (srfi private srfi-211-transformers)))
