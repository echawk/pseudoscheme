;;; SRFI 211: presyntax objects.  Written for Pseudoscheme (the SRFI's implementation is
;;; necessarily per-implementation); see 211.sld.
(define-library (srfi 211 presyntax)
  (export preidentifier? presyntax->datum preidentifier->symbol unwrap-presyntax)
  (import (srfi private srfi-211-transformers)))
