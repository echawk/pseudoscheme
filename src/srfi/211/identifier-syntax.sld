;;; SRFI 211: identifier syntax.  Written for Pseudoscheme (the SRFI's implementation is
;;; necessarily per-implementation); see 211.sld.
(define-library (srfi 211 identifier-syntax)
  (export identifier-syntax)
  (import (only (rnrs base) identifier-syntax)))
