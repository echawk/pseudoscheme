;;; SRFI 211: syntactic-closures macro transformers.  Written for Pseudoscheme (the SRFI's implementation is
;;; necessarily per-implementation); see 211.sld.
;;; SRFI 211 names these without semantics; they follow MIT Scheme's,
;;; on syntax-case (see (srfi private srfi-211-transformers)).  The
;;; input form's leaves are identifiers, not symbols, and an identifier
;;; from the input keeps its own context when closed.
(define-library (srfi 211 syntactic-closures)
  (export sc-macro-transformer rsc-macro-transformer make-syntactic-closure
          close-syntax capture-syntactic-environment
          (rename preidentifier? identifier?)
          (rename sc-identifier=? identifier=?)
          make-synthetic-identifier)
  (import (srfi private srfi-211-transformers)))
