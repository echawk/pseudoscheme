;;; SRFI 185: linear adjustable-length strings.  The portable
;;; implementation in the SRFI document (Per Bothner, John Cowan),
;;; verbatim (reference/srfi-185/185.scm; MIT licence in
;;; reference/srfi-185/LICENSE), with string-replace from SRFI 13.
;;; Pseudoscheme's strings are fixed-length, so the procedures always
;;; return a new string, and the macros set! their place to it.
(define-library (srfi 185)
  (export string-append-linear! string-replace-linear!
          string-append! string-replace!)
  (import (scheme base)
          (scheme case-lambda)
          (only (srfi 13) string-replace))
  (include "reference/srfi-185/185.scm"))
