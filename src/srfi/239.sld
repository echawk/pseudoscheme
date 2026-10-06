;;; SRFI 239: destructuring lists.  Marc Nieper-Wißkirchen's syntax-case
;;; sample implementation: the body of its (srfi :239 list-case),
;;; unmodified (reference/srfi-239/list-case-body.scm, from
;;; list-case.sls; MIT licence in reference/srfi-239/LICENSE).  The
;;; repository's syntax-rules implementation (Robby Zambito's 239.sld)
;;; isn't used: it signals an unmatched expression with error, where
;;; the SRFI's tests expect an assertion violation.  (srfi 239
;;; list-case), the R6RS (srfi :239 list-case), is 239/list-case.sld.
;;; The exported _ is (scheme base)'s.
(define-library (srfi 239)
  (export list-case _)
  (import (rnrs))
  (include "reference/srfi-239/list-case-body.scm"))
