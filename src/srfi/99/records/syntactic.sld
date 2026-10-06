;;; (srfi 99 records syntactic): SRFI 99's syntactic layer.  The body of
;;; William D Clinger's reference implementation of this library
;;; (reference/srfi-99/syntactic.scm, split out of
;;; reference/srfi-99/srfi-99.sls; licence in reference/srfi-99/LICENSE),
;;; with two changes marked PSEUDOSCHEME there: the reference code
;;; assumes that the syntax object for a list is a list, and that the
;;; one for a parent clause #f is #f; Pseudoscheme's psyntax wraps both,
;;; as R6RS allows.
;;;
;;; As in the reference implementation, the record type is made from
;;; the field names alone, so every field is mutable at the procedural
;;; level (rtd-field-mutable? is #t), even one the definition declares
;;; immutable; only the mutator is left undefined.
;;;
;;; define-record-type is a different binding from (scheme base)'s, so
;;; import (except (scheme base) define-record-type) with this library.
(define-library (srfi 99 records syntactic)
  (export define-record-type)
  (import (rnrs base)
          (rnrs lists)
          (rnrs syntax-case)
          (srfi 99 records procedural))
  (include "../../reference/srfi-99/syntactic.scm"))
