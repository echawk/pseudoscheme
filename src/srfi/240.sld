;;; SRFI 240: reconciled records.  Marc Nieper-Wißkirchen's sample
;;; implementation, on SRFI 237 (reference/srfi-240/; MIT licence in
;;; reference/srfi-240/LICENSE): the body of its (srfi :240
;;; define-record-type), unmodified, in
;;; reference/srfi-240/define-record-type-body.scm.  An R7RS-style
;;; definition becomes a SRFI 237 one; anything else is SRFI 237's.
;;; The R6RS names (srfi :240) and (srfi :240 define-record-type) are
;;; this library and 240/define-record-type.sld.
;;;
;;; The SRFI suggests providing this define-record-type from
;;; (scheme base); Pseudoscheme doesn't, so it is a different binding
;;; from (scheme base)'s: import (except (scheme base)
;;; define-record-type) with this library.
(define-library (srfi 240)
  (export define-record-type
          fields
          mutable
          immutable
          parent
          protocol
          sealed
          opaque
          nongenerative
          generative
          parent-rtd)
  (import (rnrs base)
          (rnrs syntax-case)
          (rnrs lists)
          (rnrs control)
          (prefix (rnrs) rnrs:)
          (rename (srfi 237)
                  (define-record-type :237:define-record-type)))
  (include "reference/srfi-240/define-record-type-body.scm"))
