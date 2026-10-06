;;; SRFI 136: extensible record types.  Marc Nieper-Wißkirchen's sample
;;; implementation, unmodified (reference/srfi-136/136.scm; MIT licence
;;; in reference/srfi-136/LICENSE), on (srfi 137).  The library form is
;;; the shipped srfi/136.sld.  Record types here are SRFI 137 types,
;;; separate from R6RS and R7RS record types.
;;;
;;; define-record-type is a different binding from (scheme base)'s, so
;;; import (except (scheme base) define-record-type) with this library.
(define-library (srfi 136)
  (import (rename (scheme base)
                  (define-record-type scheme-define-record-type))
          (srfi 137))
  (export define-record-type
          record-type-descriptor?
          record?
          record-type-descriptor
          record-type-predicate
          record-type-name
          record-type-parent
          record-type-fields
          make-record-type-descriptor
          make-record)
  (include "reference/srfi-136/136.scm"))
