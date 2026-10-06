;;; (srfi 99 records inspection): SRFI 99's inspection layer.  The body
;;; of William D Clinger's reference implementation of this library,
;;; unmodified (reference/srfi-99/inspection.scm, split out of
;;; reference/srfi-99/srfi-99.sls; licence in reference/srfi-99/LICENSE).
;;; The reference gets rtd? from a helper library, (srfi :99-helpers
;;; records rtd?); it is defined here instead.
(define-library (srfi 99 records inspection)
  (export record? record-rtd
          rtd-name rtd-parent
          rtd-field-names rtd-all-field-names rtd-field-mutable?)
  (import (rnrs base)
          (rnrs lists)
          (rnrs records inspection)
          (only (rnrs records procedural) record-type-descriptor?))
  (begin
    (define rtd? record-type-descriptor?))
  (include "../../reference/srfi-99/inspection.scm"))
