;;; (srfi 99 records procedural): SRFI 99's procedural layer.  The body
;;; of William D Clinger's reference implementation of this library,
;;; unmodified (reference/srfi-99/procedural.scm, split out of
;;; reference/srfi-99/srfi-99.sls; licence in reference/srfi-99/LICENSE).
;;; Record types are R6RS record types.
(define-library (srfi 99 records procedural)
  (export make-rtd rtd? rtd-constructor
          rtd-predicate rtd-accessor rtd-mutator)
  (import (rnrs base)
          (rnrs lists)
          (rnrs records procedural)
          (srfi 99 records inspection))
  (include "../../reference/srfi-99/procedural.scm"))
