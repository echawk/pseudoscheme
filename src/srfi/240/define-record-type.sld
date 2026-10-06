;;; (srfi 240 define-record-type), also (srfi :240 define-record-type):
;;; SRFI 240's library; see 240.sld.
(define-library (srfi 240 define-record-type)
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
  (import (srfi 240)))
