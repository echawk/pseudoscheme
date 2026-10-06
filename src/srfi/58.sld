;;; SRFI 58: Array notation.  The reader reads #2A((1 2) (3 4)) and
;;; #1A:floR64b(...) (src/read.scm), as SRFI 163's syntax, which extends
;;; it (src/srfi/163.sld).  The arrays are SRFI 164's, not SRFI 47's or
;;; 63's; a SRFI 58 type is checked but not kept.  This library exports
;;; nothing.
(define-library (srfi 58)
  (export)
  (import (scheme base)))
