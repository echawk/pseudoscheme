;;; SRFI 38: external representation for data with shared structure.
;;; write-with-shared-structure is Ray Dillinger's reference
;;; implementation (reference/srfi-38/srfi-38.scm, unmodified; MIT
;;; licence in reference/srfi-38/LICENSE).  It labels every shared
;;; pair, vector and non-empty string, as the SRFI requires; R7RS's
;;; write-shared labels pairs and vectors only, so it isn't used.
;;;
;;; read-with-shared-structure is R7RS's read, which reads datum labels
;;; (#N= and #N#) for any datum, and reads all of R7RS's syntax, which
;;; the reference implementation's own reader (also in srfi-38.scm, and
;;; renamed away below) does not: bytevectors, block comments,
;;; #true/#false, |symbols| and so on.  (The included file's own
;;; read-with-shared-structure is defined but not exported.)
;;;
;;; Limitations: the reference writer tracks objects in association
;;; lists, so its time is quadratic in the number of pairs, vectors and
;;; strings in the datum.  Its optional third argument (optarg) is
;;; accepted and ignored, as the SRFI allows.
(define-library (srfi 38)
  (export write-with-shared-structure
          (rename write-with-shared-structure write/ss)
          (rename read read-with-shared-structure)
          (rename read read/ss))
  (import (scheme base)
          (scheme write)
          (scheme read)
          (scheme cxr)        ; the reference reader uses caadr
          (scheme char))      ; and char-downcase
  (include "reference/srfi-38/srfi-38.scm"))
