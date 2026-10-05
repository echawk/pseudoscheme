;;; SRFI 48: intermediate format strings.  Kenneth A Dickey's reference
;;; implementation as revised by Hamayama (test/srfi-48.scm in the SRFI
;;; repository), unmodified (reference/srfi-48/srfi-48.scm; MIT licence
;;; in reference/srfi-48/LICENSE).  ~W (SRFI 38's
;;; write-with-shared-structure) is R7RS's write-shared; ~Y pretty-prints
;;; with plain write.
(define-library (srfi 48)
  (export format)
  (import (scheme base) (scheme char) (scheme complex) (scheme cxr) (scheme write))
  (begin
    (define exact->inexact inexact)
    (define inexact->exact exact)
    (define write-with-shared-structure write-shared))
  (include "reference/srfi-48/srfi-48.scm"))
