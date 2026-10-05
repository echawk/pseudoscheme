;;; SRFI 54: formatting.  Joo ChurlSoo's implementation, unmodified
;;; from the SRFI document (reference/srfi-54/srfi-54.scm; MIT licence,
;;; per the SRFI document, in reference/srfi-54/LICENSE).
(define-library (srfi 54)
  (export cat)
  (import (scheme base) (scheme char) (scheme complex) (scheme write))
  (begin
    (define exact->inexact inexact)
    (define inexact->exact exact))
  (include "reference/srfi-54/srfi-54.scm"))
