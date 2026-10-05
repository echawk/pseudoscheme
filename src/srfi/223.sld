;;; SRFI 223: generalized binary search procedures.  Daphne
;;; Preston-Kendal's sample implementation, unmodified
;;; (reference/srfi-223/srfi-223.scm; MIT licence, per the SRFI
;;; document, in reference/srfi-223/LICENSE).  The library form follows
;;; the shipped srfi-223.sld.
(define-library (srfi 223)
  (export bisect-left bisect-right bisection
          vector-bisect-left vector-bisect-right)
  (import (scheme base)
          (scheme case-lambda))
  (include "reference/srfi-223/srfi-223.scm"))
