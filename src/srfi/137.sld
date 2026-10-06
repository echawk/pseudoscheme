;;; SRFI 137: minimal unique types.  The implementation in the SRFI
;;; repository (cowan/types.scm, by John Cowan and Marc
;;; Nieper-Wißkirchen), unmodified (reference/srfi-137/types.scm; MIT
;;; licence, per the SRFI document, in reference/srfi-137/LICENSE).  The
;;; library form follows the shipped cowan/types.sld.
(define-library (srfi 137)
  (export make-type)
  (import (scheme base))
  (include "reference/srfi-137/types.scm"))
