;;; SRFI 161: unifiable boxes.  Marc Nieper-Wißkirchen's sample
;;; implementation, unmodified (reference/srfi-161/161.scm; MIT licence
;;; in reference/srfi-161/LICENSE).  The library form is the shipped
;;; srfi/161.sld with its include path changed.
(define-library (srfi 161)
  (export ubox ubox? ubox-ref ubox-set! ubox=?
          ubox-unify! ubox-union! ubox-link!)
  (import (scheme base))
  (include "reference/srfi-161/161.scm"))
