;;; SRFI 71: extended let-syntax for multiple values.  Sebastian
;;; Egner's reference implementation (generic part), unmodified
;;; (reference/srfi-71/letvalues.scm; MIT licence, per the SRFI
;;; document, in reference/srfi-71/LICENSE).  As its header asks, the
;;; R7RS let, let* and letrec are saved as r5rs-let etc., and its
;;; srfi-let etc. are exported as let, let* and letrec; so import
;;; (except (scheme base) let let* letrec) with this library.
(define-library (srfi 71)
  (export (rename srfi-let let) (rename srfi-let* let*)
          (rename srfi-letrec letrec)
          uncons uncons-2 uncons-3 uncons-4 uncons-cons unlist unvector
          values->list values->vector)
  (import (scheme base)
          (rename (only (scheme base) let let* letrec)
                  (let r5rs-let) (let* r5rs-let*) (letrec r5rs-letrec))
          (scheme cxr))
  (include "reference/srfi-71/letvalues.scm"))
