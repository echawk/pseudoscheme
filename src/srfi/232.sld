;;; SRFI 232: flexible curried procedures.  Wolfgang Corcoran-Mathe's
;;; sample implementation (reference/srfi-232/srfi-232.scm; MIT licence
;;; in reference/srfi-232/LICENSE).  The included
;;; reference/srfi-232/srfi-232-body.scm is that file without its
;;; leading (import ...) form, which is this library's import instead.
(define-library (srfi 232)
  (export curried define-curried)
  (import (scheme base)
          (scheme case-lambda))
  (include "reference/srfi-232/srfi-232-body.scm"))
