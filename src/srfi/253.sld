;;; SRFI 253: data (type-)checking.  Artyom Bologov's sample
;;; implementation (reference/srfi-253/impl.scm, unmodified; MIT licence
;;; in reference/srfi-253/LICENSE).  Its shipped 253.sld reads impl.scm
;;; with include-library-declarations, which it notes no implementation
;;; but Guile can load; impl.scm is a sequence of cond-expands around
;;; definitions, so here it is an ordinary include in the library body.
;;; Its cond-expands take the generic branches (r7rs, srfi-16), and its
;;; srfi-145 branch, which expects assume from the host: so (srfi 145)
;;; is imported, and failed checks raise its "invalid assumption" error.
;;;
;;; check-arg is a different binding from (srfi private shim)'s.
(define-library (srfi 253)
  (export check-arg values-checked
          check-case
          lambda-checked define-checked
          case-lambda-checked
          define-record-type-checked)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 145))
  (include "reference/srfi-253/impl.scm"))
