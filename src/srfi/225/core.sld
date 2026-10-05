;;; Part of SRFI 225 (see ../225.sld): the sample implementation's
;;; srfi/225/core.sld, with include paths pointing into ../reference/srfi-225/.

(define-library
  (srfi 225 core)

  (import (scheme base)
          (scheme case-lambda)
          (srfi 1)
          (srfi 128)
          (srfi 225 indexes))
  (cond-expand
    ((library (srfi 145)) (import (srfi 145)))
    (else (include "../reference/srfi-225/assumptions.scm")))

  (include "../reference/srfi-225/core-impl.scm")
  (include-library-declarations "../reference/srfi-225/core-exports.scm")
  (export make-dto-private
          make-modified-dto
          procvec
          dict-procedures-count))
