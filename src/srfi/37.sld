;;; SRFI 37: args-fold, a program argument processor.  Anthony
;;; Carrico's reference implementation, unmodified
;;; (reference/srfi-37/srfi-37-reference.scm, 3-clause BSD licence in
;;; its header).
(define-library (srfi 37)
  (export option option-names option-required-arg? option-optional-arg?
          option-processor option? args-fold)
  (import (scheme base))
  (include "reference/srfi-37/srfi-37-reference.scm"))
