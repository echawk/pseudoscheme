;;; SRFI 214: flexvectors.  Adam Nelson's sample implementation, on
;;; SRFIs 1 and 145, unmodified (reference/srfi-214/flexvectors-body1.scm
;;; and flexvectors-body2.scm; MIT licence, per the SRFI document, in
;;; reference/srfi-214/LICENSE).  The library form is the shipped
;;; flexvectors.sld, renamed (srfi 214), with its include paths changed.

(define-library (srfi 214)
  (export ; Constructors
          make-flexvector flexvector
          flexvector-unfold flexvector-unfold-right
          flexvector-copy flexvector-reverse-copy
          flexvector-append flexvector-concatenate flexvector-append-subvectors

          ; Predicates
          flexvector? flexvector-empty? flexvector=?

          ; Selectors
          flexvector-ref flexvector-front flexvector-back flexvector-length

          ; Mutators
          flexvector-add! flexvector-add-front! flexvector-add-back!
          flexvector-remove! flexvector-remove-front! flexvector-remove-back!
          flexvector-add-all! flexvector-remove-range! flexvector-clear!
          flexvector-set! flexvector-swap!
          flexvector-fill! flexvector-reverse!
          flexvector-copy! flexvector-reverse-copy!
          flexvector-append!

          ; Iteration
          flexvector-fold flexvector-fold-right
          flexvector-map flexvector-map! flexvector-map/index flexvector-map/index!
          flexvector-append-map flexvector-append-map/index
          flexvector-filter flexvector-filter! flexvector-filter/index flexvector-filter/index!
          flexvector-for-each flexvector-for-each/index
          flexvector-count flexvector-cumulate

          ; Searching
          flexvector-index flexvector-index-right
          flexvector-skip flexvector-skip-right
          flexvector-binary-search
          flexvector-any flexvector-every flexvector-partition

          ; Conversion
          flexvector->vector flexvector->list flexvector->string
          vector->flexvector list->flexvector string->flexvector
          reverse-flexvector->list reverse-list->flexvector
          generator->flexvector flexvector->generator)

  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (srfi 1)
          (srfi 145))

  (include "reference/srfi-214/flexvectors-body1.scm")
  (include "reference/srfi-214/flexvectors-body2.scm"))
