;;; SRFI 162: comparators sublibrary.  John Cowan's sample
;;; implementation, unmodified (reference/srfi-162/162-impl.scm; MIT
;;; licence, in reference/srfi-162/LICENSE), on top of (srfi 128).
;;;
;;; SRFI 162 asks implementers to add its procedures and comparators to
;;; their SRFI 128 library rather than package them separately.  This
;;; library (which leaves (srfi 128) as it is) re-exports all of
;;; (srfi 128) together with SRFI 162's additions, so (srfi 162) can be
;;; used in place of (srfi 128).  The sample's pair-comparator is
;;; exported too, as the SRFI document specifies it.
(define-library (srfi 162)
  (export
   ;; SRFI 128
   comparator? comparator-ordered? comparator-hashable?
   make-comparator
   make-pair-comparator make-list-comparator make-vector-comparator
   make-eq-comparator make-eqv-comparator make-equal-comparator
   boolean-hash char-hash char-ci-hash string-hash string-ci-hash
   symbol-hash number-hash
   make-default-comparator default-hash comparator-register-default!
   comparator-type-test-predicate comparator-equality-predicate
   comparator-ordering-predicate comparator-hash-function
   comparator-test-type comparator-check-type comparator-hash
   hash-bound hash-salt
   =? <? >? <=? >=?
   comparator-if<=>
   ;; SRFI 162
   comparator-max comparator-min
   comparator-max-in-list comparator-min-in-list
   default-comparator boolean-comparator real-comparator
   char-comparator char-ci-comparator
   string-comparator string-ci-comparator
   pair-comparator list-comparator vector-comparator
   eq-comparator eqv-comparator equal-comparator)
  (import (scheme base)
          (scheme char)
          (srfi 128))
  (include "reference/srfi-162/162-impl.scm"))
