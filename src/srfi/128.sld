;;; SRFI 128: comparators (reduced).  John Cowan's reference
;;; implementation, unmodified (reference/srfi-128/128.body1.scm and
;;; 128.body2.scm, MIT licence in reference/srfi-128/LICENSE).  This
;;; library form follows the shipped srfi/128.sld; equal-hash comes
;;; from R6RS (rnrs hashtables).  string-hash and string-ci-hash are
;;; exported as wrappers that accept (and ignore) a second argument, as
;;; SRFI 128 permits, so that (srfi 125) can export the very same
;;; bindings and the two libraries can be imported together.

;;; SPDX-FileCopyrightText: 2015 John Cowan <cowan@ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 128)
  (export comparator? comparator-ordered? comparator-hashable?
          make-comparator
          make-pair-comparator make-list-comparator make-vector-comparator
          make-eq-comparator make-eqv-comparator make-equal-comparator
          boolean-hash char-hash char-ci-hash
          (rename string-hash/bound string-hash)
          (rename string-ci-hash/bound string-ci-hash)
          symbol-hash number-hash
          make-default-comparator default-hash comparator-register-default!
          comparator-type-test-predicate comparator-equality-predicate
          comparator-ordering-predicate comparator-hash-function
          comparator-test-type comparator-check-type comparator-hash
          hash-bound hash-salt
          =? <? >? <=? >=?
          comparator-if<=>)
  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme inexact)
          (scheme complex))
  (cond-expand
   ((library (rnrs hashtables))
    (import (only (rnrs hashtables) equal-hash)))
   (else
    (begin (define (equal-hash x) 0))))
  (include "reference/srfi-128/128.body1.scm")
  (include "reference/srfi-128/128.body2.scm")
  (begin
    (define (string-hash/bound s . bound) (string-hash s))
    (define (string-ci-hash/bound s . bound) (string-ci-hash s))))
