;;; SRFI 42: eager comprehensions.  Sebastian Egner's reference
;;; implementation, unmodified (reference/srfi-42/ec.scm; the SRFI
;;; document's MIT licence, Copyright (C) Sebastian Egner (2003), covers
;;; it).  ec.scm uses nested and index as syntax-rules literals without
;;; binding them; here they are bound (as syntax that is an error outside
;;; a comprehension) and exported, so they match when used from another
;;; library, and (srfi 78) can import nested.
(define-library (srfi 42)
  (export do-ec list-ec append-ec string-ec string-append-ec vector-ec
          vector-of-length-ec sum-ec product-ec min-ec max-ec any?-ec
          every?-ec first-ec last-ec fold-ec fold3-ec
          : :list :string :vector :integers :range :real-range :char-range
          :port :dispatched :do :let :parallel :while :until
          :-dispatch-ref :-dispatch-set! make-initial-:-dispatch
          dispatch-union :generator-proc
          nested index)
  (import (scheme base) (scheme cxr) (scheme read)
          (only (scheme r5rs) exact->inexact))
  (begin
    (define-syntax nested
      (syntax-rules ()
        ((_ . args) (syntax-error "nested used outside an eager comprehension"))))
    (define-syntax index
      (syntax-rules ()
        ((_ . args) (syntax-error "index used outside a generator")))))
  (include "reference/srfi-42/ec.scm"))
