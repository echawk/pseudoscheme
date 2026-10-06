;;; SRFI 224: integer mappings.  Wolfgang Corcoran-Mathe's sample
;;; implementation (big-endian Patricia tries), on SRFIs 1, 128, 143,
;;; 145 and 158, unmodified (reference/srfi-224/matchers.scm, trie.scm
;;; and 224.scm; MIT licence in reference/srfi-224/LICENSE).  The library
;;; form is the shipped srfi/224.sld with its include paths changed.

;;; SPDX-FileCopyrightText: 2021 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 224)
  (import (scheme base)
          (scheme case-lambda)
          (only (srfi 1) fold every)
          (only (srfi 128) comparator? =?)
          (srfi 143))

  (cond-expand
    ((library (srfi 145))
     (import (srfi 145)))
    (else
     (begin
      (define-syntax assume
        (syntax-rules ()
          ((_ expr . _)
           (or expr (car 0))))))))

  (cond-expand
    ((library (srfi 158))
     (import (only (srfi 158) make-coroutine-generator)))
    (else
     (begin
      ;; From the SRFI 158 sample impl. by Kawai, Cowan, & Gilray.
      ;; (MIT license)
      (define (make-coroutine-generator proc)
        (define return #f)
        (define resume #f)
        (define (yield v)
          (call/cc (lambda (r) (set! resume r) (return v))))
        (lambda ()
          (call/cc
           (lambda (cc)
             (set! return cc)
             (if resume
                 (resume (if #f #f))  ; void? or yield again?
                 (begin (proc yield)
                        (set! resume (lambda (v) (return (eof-object))))
                        (return (eof-object)))))))))))

  (export
   ;; Constructors
   fxmapping fxmapping-unfold fxmapping-accumulate alist->fxmapping
   alist->fxmapping/combinator

   ;; Predicates
   fxmapping? fxmapping-contains? fxmapping-empty? fxmapping-disjoint?

   ;; Accessors
   fxmapping-min fxmapping-max fxmapping-ref fxmapping-ref/default

   ;; Updaters
   fxmapping-adjoin fxmapping-adjoin/combinator fxmapping-adjust
   fxmapping-set fxmapping-delete fxmapping-delete-all fxmapping-alter
   fxmapping-update fxmapping-delete-min fxmapping-delete-max
   fxmapping-update-min fxmapping-update-max fxmapping-pop-min
   fxmapping-pop-max

   ;; The whole fxmapping
   fxmapping-size fxmapping-count fxmapping-any? fxmapping-find
   fxmapping-every?

   ;; Traversal
   fxmapping-fold fxmapping-fold-right fxmapping-map fxmapping-map->list
   fxmapping-for-each
   fxmapping-relation-map

   ;; Filter
   fxmapping-filter fxmapping-remove fxmapping-partition

   ;; Copying and conversion
   fxmapping-keys fxmapping-values fxmapping->alist
   fxmapping->decreasing-alist fxmapping->generator
   fxmapping->decreasing-generator

   ;; Comparison
   fxmapping=? fxmapping<? fxmapping>? fxmapping<=? fxmapping>=?

   ;; Set theory operations
   fxmapping-union fxmapping-intersection fxmapping-difference fxmapping-xor
   fxmapping-union/combinator fxmapping-intersection/combinator

   ;; Submappings
   fxmapping-open-interval fxmapping-closed-interval
   fxmapping-open-closed-interval fxmapping-closed-open-interval
   fxsubmapping= fxsubmapping< fxsubmapping<= fxsubmapping>= fxsubmapping>
   fxmapping-split
   )

  (include "reference/srfi-224/matchers.scm")
  (include "reference/srfi-224/trie.scm")
  (include "reference/srfi-224/224.scm"))
