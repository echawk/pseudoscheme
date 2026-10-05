;;; SRFI 231: intervals and generalized arrays.  Not the SRFI's sample
;;; implementation, which is written for Gambit (define-type, declare,
;;; define-macro, #!optional and ## primitives throughout), but Alex
;;; Shinn's portable R7RS one from chibi-scheme (lib/srfi/231.sld,
;;; 231/base.sld, 231/base.scm and 231/transforms.scm; 3-clause BSD
;;; licence in reference/srfi-231/LICENSE).  The .scm files are in
;;; reference/srfi-231/, unmodified; (srfi 231 base) is in 231/base.sld.
;;;
;;; Changes, marked PSEUDOSCHEME: (chibi assert) is replaced by R6RS's
;;; assert, and the u1vectors behind u1-storage-class, a chibi extension
;;; to SRFI 160, come from (srfi 231 private u1vector) (231/private/).
;;; As in chibi on other hosts, f8- and f16-storage-class are
;;; f32-storage-class.
;;;
;;; Limitation: chibi makes a frozen array's backing store immutable;
;;; here array-freeze! (and so an immutable array-copy) only marks the
;;; array itself, so arrays that already shared its body (array-curry,
;;; array-extract, ...) can still set elements through it.

(define-library (srfi 231)
  (import (scheme base)
          (scheme list)
          (scheme vector)
          (scheme sort)
          (srfi 160 base)
          (srfi 231 private u1vector)  ; PSEUDOSCHEME: added
          (srfi 231 base)
          (only (rnrs base) assert))  ; PSEUDOSCHEME: was (chibi assert)
  (export
   ;; Miscellaneous Functions
   translation? permutation?
   ;; Indexes
   index-rotate index-first index-last index-swap
   ;; Intervals
   make-interval interval? interval-dimension interval-lower-bound
   interval-upper-bound interval-lower-bounds->list
   interval-upper-bounds->list interval-lower-bounds->vector
   interval-upper-bounds->vector interval= interval-volume
   interval-subset? interval-contains-multi-index? interval-projections
   interval-fold-left interval-fold-right
   interval-for-each interval-dilate interval-intersect
   interval-translate interval-permute
   interval-scale interval-cartesian-product
   interval-width interval-widths
   interval-empty?
   ;; Storage Classes
   make-storage-class storage-class? storage-class-getter
   storage-class-setter storage-class-checker storage-class-maker
   storage-class-copier storage-class-length storage-class-default
   generic-storage-class s8-storage-class s16-storage-class
   s32-storage-class s64-storage-class u1-storage-class
   u8-storage-class u16-storage-class u32-storage-class
   u64-storage-class f8-storage-class f16-storage-class
   f32-storage-class f64-storage-class
   c64-storage-class c128-storage-class
   char-storage-class
   storage-class-data? storage-class-data->body
   ;; Arrays
   make-array array? array-domain array-getter array-dimension
   array-empty?
   mutable-array? array-setter specialized-array-default-safe?
   specialized-array-default-mutable? make-specialized-array
   make-specialized-array-from-data
   specialized-array? array-storage-class array-indexer array-body
   array-safe? array-packed? specialized-array-share
   array-copy array-curry array-extract array-tile array-translate
   array-permute array-reverse array-sample
   array-outer-product array-map array-for-each array-fold-left
   array-fold-right array-reduce array-any array-every
   array-inner-product array-stack array-append array-block
   array->list list->array array->vector vector->array
   array->list* list*->array array->vector* vector*->array
   array-assign! array-ref array-set! array-decurry
   specialized-array-reshape
   array-copy! array-stack! array-decurry! array-append! array-block!
   array-freeze!
   )
  (include "reference/srfi-231/transforms.scm")
  (cond-expand
   ((and chibi (library (srfi 160 mini)))
    (import (srfi 160 mini))
    (begin
      (define-storage-class f8-storage-class
        f8vector-ref f8vector-set! f8? f8vector? make-f8vector f8vector-length 0)
      (define-storage-class f16-storage-class
        f16vector-ref f16vector-set! f16? f16vector? make-f16vector f16vector-length 0)))
   (else
    (begin
      (define f8-storage-class f32-storage-class)
      (define f16-storage-class f32-storage-class))))
  )
