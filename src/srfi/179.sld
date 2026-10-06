;;; SRFI 179: nonempty intervals and generalized arrays (updated).  Not
;;; the SRFI's sample implementation, which is written for Gambit, but
;;; Alex Shinn's portable R7RS one from chibi-scheme (lib/srfi/179.sld,
;;; 179/base.sld, 179/base.scm and 179/transforms.scm; 3-clause BSD
;;; licence in reference/srfi-179/LICENSE), as for SRFI 231 (231.sld).
;;; The .scm files are in reference/srfi-179/, unmodified but for the
;;; two changes below; (srfi 179 base), chibi's internal library, is
;;; in 179/base.sld.
;;;
;;; Changes, marked PSEUDOSCHEME: array-tile's argument check in
;;; transforms.scm required each tile size to be at most the domain's
;;; upper bound, which rejects valid sizes when a lower bound is
;;; negative (the SRFI's own tests make such calls); it now requires
;;; them to be positive, as the SRFI says.  And chibi ignores safety
;;; (base.scm says so): an index into a dimension of width 1 was never
;;; checked, for its coefficient is 0; here %make-specialized in
;;; base.scm makes a safe array's getter and setter check that their
;;; indices are in the domain.  (chibi assert) is replaced by R6RS's
;;; assert, and the u1vectors behind u1-storage-class, a chibi extension
;;; to SRFI 160, come from SRFI 231's (srfi 231 private u1vector).  And
;;; specialized-array-default-safe? and specialized-array-default-mutable?,
;;; parameters in chibi, can be called with an argument to set them, as
;;; the SRFI specifies, with settable-parameter from SRFI 215's helper
;;; library (srfi private srfi-215-parameter).  As in chibi,
;;; f8-storage-class and f16-storage-class are #f.
;;;
;;; The names clash with SRFI 231's (its successor) and, for make-array,
;;; array?, array-ref and array-set!, with SRFIs 25, 47, 63 and 164.

(define-library (srfi 179)
  (import (scheme base)
          (scheme list)
          (scheme vector)
          (scheme sort)
          (srfi 160 base)
          (srfi 231 private u1vector)   ; PSEUDOSCHEME: added
          (except (srfi 179 base)       ; PSEUDOSCHEME: was (srfi 179 base)
                  specialized-array-default-safe?
                  specialized-array-default-mutable?)
          (prefix (only (srfi 179 base) ; PSEUDOSCHEME: added
                        specialized-array-default-safe?
                        specialized-array-default-mutable?)
                  %)
          (srfi private srfi-215-parameter)  ; PSEUDOSCHEME: added
          (only (rnrs base) assert))  ; PSEUDOSCHEME: was (chibi assert)
  (export
   ;; Miscellaneous Functions
   translation? permutation?
   ;; Intervals
   make-interval interval? interval-dimension interval-lower-bound
   interval-upper-bound interval-lower-bounds->list
   interval-upper-bounds->list interval-lower-bounds->vector
   interval-upper-bounds->vector interval= interval-volume
   interval-subset? interval-contains-multi-index? interval-projections
   interval-for-each interval-dilate interval-intersect
   interval-translate interval-permute interval-rotate
   interval-scale interval-cartesian-product
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
   ;; Arrays
   make-array array? array-domain array-getter array-dimension
   mutable-array? array-setter specialized-array-default-safe?
   specialized-array-default-mutable? make-specialized-array
   specialized-array? array-storage-class array-indexer array-body
   array-safe? array-elements-in-order? specialized-array-share
   array-copy array-curry array-extract array-tile array-translate
   array-permute array-rotate array-reverse array-sample
   array-outer-product array-map array-for-each array-fold
   array-fold-right array-reduce array-any array-every
   array->list list->array array-assign! array-ref array-set!
   specialized-array-reshape
   )
  (begin
    ;; PSEUDOSCHEME: the SRFI makes these procedures that set the
    ;; default when given an argument; chibi's are parameters, which
    ;; here can't be called with one.
    (define specialized-array-default-safe?
      (settable-parameter %specialized-array-default-safe?))
    (define specialized-array-default-mutable?
      (settable-parameter %specialized-array-default-mutable?)))
  (include "reference/srfi-179/transforms.scm"))
