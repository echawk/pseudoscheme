;;; SRFI 95: sorting and merging.  The SRFI's implementation, SLIB's
;;; sort.scm (Richard A. O'Keefe, Aubrey Jaffer; public domain, the
;;; SRFI document under the MIT licence in reference/srfi-95/LICENSE),
;;; in reference/srfi-95/sort.scm.  It is written on SRFI 63 arrays; the
;;; rank-1 arrays here are vectors and strings, and the few array
;;; procedures it needs are defined below on them.
;;;
;;; One change to the reference file, marked PSEUDOSCHEME there:
;;; sorted? took every two-element vector or string as sorted.
(define-library (srfi 95)
  (export sorted? merge merge! sort sort!)
  (import (scheme base) (scheme cxr))
  (begin
    (define (require feature) #t)
    (define (identity x) x)
    (define (array? x) (or (vector? x) (string? x)))
    (define (array-dimensions a)
      (list (if (vector? a) (vector-length a) (string-length a))))
    (define (array-ref a i)
      (if (vector? a) (vector-ref a i) (string-ref a i)))
    (define (array-set! a x i)
      (if (vector? a) (vector-set! a i x) (string-set! a i x)))
    (define (make-array prototype k)
      (if (vector? prototype) (make-vector k) (make-string k))))
  (include "reference/srfi-95/sort.scm"))
